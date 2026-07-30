/**
 * blendR Interactive Web Application Engine
 * Blended Survival Curves for Health Economics and HTA
 */

// State Object
const state = {
  minT: 48,
  maxT: 150,
  alpha: 3.0,
  beta: 3.0,
  tMax: 180,
  obsModel: 'exponential',
  extModel: 'gompertz',
  obsParam: 0.018, // rate parameter for exponential
  extParam1: 0.005, // rate/shape
  extParam2: 0.02, // scale/rate
  preset: 'ta174',
  activeTab: 'plot',
  webrStatus: 'connecting', // connecting, ready, error
  hoverIndex: null
};

// --- Mathematical Helper Functions ---

// Log Gamma Function Approximation (Lanczos approximation)
function logGamma(z) {
  const c = [
    57.1562356658629235, -59.5979603554754912,
    14.16258073985187, -1.240840219727374,
    0.0067710697740084, -0.0003392323313192,
    0.000027377685523, -0.0000004383534448,
    0.000000120486522, -0.0000000067408119
  ];
  let y = z;
  let x = z;
  let tmp = x + 4.65;
  tmp = (x + 0.5) * Math.log(tmp) - tmp;
  let ser = 0.999999999999997091;
  for (let j = 0; j < c.length; j++) {
    y += 1;
    ser += c[j] / y;
  }
  return tmp + Math.log(2.5066282746310005 * ser / x);
}

// Incomplete Beta Function (pbeta) via continued fraction
function incBeta(x, a, b) {
  if (x <= 0) return 0;
  if (x >= 1) return 1;

  // Symmetry transformation for faster convergence
  if (x > (a + 1) / (a + b + 2)) {
    return 1 - incBeta(1 - x, b, a);
  }

  const lBeta = logGamma(a) + logGamma(b) - logGamma(a + b);
  const front = Math.exp(Math.log(x) * a + Math.log(1 - x) * b - lBeta) / a;

  // Continued fraction evaluation (Lentz's method)
  const MAX_ITER = 200;
  const EPS = 1e-12;
  let f = 1.0;
  let C = 1.0;
  let D = 0.0;

  for (let m = 1; m <= MAX_ITER; m++) {
    let m2 = 2 * m;
    
    // Even step coefficient
    let numerator = m * (b - m) * x / ((a + m2 - 1) * (a + m2));
    D = 1.0 + numerator * D;
    if (Math.abs(D) < EPS) D = EPS;
    C = 1.0 + numerator / C;
    if (Math.abs(C) < EPS) C = EPS;
    D = 1.0 / D;
    f *= C * D;

    // Odd step coefficient
    numerator = -(a + m) * (a + b + m) * x / ((a + m2) * (a + m2 + 1));
    D = 1.0 + numerator * D;
    if (Math.abs(D) < EPS) D = EPS;
    C = 1.0 + numerator / C;
    if (Math.abs(C) < EPS) C = EPS;
    D = 1.0 / D;
    let delta = C * D;
    f *= delta;

    if (Math.abs(delta - 1.0) < EPS) break;
  }

  return front * f;
}

// Calculate Weight Function w(t)
function calcWeight(t, a, b, alpha, beta) {
  if (t <= a) return 0;
  if (t >= b) return 1;
  const x = (t - a) / (b - a);
  return incBeta(x, alpha, beta);
}

// Survival Functions
function calcObsSurv(t, model, param) {
  if (t < 0) return 1;
  switch (model) {
    case 'exponential':
      return Math.exp(-param * t);
    case 'weibull':
      return Math.exp(-param * Math.pow(t, 1.25));
    case 'piecewise':
      // Piecewise exponential simulation
      if (t <= 36) return Math.exp(-0.015 * t);
      if (t <= 96) return Math.exp(-0.015 * 36 - 0.022 * (t - 36));
      return Math.exp(-0.015 * 36 - 0.022 * 60 - 0.035 * (t - 96));
    default:
      return Math.exp(-param * t);
  }
}

function calcExtSurv(t, model, param1, param2) {
  if (t < 0) return 1;
  switch (model) {
    case 'exponential':
      return Math.exp(-param1 * t);
    case 'gompertz':
      // S(t) = exp(-a/b * (exp(b*t) - 1))
      const a = param1;
      const b = param2;
      return Math.exp(-(a / b) * (Math.exp(b * t) - 1));
    case 'weibull':
      return Math.exp(-param1 * Math.pow(t, 0.85));
    default:
      return Math.exp(-param1 * t);
  }
}

// Generate Full Time Series Data
function generateData() {
  const times = [];
  const obs = [];
  const ext = [];
  const blend = [];
  const weights = [];

  const step = Math.max(1, Math.floor(state.tMax / 180));
  
  for (let t = 0; t <= state.tMax; t += step) {
    const sObs = calcObsSurv(t, state.obsModel, state.obsParam);
    const sExt = calcExtSurv(t, state.extModel, state.extParam1, state.extParam2);
    const w = calcWeight(t, state.minT, state.maxT, state.alpha, state.beta);
    
    // Blended curve math: S_blend = S_obs^(1-w) * S_ext^w
    const sBlend = Math.pow(sObs, 1 - w) * Math.pow(sExt, w);

    times.push(t);
    obs.push(sObs);
    ext.push(sExt);
    blend.push(sBlend);
    weights.push(w);
  }

  return { times, obs, ext, blend, weights };
}

// Calculate Median Survival Time (where S(t) = 0.5)
function getMedianSurvival(times, surv) {
  for (let i = 0; i < surv.length - 1; i++) {
    if (surv[i] >= 0.5 && surv[i + 1] <= 0.5) {
      // Linear interpolation
      const frac = (0.5 - surv[i]) / (surv[i + 1] - surv[i]);
      return (times[i] + frac * (times[i + 1] - times[i])).toFixed(1);
    }
  }
  return surv[surv.length - 1] > 0.5 ? `>${times[times.length - 1]}` : '<1';
}

// --- UI & Rendering Engine ---

function updateStats(data) {
  const medObs = getMedianSurvival(data.times, data.obs);
  const medExt = getMedianSurvival(data.times, data.ext);
  const medBlend = getMedianSurvival(data.times, data.blend);

  document.getElementById('stat-obs').innerText = `${medObs} mo`;
  document.getElementById('stat-ext').innerText = `${medExt} mo`;
  document.getElementById('stat-blend').innerText = `${medBlend} mo`;
  document.getElementById('stat-window').innerText = `[${state.minT}, ${state.maxT}] mo`;
}

// Canvas Renderer
function drawChart(data) {
  const canvas = document.getElementById('main-chart');
  if (!canvas) return;

  const container = canvas.parentElement;
  if (!container) return;

  const width = container.clientWidth;
  const height = container.clientHeight;
  if (width <= 0 || height <= 0) return;

  const dpr = window.devicePixelRatio || 1;
  const targetW = Math.floor(width * dpr);
  const targetH = Math.floor(height * dpr);

  if (canvas.width !== targetW || canvas.height !== targetH) {
    canvas.width = targetW;
    canvas.height = targetH;
  }

  const ctx = canvas.getContext('2d');
  ctx.save();
  ctx.scale(dpr, dpr);

  const margin = { top: 30, right: 30, bottom: 45, left: 55 };
  const plotW = width - margin.left - margin.right;
  const plotH = height - margin.top - margin.bottom;

  // Clear canvas
  ctx.clearRect(0, 0, width, height);

  // X and Y Scalers
  const xScale = (t) => margin.left + (t / state.tMax) * plotW;
  const yScale = (s) => margin.top + (1 - s) * plotH;

  // Draw Grid Lines
  ctx.strokeStyle = 'rgba(255, 255, 255, 0.07)';
  ctx.lineWidth = 1;

  for (let s = 0; s <= 1; s += 0.2) {
    const y = yScale(s);
    ctx.beginPath();
    ctx.moveTo(margin.left, y);
    ctx.lineTo(width - margin.right, y);
    ctx.stroke();

    // Y Axis Labels
    ctx.fillStyle = '#94a3b8';
    ctx.font = '11px Inter, sans-serif';
    ctx.textAlign = 'right';
    ctx.fillText((s).toFixed(1), margin.left - 10, y + 4);
  }

  const tStep = Math.ceil(state.tMax / 6);
  for (let t = 0; t <= state.tMax; t += tStep) {
    const x = xScale(t);
    ctx.beginPath();
    ctx.moveTo(x, margin.top);
    ctx.lineTo(x, height - margin.bottom);
    ctx.stroke();

    // X Axis Labels
    ctx.fillStyle = '#94a3b8';
    ctx.font = '11px Inter, sans-serif';
    ctx.textAlign = 'center';
    ctx.fillText(`${t}`, x, height - margin.bottom + 20);
  }

  // Draw Blending Window Shaded Region
  if (state.minT < state.tMax) {
    const xMin = xScale(state.minT);
    const xMax = xScale(Math.min(state.maxT, state.tMax));
    
    ctx.fillStyle = 'rgba(245, 158, 11, 0.08)';
    ctx.fillRect(xMin, margin.top, Math.max(0, xMax - xMin), plotH);

    ctx.strokeStyle = 'rgba(245, 158, 11, 0.4)';
    ctx.setLineDash([4, 4]);
    
    ctx.beginPath();
    ctx.moveTo(xMin, margin.top);
    ctx.lineTo(xMin, height - margin.bottom);
    ctx.stroke();

    ctx.beginPath();
    ctx.moveTo(xMax, margin.top);
    ctx.lineTo(xMax, height - margin.bottom);
    ctx.stroke();

    ctx.setLineDash([]);
  }

  // Helper to draw a curve
  function drawCurve(yData, color, lineWidth, dash = []) {
    ctx.strokeStyle = color;
    ctx.lineWidth = lineWidth;
    ctx.setLineDash(dash);
    ctx.beginPath();
    for (let i = 0; i < data.times.length; i++) {
      const x = xScale(data.times[i]);
      const y = yScale(yData[i]);
      if (i === 0) ctx.moveTo(x, y);
      else ctx.lineTo(x, y);
    }
    ctx.stroke();
    ctx.setLineDash([]);
  }

  if (state.activeTab === 'weight') {
    // Draw Weight Curve Mode
    drawCurve(data.weights, '#f59e0b', 3);
    
    ctx.fillStyle = '#f59e0b';
    ctx.font = 'bold 12px Inter, sans-serif';
    ctx.textAlign = 'left';
    ctx.fillText('Weight Function w(t) [Beta CDF Transition]', margin.left + 15, margin.top + 25);
  } else {
    // Standard Survival Curves Mode
    drawCurve(data.obs, '#10b981', 2, [4, 4]); // Observed: Green dashed
    drawCurve(data.ext, '#3b82f6', 2, [4, 4]); // External: Blue dashed
    
    // Blended Curve (Bold Purple with shadow glow)
    ctx.shadowColor = 'rgba(168, 85, 247, 0.5)';
    ctx.shadowBlur = 10;
    drawCurve(data.blend, '#a855f7', 3.5);
    ctx.shadowBlur = 0; // Reset shadow
  }

  // Draw Axes Titles
  ctx.fillStyle = '#cbd5e1';
  ctx.font = '500 12px Inter, sans-serif';
  ctx.textAlign = 'center';
  ctx.fillText('Time (Months)', margin.left + plotW / 2, height - 10);

  ctx.save();
  ctx.translate(15, margin.top + plotH / 2);
  ctx.rotate(-Math.PI / 2);
  ctx.fillText(state.activeTab === 'weight' ? 'Weight w(t)' : 'Survival Probability S(t)', 0, 0);
  ctx.restore();

  // Hover Tooltip Crosshair
  if (state.hoverIndex !== null && state.hoverIndex < data.times.length) {
    const idx = state.hoverIndex;
    const hoverT = data.times[idx];
    const hoverX = xScale(hoverT);

    ctx.strokeStyle = 'rgba(255, 255, 255, 0.3)';
    ctx.lineWidth = 1;
    ctx.setLineDash([2, 2]);
    ctx.beginPath();
    ctx.moveTo(hoverX, margin.top);
    ctx.lineTo(hoverX, height - margin.bottom);
    ctx.stroke();
    ctx.setLineDash([]);

    // Tooltip Box
    const ttText = [
      `t = ${hoverT} mo`,
      `S_obs: ${(data.obs[idx] * 100).toFixed(1)}%`,
      `S_ext: ${(data.ext[idx] * 100).toFixed(1)}%`,
      `S_blend: ${(data.blend[idx] * 100).toFixed(1)}%`,
      `Weight w: ${(data.weights[idx]).toFixed(2)}`
    ];

    const boxW = 135;
    const boxH = 95;
    let boxX = hoverX + 15;
    if (boxX + boxW > width - margin.right) boxX = hoverX - boxW - 15;
    const boxY = margin.top + 20;

    ctx.fillStyle = 'rgba(15, 23, 42, 0.9)';
    ctx.strokeStyle = 'rgba(255, 255, 255, 0.2)';
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.roundRect(boxX, boxY, boxW, boxH, 8);
    ctx.fill();
    ctx.stroke();

    ctx.font = '11px Inter, sans-serif';
    ctx.textAlign = 'left';
    ttText.forEach((txt, lineIdx) => {
      ctx.fillStyle = lineIdx === 0 ? '#fff' : lineIdx === 3 ? '#c084fc' : '#94a3b8';
      ctx.fillText(txt, boxX + 10, boxY + 18 + lineIdx * 16);
    });
  }
  ctx.restore();
}

// Generate R Reproducibility Code
function generateRCode() {
  return `# blendR Survival Blending Reproducibility Script
library(blendR)
library(survHE)

# 1. Load or simulate observed & external survival models
data("TA174_FCR", package = "blendR")

# External simulated survival data
data_sim <- ext_surv_sim(t_info = 144, S_info = 0.05, T_max = ${state.tMax})

# 2. Fit observed and external survival models
obs_Surv <- fit.models(formula = Surv(death_t, death) ~ 1,
                       data = dat_FCR,
                       distr = "${state.obsModel}")

ext_Surv <- fit.models(formula = Surv(time, event) ~ 1,
                       data = data_sim,
                       distr = "${state.extModel}")

# 3. Define blending interval and Beta parameters
blend_interv <- list(min = ${state.minT}, max = ${state.maxT})
beta_params  <- list(alpha = ${state.alpha}, beta = ${state.beta})

# 4. Perform survival curve blending
ble_Surv <- blendsurv(obs_Surv, ext_Surv, blend_interv, beta_params)

# 5. Visualize blended survival curve
plot(ble_Surv)`;
}

// Synchronize UI elements with state
function updateUI() {
  const data = generateData();
  updateStats(data);
  drawChart(data);

  // Code Block Update
  const codeEl = document.getElementById('r-code-output');
  if (codeEl) codeEl.textContent = generateRCode();

  // Slider value badges
  document.getElementById('val-minT').innerText = `${state.minT} mo`;
  document.getElementById('val-maxT').innerText = `${state.maxT} mo`;
  document.getElementById('val-alpha').innerText = state.alpha.toFixed(1);
  document.getElementById('val-beta').innerText = state.beta.toFixed(1);
  document.getElementById('val-tMax').innerText = `${state.tMax} mo`;
}

// Preset Handlers
function applyPreset(name) {
  state.preset = name;
  document.querySelectorAll('.btn-preset').forEach(b => b.classList.remove('active'));
  const activeBtn = document.querySelector(`.btn-preset[data-preset="${name}"]`);
  if (activeBtn) activeBtn.classList.add('active');

  switch (name) {
    case 'ta174':
      state.minT = 48;
      state.maxT = 150;
      state.alpha = 3.0;
      state.beta = 3.0;
      state.obsModel = 'exponential';
      state.extModel = 'gompertz';
      break;
    case 'early':
      state.minT = 24;
      state.maxT = 96;
      state.alpha = 2.0;
      state.beta = 4.0;
      break;
    case 'late':
      state.minT = 72;
      state.maxT = 180;
      state.alpha = 4.0;
      state.beta = 2.0;
      break;
    case 'steep':
      state.minT = 60;
      state.maxT = 120;
      state.alpha = 8.0;
      state.beta = 8.0;
      break;
  }

  // Update Slider Controls
  document.getElementById('slider-minT').value = state.minT;
  document.getElementById('slider-maxT').value = state.maxT;
  document.getElementById('slider-alpha').value = state.alpha;
  document.getElementById('slider-beta').value = state.beta;
  document.getElementById('select-obsModel').value = state.obsModel;
  document.getElementById('select-extModel').value = state.extModel;

  updateUI();
}

// Attach Event Listeners
function initEventListeners() {
  // Sliders
  document.getElementById('slider-minT').addEventListener('input', (e) => {
    state.minT = parseInt(e.target.value);
    if (state.minT >= state.maxT) {
      state.maxT = state.minT + 10;
      document.getElementById('slider-maxT').value = state.maxT;
    }
    updateUI();
  });

  document.getElementById('slider-maxT').addEventListener('input', (e) => {
    state.maxT = parseInt(e.target.value);
    if (state.maxT <= state.minT) {
      state.minT = Math.max(0, state.maxT - 10);
      document.getElementById('slider-minT').value = state.minT;
    }
    updateUI();
  });

  document.getElementById('slider-alpha').addEventListener('input', (e) => {
    state.alpha = parseFloat(e.target.value);
    updateUI();
  });

  document.getElementById('slider-beta').addEventListener('input', (e) => {
    state.beta = parseFloat(e.target.value);
    updateUI();
  });

  document.getElementById('slider-tMax').addEventListener('input', (e) => {
    state.tMax = parseInt(e.target.value);
    updateUI();
  });

  // Model Selectors
  document.getElementById('select-obsModel').addEventListener('change', (e) => {
    state.obsModel = e.target.value;
    updateUI();
  });

  document.getElementById('select-extModel').addEventListener('change', (e) => {
    state.extModel = e.target.value;
    updateUI();
  });

  // Presets
  document.querySelectorAll('.btn-preset').forEach(btn => {
    btn.addEventListener('click', () => applyPreset(btn.dataset.preset));
  });

  // Tabs
  document.querySelectorAll('.tab-btn').forEach(btn => {
    btn.addEventListener('click', () => {
      document.querySelectorAll('.tab-btn').forEach(b => b.classList.remove('active'));
      btn.classList.add('active');
      state.activeTab = btn.dataset.tab;

      const chartW = document.getElementById('chart-wrapper');
      const codeW = document.getElementById('code-wrapper');

      if (state.activeTab === 'code') {
        chartW.style.display = 'none';
        codeW.style.display = 'block';
      } else {
        chartW.style.display = 'flex';
        codeW.style.display = 'none';
        updateUI();
      }
    });
  });

  // Canvas Hover Tooltip
  const canvas = document.getElementById('main-chart');
  canvas.addEventListener('mousemove', (e) => {
    const rect = canvas.getBoundingClientRect();
    const x = e.clientX - rect.left;
    const margin = { left: 55, right: 30 };
    const plotW = rect.width - margin.left - margin.right;
    
    if (x >= margin.left && x <= rect.width - margin.right) {
      const frac = (x - margin.left) / plotW;
      const hoverT = Math.round(frac * state.tMax);
      const data = generateData();
      let closestIdx = 0;
      let minDiff = Infinity;
      for (let i = 0; i < data.times.length; i++) {
        const diff = Math.abs(data.times[i] - hoverT);
        if (diff < minDiff) {
          minDiff = diff;
          closestIdx = i;
        }
      }
      state.hoverIndex = closestIdx;
    } else {
      state.hoverIndex = null;
    }
    drawChart(generateData());
  });

  canvas.addEventListener('mouseleave', () => {
    state.hoverIndex = null;
    drawChart(generateData());
  });

  // Copy Code Button
  document.getElementById('btn-copy-code').addEventListener('click', () => {
    navigator.clipboard.writeText(generateRCode()).then(() => {
      const btn = document.getElementById('btn-copy-code');
      btn.innerText = 'Copied!';
      setTimeout(() => btn.innerHTML = '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M16 4h2a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h2"></path><rect x="8" y="2" width="8" height="4" rx="1" ry="1"></rect></svg> Copy R Code', 2000);
    });
  });

  // Export PNG Button
  document.getElementById('btn-export-png').addEventListener('click', () => {
    const canvas = document.getElementById('main-chart');
    const link = document.createElement('a');
    link.download = `blendR-plot-min${state.minT}-max${state.maxT}.png`;
    link.href = canvas.toDataURL('image/png');
    link.click();
  });

  // Handle Window Resize
  window.addEventListener('resize', () => updateUI());
}

// Initialize WebR WebAssembly engine background loader
async function initWebR() {
  const statusDot = document.getElementById('webr-status-dot');
  const statusText = document.getElementById('webr-status-text');

  try {
    // Attempt dynamic import of WebR module
    const { WebR } = await import('https://webr.r-wasm.org/v0.3.3/webr.mjs');
    const webR = new WebR();
    await webR.init();
    
    state.webrStatus = 'ready';
    if (statusDot) statusDot.classList.add('active');
    if (statusText) statusText.innerText = 'WebR Engine (R in Wasm)';
  } catch (err) {
    console.log('WebR fast fallback active (Native JS Execution):', err);
    state.webrStatus = 'fallback';
    if (statusDot) {
      statusDot.style.backgroundColor = '#6366f1';
      statusDot.style.boxShadow = '0 0 8px #6366f1';
    }
    if (statusText) statusText.innerText = 'blendR Client Engine';
  }
}

// Entry point
document.addEventListener('DOMContentLoaded', () => {
  initEventListeners();
  updateUI();
  initWebR();
});
