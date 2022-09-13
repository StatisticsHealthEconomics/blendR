# blendR

This is the `GitHub` repository to host the `R` code used to run the example in the paper *"Blended Survival Curves: A new Approach to Extrapolation for Time-to-Event Outcomes from Clinical Trials in Health Technology Assessment"*.

The [example dataset](Data/TA174.RData) is based on the [CLL-8 trial](https://doi.org/10.1016/S0140-6736(10)61381-5) data, which were also used in [NICE technology appraisal TA174](https://www.nice.org.uk/guidance/ta174). A detailed report [`case_study_blend.html`](Scripts/case_study_blend.html) in the `Scripts` folder explains how to perform the blending analysis, step-by-step. 

In addition, the corresponding [`Rmd`](Scripts/case_study_blend.Rmd) file is used to manipulate and restructure the information to produce the modelling and all `R` code from the `markdown` document are extracted to the script [`CODE_case_study.R`](Scripts/CODE_case_study.R).   
