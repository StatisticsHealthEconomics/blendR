options(shiny.maxRequestSize=1024*1024^2)

source("Functions.R")

function(input, output, session){
  
  session$onSessionEnded(stopApp)
  
  shiny::observe({
    
    ####################
    ### OBSERVED TAB ###
    ####################
    

  dat <- shiny::reactive({
    
    if(input$obsdat == "R"){
      shiny::req(input$obs_data_R)
      inFile_dat <- input$obs_data_R
      File <- inFile_dat$datapath
      # load the file into new environment and get it from there
      e = new.env()
      name <- load(File, envir = e)
      obsdata <- e[[name]]
    }
    if(input$obsdat == "Spreadsheet"){
      shiny::req(input$obs_data_csv)
      inFile_dat <- input$obs_data_csv
      obsdata <- read.csv(inFile_dat$datapath, sep = ",")
      
    }
    return(obsdata)
    
  })  

  
  obs_surv <- shiny::reactive({
    shiny::req(input$time, input$event, dat(), input$min, input$max, input$step)
    
     surv_est_inla(time = input$time, event = input$event, data = dat(),  
                              cutpoints = seq(input$min, input$max, by =input$step))
    
  })
  
  output$obsplot <- shiny::renderPlot({
    shiny::req(obs_surv())
    obsurvplot(time = obs_surv()$timepoint, S_obs = obs_surv()$S_obs, KM_time = obs_surv()$KM$time, KM_surv = obs_surv()$KM$surv)
    
  })
  
  ####################
  ### EXTERNAL TAB ###
  ####################
  
  
  output$est_points <- shiny::renderUI({
    lapply(1:input$npoints, function(i){
      list(shiny::numericInput(paste0("time_point_no",i), paste("Elicited time point",i),  value = 120, min = 0),
           shiny::numericInput(paste0("surv_est_no",i), paste("Survival estimate (MLV) for time point", i), value = 0.1, min = 0, max = 1)
           )
    })
  })
  
  ext_surv <- shiny::eventReactive(input$plot,{
    
   tp_ext   <- unlist(lapply(1:input$npoints, function(i) input[[paste0('time_point_no', i)]]))
   surv_est <- unlist(lapply(1:input$npoints, function(i) input[[paste0('surv_est_no', i)]])) 
    
   ext_surv_est(t_info = tp_ext,
                S_info = surv_est,
                T_max = input$T_max,
                times_est = obs_surv()$timepoint,
                distr = "gom")
  
    })
  
  output$extplot <- shiny::renderPlot({
    
    extsurvplot(time = obs_surv()$timepoint, S_ext = ext_surv()$S_ext)
  })
  
  
  ble_surv <- shiny::reactive({
    
    weight <- pbeta((obs_surv()$timepoint - input$a)/(input$b - input$a), input$alpha, input$beta)
    
    n_sim <- ncol(ext_surv()$S_ext)
    
    ble_Surv <- matrix(NA, nrow = length(obs_surv()$timepoint), ncol = n_sim)
    
    for (i in seq_len(n_sim)) {
      ble_Surv[, i] <-
        obs_surv()$S_obs[, i]^(1 - weight) * ext_surv()$S_ext[, i]^weight
    }
    
    return(ble_Surv)
    
  })
  
  output$bleplot <- shiny::renderPlot({
    
    blesurvplot(obs_Surv = obs_surv(), ext_Surv = ext_surv(), ble_Surv = ble_surv(), tp = obs_surv()$timepoint)
  })
  
  

  
  
  
  
  })
  
}
  
  
  