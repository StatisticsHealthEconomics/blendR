options(shiny.maxRequestSize=1024*1024^2)

library(blendR)
library(survHE)
library(INLA)


function(input, output, session){

  session$onSessionEnded(stopApp)

  shiny::observe({

    ####################
    ### OBSERVED TAB ###
    ####################


    dat <- shiny::reactive({
      shiny::req(input$time, input$event)
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

      process_dat <- data.frame(death_t = obsdata[, input$time], death = obsdata[, input$event])

      return(process_dat)

    })


    obs_inla <- shiny::reactive({
      shiny::req(input$time, input$event, dat(), input$min, input$max, input$step)

      blendR::surv_est_inla(data = dat(),
                       cutpoints = seq(input$min, input$max, by =input$step))

    })


      obs_surv <- shiny::reactive({

        if (input$step <1){

          surv_step <- input$step

        }else{

          surv_step <- 1
        }

        S_obs <- make_surv(obs_inla(), t = seq(input$min, input$max, by = 1), nsim = 100)

        km <- survfit(as.formula(paste("Surv(", input$time, ",", input$event, ")~1", sep = "")), data = dat())

        return(list(
          time  = seq(input$min, input$max, by = surv_step),
          S_obs = S_obs,
          KM = km,
          mod = obs_inla
        ))

      })



    output$obsplot <- shiny::renderPlot({
      shiny::req(obs_surv())

      ggplot() +
        geom_line(aes(obs_surv()$KM$time, obs_surv()$KM$surv, colour = "Kaplan-Meier"), size = 1.25, linetype = "dashed") +
        xlim(0, max(obs_surv()$time)) + ylim(0,1) +
        geom_line(aes(obs_surv()$time,rowMeans(obs_surv()$S_obs), colour = "Data fitting"), size = 1)+
        geom_ribbon(aes(x=obs_surv()$time, y = rowMeans(obs_surv()$S_obs),
                        ymin = apply(obs_surv()$S_obs, 1, quantile, probs = 0.025),ymax = apply(obs_surv()$S_obs, 1, quantile, probs = 0.975)), alpha = 0.1)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
              axis.line = element_line(colour = "black"), text = element_text(size = 12))+
        xlab("Time") + ylab("Survival")+
        scale_colour_manual(name = "model", values = c("Data fitting"="#7CAE00", "Kaplan-Meier"="brown"))


    })

    ####################
    ### EXTERNAL TAB ###
    ####################



    output$est_points <- shiny::renderUI({
      lapply(1:input$npoints, function(i){
        list(shiny::numericInput(paste0("time_point_no",i), paste("Elicited time point",i),  value = 100, min = 0),
             shiny::numericInput(paste0("surv_est_no",i), paste("Survival estimate (MLV) for time point", i), value = 0.2, min = 0, max = 1)
        )
      })
    })

    ext_mod <- shiny::eventReactive(input$plot,{

      tp_ext <- unlist(lapply(1:input$npoints, function(i) input[[paste0('time_point_no', i)]]))
      surv_est <- unlist(lapply(1:input$npoints, function(i) input[[paste0('surv_est_no', i)]]))

      data_sim <- ext_surv_sim(t_info = tp_ext,
                               S_info = surv_est,
                               T_max  = input$T_max)

       fit.models(formula = Surv(time, event)~1,
                            data = data_sim,
                            distr = "gom",
                            method = "hmc",
                            priors = list(gom = list(a_alpha = 0.1,
                                                     beta = 0.1)))


    })

      ext_surv <- shiny::reactive({

        S_ext <- make_surv(ext_mod(), t = obs_surv()$time, nsim = 100)

        return(list(
          time = obs_surv()$time,
          S_ext = S_ext,
          mod = ext_mod
        ))


      })




    output$extplot <- shiny::renderPlot({
      shiny::req(ext_surv())

      ggplot() +
        xlim(0, max(ext_surv()$time)) + ylim(0,1) +
        geom_line(aes(ext_surv()$time, rowMeans(ext_surv()$S_ext)), color = "#00BFC4", size = 1.25)+
        geom_ribbon(aes(x=ext_surv()$time, y = rowMeans(ext_surv()$S_ext), ymin = apply(ext_surv()$S_ext, 1, quantile, probs = 0.025),
                        ymax = apply(ext_surv()$S_ext, 1, quantile, probs = 0.975)), alpha = 0.1) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
              axis.line = element_line(colour = "black"), text = element_text(size = 12))+
        xlab("Time") + ylab("Survival")

    })


    blend_interv <- shiny::reactive({

      return(list(min = input$a, max = input$b))

    })


    beta_params <- shiny::reactive({

      return(list(alpha = input$alpha, beta = input$beta))
    })

    ble_surv <- shiny::reactive({
      shiny::req(obs_surv(), ext_surv())

      blendsurv(obs_inla(), ext_mod(), blend_interv(), beta_params())

    })

    output$bleplot <- shiny::renderPlot({
      shiny::req(ble_surv())

      plot(ble_surv())
    })







  })

}


  
