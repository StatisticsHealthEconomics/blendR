shiny::shinyUI(
  {shiny::tagList(
    shiny::tags$style(type = "text/css", ".shiny-output-error { visibility: hidden; }",
                      ".shiny-output-error:before { visibility: hidden; }",
                      "div {text-align:justify;}"
                      ),
    shiny::tags$head(shiny::tags$style(".shiny-plot-output{height:85vh !important;}")),
    shiny::tags$head(shiny::tags$style(".shiny-plot-output{width:95vh !important;}")),

    shiny::navbarPage(theme = shinythemes::shinytheme('flatly'),
                      title = "Blended Survival Model",

                      ##################
                      #introduction tab#
                      ##################

                      {shiny::tabPanel("Welcome",
                                       shiny::fluidRow(
                                         shiny::column(
                                           width = 10, offset = 1,
                                           shiny::withMathJax(HTML(paste0(
                                             shiny::tags$h1("Welcome to blended survival model (BSMweb)"),
                                             shiny::tags$br(),
                                             "We provide a web interface to run the Blended
                                             survival model and achieve immediate visual inspection
                                             for a range of plausible scenarios. Below is a graphical representation of the blended curve method.",
                                             shiny::tags$br(), shiny::tags$br(),
                                                           shiny::tags$img(src = 'blendscheme.png', width = '60%',
                                                                           style ="display: block; margin-left: auto; margin-right: auto;",
                                                                           alt ='Graphical representation of the blended curve method'
                                                           ),
                                             shiny::tags$br(),
                                             "The ",
                                             shiny::tags$strong("“blending”"),
                                             " idea is to consider two separate processes to describe the long-term
                                             horizon survival. The first one (green curve) is driven exclusively by the observed data. Similar to
                                             a “standard” HTA analysis, we use this to determine an estimate over the entire time
                                             horizon of a survival curve. For the second component of the blending process, we consider
                                             a separate “external” survival curve (blue one), which is not informed by the observed data — for                                                    instance,  we could use “hard” information, e.g. derived from a different data source (such as                                                       registries or observational studies), or construct a model
                                             that is purely based on subjective knowledge elicited from experts. Finally, the two curves are
                                             blended into one, which gradually abandons the extrapolation from the observed data (thus avoiding
                                             issues with the inherent overfitting and unrealistic estimates) and merges into the long-term                                                        extrapolation from the external evidence.",
                                             shiny::tags$br(), shiny::tags$br(),
                                             shiny::tags$strong("BSMweb"),
                                             " allows the user to upload the trial or digitised data at the 'Observed data' tab using two formats:",
                                             shiny::tags$ol(
                                               shiny::tags$li(shiny::HTML(paste0(
                                                 "A spreadsheet, in ",shiny::tags$a(href='https://en.wikipedia.org/wiki/Comma-separated_values','                                                     .csv',target='_blank')," format, e.g. a file produced by MS Excel."
                                               ))),
                                               shiny::tags$li(shiny::HTML(paste0(
                                                 "A R data file in ", shiny::tags$a(href='https://bookdown.org/ndphillips/YaRrr/rdata-files.html',
                                                                                    '.RData', target='blank'), ", but the file should only contain
                                                 one object as the survival data including two variables",
                                                 shiny::tags$code("time"), " and ",
                                                 shiny::tags$code("event"), "."
                                               )))
                                             ),
                                             shiny::tags$br(),
                                             "An example (",
                                             shiny::tags$code(".RData"), ") can be obtained ",
                                             shiny::tags$a(href='https://github.com/StatisticsHealthEconomics/blendR-paper/blob/main/Data/TA174_FCR.RData','here', target = '_blank'), ". The example dataset is based on the CLL-8 trial data, which were also used in
                                             NICE technology appraisal TA174. Once the observed data are uploaded, the user can determine the                                                     estimated time horizon and define the grid of time values for estimating the survival probabilities
                                             at the 'Observed data' tab. An external curve is produced in the 'External info' tab, where the user
                                             can specify the external knowledge about the survival. The tab 'Blended curve' will provide graphical
                                             results of the blended estimate following the weight function with parameters (\\(a, b, \\alpha, \\beta\\))"," determined by the user.",
                                             shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),
                                             "Copyright: ",
                                             shiny::tags$a(href='mailto: zhaojing.che.19@ucl.ac.uk', 'Zhaojing Che'),
                                             ", ",
                                             shiny::tags$a(href='mailto: g.baio@ucl.ac.uk','Gianluca Baio'),
                                             ", ",
                                             shiny::tags$a(href='mailto: n.green@ucl.ac.uk','Nathan Green')


                                           )))
                                         )
                                       ))

                      },

                      ##############
                      #Observed tab#
                      ##############

                      {shiny::tabPanel("1. Observed data",
                                       {shiny::sidebarPanel(
                                         shiny::tags$div(
                                           shiny::p(shiny::h3(shiny::strong("Observed data"))),
                                           "In this panel, the user can upload the trial
                                           data using two formats (Spreadsheet or RData) and fit a model to the observed data.",
                                           shiny::tags$br(), shiny::tags$br(), shiny::br()
                                         ),
                                         shiny::selectInput("obsdat", "1. Import trial data from:",
                                                            choices = c("R", "Spreadsheet"),
                                                            selected = "R"),
                                         shiny::conditionalPanel(condition = "input.obsdat=='R'",
                                                                 fileInput("obs_data_R", label = "Choose RData File (e.g. FCR_ta174.RData)")
                                                                 ),
                                         shiny::conditionalPanel(condition = "input.obsdat=='Spreadsheet'",
                                                                 shiny::fileInput('obs_data_csv', 'Choose CSV File (e.g. TA174_FC.csv)',
                                                                                  accept = c('text/csv',
                                                                                             'text/comma-separated-values,text/plain',
                                                                                             '.csv'))
                                                                 ),
                                         shiny::textInput("time",  label = "Name of variable TIME",
                                                          value = "death_t"
                                                          ),
                                         shiny::textInput("event", label = "Name of variable EVENT",
                                                          value = "death"
                                                          ),
                                         shiny::selectInput("modchoice", "2. Choose a model which is used to fit the data",
                                                            choices = "Piecewise Exponential Model", selected = "Piecewise Exponential Model"),
                                         shiny::p(shiny::h5(shiny::strong("3. Define the grid of time values for estimating the survival."))),
                                         shiny::fluidRow(
                                           shiny::column(4,
                                                         shiny::numericInput("min", "min", value = 0, min = 0)),
                                           shiny::column(4,
                                                         shiny::numericInput("max", "max", value = 180, min = 0)),
                                           shiny::column(4,
                                                         shiny::numericInput("step", "step", value = 5, min = 0)
                                                         )
                                         ),
                                         "NB: Value of max (time horizon) is usually larger than the last observed point."


                                       )},

                        shiny::mainPanel(
                          # THIS CODE PREVENTS FROM SHOWING THE ERROR MESSAGES
                          shiny::tags$style(type = "text/css",
                                            ".shiny-output-error { visibility: hidden; }",
                                            ".shiny-output-error:before { visibility: hidden; }"),

                          shiny::plotOutput('obsplot')
                        )

                      )},

                      {shiny::tabPanel("2. External info",
                                       {shiny::sidebarPanel(
                                         shiny::tags$div(
                                           shiny::p(shiny::h3(shiny::strong("External info"))),
                                           "In this panel, the expert can provide the clinical opinion to
                                           construct the survival curve entirely driven by the external
                                           information.",
                                           shiny::tags$br(), shiny::tags$br()
                                         ),
                                         # Ask for the number of elicited survival points
                                         "For example, we may suggest that most likely value (MLV) of survival probability at 10 y (120 mo) is 10%. One or multiple elicited points can be included and in fact, the resulting curve can align more closely with substantive expert beliefs, given more elicited points.",
                                         shiny::tags$br(), shiny::br(),
                                         "Note: if number of elicited points is more than 1, the inserted points should be in chronological order.",
                                         shiny::tags$br(), shiny::br(),
                                         shiny::numericInput("npoints", "Number of elicited points",
                                                             value = 1, min = 1),
                                         shiny::uiOutput("est_points"),
                                         shiny::numericInput("T_max", "Maximum time point for lifetime estimate",
                                                             value = 180, min = 1),
                                         shiny::actionButton(inputId="plot","Plot")
                                       )},

                                       shiny::mainPanel(
                                         shiny::tags$style(type = "text/css",
                                                           ".shiny-output-error { visibility: hidden; }",
                                                           ".shiny-output-error:before { visibility: hidden; }"),

                                         shiny::plotOutput('extplot')

                                       )
                                       )},
                      {shiny::tabPanel("3. Blended curve",
                                       {shiny::sidebarPanel(
                                         shiny::tags$div(
                                           shiny::p(shiny::h3(shiny::strong("Blended curve"))),
                                           "In this panel, we blend the two survival curves: observed and external
                                           ones together following the weight function selected by the user.",
                                           shiny::tags$br(), shiny::tags$br(),
                                           "The blended survival is equivalent to the model fitted
to the short-term data (KM curve) between 0 and a (green curve); then gradually approaching
the external estimate between a and b (red curve); eventually consistent with the expected behaviour
(blue curve) in the Long term beyond b.",
                                           shiny::tags$br(), shiny::br()
                                         ),
                                         shiny::p(shiny::h5(shiny::strong("1. Define the blending interval [a, b]"))),
                                         shiny::fluidRow(
                                           shiny::column(4,
                                                         shiny::numericInput("a", "a", value = 20, min = 0)),
                                           shiny::column(4,
                                                         shiny::numericInput("b", "b", value = 140, min = 0))
                                         ),
                                         shiny::withMathJax(tags$div(
                                           "Blending trend (weight function): two parameters control the way two
                                           distribution influence the blended curve, e.g. \\(\\alpha > \\beta\\)
                                           means initially slow blending speed, relatively \\(\\alpha < \\beta\\)
                                           means fast choice."

                                         )),
                                         shiny::tags$br(),
                                         shiny::strong(withMathJax(sprintf('2. Define the parameters (\\(\\alpha, \\beta\\)) of the weight function'))),
                                         shiny::fluidRow(
                                           shiny::column(4,
                                                         shiny::numericInput("alpha", withMathJax("$$\\alpha$$"), value = 3, min = 0)),
                                           shiny::column(4,
                                                         shiny::numericInput("beta", withMathJax("$$\\beta$$"), value = 3, min = 0))
                                         )
                                       )},

                                       shiny::mainPanel(
                                         shiny::tags$style(type = "text/css",
                                                           ".shiny-output-error { visibility: hidden; }",
                                                           ".shiny-output-error:before { visibility: hidden; }"),

                                         shiny::plotOutput('bleplot')

                                       )

                                       )},
                      {shiny::tabPanel("4. Report",
                                       shiny::sidebarPanel(
                                         shiny::tags$div(
                                           shiny::tags$strong("Please select the required output and the document format:"),
                                           shiny::tags$br(),shiny::tags$br()
                                         ),
                                         shiny::checkboxGroupInput('survplots', "Survival Plots",choices = c("Observed data",
                                                                                                             "External info",
                                                                                                             "Blended curve"),
                                                                   selected = NULL, inline = FALSE),
                                         shiny::checkboxGroupInput('survest', "Survival Estimates", choices = c("Observed part",
                                                                                                            "External part",
                                                                                                            "Blended result"
                                                                                                            ),
                                                                   selected = NULL, inline = FALSE),
                                         shiny::radioButtons('format', 'Document format', c('PDF', 'Word'),inline = TRUE),
                                         shiny::downloadButton('downloadReport', 'Download report'),
                                         shiny::br(),
                                         shiny::p("NB: generating the document can take some time.")
                                       )
                      )}


                      )

  )

  }
)
