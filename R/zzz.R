
if (.Platform$OS.type == "windows") {
  if ("survHE" %in% rownames(installed.packages()) == FALSE) {
    install.packages("survHE",
                     repos = c("http://www.statistica.it/gianluca/R",
                               "https://cran.rstudio.org",
                               "https://inla.r-inla-download.org/R/stable"),
                     dependencies = TRUE)
  }
}
