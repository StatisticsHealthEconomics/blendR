
if (.Platform$OS.type == "windows") {
  if ("survHE" %in% rownames(installed.packages()) == FALSE) {
    install.packages("survHE",
                     repos = c("http://www.statistica.it/gianluca/R",
                               "https://cran.rstudio.org",
                               "https://inla.r-inla-download.org/R/stable"),
                     dependencies = TRUE)
  }
}

if (.Platform$OS.type == "windows") {
  if ("INLA" %in% rownames(installed.packages()) == FALSE) {

    if (!require("BiocManager", quietly = TRUE))
      install.packages("BiocManager")

    BiocManager::install("graph")
    BiocManager::install("Rgraphviz")

    # requires the latest version of R
    install.packages("INLA",
                     repos = c(getOption("repos"),
                               INLA = "https://inla.r-inla-download.org/R/stable"),
                     dep = TRUE)
  }
}

