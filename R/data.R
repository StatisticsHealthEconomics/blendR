#' Survival data
#'
#' A brief description of the FCR survival dataset and the trial or population it covers.
#'
#' @format A data frame (tibble) with 403 rows and 5 variables:
#' \describe{
#'   \item{patid}{Patient identifier (integer).}
#'   \item{treat}{Treatment group indicator (integer, e.g., 1 = treated, 0 = control).}
#'   \item{death}{Death event indicator (integer, e.g., 1 = event, 0 = censored).}
#'   \item{death_t}{Time to death or censoring in original units (numeric).}
#'   \item{death_ty}{Time to death or censoring in years (numeric).}
#' }
#' @keywords datasets
"dat_FCR"
