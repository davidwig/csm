###################
# Get FRED Data
###################

#' Get Fred-MD Data
#'
#' This function returns the current FRED-MD dataset.
#'
#' @return A tibble object
#' @export

get_fred_md <- function() {
  my_data <- readr::read_csv("https://files.stlouisfed.org/files/htdocs/fred-md/monthly/current.csv")
  my_data <- dplyr::slice(my_data, -1)
  out <- dplyr::mutate(my_data, sasdate = lubridate::mdy(sasdate))
  return(out)
}

#' Get Fred-QD Data
#'
#' This function returns the current FRED-QD dataset.
#'
#' @return A tibble object
#' @export

get_fred_qd <- function() {
  my_data <- readr::read_csv("https://files.stlouisfed.org/files/htdocs/fred-md/quarterly/current.csv")
  my_data <- dplyr::slice(my_data, -c(1:2))
  out <- dplyr::mutate(my_data, sasdate = lubridate::mdy(sasdate))
  return(out)
}

#' Go To FRED-MD/FRED-QD Website
#'
#' This function opens the website with latest versions, codes, and documentation.
#'
#' @return Website in default browser
#' @export

fred_md_site <- function() {
  browseURL("https://research.stlouisfed.org/econ/mccracken/fred-databases/")
}

#' Go To FRED-MD/FRED-QD Website
#'
#' This function differences time series until KPSS Test deems stationary at 95% confidence level.
#'
#' @param x Input vector
#' @return Output vector
#' @export

diff_til_stat <- function(x) {
  if(!is.numeric(x)) stop("Input `x` must be a numeric vector")
  if(forecast::ndiffs(x) == 0) {
    out <- x
  } else {
    out <- c(rep(NA, forecast::ndiffs(x)), diff(x, forecast::ndiffs(x)))
  }
  return(out)
}
