###################
# Best Model
###################

#' "Best Model" Procedure, OLS Subset Selection
#'
#' This function fits an OLS model that optimizes BIC or AIC
#' based on provided data.
#'
#' @param y A one-dimensional dataframe with the desired dependent variable
#' @param x A multi-dimensional dataframe with desired indepdent variables to be considered
#' @param crit Which criterion should the model optimize?
#' @param quick If TRUE, uses exhaustive search. If FALSE, uses stepwise search.
#' @return An lm object of the "best model"
#' @export

best_model <- function(y, x, crit = "BIC", quick = FALSE) {

  ### Error catching
  if(quick != TRUE & quick != FALSE) stop("Argument `quick` must be TRUE or FALSE")
  if(!crit %in% c("BIC", "AIC")) stop("Argument `crit` must be string 'BIC' or 'AIC'")
  if(is.null(y)) stop("Argument `y` must be specified")
  if(is.null(x)) stop("Argument `x` must be specified")
  if(!is.data.frame(y)) stop("Argument `y` must be a data.frame object")
  if(!is.data.frame(x)) stop("Argument `x` must be a data.frame object")
  if(ncol(x) > 14 & quick == FALSE) stop("Data is large. Please set argument `quick` to TRUE.")

  ### Define dataframe
  df <- dplyr::bind_cols(x, y)

  ### Search
  if(quick == TRUE) {
    my_search <- bestglm::bestglm(df, IC = crit)
    fmla <- formula(my_search[["BestModel"]])
  } else {
    my_fmla <- paste0(colnames(y), "~ .")
    my_k <- ifelse(crit == "AIC", 2, log(nrow(df)))
    my_search <- step(lm(my_fmla, data = df), trace = FALSE, k = my_k)
    fmla <- formula(my_search)
  }

  ### Return
  out <- lm(fmla, data = df)
  return(out)

}
