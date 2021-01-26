#' Minimum Redundancy Maximal Relevancy
#'
#' @details
#' Performs MRMR for feature selection
#'
#' @param x input data where columns are variables and rows are observations (all numeric)
#' @param y decision variable as a boolean vector of length equal to number of observations
#' @param params number of attributes to select. Must not exceed, feature.number there cannot be more than the number of columns
#' @return A \code{\link{data.frame}} with selected features
#'
#' @examples
#'
#' \donttest{
#'
#' decisions <- data$class
#' data$class <- NULL
#'
#' fs.mrmr(data, decisions, params = list(feature.number = 100))
#' }
#'
#' @import praznik
#'
#' @export
fs.mrmr <- function(x, y, params = list(feature.number = 100)){
  if (!is.data.frame(x)) x = as.data.frame(x)
  if (params$feature.number > ncol(x)){
    stop('fs.mrmr :feature.number there cannot be more than the number of columns ')
  }
  result <- MRMR(x, y, params$feature.number)
  var.imp <- data.frame(name = names(result$score),scores = as.numeric(data.frame(result$score)[,1]))
  return(var.imp)
}
