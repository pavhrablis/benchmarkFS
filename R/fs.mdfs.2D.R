#' Build MultiDimensional Feature Selector from IGs uses GPU,  is a parallel computing platform CUDA
#'
#' @details
#' Build MultiDimensional Feature Selector from IGs, requires mandatory installation of CUDA
#'
#' @param x input data where columns are variables and rows are observations (all numeric)
#' @param y decision variable as a boolean vector of length equal to number of observations
#' @param params method as accepted by \code{\link[stats]{p.adjust}} (\code{"BY"}
#' is recommended for FDR, see Details) or SGoF \code{\link[sgof]{SGoF}}
#' @return A \code{\link{data.frame}} with selected features and p.value
#'
#' \donttest{
#'
#' decisions <- data$class
#' data$class <- NULL
#'
#' fs.mdfs.2D(data, decisions, params = list(adjust = 'SGoF'))
#' }
#'
#' @import MDFS
#' @import sgof
#' @importFrom stats p.adjust
#'
#' @export
fs.mdfs.2D <- function(x, y, params = list(adjust = 'holm', alpha = 0.05)){
  if (!is.data.frame(x)) data = as.data.frame(x)
  dim0 = 2
  div0 = 1
  acceleration.type = TRUE
  adjust = params$adjust
  if (adjust == 'SGoF'){
    result = MDFS(data = x, decision = y, dimensions = dim0, divisions = div0, use.CUDA = acceleration.type,
                  p.adjust.method = 'none')
    var.names = names(x)
    index.imp = result$relevant.variables
    var.imp.frame = data.frame(name = var.names, Pvalue = result$p.value)[index.imp,]
    var.imp.frame.sort = var.imp.frame[order(var.imp.frame$Pvalue, decreasing = F),]
    SGoF.p.value = SGoF(var.imp.frame.sort$Pvalue, alpha = 0.05, gamma = 0.05)
    var.imp.frame.sort$adjustPvalue = SGoF.p.value$Adjusted.pvalues
    row.names(var.imp.frame.sort) = NULL
    var.imp = var.imp.frame.sort[which(var.imp.frame.sort$adjustPvalue < params$alpha),]
    return(var.imp)
  } else {
    result = MDFS(data = x, decision = y, dimensions = dim0, divisions = div0, use.CUDA = acceleration.type,
                  p.adjust.method = adjust)
    var.names = names(x)
    index.imp = RelevantVariables(result$MDFS,
                                  level = params$alpha,
                                  p.adjust.method = params$adjust)
    var.imp.frame = data.frame(name = var.names, Pvalue = result$p.value, adjustPvalue = result$adjusted.p.value)[index.imp,]
    var.imp = var.imp.frame[order(var.imp.frame$adjustPvalue, decreasing = F),]
    return(var.imp)
  }
}
