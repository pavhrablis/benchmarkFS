#' Selection of features in cross-validation
#'
#' @details Uses the methods of selection of features  available in this library
#'
#' @param x input data where columns are variables and rows are observations (all numeric)
#' @param y decision variable as a boolean vector of length equal to number of observations
#' @param method the name of the selection method for traits available in this library
#' @param list.index.cross A \code{\link{list}} with indexes obtained in cross-validation
#' @param params A \code{\link{list}} parameters required for this method of feature selection
#' @return  A \code{\link{list}} with selected features for each iteration of cross-validation
#' @examples
#' \donttest{
#'
#' class <- data$class
#' data$class <- NULL
#'
#' list.index.cross <- cross.validation(x = data, y = class, method= 'cv.kfold', k = 3, niter = 10)
#'
#' list.selected.var <- feature.selection(x = data,
#'                              y = class,
#'                              method = 'fs.utest',
#'                              list.index.cross = list.index.cross,
#'                              params = list(adjust = 'holm'))
#'
#' }
#' @export
feature.selection = function(x, y, method, list.index.cross, params = list()){
  if (!is.data.frame(data)) x = as.data.frame(x)
  niter = length(list.index.cross)
  ncross = length(list.index.cross[[1]]$training)
  var.imp.selection = function(m, x, y, method, list.index.cross ,niter, ncross, params = list()){
    index = expand.grid(j=1:ncross, p=1:niter)
    p = index[m,2]
    j = index[m,1]
    list.index.cross.train = list.index.cross[[p]]
    x$class <- y
    data.train.cross = na.omit(x[list.index.cross.train$training[[j]],])
    decision.train.cross <- data.train.cross$class
    data.train.cross$class <- NULL
    params <- list(data.train.cross, decision.train.cross, params)
    var.imp <- do.call(method, params)
    return(var.imp)

  }

  N = niter*ncross
  var.each.iter = lapply(1:N, function(m) var.imp.selection(m,
                                                            x,
                                                            y,
                                                            method = method,
                                                            list.index.cross,
                                                            niter,
                                                            ncross,
                                                            params = params))
  return(var.each.iter)
}
