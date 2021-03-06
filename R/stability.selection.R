#' Compute Lustgarten’s stability measure
#'
#' @details
#' Compute Lustgarten’s stability measure for variables selected in cross-validation
#'
#' @param list.selected.var A \code{\link{list}} with selected variables in cross-validation
#' @param list.index.cross A \code{\link{list}} with indexes obtained in cross-validation
#' @param nvar the number of first variables for which to calculate
#' @return value stability of selection (Lustgarten’s stability measure)
#'
#' @examples
#'
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
#'                              list.index.cross = indexes,
#'                              params = list(adjust = 'holm'))
#' asm <- stabilty.selection(list.selected.var, list.index.cross, 5)
#'
#' }
#'
#' @export
stabilty.selection <- function(list.selected.var, list.index.cross, nvar){
  na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))])}
  var.list <- c()
  for(i in 1:length(list.selected.var)){
    var.list <- append(var.list, list(as.character(list.selected.var[[i]]$name)))
  }

  sim = list()
  niter = length(list.index.cross)
  ncross = length(list.index.cross[[1]]$training)
  id.combine = combn(ncross*niter,2, FUN = NULL, simplify = F)
  N = ncross*niter
  n = length(unique(unlist(var.list)))

  for(m in 1:length(id.combine)){
    i = id.combine[[m]][1]
    j = id.combine[[m]][2]
    variset = na.omit.list(var.list[[i]][1:nvar])
    varjset = na.omit.list(var.list[[j]][1:nvar])
    k1 = length(variset)
    k2 = length(varjset)
    len.var.intersect = length(intersect(variset,varjset))
    r = len.var.intersect
    len.N.var = length(unique(union(variset,varjset)))
    if((min(c(k1,k2))-max(c(0,k1+k2-n))) == 0){sim[[m]] = 1}
    else{
      sim[[m]] = (r-(k1*k2/n))/(min(c(k1,k2))-max(c(0,k1+k2-n)))
    }
  }
  avg.sim = 2*sum(unlist(sim))/(N*(N-1))
  return(avg.sim)
}


#' Compute Lustgarten's stability measure ASM (N) dependence for top-N variables N = 5,10,15,20,30,40,50,75,100
#'
#' @details
#' Compute Lustgarten's stability measure ASM (N) dependence for top-N variables N = 5,10,15,20,30,40,50,75,100
#' for variables selected in cross-validation
#'
#' @param list.selected.var A \code{\link{list}} with selected variables in cross-validation
#' @param list.index.cross A \code{\link{list}} with indexes obtained in cross-validation
#' @return  A \code{\link{data.frame}} with the result Lustgarten's stability measure ASM (N) dependence for top-N variables N = 5,10,15,20,30,40,50,75,100
#'
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
#'                              list.index.cross = indexes,
#'                              params = list(adjust = 'holm'))
#' result.stability <- stabilty.selection.top.var(list.selected.var, list.index.cross)
#'
#' }
#'
#' @export
stability.selection.top.var <- function(list.selected.var, list.index.cross){
  N <- c(5,10,15,20,30,40,50,75,100)
  i = 1
  len0= list()
  for(n in N){
    len0[[i]] = stabilty.selection(list.selected.var = list.selected.var,
                                             list.index.cross = list.index.cross,
                                             nvar = n)
    i = i+1
  }
  stability.asm <- unlist(len0)
  return(data.frame(cbind(N, stability.asm)))
}

