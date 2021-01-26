#' Showing result benchmark
#'
#' @details
#' The function creates a graph showing Lustgarten's stability measure ASM (N)
#' for top-N variables N = 5,10,15,20,30,40,50,75,100 for selected FS
#' The function creates a graph showing the dependence of the selected metrics model(N)
#' N = 5,10,15,20,30,40,50,75,100 for selected FS
#'
#'
#' @param data  A \code{\link{data.frame}} with data result metrics model and  Lustgarten’s stability measure
#' @param y what is showing , please selected : stability, acc, auc, mcc.
#' @return graph showing
#'
#' @examples
#'
#' \donttest{
#'
#' decisions <- data$class
#' data$class <- NULL
#'
#' result <- benchmarkFS(data,
#'            decisions,
#'            methods = c('fs.utest', 'fs.mrmr'),
#'            method = 'kfoldcv',
#'            params.cv = list(k = 3, iter = 10),
#'            level.cor = 0.75,
#'            params = list(adjust = 'SGoF', feature.number = 10, alpha = 0.05),
#'            asm = c('fs.utest', 'fs.mrmr'),
#'            model = c('fs.utest', 'fs.mrmr')
#'            )
#'
#'
#'
#' plot.result(result$stability, 'stability')
#'
#' plot.result(result$model, 'acc')
#'
#' }
#'
#' @import ggplot2
#' @export
plot.result <- function(data, y){
  if(!(y %in% c('stability', 'acc', 'auc', 'mcc'))){
    stop('available stability, acc, auc, mcc')
  }
  if(y == 'stability'){
    result <- ggplot(data = data, aes(x = nvar, y = stability, group= method ,color = method)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks= c(seq(0,100, by = 10))) +
      labs(title= "Lustgarten’s stability measure", y="ASM(N)", x = "Top(N)")
    return(result)
  }
  if(y == 'acc'){
    result <- ggplot(data = data, aes(x = nvar, y = mean.acc, group= method ,color = method)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks= c(seq(0,100, by = 10))) +
      labs(title= "Accuracy", y="ACC", x = "Top(N)")

    return(result)
  }
  if(y == 'auc'){
    result <- ggplot(data = data, aes(x = nvar, y = mean.auc, group= method ,color = method)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks= c(seq(0,100, by = 10))) +
      labs(title= "AUC", y="AUC", x = "Top(N)")

    return(result)
  }
  if(y == 'mcc'){
    result <- ggplot(data = data, aes(x = nvar, y = mean.mcc, group= method ,color = method)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks= c(seq(0,100, by = 10))) +
      labs(title= "Matthews Correlation Coefficient", y="MCC", x = "Top(N)")

    return(result)
  }
}
