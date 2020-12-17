#' @export
list.methods <- function(){
  everything <- sort(getNamespaceExports("benchmarkFS"))
  message("All feature selection algorithm wrappers in benchmarkFS:\n")
  print(everything[grepl(pattern="^[f]s", everything)])
}
