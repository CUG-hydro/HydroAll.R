#' @export
print.OUTPUT_HydroALL <- function(x) {
  fprintf("Model: %s\n", x$model$name)

  cat("Parameters:\n")
  print(x$model$par)

  cat("\nGOF:\n")
  print(x$gof)

  cat("\nData:\n")
  print(x$data)
}
