matched <- function(f) {
  f <- match.fun(f)
  function(..., na.rm = FALSE) {
    dots <- list(...)
    if (na.rm) {
      keep <- !Reduce(`|`, lapply(dots, is.na))
      dots <- lapply(dots, `[`, keep)
    }
    do.call(f, dots)
  }
}
