to_scalar <- function(x) {
  trunc(as.numeric(x[1]))
}

rolling_window <- function(n, window) {
  steps <- seq_len(n - window + 1)
  Map(seq, from = steps, length.out = window)
}
