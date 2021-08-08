geks <- function(f) {
  index <- match.fun(f)
  function(price, quantity, period, product, na.rm = FALSE) {
    if (gpindex:::different_lengths(price, quantity, period, product)) {
      stop("'price', 'quantity', 'period', and 'product' must be the same length")
    }
    period <- as.factor(period)
    n <- nlevels(period)
    if (!n) return(structure(numeric(0), names = character(0)))
    price <- split(price, period)
    quantity <- split(quantity, period)
    product <- as.factor(product)
    attributes(product) <- NULL # faster to match on integer codes
    product <- split(product, period)
    # making base prices/quantities is the slowest part of the calculation;
    # the algorithm calculates the lower-triangular part of the GEKS matrix
    # to avoid making relatives with different bases, then uses the 
    # time-reversal property of the 'index' function to get the GEKS index
    lt <- lapply(seq_len(n), function(i) {
      if (i == 1) return(rep_len(1, n))
      s <- seq_len(i - 1)
      pad <- rep_len(1, n - i + 1)
      # matching is only done for the lower-triangular part of the matrix
      m <- .mapply(match, list(product[s], product[i]), list(incomparables = NA))
      bp <- .mapply(`[`, list(price[i], m), list())
      bq <- .mapply(`[`, list(quantity[i], m), list())
      ans <- .mapply(index, list(price[s], bp, quantity[s], bq), list(na.rm = na.rm))
      c(unlist(ans), pad)
    })
    mat <- do.call(rbind, lt)
    # exploit time-reversal
    mat[upper.tri(mat)] <- 1 / t(mat)[upper.tri(mat)]
    res <- apply(mat, 2, geometric_mean, na.rm = na.rm)
    structure(res / res[1], names = levels(period))
  }
}

tornqvist_geks <- geks(geometric_index("Tornqvist"))

fisher_geks <- geks(fisher_index)
