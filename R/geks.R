geks_matrix <- function(index, price, quantity, period, product, n, nper, na.rm) {
  # making base prices/quantities is the slowest part of the calculation;
  # the algorithm calculates the lower-triangular part of the GEKS matrix
  # to avoid making relatives with different bases, then uses the 
  # time-reversal property of the 'index' function
  rows <- seq_len(nper)
  lt <- lapply(rows, function(i) {
    if (i <= max(nper - n, 1)) return(rep_len(1, nper))
    js <- seq_len(i - 1)
    pad <- rep_len(1, nper - i + 1)
    # matching is only done for the lower-triangular part of the matrix
    m <- .mapply(match, list(product[js], product[i]), list(incomparables = NA))
    bp <- .mapply(`[`, list(price[i], m), list())
    bq <- .mapply(`[`, list(quantity[i], m), list())
    ans <- .mapply(index, list(price[js], bp, quantity[js], bq), list(na.rm = na.rm))
    c(unlist(ans, use.names = FALSE), pad)
  })
  res <- do.call(rbind, lt)
  # exploit time-reversal
  res[upper.tri(res)] <- 1 / t(res)[upper.tri(res)]
  res
}

geks <- function(f) {
  index <- match.fun(f)
  function(price, quantity, period, product, n = nlevels(period), na.rm = FALSE) {
    if (gpindex:::different_lengths(price, quantity, period, product)) {
      stop("'price', 'quantity', 'period', and 'product' must be the same length")
    }
    period <- as.factor(period)
    nper <- nlevels(period)
    if (!nper) return(structure(numeric(0), names = character(0)))
    n <- to_scalar(n)
    if (n < 1) stop("'n' must be greater than or equal to 1")
    if (n > nper) stop("'n' must be less than the number of levels in 'period'")
    price <- split(price, period)
    quantity <- split(quantity, period)
    product <- as.factor(product)
    attributes(product) <- NULL # faster to match on integer codes
    product <- split(product, period)
    mat <- geks_matrix(index, price, quantity, period, product, n, nper, na.rm)
    keep <- seq(nper - n + 1, nper)
    res <- apply(mat[, keep, drop = FALSE], 2, geometric_mean, na.rm = na.rm)
    structure(res / res[1], names = levels(period)[keep])
  }
}

tornqvist_geks <- geks(geometric_index("Tornqvist"))

fisher_geks <- geks(fisher_index)

balanced <- function(f, ...) {
  f <- match.fun(f)
  nbargs <- list(...)
  function(..., na.rm = FALSE) {
    dots <- list(...)
    if (na.rm) {
      keep <- !Reduce(`|`, lapply(dots, is.na))
      dots <- lapply(dots, `[`, keep)
    }
    do.call(f, c(dots, nbargs))
  }
}

