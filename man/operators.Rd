\name{operators}
\alias{matched}

\title{
More function operators
}

\description{
Extra operators to augment the way a function behaves.
\itemize{
\item{\code{matched()} makes a function work with matched data when \code{na.rm = TRUE}.}
}
}

\usage{
matched(f)
}

\arguments{
\item{f}{A function (usually one of \code{\link{price_index}}).}
}

\value{
\code{matched()} returns a function like \code{f}, except that missing values are matched across \emph{all} inputs when \code{na.rm = TRUE}. This is like using \code{\link[=complete.cases]{complete.cases()}}.
}

\seealso{
\code{\link{price_index}} for price-index functions that often need missing values matched across all price and quantity arguments.
}

\examples{
price <- 1:6
quantity <- 6:1
period <- rep(1:3, 2)
product <- rep(letters[1:2], each = 3)

tornqvist_geks(price, quantity, period, product)

# Missing data
quantity[2] <- NA

# Use all non-missing data
fisher_geks(price, quantity, period, product, na.rm = TRUE)

# Remove records with any missing data
fg <- geks(matched(fisher_index))
fg(price, quantity, period, product, na.rm = TRUE)
}
