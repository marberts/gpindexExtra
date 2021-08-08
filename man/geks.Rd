\name{geks}
\alias{geks}
\alias{tornqvist_geks}
\alias{fisher_geks}

\title{
GEKS index
}

\description{
Calculate an inter-temporal GEKS price index.
}

\usage{
geks(f)

tornqvist_geks(price, quantity, period, product, na.rm = FALSE)

fisher_geks(price, quantity, period, product, na.rm = FALSE)
}

\arguments{
\item{f}{A \code{\link{price_index}} function that uses information on both base and current-period prices and quantities, and satisfies the time-reversal test. Usually a Tornqvist, Fisher, or Walsh index.}
\item{price, quantity}{A numeric vector of prices/quantities.}
\item{period}{A factor, or something that can be coerced into one, that gives the corresponding time period for each element in \code{price} and \code{quantity}. The ordering of time periods follows the levels of \code{period} to agree with \code{\link[=cut.Date]{cut()}}.}
\item{product}{A factor, or something that can be coerced into one, that gives the corresponding product identifier for each element in \code{price} and \code{quantity}.}
\item{na.rm}{Should missing values for \code{price} and \code{quantity} be removed when calculating the index? By default missing values will return a missing value for the index.}
}

\value{
\code{geks()} returns a function to calculate a GEKS index with the desired index-number formula.

\code{tornqvist_geks()} and \code{fisher_geks()} both return a named numeric vector giving the value of the respective GEKS index with the first period as the base period.
}

\seealso{
\code{\link{price_index}} for price-index functions that can be used in \code{geks()}.

\code{\link{matched}} to control how missing data are handled (see the note in \code{\link{price_index}}).
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
