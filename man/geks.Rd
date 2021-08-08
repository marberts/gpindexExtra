\name{geks}
\alias{geks}
\alias{tornqvist_geks}

\title{
GEKS index
}

\description{
Calculate a GEKS index.
}

\usage{
geks(f = geometric_index("Tornqvist"))

tornqvist_geks(price, quantity, period, product, na.rm = FALSE)
}

\arguments{
\item{f}{A price-index function that satisfies the time-reversal test, usually a Tornqvist (the default), Fisher, or Walsh index.}
\item{price, quantity}{A numeric vector of prices/quantities.}
\item{period}{A factor, or something that can be coerced into one, that gives the corresponding time period for each element in \code{price} and \code{quantity}. The ordering of time periods follows the levels of \code{period} to agree with \code{\link[=cut.Date]{cut()}}.}
\item{product}{A factor, or something that can be coerced into one, that gives the corresponding product identifier for each element in \code{price} and \code{quantity}.}
\item{na.rm}{Should missing values for \code{price} and \code{quantity} be removed when calculating the index? By default missing values will return a missing value for the index.}
}

\value{
A named numeric vector giving the value of the GEKS index with the first period as the base period.
}

\examples{
}
