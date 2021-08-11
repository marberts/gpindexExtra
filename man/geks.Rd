\name{geks}
\alias{geks}
\alias{tornqvist_geks}
\alias{fisher_geks}
\alias{balanced}

\title{
GEKS index
}

\description{
Calculate an inter-temporal GEKS price index.
}

\usage{
geks(f)

tornqvist_geks(price, quantity, period, product, 
               window = nlevels(period), n = window - 1, na.rm = FALSE)

fisher_geks(price, quantity, period, product, 
            window = nlevels(period), n = window - 1, na.rm = FALSE)

balanced(f, ...)
}

\arguments{
\item{f}{A \code{\link{price_index}} function that uses information on both base and current-period prices and quantities, and satisfies the time-reversal test. Usually a Tornqvist, Fisher, or Walsh index.}
\item{price, quantity}{A numeric vector of prices/quantities.}
\item{period}{A factor, or something that can be coerced into one, that gives the corresponding time period for each element in \code{price} and \code{quantity}. The ordering of time periods follows the levels of \code{period} to agree with \code{\link[=cut.Date]{cut()}}.}
\item{product}{A factor, or something that can be coerced into one, that gives the corresponding product identifier for each element in \code{price} and \code{quantity}.}
\item{window}{The length of the window. The default is a window that encompasses all periods in 
\code{period}. Values that are neither integers nor length 1 are silently truncated to a length 1 integer.}
\item{n}{A number giving the length of the index series. For example, if there are 13 periods in \code{window}, setting \code{n = 1} gives the index for period 13. The default gives an index for each period in \code{window}. Values that are neither integers nor length 1 are silently truncated to a length 1 integer.}
\item{na.rm}{Should missing values for \code{price} and \code{quantity} be removed when calculating the index? By default missing values will return a missing value for the index.}
\item{...}{Additional arguments to \code{f} for which missing values should \emph{not} be removed.}
}

\details{
The GEKS index is described in chapter 7 of Balk (2008) and by Ivancic et al. (2011).

The \code{balanced()} operator is designed to address the note about missing values in \code{\link{price_index}}.
}

\value{d
\code{geks()} returns a function to calculate a GEKS index with the desired index-number formula.

\code{tornqvist_geks()} and \code{fisher_geks()} both return a list with a named numeric vector giving the value of the respective period-over-period GEKS index for each window.

\code{balanced()} returns a function like \code{f}, except that missing values are balanced across \emph{all} inputs when \code{na.rm = TRUE}. This is like using \code{\link[=complete.cases]{complete.cases()}}.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

Ivancic, L., Diewert, W. E., and Fox, K. J. (2011). Scanner data, time aggregation and the construction of price indexes. \emph{Journal of Econometrics}, 161(1): 24--35.
}

\seealso{
\code{\link{price_index}} for price-index functions that can be used in \code{geks()}.
}

\examples{
price <- 1:6
quantity <- 6:1
period <- rep(1:3, 2)
product <- rep(letters[1:2], each = 3)

tornqvist_geks(price, quantity, period, product)

tornqvist_geks(price, quantity, period, product, window = 2)

# Missing data
quantity[2] <- NA

# Use all non-missing data
fisher_geks(price, quantity, period, product, na.rm = TRUE)

# Remove records with any missing data
fg <- geks(balanced(fisher_index))
fg(price, quantity, period, product, na.rm = TRUE)
}
