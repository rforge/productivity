\name{Shadowp}
\alias{Shadowp}
\title{Shadow prices used in productivity and profitability computations}

\description{From any object created by either \code{\link{fareprim}}, \code{\link{fisher}}, 
\code{\link{laspeyres}}, \code{\link{lowe}}, or \code{\link{paasche}} function, this function extracts 
the deflated cost input shadow prices along with the deflated revenue output shadow prices 
used in productivity and profitability computations.}

\arguments{
  \item{object}{Object of class \verb{'FarePrimont'}, \verb{'Fisher'}, \verb{'Laspeyres'}, 
  \verb{'Lowe'}, or \verb{'Paasche'}.}
  \item{\dots}{Currently not used.}
}

\details{
For all the price-based indices (i.e. Fisher, Lowe, Laspeyres and Paasche), deflated input and output shadow 
prices \bold{for each observation} are return.

For the Färe-Primont index, the deflated input and output shadow price \code{of the representative observation} 
is returned.
\itemize{
\item An object of class \verb{'FarePrimont'} is usually a result of a call to \code{\link{fareprim}}
\item An object of class \verb{'Fisher'} is usually a result of a call to \code{\link{fisher}}
\item An object of class \verb{'Laspeyres'} is usually a result of a call to \code{\link{laspeyres}}
\item An object of class \verb{'Lowe'} is usually a result of a call to \code{\link{lowe}}
\item An object of class \verb{'Paasche'} is usually a result of a call to \code{\link{paasche}}}
}

\value{The function returns a data frame containing all the elements and observations 
included in the \code{"Shadowp"} constituent element of \code{object}.}

\author{Yann Desjeux, K Hervé Dakpo, Laure Latruffe}

\seealso{For details and information on returned values, see \code{\link{fareprim}}, 
\code{\link{fisher}}, \code{\link{laspeyres}}, \code{\link{lowe}}, 
or \code{\link{paasche}}.\cr
See also:\cr
- \code{\link{Changes}} for productivity and profitability change indices; and \cr
- \code{\link{Levels}} for productivity and profitability levels.}

\examples{
\dontrun{
  FISHER <- fisher(data = usagri, id.var = "States", time.var = "Years", x.vars = c(7:10), 
  y.vars = c(4:6), w.vars = c(14:17), p.vars = c(11:13), orientation = "out", scaled = TRUE)
    Fisher.shadowprices <- Shadowp(FISHER)
    head(Fisher.shadowprices)
}
}

\keyword{manip}
