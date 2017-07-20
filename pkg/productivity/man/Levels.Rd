\name{Levels}
\alias{Levels}
\title{Productivity and profitability levels}
\description{This function extracts productivity and profitability (when available) levels 
from any object created by either \code{\link{fareprim}}, \code{\link{fisher}}, \code{\link{laspeyres}}, 
\code{\link{lowe}}, \code{\link{malm}}, or \code{\link{paasche}} function.
}

\usage{Levels(object, ...)}

\arguments{
  \item{object}{Object of class \verb{'FarePrimont'}, \verb{'Fisher'}, \verb{'Laspeyres'}, 
  \verb{'Lowe'}, \verb{'Malmquist'}, or \verb{'Paasche'}.}
  \item{\dots}{Currently not used.}
}

\details{
\itemize{
\item An object of class \verb{'FarePrimont'} is usually a result of a call to \code{\link{fareprim}}
\item An object of class \verb{'Fisher'} is usually a result of a call to \code{\link{fisher}}
\item An object of class \verb{'Laspeyres'} is usually a result of a call to \code{\link{laspeyres}}
\item An object of class \verb{'Lowe'} is usually a result of a call to \code{\link{lowe}}
\item An object of class \verb{'Malmquist'} is usually a result of a call to \code{\link{malm}}
\item An object of class \verb{'Paasche'} is usually a result of a call to \code{\link{paasche}}}
}

\value{The function returns a data frame containing all the elements and observations 
included in the \code{"Levels"} constituent element of \code{object}.}

\author{Yann Desjeux, K Hervé Dakpo, Laure Latruffe}

\seealso{For details and information on returned values, see \code{\link{fareprim}}, 
\code{\link{fisher}}, \code{\link{laspeyres}}, \code{\link{lowe}}, \code{\link{malm}}, 
or \code{\link{paasche}}.\cr
See also:\cr
- \code{\link{Changes}} for productivity and profitability change indices; and \cr
- \code{\link{Shadowp}} for shadow prices.}

\examples{
\dontrun{
  LOWE <- lowe(data = usagri, id.var = "States", time.var = "Years", x.vars = c(7:10), 
  y.vars = c(4:6), w.vars = c(14:17), p.vars = c(11:13), scaled = TRUE)
    Lowe.levels <- Levels(LOWE)
    dim(Lowe.levels)
    head(Lowe.levels)
}
}

\keyword{manip}
