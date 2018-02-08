\name{Shadowp}
\alias{Shadowp}

\title{Shadow prices used in productivity and profitability computations}

\description{From any object created by either \code{\link{fareprim}}, \code{\link{fisher}}, 
\code{\link{hicksmoorsteen}}, \code{\link{laspeyres}}, \code{\link{lowe}}, or \code{\link{paasche}} 
function, and provided that in the function's call the argument \code{shadow = TRUE}, this function 
extracts the deflated cost input and revenue output shadow prices.}

\usage{Shadowp(object, \dots)}

\arguments{
  \item{object}{Object of class \verb{'FarePrimont'}, \verb{'Fisher'}, \verb{'HicksMoorsteen'}, 
  \verb{'Laspeyres'}, \verb{'Lowe'}, or \verb{'Paasche'}.}
  \item{\dots}{Currently not used.}
}

\details{
Shadow prices are derived from dual input- and output-oriented DEA models, and may be subject to the 
linear programming solver used (\pkg{lpSolveAPI} in our case).

For Fisher, Hicks-Moorsteen, Laspeyres, Lowe and Paasche indices, deflated input and output shadow 
prices \bold{for each observation} are returned.

In case of Hicks-Moorsteen, shadow prices used to compute malmquist-\emph{hs} and malmquist-\emph{nt} are returned.

For Färe-Primont index, the deflated input and output shadow prices \bold{of the representative observation} 
are returned.

\itemize{
\item An object of class \verb{'FarePrimont'} is a result of a call to \code{\link{fareprim}}
\item An object of class \verb{'Fisher'} is a result of a call to \code{\link{fisher}}
\item An object of class \verb{'HicksMoorsteen'} is a result of a call to \code{\link{hicksmoorsteen}}
\item An object of class \verb{'Laspeyres'} is a result of a call to \code{\link{laspeyres}}
\item An object of class \verb{'Lowe'} is a result of a call to \code{\link{lowe}}
\item An object of class \verb{'Paasche'} is a result of a call to \code{\link{paasche}}}
}

\value{The function returns a data frame containing all the elements and observations 
included in the \code{"Shadowp"} constituent element of \code{object}.}

\author{Yann Desjeux, K Hervé Dakpo, Laure Latruffe}

\seealso{
- \code{\link{Changes}} for productivity and profitability change indices; and \cr
- \code{\link{Levels}} for productivity and profitability levels.}

\examples{
\dontrun{
  FISHER <- fisher(data = usagri, id.var = "States", time.var = "Years", x.vars = c(7:10), 
  y.vars = c(4:6), w.vars = c(14:17), p.vars = c(11:13), orientation = "out", scaled = TRUE, 
  shadow = TRUE)
  Fisher.shadowprices <- Shadowp(FISHER)
  head(Fisher.shadowprices)
}
}

\keyword{manip}
