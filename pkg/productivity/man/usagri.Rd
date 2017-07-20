\name{usagri}
\alias{usagri}
\docType{data}
\title{Price indices and implicit quantities of USA farm outputs and inputs by State, 1995-2004}
\description{
This data set from the United States Department of Agriculture (USDA) and its 
Economic Research Service department contains USA agriculture's input and output 
quantities along with their respective price indices for 48 States.

All quantities are expressed in thousand US$1996 and prices are relative to 
Alabama 1996 = 1.}
\usage{usagri}
\format{
  A data frame with 480 observations on the following 17 variables.
  \describe{
    \item{\code{States}}{48 States of the USA identified with two capital letters.}
    \item{\code{States.num}}{State number.}
    \item{\code{Years}}{Year.}
    \item{\code{q.livestock}}{Livestock and animal products' quantity, in thousand US$1996.}
    \item{\code{q.crop}}{Crops' quantity, in thousand US$1996.}
    \item{\code{q.other}}{Other farm-related productions' quantity, in thousand US$1996.}
    \item{\code{q.capital}}{Capital services' quantity, in thousand US$1996.}
    \item{\code{q.land}}{Land services' quantity, in thousand US$1996.}
    \item{\code{q.labor}}{Labor services' quantity, in thousand US$1996.}
    \item{\code{q.materials}}{Total intermediate input quantity, in thousand US$1996.}
    \item{\code{p.livestock}}{Livestock and animal products' relative price (1 = Alabama 1996).}
    \item{\code{p.crop}}{Crops' relative price (1 = Alabama 1996).}
    \item{\code{p.other}}{Other farm-related productions' relative price (1 = Alabama 1996).}
    \item{\code{p.capital}}{Capital services' relative price (1 = Alabama 1996).}
    \item{\code{p.land}}{Land service flows' relative price (1 = Alabama 1996).}
    \item{\code{p.labor}}{Labor services' relative price (1 = Alabama 1996).}
    \item{\code{p.materials}}{Total intermediate inputs' relative price (1 = Alabama 1996).}
  }
}
\details{
Further details on the data and the different variables can be found in the references.}
\source{
\url{http://www.ers.usda.gov/data-products/agricultural-productivity-in-the-us.aspx}
}
\references{
Ball V.E., Gollop F.M., Kelly-Hawke A., and Swinand G.P. (1999), Patterns of state 
productivity growth in the US farm sector: Linking state and aggregate models. 
\emph{American Journal of Agricultural Economics}, \bold{81}, 164--179.

Ball V.E., Hallahan C., and Nehring R. (2004), Convergence of productivity: An 
analysis of the catch-up hypothesis within a panel of states. \emph{American Journal 
of Agricultural Economics}, \bold{86}(5), 1315--1321.
}
\examples{
head(usagri)
str(usagri)
summary(usagri)
}

\keyword{datasets}
