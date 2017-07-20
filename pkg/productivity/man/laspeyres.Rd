\name{laspeyres}
\alias{laspeyres}
\alias{print.Laspeyres}
\title{Laspeyres productivity and profitability index}
\description{Using Data Envelopment Analysis (DEA), this function measures 
productivity and profitability in levels and changes with Laspeyres index.

Deflated shadow prices of inputs and outputs are also computed.

The Laspeyres productivity index uses the previous period prices as aggregators.}

\usage{laspeyres(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars, tech.change = TRUE, 
  tech.reg = TRUE, rts = c("vrs", "crs", "nirs", "ndrs"), orientation = c("out", 
  "in", "in-out"), parallel = FALSE, cores = max(1, detectCores() - 1), scaled = FALSE)

\method{print}{Laspeyres}(x, digits = NULL, \dots)}

\arguments{
  \item{data}{A dataframe containing the required information for measuring productivity and 
  profitability.}
  \item{id.var}{Firms' ID variable. Can be an integer or a text string.}
  \item{time.var}{Time period variable. Can be an integer or a text string.}
  \item{x.vars}{Input quantity variables. Can be a vector of text strings or integers.}
  \item{y.vars}{Output quantity variables. Can be a vector of text strings or integers.}
  \item{w.vars}{Input price variables. Can be a vector of text strings or integers.}
  \item{p.vars}{Output price variables. Can be a vector of text strings or integers.}
  \item{tech.change}{Logical. If \code{TRUE} (default), the model allows for technological change. 
  See also the \code{Details} section.}
  \item{tech.reg}{Logical. If \code{TRUE} (default), the model allows for negative technological change 
  (i.e. technological regress). See also the \code{Details} section.}
  \item{rts}{Character string specifying the returns to scale assumption. 
  The default value is \code{"vrs"} (variable returns to scale). Other possible options 
  are \code{"crs"} (constant returns to scale), \code{"nirs"} (non-increasing returns to scale), 
  or \code{"ndrs"} (non-decreasing returns to scale).}
  \item{orientation}{Character string specifying the orientation. 
  The default value is \code{"out"} (output-orientation). Other possible options 
  are \code{"in"} (input-orientation), and \code{"in-out"} (both input- and output-orientations). 
  For the latter, the geometric mean of input- and output-orientations' results is returned.}
  \item{parallel}{Logical. Allows parallel computation. If \code{FALSE} (default) the estimation is conducted 
  in sequential mode. If \code{TRUE} parallel mode is activated using the number of cores specified in \code{cores}.}
  \item{cores}{Integer. Used only if \code{parallel = TRUE}. It specifies the number of cores to be used 
  for parallel computation. By default, \code{cores = max(1, detectCores() - 1)}.}
  \item{scaled}{Logical. Default is \code{FALSE}. When set to \code{TRUE}, the input and output 
  quantities are rescaled. See also the \code{Details} section.}
  \item{x}{An object of class \verb{'Laspeyres'}.}
  \item{digits}{The minimum number of significant digits to be printed in values.
  Default = \code{max(3, getOption("digits") - 3)}.} 
  \item{\dots}{Currently not used.} 
}

\details{
\code{laspeyres()} allows for parallel computations (when \code{parallel = TRUE}, possibly by 
registering a parallel backend (\pkg{doParallel} and \pkg{foreach} packages)). The \code{cores} argument 
can be used to specify the number of cores to use. However, when the sample size is small, it is 
recommended to keep the \code{parallel} option to its default value (\code{FALSE}).

All DEA linear programs are implemented using the package \pkg{Rglpk}.

The \code{tech.change} option allows to prohibit technological change. When \code{tech.change} 
is set to \code{FALSE}, this overrides the effect of \code{tech.reg}. The \code{tech.reg} option, 
when set to \code{FALSE}, rules out negative technological change (i.e. technological regress). 

The \code{scaled} option is useful when working with very large (>1e5) and/or very small (<1e-4) values. 
By default \code{scaled = FALSE}. In such case, \code{laspeyres()} may issue a warning when very large 
(or very small) values are present in the input and output quantity variables.
Note that the Laspeyres index may be sensitive to the rescaling, especially the mix efficiency component.

The Laspeyres index is not transitive and therefore each firm is compared to itself in the previous period. 
Since there is no previous period for the first period, the results for this first period are replaced by \code{NA}. 
}

\value{
\code{laspeyres()} returns a list of class \verb{'Laspeyres'} for which a summary of productivity and profitability 
measures in levels and changes, as well as a summary of input (\code{x.vars}) and output (\code{y.vars}) 
deflated shadow prices, is printed.

This list contains the following items:

\item{Levels}{Several elements are provided, depending on the \code{orientation} specified:
  \tabular{ll}{
    \code{REV} \tab Revenues\cr
    \code{COST} \tab Costs\cr 
    \code{PROF} \tab Profitability\cr
    \code{P} \tab Aggregated output prices\cr
    \code{W} \tab Aggregated input prices\cr
    \code{TT} \tab Terms of trade (i.e. \code{P/W})\cr
    \code{AO} \tab Aggregated outputs\cr
    \code{AI} \tab Aggregated inputs\cr
    \code{TFP} \tab Total Factor Productivity (TFP)\cr
    \code{MP} \tab Maximum productivity\cr
    \code{TFPE} \tab TFP efficiency score\cr
    \code{OTE} \tab Output-oriented technical efficiency score\cr
    \code{OSE} \tab Output-oriented scale efficiency score\cr
    \code{RAE} \tab Revenue allocative efficiency\cr
               \tab \code{  }(equivalent to output-oriented mix efficiency score)\cr
    \code{ROSE} \tab Residual output-oriented scale efficiency score\cr
    \code{OSME} \tab Output-oriented scale-mix efficiency score\cr
    \code{ITE} \tab Input-oriented technical efficiency score\cr
    \code{ISE} \tab Input-oriented scale efficiency score\cr
    \code{CAE} \tab Cost allocative efficiency\cr
               \tab \code{  }(equivalent to input-oriented mix efficiency score)\cr
    \code{RISE} \tab Residual input-oriented scale efficiency score\cr
    \code{ISME} \tab Input-oriented scale-mix efficiency score\cr
    \code{OTE.ITE} \tab Geometric mean of \code{OTE} and \code{ITE}\cr
                   \tab \code{  }(when \code{orientation = "in-out"})\cr
    \code{OSE.ISE} \tab Geometric mean of \code{OSE} and \code{ISE}\cr
                   \tab \code{  }(when \code{orientation = "in-out"})\cr
    \code{RAE.CAE} \tab Geometric mean of \code{RAE} and \code{CAE}\cr
                   \tab \code{  }(when \code{orientation = "in-out"})\cr
    \code{ROSE.RISE} \tab Geometric mean of \code{ROSE} and \code{RISE}\cr
                     \tab \code{  }(when \code{orientation = "in-out"})\cr
    \code{OSME.ISME} \tab Geometric mean of \code{OSME} and \code{ISME}\cr
                     \tab \code{  }(when \code{orientation = "in-out"})\cr
    \code{RME} \tab Residual mix efficiency score\cr
    \code{RE} \tab Revenue efficiency\cr
              \tab \code{  }(when \code{orientation = "out"})\cr
    \code{CE} \tab Cost efficiency\cr
              \tab \code{  }(when \code{orientation = "in"})\cr
    \code{RE.CE} \tab Geometric mean of \code{RE} and \code{CE}\cr
                 \tab \code{  }(when \code{orientation = "in-out"})}
}

\item{Changes}{Change indices of the different elements of \code{Levels} are provided. 
Each change is prefixed by \code{"d"} (e.g. profitability change is denoted \code{dPROF}, 
output-oriented efficiency change is denoted \code{dOTE}, etc.). 
Each firm is compared to itself in the previous period. Since there is no previous period for the 
first period, the results for this first period are replaced by \code{NA}.}

\item{Shadowp}{The deflated cost input shadow prices and the deflated revenue output shadow prices. 
These prices are derived from dual input- and output-oriented DEA models for each observation in the sample.\cr}

From an object of class \verb{'Laspeyres'} obtained from \code{laspeyres()}, the 
\itemize{
\item \code{\link{Levels}} function extracts productivity and profitability \bold{levels};
\item \code{\link{Changes}} function extracts productivity and profitability \bold{change indices}; and 
\item \code{\link{Shadowp}} function extracts input and output \bold{deflated shadow prices}.}
}

\references{
Coelli T.J., D.S.P. Rao, C.J. O'Donnell, and G.E. Battese (2005), 
\emph{An Introduction to Efficiency and Productivity Analysis}. Springer Eds.

O'Donnell C.J. (2011), The sources of productivity change in the manufacturing 
sectors of the U.S. economy. School of Economics, University of Queensland, Australia.
URL: \url{http://www.uq.edu.au/economics/cepa/docs/WP/WP072011.pdf }
}

\author{K Hervé Dakpo, Yann Desjeux, Laure Latruffe}

\note{All output-oriented efficiency scores are computed \emph{a la} Shephard, while all 
input-oriented efficiency scores are computed \emph{a la} Farrell.
Hence, all efficiency scores are greater than zero and are lower or equal to one.}

\section{Warning}{The \code{laspeyres()} function will not work with unbalanced data.}

\seealso{See \code{\link{Levels}} to retrieve a data frame with Laspeyres 
productivity and profitability in levels and components.\cr
See \code{\link{Changes}} to retrieve a data frame with Laspeyres 
productivity and profitability changes and components.\cr
See \code{\link{Shadowp}} to retrieve deflated input and output shadow prices.\cr
See also \code{\link{fisher}} and \code{\link{paasche}} for computation with alternative indices.}

\examples{
## Laspeyres profitability and productivity levels and changes' computations
\dontrun{
  Laspeyres.prod <- laspeyres(data = usagri, id.var = "States", time.var = "Years", 
  x.vars = c(7:10), y.vars = c(4:6), w.vars = c(14:17), p.vars = c(11:13), orientation = "out", 
  scaled = TRUE)
    Laspeyres.prod
}
}

\keyword{models}
