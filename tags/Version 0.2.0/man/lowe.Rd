\name{lowe}
\alias{lowe}
\title{Lowe productivity and profitability indices and components}
\description{This function estimates and decomposes  Lowe productivity and 
profitability levels and change indices using Data Envelopment Analysis (DEA).}

\usage{lowe(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars, 
tech.change = TRUE, tech.reg = TRUE, rts = c("vrs", "crs", "nirs", "ndrs"), 
orientation = c("out", "in", "in-out"), cores = detectCores() - 1, scaled = FALSE, 
indices = FALSE, by.id = NULL, by.year = NULL, out.levels = NULL, out.indices = NULL)}

\arguments{
  \item{data}{A dataframe containing the required information for productivity and 
  profitability assessment.}
  \item{id.var}{Number or text string which is the identifier variable of the evaluated firms.}
  \item{time.var}{Number or text string which represents the time period variable.}
  \item{x.vars}{Input quantity variables. Can be a vector of text strings or integers.}
  \item{y.vars}{Output quantity variables. Can be a vector of text strings or 
  integers.}
  \item{w.vars}{Input price variables. Can be a vector of text strings or integers.}
  \item{p.vars}{Output price variables. Can be a vector of text strings or integers.}
  \item{tech.change}{Logical. If \code{TRUE} (default), the model allows for technological change. See also the \code{Details} section.}
  \item{tech.reg}{Logical. If \code{TRUE} (default), the model allows for negative technological change (i.e. technological regress). 
  See also the \code{Details} section.}
  \item{rts}{Character string specifying the returns to scale assumption to be considered. 
  The default value is \code{"vrs"} (variable returns to scale). Other possible options 
  are \code{"crs"} (constant returns to scale), \code{"nirs"} (non-increasing returns to scale), 
  or \code{"ndrs"} (non-decreasing returns to scale).}
  \item{orientation}{Character string specifying the orientation to be considered. 
  The default value is \code{"out"} (output orientation). Other possible options 
  are \code{"in"} (input orientation), and \code{"in-out"} (both  input and output orientations). 
  For this latter, the geometric mean of input and output orientations is returned.}
  \item{cores}{Integer. Specifies the number of cores to be used for parallel computing. By default, 
  cores is set to the total number of CPU cores available from the machine, minus one. 
  It is automatically set to one in cases where the machine is made of one core only. 
  However, when the sample size is small it is recommended to set \code{cores} to one.}
  \item{scaled}{Logical. Default is \code{FALSE}. When set to \code{TRUE}, the input and output 
  quantities are rescaled, so that they all have unit means. See also the \code{Details} section.}
  \item{indices}{Logical. If \code{TRUE}, \code{lowe} returns, in addition to the 
  levels, indices that reflect changes in a measure. Default is \code{FALSE}. See also the \code{Details} section.}
  \item{by.id}{Optional. Integer specifying the reference observation used for computing 
  the indices. \code{by.id} must be comprised between one and the total number of firms per period. 
  To be specified if \code{indices = TRUE}. See also the \code{Details} section.}
  \item{by.year}{Optional. Integer specifying the reference year used for computing the indices. 
  \code{by.year} must be comprised between one and the total number of time periods. 
  To be specified if \code{indices = TRUE}. See also the \code{Details} section.}
  \item{out.levels}{A connection, or a character string naming the output file to write 
  productivity and profitability levels to, in a CSV format (See \code{\link[utils]{write.csv}}).
  If it does not contain an absolute path, the file name is relative to the current working directory, \code{\link[=getwd]{getwd()}}.}
  \item{out.indices}{A connection, or a character string naming the output file to write 
  productivity and profitability indices to, in a CSV format (See \code{\link[utils]{write.csv}}).
  If it does not contain an absolute path, the file name is relative to the current working directory, \code{\link[=getwd]{getwd()}}.}
}

\details{
This function \code{lowe()} computes Lowe productivity and profitability levels and change indices using 
a parallel backend (\pkg{doParallel} and \pkg{foreach} packages). 
The \code{cores} option can be used to specify the number of cores to use for the 
parallel computing. However, when the sample size is small, we recommend to set 
\code{cores} to one .

All DEA linear programs are implemented using the package \pkg{Rglpk}.

The \code{tech.change} option can totally prohibit any technological change. When \code{tech.change} 
is set to \code{FALSE}, this cancels the effect of \code{tech.reg} whatever the value of this latter.
The \code{tech.reg} option, when set to \code{FALSE}, rules out negative technological change (i.e. technological regress). 
In this case technological change will increment between consecutive periods. 

The \code{scaled} option is useful when working with very large and/or very small values. 
By default this option value is \code{FALSE}. However, in the case where \code{scaled = FALSE}, \code{lowe()} may issue a 
warning when large (small) values are present in the input and output quantity variables. 
The Lowe index may be sensitive to the rescaling especially the mix efficiency component.

When \code{indices} is set to \code{TRUE}, \code{lowe()} returns a list of two dataframes: 
the first one contains the productivity and profitability levels, while the second one includes the 
corresponding change indices. By default \code{lowe()} returns a list with a single dataframe 
of productivity and profitability levels.

By default \code{by.id = NULL} and \code{by.year = NULL}. Hence, when \code{indices = TRUE}, 
each observation is then compared to itself in the first period. The user can easily modify these 
options by specifying a particular observation in a particular period as the reference. When the user specifies an 
observation in \code{by.id} and when \code{by.year = NULL}, then the reference observation is \code{by.id} in 
the first period. If \code{by.year} is specified and \code{by.id = NULL}, then each observation 
is compared to itself in the specified period of time.
}

\value{
\code{lowe()} returns a list of class \verb{"Lowe"}.
When \code{indices = TRUE}, this list contains the following dataframes:

\item{Levels}{Several elements are provided in this dataframe along with \code{id.var} and \code{by.year} 
depending on the chosen orientation set through \code{orientation}: 
\tabular{ll}{\code{REV} \tab Revenues\cr
\code{COST} \tab Costs\cr 
\code{PROF} \tab Profitability ratio\cr
\code{P} \tab Aggregated output prices\cr
\code{W} \tab Aggregated input prices\cr
\code{TT} \tab Terms of trade (i.e. \code{P/W})\cr
\code{AO} \tab Aggregated outputs\cr
\code{AI} \tab Aggregated inputs\cr
\code{TFP} \tab Total Factor Productivity\cr
\code{MP} \tab Maximum productivity\cr
\code{TFPE} \tab TFP efficiency score\cr
\code{OTE} \tab Output-oriented technical efficiency score\cr
\code{OSE} \tab Output-oriented scale efficiency score\cr
\code{OME} \tab Output-oriented mix efficiency score\cr
\code{ROSE} \tab Residual output-oriented scale efficiency score\cr
\code{OSME} \tab Output-oriented scale-mix efficiency score\cr
\code{ITE} \tab Input-oriented technical efficiency score\cr
\code{ISE} \tab Input-oriented scale efficiency score\cr
\code{IME} \tab Input-oriented mix efficiency score\cr
\code{RISE} \tab Residual input-oriented scale efficiency score\cr
\code{ISME} \tab Input-oriented scale-mix efficiency score\cr
\code{OTE.ITE} \tab Geometric mean of \code{OTE} and \code{ITE}\cr
\code{OSE.ISE} \tab Geometric mean of \code{OSE} and \code{ISE}\cr
\code{OME.IME} \tab Geometric mean of \code{OME} and \code{IME}\cr
\code{ROSE.RISE} \tab Geometric mean of \code{ROSE} and \code{RISE}\cr
\code{OSME.ISME} \tab Geometric mean of \code{OSME} and \code{ISME}\cr
\code{RME} \tab Residual mix efficiency score}
}

\item{Indices}{Only provided if \code{indices = TRUE}. This dataframe contains the change indices 
of the different elements computed in \code{Levels}. Each element is named with a prefix 
"d" in front of the level name. For instance profitability change is named \code{dPROF} and 
output-oriented efficiency change is named \code{dOTE}.}
}

\references{
C.J. O'Donnell C.J. (2008), An aggregate quantity-price framework for measuring 
and decomposing productivity and profitability change. School of Economics, 
University of Queensland, Australia.

C.J. O'Donnell C.J. (2011), The sources of productivity change in the manufacturing 
sectors of the U.S. economy. School of Economics, University of Queensland, Australia.

C.J. O'Donnell C.J. (2012), Nonparametric estimates of the components of productivity 
and profitability change in U.S. aAgriculture. \emph{American Journal of Agricultural 
Economics}, \bold{94}(4), 873--890.}

\author{K Hervé Dakpo, Yann Desjeux, Laure Latruffe}

\note{All output efficiency scores are estimated \emph{a la} Shephard, while all input
efficiency scores are estimated \emph{a la} Farrell.
Hence, all efficiency scores are greater than zero and are lower or equal to one.}

\section{Warning}{The \code{lowe()} function might not properly work with unbalanced 
panel data, so the user should make sure to use a balanced panel.}

\seealso{See also \code{\link{fareprim}}
}

\examples{
# profitability and productivity levels and indices' computations
\dontrun{
Lowe.prod <- lowe(data = usagri, id.var = "States", time.var = "Years", 
x.vars = c(7:10), y.vars = c(4:6), w.vars = c(14:17), p.vars = c(11:13), 
orientation = "in-out", scaled = TRUE, indices = TRUE, by.id = 1, by.year = 1)
  summary(Lowe.prod[["Levels"]])
  summary(Lowe.prod[["Indices"]])
}
}
