\name{malm}
\alias{malm}
\title{Malmquist productivity indices and components}
\description{This function computes Malmquist productivity indices.

When required, indices and their various components are also returned.}

\usage{malm(data, id.var, time.var, x.vars, y.vars, tech.reg = TRUE, 
rts = c("vrs", "crs", "nirs", "ndrs"), orientation = c("out", "in"), 
cores = detectCores() - 1, scaled = TRUE, indices = FALSE, out.levels = NULL, 
out.indices = NULL)}


\arguments{
  \item{data}{A dataframe containing the required information for productivity assessment.}
  \item{id.var}{Number or text string which is the identifier variable of the evaluated firms.}
  \item{time.var}{Number or text string which represents the time period variable.}
  \item{x.vars}{Input quantity variables. Can be a vector of text strings or integers.}
  \item{y.vars}{Output quantity variables. Can be a vector of text strings or integers.}
  \item{tech.reg}{Logical. If \code{TRUE} (default), the model allows for negative technological change (i.e. technological regress). 
  See also the \code{Details} section.}
  \item{rts}{Character string specifying the returns to scale assumption to be considered. 
  The default value is \code{"vrs"} (variable returns to scale). Other possible options 
  are \code{"crs"} (constant returns to scale), \code{"nirs"} (non-increasing returns to scale), 
  or \code{"ndrs"} (non-decreasing returns to scale).}
  \item{orientation}{Character string specifying the orientation to be considered. 
  The default value is \code{"out"} (output orientation). The other possible option is 
  \code{"in"} (input orientation).}
  \item{cores}{Integer. Specifies the number of cores to be used for parallel computing. By default, 
  cores is set to the total number of CPU cores available from the machine, minus one. 
  It is automatically set to one in cases where the machine is made of one core only. 
  However, when the sample size is small it is recommended to set \code{cores} to one.}
  \item{scaled}{Logical. When set to \code{TRUE} (Default), the input and output quantities are rescaled,
  so that they all have unit means. See also the \code{Details} section.}
  \item{indices}{Logical. If \code{TRUE}, \code{malm()} returns, in addition to the 
  levels, indices that reflect changes in a measure. Default is \code{FALSE}. 
  See also the \code{Details} section.}
  \item{out.levels}{A connection, or a character string naming the output file to write 
  distance function values required for the Malmquist productivity index to, in a CSV format (See \code{\link[utils]{write.csv}}).
  If it does not contain an absolute path, the file name is relative to the current working directory, \code{\link[=getwd]{getwd()}}.}
  \item{out.indices}{A connection, or a character string naming the output file to write 
  productivity indices to, in a CSV format (See \code{\link[utils]{write.csv}}).
  If it does not contain an absolute path, the file name is relative to the current working directory, \code{\link[=getwd]{getwd()}}.}
}

\details{
The function \code{malm()} computes distance function values and Malmquist indices using a parallel 
backend (\pkg{doParallel} and \pkg{foreach} packages). 
The \code{cores} option can be used to specify the number of cores to use for the 
parallel computing. However, when the sample size is small, we recommend to set 
\code{cores} to one.

All DEA linear programs are implemented using the package \pkg{Rglpk}.

The \code{tech.reg} option, when set to \code{FALSE}, rules out technological regression. 
Therefore, the technological change will increment between consecutive periods. 

The \code{scaled} option is useful when working with very large and/or very small values.
By default this option value is \code{TRUE}. When \code{scaled = FALSE}, \code{malm()} may issue a 
warning when large (small) values are present in the input and output quantity variables. 
It is worth noting that all the distance functions required for the Malmquist index estimation 
are radial measures which verify the translation invariance property. Hence, unless very large or 
very small values are present, the Malmquist index is insensitive to the rescaling option.

When \code{indices} is set to \code{TRUE} \code{malm()} returns a list of two dataframes: 
the first one contains the distance functions useful to estimate the Malmquist indices and 
components, while the second one is the Malmquist productivity indices and their decomposition. 
By default \code{malm()} returns a list with a single dataframe of intermediate distance 
function values. For the indices, each observation in period \code{t} is compared to 
itself in period \code{t-1}.
}

\value{
\code{malm()} returns a list of class \verb{"Malmquist"}.
When \code{indices = TRUE}, this list contains the following dataframes:

\item{Levels}{It contains the Shephard distance function estimates, useful to compute and decompose 
the Malmquist productivity index. These distance functions use input and output quantities 
for period \code{1} and period \code{0}.
The prefix "c" stands for constant returns to scale (\code{"crs"}) and "v" for all the other types of returns to scale.
The suffix "o" means output-oriented while "i" refers to input-oriented.
The distance function names are displayed with three digits: 
\emph{(i)} the first digit represents the period of the reference technology, \emph{(ii)} the second 
digit represents the period of the inputs, and \emph{(iii)} the third digit represents the period of the outputs. 
For instance \code{c010o} means output-oriented efficiency under constant returns to scale (CRS), 
with the reference technology of period \code{0}, inputs of period \code{1} and outputs of period \code{0}.

In addition to the \code{id.var} variable and periods \code{1} and \code{0}, the dataframe 
therefore contains, depending on the chosen orientation set through \code{orientation}: 
\code{c111o}, \code{c100o}, \code{c011o}, \code{c000o}, \code{c110o}, \code{c010o}, 
\code{c111i}, \code{c100i}, \code{c011i}, \code{c000i}, \code{c110i}, and \code{c010i}.

When the returns to scale option (\code{rts}) is different from \code{"crs"}, then \code{v111o}, 
\code{v000o}, \code{v111i} and \code{v000i} are also provided depending on the orientation.}

\item{Indices}{Only provided if \code{indices = TRUE}. This dataframe contains Malmquist 
productivity change indices and their various components depending on the chosen orientation set through \code{orientation}.

In addition to the \code{id.var} variable and periods \code{1} and \code{0}, 
the dataframe also contains, depending on the orientation: \tabular{ll}{
\code{malmquist} \tab Malmquist productivity index\cr
\code{effch} \tab Efficiency change\cr
\code{tech} \tab Technological change\cr
\code{obtech} \tab Output-biased technological change\cr 
\code{ibtech} \tab Input-biased technological change\cr
\code{matech} \tab Magnitude component\cr
\code{pure.out.effch} \tab Pure output efficiency change\cr
\code{out.scalech} \tab Output scale efficiency change\cr
\code{pure.inp.effch} \tab Pure input efficiency change\cr
\code{inp.scalech} \tab Input scale efficiency change}

\strong{Note that:}
\enumerate{
\item{\code{obtech} (\emph{Output-biased technological change}), \code{ibtech} (\emph{Input-biased technological change}), 
  and \code{matech} (\emph{Magnitude component}) are components of technological change (\code{tech}).}
\item{\code{pure.out.effch} (\emph{Pure output efficiency change}), \code{out.scalech} (\emph{Output scale efficiency change}) 
  are components of efficiency change (\code{effch}), when \code{rts != "crs"} and \code{orientation = "out"}.}
  \item{\code{pure.inp.effch} (\emph{Pure input efficiency change}), and \code{inp.scalech} (\emph{Input scale efficiency change}) 
  are components of efficiency change (\code{effch}), when \code{rts != "crs"} and \code{orientation = "in"}.}}
}
}

\references{
Färe R., and Grosskopf S. (1996), \emph{Intertemporal Production Frontiers: With 
Dynamic DEA}. Springer Eds.}

\author{K Hervé Dakpo, Yann Desjeux, Laure Latruffe}

\note{For a proper understanding of the different components of the Malmquist 
productivity, one can refer to the reference.

The Malmquist and its components are computed such that both \code{orientation}'s results can be read in the same way (growth when greater than one and decline when lower than one). Moreover under \code{rts = "crs"}, both \code{orientation} options (i.e. \code{"out"} and \code{"in"}) yield the same results.}

\section{Warning }{The \code{malm()} function does not work with unbalanced
panel data, so the user should make sure to provide a balanced panel.}


\examples{
# productivity computations
# indices compare each observation in period 1
# to the same observation in period 0
\dontrun{
Malmquist <- malm(data = usagri, id.var = "States", time.var = "Years",
x.vars = c("q.capital", "q.land","q.labor","q.materials"), 
y.vars = c("q.livestock", "q.crop", "q.other"), rts = "nirs", indices = TRUE)
  summary(Malmquist[["Levels"]])
  summary(Malmquist[["Indices"]])
}
}


\keyword{models}
