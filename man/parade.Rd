\name{parade}
\alias{parade}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pen's Income Parades
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
\code{parade} produces Pen's parade graphs, useful for visualizing inequalities in income, wages or other variables. They were first proposed by Jan Pen in his well-known story of the "parade of dwarfs and a few giants" (Pen, 1971). In this story, income is captured by the vertical axis, while the population is arranged in ascending order of income ("height") along the horizontal axis. As such, Pen's income parades provide an easy-to-interpret visualization of economic inequalities (Cowell, 2000).

}
\usage{
parade(height, line.fun = NULL, line.col = "red", line.lty = 1, line.lwd = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{height}{a vector of numerical values (e.g., incomes, wages) describing the vertical bars which make up the plot.}
  \item{line.fun}{a function whose result, when applied to the data in \code{height}, will determine the position of the horizontal line. If \code{NULL}, the horizontal line will occur at the value of the arithmetic mean of the \code{height} data.}
  \item{line.col}{a specification for the horizontal line color. This can be a character string giving the color name (e.g., "blue"). Colors can also be specified in terms of their RGB components with a string of the form "#RRGGBB", in which RR, GG and BB correspond to two hexadecimal digits.}
  \item{line.lty}{the horizontal line type. It can be specified as an integer (0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash) or as one of the character strings \code{"blank"}, \code{"solid"}, \code{"dashed"}, \code{"dotted"}, \code{"dotdash"}, \code{"longdash"}, or \code{"twodash"}, where \code{"blank"} uses 'invisible lines' (i.e., does not draw them).}
  \item{line.lwd}{the horizontal line widths (a positive number).}
  \item{\dots}{additional (e.g., graphical) arguments that will be passed on to the \code{barplot} function.}
%%     ~~Describe \code{\dots} here~~
}
\section{Note:}{
No horizontal line will be drawn if any of the arguments \code{line.col}, \code{line.lty} or \code{line.lwd} are set to \code{NULL}.
}

\references{
Cowell, Frank A. (2000). Measurement of Inequality. In: Atkinson, Anthony B. and Francois Bourguignon, \emph{Handbook of Income Distribution}.  North Holland, 87-166.

Pen, Jan. (1971). \emph{Income Distribution.} Harmondsworth: Allen Lane.

}
\author{
%%  ~~who you are~~
Marek Hlavac < mhlavac at alumni.princeton.edu > \cr
Research Fellow, Central European Labour Studies Institute (CELSI), Bratislava, Slovakia \cr
}
\section{Please cite as:}{
Hlavac, Marek (2019). parade: Pen's Income Parades in R.\cr
R package version 0.1. https://CRAN.R-project.org/package=parade 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

# load data set of Hispanic workers in Chicago
data("chicago")

# generate real wages from their logged versions
chicago$real.wage <- exp(chicago$ln.real.wage)

# simple Pen's parade with a blue, dashed, thin horizontal line at the mean
parade(chicago$real.wage, line.col = "blue", line.lwd = 1, line.lty = "dashed")
}
\keyword{ income }
\keyword{ inequality }
\keyword{ visualization }

