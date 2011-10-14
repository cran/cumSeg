\name{plot.aCGHsegmented}
\alias{plot.aCGHsegmented}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot method for the class 'aCGHsegmented'
}
\description{
Plots fitted piecewise constant lines.
}
\usage{
\method{plot}{aCGHsegmented}(x, add = FALSE, y = TRUE, psi.lines = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object of class \code{"aCGHsegmented"} returned by \code{jumpoints}.
}
  \item{add}{
logical; if \code{TRUE} the fitted piecewise constant lines are added 
  to an existing plot.
}
  \item{y}{
logical; if \code{TRUE} the observations are also plotted, otherwise only the fitted lines.
}
  \item{psi.lines}{
logical; if \code{TRUE} vertical lines corresponding to the estimated changepoints are added.
}
  \item{\dots}{
possible additional graphical arguments, such as \code{col}, \code{xlab}, and so on.  
}
}
\details{
This fuction takes a fitted object returned by \code{jumpoints} and plots 
the resulting fit, namely the estimated step-function and changepoints.
}
\value{
The function simply plots the fit returned by 'jumpoints'.
}
\author{
Vito Muggeo
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{jumpoints}}
}
\keyword{ regression }
