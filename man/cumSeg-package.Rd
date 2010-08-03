\name{cumSeg-package}
\alias{cumSeg-package}
\alias{cumSeg}
\docType{package}
\title{
Change point detection and estimation in genomic sequences
}
\description{
Estimation of number and location of change points in `mean-shift'
	(`piecewise constant' or `step-function') models. 
  Particularly useful to model genomic sequences of continuous measurements.
  }
\details{
\tabular{ll}{
Package: \tab cumSeg\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-08-03\cr
License: \tab What license is it under?\cr
LazyLoad: \tab yes\cr
}
Package \code{cumSeg} estimates the number and location of change points in `mean-shift'
	(also said `piecewise constant' or `step-function') models. These models are particularly useful in biology where 
  it is of interest to know the location of some genomic sequences (e.g. in array comparative genomic hybridization analysis).
  The algorithm works by first estimating an high number of change points (via the efficient `segmented' algorithm of Muggeo (2003)) 
  and then by applying the \emph{lars} algorithm of Efron et al. (2004) to select some of them via a generalized BIC criterion. 
  The procedure appears to be robust to model mis-specifications and 
  from a computational standpoint, it is substantially independent of the number of change points to be estimated.
}
\author{
Vito M.R. Muggeo \email{vito.muggeo@unipa.it}
%##Maintainer: Vito M.R. Muggeo <vito.muggeo@unipa.it>
}
\references{
Muggeo, V.M.R., Adelfio, G., Efficient change point detection for genomic sequences 
  of continuous measurements, \emph{submitted}.

Efron, B., Hastie, T., Johnstone, I., Tibshirani, R. (2004) Least angle regression, 
\emph{Annals of Statistics} \bold{32}, 407-489.

Muggeo, V.M.R. (2003) Estimating regression models with unknown break-points.
    \emph{Statistics in Medicine} \bold{22}, 3055-3071.

}
\keyword{ package }
\keyword{ models }
\seealso{
\code{\link[DNAcopy]{DNAcopy} \link[tilingArray]{tilingArray}}
}
\examples{
\dontrun{
library(cumSeg)
data(fibroblast)
#select chromosomes 1.. but the same for chromosomes 3,9,11
z<-na.omit(fibroblast$gm03563[fibroblast$Chromosome==1]) 
o<-jumpoints(z,k=30,output="3")
plot(z)
plot(o,add=TRUE,y=FALSE,col=4)
}
}
