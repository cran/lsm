% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confint.lsm.R

\name{confint.lsm}
\alias{confint.lsm}
\title{
Confidence Intervals for lsm Objects
}

\description{
Provides a confint method for \code{lsm}  objects.
}

\usage{
## S3 method for class \code{lsm}
\method{confint}{lsm}(object, parm, level =0.95, ...)
}

\arguments{
  \item{object}{a lsm object}
  \item{parm}{parameter}
   \item{level }{confidence levels}
   \item{\dots}{additional parameters}
}

\value{
An object of class \code{lsm} analysis.
}

\examples{
# Hosmer, D. (2013) page 3: Age and coranary Heart Disease (CHD) Status of 20 subjects:

 AGE <- c(20, 23, 24, 25, 25, 26, 26, 28, 28, 29, 30, 30, 30, 30, 30, 30, 30, 32, 33, 33)
 CHD <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
 data <- data.frame (CHD, AGE)
 Ela <- lsm(CHD ~ AGE, family = binomial, data)
confint(Ela)
}

\references{
[1] Humberto Jesus Llinas. (2006). Accuracies in the theory of the logistic models. Revista Colombiana De Estadistica,29(2), 242-244.

[2] Hosmer, D. (2013). Wiley Series in Probability and Statistics Ser. : Applied Logistic Regression (3). New York: John Wiley & Sons, Incorporated.

[3] Chambers, J. M. and Hastie, T. J. (1992) Statistical Models in S. Wadsworth & Brooks/Cole.

}

\author{Jorge Villalba Acevedo}

\keyword{ confint.lsm }


 

