\name{lowbwt}
\alias{lowbwt}
\docType{data}
\title{
The Low Birth Weight Study.
}
\description{
This data set contains information on 189 birhs to women seen in the obstetrics clinic. The variables are as follows:
}
\usage{lowbwt}

\format{
  A data frame with 189 observations on the following 11 variables.
 \describe{
    \item{\code{ID}}{a numeric vector}
    \item{\code{SMOKE}}{a numeric vector}
    \item{\code{RACE}}{a numeric vector}
    \item{\code{AGE}}{a numeric vector}
    \item{\code{LWT}}{a numeric vector}
    \item{\code{BWT}}{a numeric vector}
    \item{\code{LOW}}{a numeric vector}
    \item{\code{PTL}}{a numeric vector}
    \item{\code{HT}}{a numeric vector}
    \item{\code{UI}}{a numeric vector}
    \item{\code{FTV}}{a numeric vector}
  }
}

\references{
[1] Hosmer, D. (2013). Wiley Series in Probability and Statistics Ser. : Applied Logistic Regression (3). New York: John Wiley & Sons, Incorporated.
}
\examples{
data(lowbwt)
## maybe str(lowbwt) ; plot(lowbwt) ...
}
\keyword{datasets}
