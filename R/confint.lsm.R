#' confint Method for lsm 
#' 
#' @title  Confidence Intervals for \code{lsm}  Objects
#' @description Provides a confint method for \code{lsm}  objects.
#' @param object The type of prediction required. The default is on the scale of the linear predictors. 
#' The alternative \code{response} gives the predicted probabilities. 
#' @param parm further arguments passed to or from other methods.
#' @param level The type of prediction required. The default is on the scale of the linear predictors. 
#' The alternative \code{response} gives the predicted probabilities. 
#' @param ... further arguments passed to or from other methods.
#' @return  \code{lsm} returns an object of class "\code{lsm}".
#'
#'  An object of class "\code{lsm}" is a list containing at least the
#'  following components:
#'  
#' \item{object}{a lsm object}
#' \item{parm}{parameter}
#' \item{level }{confidence levels}
#' \item{\dots}{additional parameters}
#'  
#' @details The saturated model is characterized by the assumptions 1 and 2 presented in section 2.3 by Llinas (2006, ISSN:2389-8976).
#' @references [1] Humberto Jesus Llinas. (2006). Accuracies in the theory of the logistic models. Revista Colombiana De Estadistica,29(2), 242-244.
#' @references [2] Hosmer, D. (2013). Wiley Series in Probability and Statistics Ser. : Applied Logistic Regression (3). New York: John Wiley & Sons, Incorporated.
#' @references [3] Chambers, J. M. and Hastie, T. J. (1992) Statistical Models in S. Wadsworth & Brooks/Cole.
#' @author Jorge Villalba Acevedo [cre, aut], Cartagena-Colombia.
#' @examples
#'  #Hosmer, D. (2013) page 3: Age and coranary Heart Disease (CHD) Status of 20 subjects:
#'  #AGE <- c(20, 23, 24, 25, 25, 26, 26, 28, 28, 29, 30, 30, 30, 30, 30, 30, 30, 32, 33, 33)
#'  #CHD <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
#'  # data <- data.frame (CHD, AGE)
#'  # Ela <- lsm(CHD ~ AGE, family = binomial, data)
#'  # summary(Ela)
#' @exportS3Method confint lsm
confint.lsm <-  function(object, parm, level =0.95, ...)
{ 
  
  if ((length(level) != 1L) || is.na(level) || (level <= 
                                                0) || (level >= 1)){
    stop("'conf.level' must be a single number between 0 and 1")
  }
  
  alpha <- 1 - level
  Z <- qnorm(1 - alpha/2)
  li <- object$coefficients - Z*object$Std.Error
  ls <- object$coefficients + Z*object$Std.Error
  ret <- cbind(li, ls)
  colnames(ret) <- c("lower", "upper")
  
  sal <- list(confint = ret, ratios = exp(ret[-1,]), level = level*100)
  class(sal) <- "confint.lsm"
  return(sal)
}

#' @export
print.confint.lsm <- function(x, ...)
{
  cat( x$level, ".0%", " confidence intervals for coefficients ", "\n", sep = "")
  print(x$confint)
  
  cat("\n", x$level, ".0%", " confidence intervals for odds ratios","\n",   sep = "")
  print(x$ratios)
  
}


