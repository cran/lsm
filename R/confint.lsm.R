#' confint Method for lsm
#'
#' @title  Confidence Intervals for \code{lsm}  Objects
#' @description Provides a confint method for \code{lsm}  objects.
#'
#' @param object The type of prediction required. The default is on the scale of the linear predictors. The alternative \code{response} gives the predicted probabilities.
#' @param parm calculate confidence intervals for the coefficients
#' @param level  It gives the desired confidence level for the confidence interval. For example, a default value is level = 0.95, which will generate a 95% confidence interval."
#' The alternative \code{response} gives the predicted probabilities.
#' @param ... further arguments passed to or from other methods.
#' @return  \code{lsm} returns an object of class "\code{lsm}".
#'
#'  An object of class "\code{lsm}" is a list containing at least the
#'  following components:
#'
#' \item{object}{a \code{lsm} object}
#' \item{parm}{calculate confidence intervals for the coefficients.}
#' \item{level}{confidence levels}
#' \item{\dots}{Additional arguments to be passed to methods.}
#'
#' @encoding UTF-8
#' @details The saturated model is characterized by the assumptions 1 and 2 presented in section 2.3 by Llinas (2006, ISSN:2389-8976).
#' @references [1] LLinás, H. J. (2006). Precisiones en la teoría de los modelos logísticos. Revista Colombiana de Estadística, 29(2), 239–265. https://revistas.unal.edu.co/index.php/estad/article/view/29310
#' @references [2] Hosmer, D.W., Lemeshow, S. and Sturdivant, R.X. (2013). Applied Logistic Regression, 3rd ed., New York: Wiley.
#' @references [3] Chambers, J. M. and Hastie, T. J. (1992). Statistical Models in S. Wadsworth & Brooks/Cole.
#'
#' @author Jorge Villalba Acevedo [cre, aut], (Universidad Tecnológica de Bolívar, Cartagena-Colombia).
#'
#' @examples
#'  # datos <- lsm::icu
#'  # attach(datos)
#'  # modelo <- lsm(STA~AGE + as.factor(RACE), data=icu)
#'  # confint(modelo)
#'  # confint(modelo, parm = "as.factor(RACE)2", level=0.91)



#' @exportS3Method confint lsm
confint.lsm <-  function(object, parm, level = 0.95, ...){

  # Verificar si se han proporcionado argumentos adicionales
   if (length(list(...)) > 0) {
    stop("This function does not accept additional arguments. Please review the documentation.")
   }

   if ((length(level) != 1L) || is.na(level) || (level <=
                                                0) || (level >= 1)){
    stop("'conf.level' must be a single number between 0 and 1")
   }

   if ( !missing(parm) && !all(parm %in% names(object$coef))){
    stop(paste("The coefficient ", parm, " is not present in the model."))
   } else {

  if (missing(parm)){
    alpha <- 1 - level
    Z <- qnorm(1 - alpha/2)
    li <- object$coefficients - Z * object$Std.Error
    ls <- object$coefficients + Z * object$Std.Error
    ret <- cbind(li, ls)
    colnames(ret)  <- c(paste0("lower ",100*(1-level)/2,"%"), paste0("upper ",100*(1-(1-level)/2),"%"))
    sal <- list(confint = ret, OR = exp(ret[-1,]), level = level*100)
    class(sal) <- "confint.lsm"
    return(sal)

  } else {

   if (parm == "(Intercept)"){
    alpha <- 1 - level
    Z <- qnorm(1 - alpha/2)
    li <- object$coef[parm] - Z * object$Std.Error[parm]
    ls <- object$coef[parm] + Z * object$Std.Error[parm]
    ret <- cbind(li, ls)
    colnames(ret)  <- c(paste0("lower ",100*(1-level)/2,"%"), paste0("upper ",100*(1-(1-level)/2),"%"))
    sal <- list(confint = ret, OR = "-", level = level*100)
    class(sal) <- "confint.lsm"
    return(sal)

   } else {
     alpha <- 1 - level
     Z <- qnorm(1 - alpha/2)
     li <- object$coef[parm] - Z * object$Std.Error[parm]
     ls <- object$coef[parm] + Z * object$Std.Error[parm]
     ret <- cbind(li, ls)
     colnames(ret)  <- c(paste0("lower ",100*(1-level)/2,"%"), paste0("upper ",100*(1-(1-level)/2),"%"))
     sal <- list(confint = ret, OR = exp(ret), level = level*100)
     class(sal) <- "confint.lsm"
     return(sal)

   }

  }
 }
}


#' @export
print.confint.lsm <- function(x, ...)
{
  cat( x$level, ".0%", " confidence intervals for coefficients ", "\n", sep = "")
  print(x$confint)

  cat("\n", x$level, ".0%", " confidence intervals for odds ratios","\n",   sep = "")
  print(x$OR)

}


