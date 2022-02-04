#' summary Method for lsm 
#' 
#' @title  Summarizing  Method for \code{lsm}  Objects
#' 
#' @description Provides a \code{summary} method for \code{lsm}  objects.
#'  
#' @param object An expression of the form y ~ model, where y is the outcome variable (binary or dichotomous: its values are 0 or 1).
#' @param ... further arguments passed to or from other methods.
#' @return  An object of class "\code{lsm}" is a list containing at least the
#'  following components:
#'
#'  \item{object}{a \code{lsm} object}
#'  \item{\dots}{additional parameters}
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
#' @exportS3Method summary lsm
summary.lsm <- function(object, ...)
{
  
  TC <- cbind(object$coefficients, object$Std.Error,  object$ExpB, object$Wald, object$DF, object$P.value)
  colnames(TC) <- c("Coef(B)", "Std.Error", "Exp(B)", "Wald", "DF",  "P.value")
  
  
  TAB <- cbind(Deviance = c(object$Dev_Null_vs_Logit, object$Dev_Logit_vs_Complete, object$Dev_Logit_vs_Saturate),
               DF = c(object$Df_Null_vs_Logit, object$Df_Logit_vs_Complete, object$Df_Logit_vs_Saturate),
               P.value = c(object$P.v_Null_vs_Logit, object$P.v_Logit_vs_Complete, object$P.v_Logit_vs_Saturate))
  row.names(TAB) <-c("Null_vs_Logit", "Logit_vs_Complete", "Logit_vs_Saturate")
  
  res <- list(Call = object$call, `comparison test`=TAB, coeff = TC)
  class(res) <- "summary.lsm"
  return(res)
}

#' @export
print.summary.lsm <- function(x, ...)
{
  cat("\nCall:\n")
  print(x$Call)
  cat("\nCoefficients: \n",  sep = "")
  printCoefmat(x$coeff, P.values=TRUE, has.Pvalue=TRUE)
  cat("\nAnalysis of Deviance (Chi-squared): \n")
  printCoefmat(x$`comparison test`, P.values=TRUE, has.Pvalue=TRUE)
  
}