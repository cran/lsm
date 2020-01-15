# lsm.R

#' @title Estimation of the log Likelihood of the Saturated Model
#' @description When the values of the outcome variable \code{Y} are either 0 or 1, the function \code{lsm()} calculates the estimation of the log likelihood in the saturated model. This model is characterized by Llinas (2006, ISSN:2389-8976) in section 2.3 through the assumptions 1 and 2. If \code{Y} is dichotomous and the data are grouped in \code{J} populations, it is recommended to use the function \code{lsm()} because it works very well for all \code{K}.

#' @param formula An expression of the form y ~ model, where y is the outcome variable (binary or dichotomous: its values are 0 or 1).
#' @param family an optional funtion for example binomial.
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which \code{lsm()} is called.
#' @param na.action  an optional NA.
#' @return  \code{lsm} returns an object of class "\code{lsm}".
#'
#' An object of class "\code{lsm}" is a list containing at least the
#'  following components:
#'  
#' \item{coefficients}{Vector of coefficients estimations.} 
#'
#' \item{Std.Error}{Vector of the coefficients’s standard error.} 
#'
#' \item{Exp(B)}{Vector with the exponential of the coefficients.} 
#'
#' \item{Wald}{Value of the Wald statistic.} 
#'
#' \item{D.f}{Degree of freedom for the Chi-squared distribution.} 
#'
#' \item{ P.value}{P-value with the Chi-squared distribution. } 
#'
#' \item{Log_Lik_Complete}{Estimation of the log likelihood in the complete model.} 
#'
#' \item{Log_Lik_Null}{Estimation of the log likelihood in the null model.} 
#'
#' \item{Log_Lik_Logit}{Estimation of the log likelihood in the logistic model.} 
#'
#' \item{Log_Lik_Saturate}{Estimation of the log likelihood in the saturate model.} 
#'
#' \item{Populations}{Number of populations in the saturated model.} 
#'
#' \item{Dev_Null_vs_Logit }{Value of the test statistic  (Hypothesis: null vs logistic models).} 
#'
#' \item{Dev_Logit_vs_Complete}{ Value of the test statistic  (Hypothesis:  logistic vs complete models).} 
#'
#' \item{Dev_Logit_vs_Saturate}{ Value of the test statistic  (Hypothesis: logistic vs saturated models).} 
#'
#' \item{Df_Null_vs_Logit }{Degree of freedom for the test statistic’s distribution (Hypothesis: null vs logistic models).} 
#'
#' \item{Df_Logit_vs_Complete }{ Degree of freedom for the test statistic’s distribution (Hypothesis: logistic vs saturated models).} 
#'
#'\item{Df_Logit_vs_Saturate}{ Degree of freedom for the test statistic’s distribution (Hypothesis: Logistic vs saturated models)} 
#'
#' \item{P.v_Null_vs_Logit}{\code{p-values} for the hypothesis test: null vs logistic models.} 
#'
#' \item{P.v_Logit_vs_Complete }{\code{p-values} for the hypothesis test:  logistic vs complete models.} 
#'
#' \item{P.v_Logit_vs_Saturate}{}{\code{p-values} for the hypothesis test: logistic vs saturated models.} 
#'
#' \item{Logit}{Estimation of the logit function (the log-odds)} 
#'
#' \item{p_hat}{Estimation of the probability that the outcome variable takes the value 1, given one population} 
#'
#' \item{fitted.values}{Vector with the values of the log_Likelihood in each \code{jth} population.}
#'
#' \item{z_j}{Vector with the values of each \code{Zj} (the sum of the observations in the \code{jth} population).}
#'
#' \item{n_j}{Vector with the \code{nj} (the number of the observations in each \code{jth} population).}
#'
#' \item{p_j}{Vector with the estimation of each \code{pj} (the probability of success in the \code{jth} population).}
#'
#' \item{v_j}{Vector with the variance of the Bernoulli variables in the \code{jth} population.}
#'
#' \item{m_j}{Vector with the expected values of \code{Zj} in the \code{jth} population.}
#'
#' \item{V_j}{Vector with the variances of \code{Zj} in the \code{jth} population.}
#'
#' \item{V}{Variance and covariance matrix of \code{Z}, the vector that contains all the \code{Zj}.}
#'
#' \item{S_p}{Score vector in the saturated model.}
#'
#' \item{I_p}{Information matrix in the saturated model.}
#'
#' \item{Zast_j}{Vector with the values of the standardized variable of \code{Zj}.}
#'
#' @details The saturated model is characterized by the assumptions 1 and 2 presented in section 2.3 by Llinas (2006, ISSN:2389-8976).
#' @references [1] Humberto Jesus Llinas. (2006). Accuracies in the theory of the logistic models. Revista Colombiana De Estadistica,29(2), 242-244.
#' @references [2] Hosmer, D. (2013). Wiley Series in Probability and Statistics Ser. : Applied Logistic Regression (3). New York: John Wiley & Sons, Incorporated.
#' @references [3] Chambers, J. M. and Hastie, T. J. (1992) Statistical Models in S. Wadsworth & Brooks/Cole.
#' @author Humberto Llinas Solano [aut], Universidad del Norte, Barranquilla-Colombia \\ Omar Fabregas Cera [aut], Universidad del Norte, Barranquilla-Colombia \\ Jorge Villalba Acevedo [cre, aut], Cartagena-Colombia.
#'@examples
#' # Hosmer, D. (2013) page 3: Age and coranary Heart Disease (CHD) Status of 20 subjects:
#'
#' library(lsm)
#'
#' AGE <- c(20,23,24,25,25,26,26,28,28,29,30,30,30,30,30,30,30,32,33,33)
#' CHD <- c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
#'
#' data <- data.frame (CHD,  AGE )
#' lsm(CHD ~ AGE , family=binomial, data)
#'
#' ## For more ease, use the following notation.
#'
#' lsm(y~., data)
#'
#' # Other case.
#'
#' y <- c(1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1)
#' x1 <- c(2, 2, 2, 5, 5, 5, 5, 8, 8, 11, 11, 11)
#'
#' data <- data.frame (y, x1)
#' ELAINYS <-lsm(y ~ x1, family=binomial, data)
#' summary(ELAINYS)
#'
#'
#' # Other case.
#'
#'
#'
#' y <- as.factor(c(1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1))
#' x1 <- as.factor(c(2, 2, 2, 5, 5, 5, 5, 8, 8, 11, 11, 11))
#'
#' data <- data.frame (y, x1)
#' ELAINYS1 <-lsm(y ~ x1, family=binomial, data)
#' confint(ELAINYS1)
#' 
#' @export

lsm <- function(formula, family=binomial, data, na.action)
{
  mf <- model.frame(formula = formula, data = data)
  data1 <- data
  res <- do.call(rbind, (tapply(as.vector(mf[, 1]), t(apply((mf[, -1,drop =FALSE]), 1, paste0,collapse = "-")),function(x) c(z = sum(as.numeric(x)), n = length(as.numeric(x)),p = mean(as.numeric(x))))))
  zj <- res[, 1]
  nj <- res[, 2]
  pj <- res[, 3]
  vj <- pj * (1 - pj)
  mj <- nj * pj
  Vj <- nj * vj
  V <- diag(vj)
  ####################################################
  sp <- as.matrix((zj - nj * pj)/ vj)
  ip <- diag(nj / vj)
  Zj <- (zj - nj * pj) / sqrt(nj * vj)
  sj <- (res[, 1] * log(res[, 3]) + (res[, 2] - res[, 1]) * log(1 - res[, 3]))
  Lj <- ifelse((res[, 3]) == 0 | (res[, 3]) == 1, 0, sj)
  Sat <- sum (Lj)
  Com  <- 0 
  y_n <- as.numeric_version(mf[,1])
  y_n <- as.numeric(y_n)
  Media <- mean(y_n)
  n <- length(mf[, 1])
  Nul <- n * (Media * log(Media) + (1 - Media) * log(1 - Media))
  data1 <- lapply(data, function(x){
    if (is.factor(x)) 
      x <- as.numeric_version(x) 
    x <-  as.numeric(x)
    return(x)
  })
  coef <- coefficients(glm(formula, family , data1))
  B <- as.matrix(coef)
  x_n <-  sapply(mf[,-1], function(x){
    if (is.factor(x)) 
      x <- as.numeric_version(x) 
    x <-  as.numeric(x)
    return(x)
  })
  x_n <- as.data.frame(x_n)
  
  X <- as.matrix(cbind(1,x_n))
  Bt <- as.matrix(coef)
  g_t <- X %*% Bt
  p_ <- function(g_){exp(g_t)/(1+exp(g_t))}
  p_t <- p_(g_t)
  q_i <- 1 - p_t
  Logi <- sum((y_n * log(p_t) + (1 - y_n) * log(q_i)))
  #########################################
  I <- p_t * q_i 
  h <- as.vector(I)
  V <- diag(h, length(h))
  o <-(t(X) %*% V %*% X)
  varB <- solve(o)
  SEBj <- (diag(varB))^(1/2)
  W <- t(B) %*% o %*% B
  z <- (coef/SEBj)^2
  #p_vz <- 2*pnorm(abs(z), lower.tail=FALSE)
  d_f <- (rep(1, length(z)))
  P_valor <- 1 - pchisq(z, d_f)
  exb <- exp(coef)
  OR <- exp(coef[2])
  
  #Comparativo de Modelos#
  
  #Logístico vs Completo#
  k <-1
  Dvu <- 2*(Com - Logi)
  gu <- (n-(k + 1))
  p_vu <- pchisq(c(Dvu), df=gu, lower.tail=FALSE)
  #Decisi1<-ifelse(p_val1<0.05,"Se rechaza H_0","No se rechaza H_0")
  
  #Nulo vs Logístico#
  Dvd <- 2*(Logi - Nul)
  p_vd <- pchisq(c(Dvd), df=k, lower.tail=FALSE)
  #Decisi<-ifelse(p_val<0.05,"Se rechaza H_0","No se rechaza H_0")
  
  #Logítico vs Saturado#
  J <- length(res) / 3
  Dvt <- 2*(Sat - Logi)
  gt <- (J-(k + 1))
  p_vt <- pchisq(c(Dvt), df=gt, lower.tail=FALSE)
  #Decisi2<-ifelse(p_val2<0.05,"Se rechaza H_0","No se rechaza H_0")

  Ela <- list(coefficients = coef,
              Std.Error = SEBj,
              ExpB = exb,
              Wald = z,
              D.f = d_f,
              P.value = P_valor,
              ############################################
              Log_Lik_Complete = Com,
              Log_Lik_Null  = Nul,
              Log_Lik_Logit = Logi,
              Log_Lik_Saturate = Sat,
              Populations = J,
              ########################################
              Dev_Null_vs_Logit  = Dvd,
              Dev_Logit_vs_Complete = Dvu,
              Dev_Logit_vs_Saturate = Dvt,
              Df_Null_vs_Logit = k,
              Df_Logit_vs_Complete = gu,
              Df_Logit_vs_Saturate = gt,
              P.v_Null_vs_Logit = p_vd,
              P.v_Logit_vs_Complete = p_vu,
              P.v_Logit_vs_Saturate = p_vt,
              ########################################
              Logit = g_t,
              p_hat = p_t, 
              ########################################
              z_j = as.matrix(zj), 
              n_j = nj,
              p_j = pj, 
              fitted.values = Lj, 
              ########################################
              mcov = varB,
              mcor = cor(varB),
              v_j = vj, 
              m_j = as.matrix(mj),
              V_j = Vj, 
              V = V,
              S_p = sp, 
              I_p = ip, 
              Zast_j = as.matrix(Zj))
  
  Ela$call <- match.call()
  class(Ela) <- "lsm"
  return(Ela)
}

#' @export
print.lsm <- function(x, ...)
{
  TB <- cbind(x$coefficients, x$Std.Error,  x$ExpB, x$Wald, x$D.f, x$P.value)
  colnames(TB) <- c("Coef(B)", "Std.Error", "Exp(B)", "Wald", "D.f",  "P.value")
  
  cat("\nCall:\n")
  print(x$call)
 
  cat("\nEstimated Regression Model (Maximum Likelihood) \n")
  printCoefmat(TB, P.values=TRUE, has.Pvalue=TRUE)
  
  cat("\nLog_Likelihood: \n")
  LL <- cbind(x$Log_Lik_Complete, x$Log_Lik_Null, x$Log_Lik_Logit, x$Log_Lik_Saturate)
  dimnames(LL) <- list("Estimation", c("Complete", "Null", "Logit", "Saturate"))
  print(t(LL))
  
  cat("\nPopulations in Saturate Model: ", x$Populations, "\n\n", sep = "")
}

#' @export
summary.lsm <- function(object, ...)
{
  TAB <- cbind(Deviance = c(object$Dev_Null_vs_Logit, object$Dev_Logit_vs_Complete, object$Dev_Logit_vs_Saturate),
               D.f = c(object$Df_Logit_vs_Complete, object$Df_Logit_vs_Complete, object$Df_Logit_vs_Saturate),
               P.value = c(object$P.v_Null_vs_Logit, object$P.v_Logit_vs_Complete, object$P.v_Logit_vs_Saturate))
  row.names(TAB) <-c("Null_vs_Logit", "Logit_vs_Complete", "Logit_vs_Saturate")
  
  res <- list(Call = object$call, anova=TAB)
  class(res) <- "summary.lsm"
  return(res)
}

#' @export
print.summary.lsm <- function(x, ...)
{
  cat("\nCall:\n")
  print(x$Call)
  cat("\nAnalysis of Deviance (Chi-squared): \n")
  printCoefmat(x$anova, P.values=TRUE, has.Pvalue=TRUE)
}

#' @export
confint.lsm <-  function(object, parm, level =0.95, ...)
{ 
  
  if ((length(level) != 1L) || is.na(level) || (level <= 
  0) || (level >= 1)){
    stop("'conf.level' must be a single number between 0 and 1")
    }
  
  alpha <- 1 - level
  z <- qnorm(1 - alpha/2)
  li <- object$coefficients - z*object$Std.Error
  ls <- object$coefficients + z*object$Std.Error
  ret <- cbind(li, ls)
  colnames(ret) <- c("lower", "upper")
  odds <- cbind(exp(li[-1]), exp(ls[-1]))
  colnames(odds) <- c("lower", "upper")
  sal <- list(confint=ret, ratios=odds, level = level*100)
  class(sal) <- "confint.lsm"
  print(sal)
}

#' @export
print.confint.lsm <- function(x, ...)
{
  cat( x$level, ".0%", " confidence intervals for coefficients ", "\n", sep = "")
  print(x$confint)
 
  cat("\n", x$level, ".0%", " confidence intervals for odds ratios","\n",   sep = "")
  print(x$ratios)
}


