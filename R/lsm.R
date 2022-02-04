# lsm.R

#' Estimation of the log Likelihood of the Saturated Model

#' @title Estimation of the log Likelihood of the Saturated Model
#' @description When the values of the outcome variable \code{Y} are either 0 or 1, the function \code{lsm()} calculates the estimation of the log likelihood in the saturated model. This model is characterized by Llinas (2006, ISSN:2389-8976) in section 2.3 through the assumptions 1 and 2. If \code{Y} is dichotomous and the data are grouped in \code{J} populations, it is recommended to use the function \code{lsm()} because it works very well for all \code{K}.

#' @param formula An expression of the form y ~ model, where y is the outcome variable (binary or dichotomous: its values are 0 or 1).
#' @param family an optional funtion for example binomial.
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which \code{lsm()} is called.
#' @return  \code{lsm} returns an object of class "\code{lsm}".
#'
#' An object of class "\code{lsm}" is a list containing at least the
#'  following components:
#'  
#' \item{coefficients}{Vector of coefficients estimations.} 
#'
#' \item{Std.Error}{Vector of the coefficients’s standard error.} 
#'
#' \item{ExpB}{Vector with the exponential of the coefficients.} 
#'
#' \item{Wald}{Value of the Wald statistic.} 
#'
#' \item{DF}{Degree of freedom for the Chi-squared distribution.} 
#'
#' \item{P.value}{P-value with the Chi-squared distribution. } 
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
#' \item{Df_Logit_vs_Saturate}{ Degree of freedom for the test statistic’s distribution (Hypothesis: Logistic vs saturated models)} 
#'
#' \item{P.v_Null_vs_Logit}{\code{p-values} for the hypothesis test: null vs logistic models.} 
#'
#' \item{P.v_Logit_vs_Complete }{\code{p-values} for the hypothesis test:  logistic vs complete models.} 
#'
#' \item{P.v_Logit_vs_Saturate}{\code{p-values} for the hypothesis test: logistic vs saturated models.} 
#'
#' \item{Logit}{Vector with the log-odds.} 
#'
#' \item{p_hat}{Vector with the probabilities that the outcome variable takes the value 1, given the \code{jth} population.} 
#'
#' \item{odd}{Vector with the values of the odd in each \code{jth} population.}
#' 
#' \item{OR}{Vector with the values of the odd ratio for each coefficient of the variables.}
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
#' \item{mcov}{Variance and covariance matrix for coefficient estimates.}
#'
#' \item{mcor}{Correlation matrix for coefficient estimates.}
#'
#' \item{Esm}{Estimates in the saturated model.}
#' 
#' \item{Elm}{Estimates in the logistic model.}
#'
#' @encoding UTF-8
#' @details The saturated model is characterized by the assumptions 1 and 2 presented in section 2.3 by Llinas (2006, ISSN:2389-8976).
#' @references [1] Humberto Jesus Llinas. (2006). Accuracies in the theory of the logistic models. Revista Colombiana De Estadistica,29(2), 242-244.
#' @references [2] Hosmer, D. (2013). Wiley Series in Probability and Statistics Ser. : Applied Logistic Regression (3). New York: John Wiley & Sons, Incorporated.
#' @references [3] Chambers, J. M. and Hastie, T. J. (1992) Statistical Models in S. Wadsworth & Brooks/Cole.
#' @author Humberto Llinas Solano [aut], Universidad del Norte, Barranquilla-Colombia \\ Omar Fabregas Cera [aut], Universidad del Norte, Barranquilla-Colombia \\ Jorge Villalba Acevedo [cre, aut], Universidad Tecnológica de Bolívar, Cartagena-Colombia.
#' @examples
#' # Hosmer, D. (2013) page 3: Age and coranary Heart Disease (CHD) Status of 20 subjects:
#'
#' #library(lsm)
#'
#' #AGE <- c(20,23,24,25,25,26,26,28,28,29,30,30,30,30,30,30,30,32,33,33)
#' #CHD <- c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
#'
#' #data <- data.frame (CHD,  AGE )
#' #lsm(CHD ~ AGE , family=binomial, data)
#'
#' ## For more ease, use the following notation.
#'
#' #lsm(y~., data)
#'
#' # Other case.
#'
#' #y <- c(1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1)
#' #x1 <- c(2, 2, 2, 5, 5, 5, 5, 8, 8, 11, 11, 11)
#'
#' #data <- data.frame (y, x1)
#' #ELAINYS <-lsm(y ~ x1, family=binomial, data)
#' #summary(ELAINYS)
#'
#' # Other case.
#' 
#' #y <- as.factor(c(1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1))
#' #x1 <- as.factor(c(2, 2, 2, 5, 5, 5, 5, 8, 8, 11, 11, 11))
#'
#' #data <- data.frame (y, x1)
#' #ELAINYS1 <-lsm(y ~ x1, family=binomial, data)
#' #confint(ELAINYS1)
#' 
#' @export

lsm <- function(formula, family=binomial, data = environment(formula))
{
  
  
  ndata <- as.data.frame(data)
  
  if(anyNA(ndata)==TRUE){
    ndata = ndata[complete.cases(ndata),]
  }
  
  mf <- model.frame(formula = formula, data = ndata)
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  y <- model.response(mf)
  X <- model.matrix(mf)
  varclass <- (attr(mt,"dataClasses"))[-1]
  
  xnames <- names(attr(mt,"dataClasses"))[-1] 
  
  yy <- unique(mf[,1])
  u <- NULL
  dat <- as.data.frame(mf)
  a <- dim(dat)[2]
  
  for(i in 1:length(yy)){
    u[[i]] <- ifelse(mf[,1] == yy[i], 1, 0)
    dat[,i+a] <- u[[i]]
  }
  
  dat[,-1] %>%
    group_by(mf[, names(mf)[-1], drop = FALSE]) %>%
    count() -> nj
  
  dat[,-1] %>%
    group_by(mf[, names(mf)[-1], drop = FALSE]) %>%
    summarize(across(everything(),list(z=sum, p=mean)), .groups = 'drop')  -> zz
  
  
  zj <- select(zz, contains("z"))
  pj <- select(zz, contains("p"))
  zj <- as.matrix(zj[2])
  pj <- as.matrix(pj[2])
  colnames(zj) <- " "
  colnames(pj) <- " "
  nn <- as.matrix(nj["n"])
  nj <- as.matrix(nj)
  
  Lp = ifelse(zj == 0 | zj == nn, 0, zj*log(pj)+(nn-zj)*log(1-pj))
  
  Sat <- sum(Lp)
  
  J <- dim(nj)[1]
  
  tab1 <- cbind(nj,zj,pj,Lp)
  colnames(tab1)[dim(nj)[2]:dim(tab1)[2]] <-c("nj","zj","pj","Lp") 
  tab1
  
  #vj <- pj * (1 - pj)
  #mj <- nj * pj
  #Vj <- nj * vj
  #V <- diag(vj)
  ####################################################
  #sp <- as.matrix((zj - nj * pj)/ vj)
  #ip <- diag(nj / vj)
  #Zj <- (zj - nj * pj) / sqrt(nj * vj)
  
  if(! is.factor(y)) y <- as.factor(y)
  y       <- unclass(y)
  ylevels <- levels(y)
  y   <- as.integer(y) - 1
  #names(y) <- names(mf)[1]
  
  Media <- mean(y)
  n1 <- length(mf[, 1])
  Nul <- n1 * (Media * log(Media) + (1 - Media) * log(1 - Media))
  Nul
  
  Com  <- 0
  
  fmula <- as.formula(paste("y ~ ", paste(xnames, collapse= "+")))
  
  coef <- coefficients(glm(fmula, family , mf))
  
  B <- as.matrix(coef)
  
  #X <- as.matrix(cbind(1,mf[,-1]))
  
  if(anyNA(B)==TRUE){
    aa <- which(is.na(B))
    B <- na.omit(B)
    X <- X[,-aa]
  }
  
  cod <-  function(x){ 
    
    if (is.factor(x) | is.character(x))
      x <- as.factor(x)
    x <- unclass(x)
    
  }
  
  x_n <-  as.data.frame(sapply(mf, cod))
  
  x_n %>%
    group_by(x_n[, names(mf)[-1], drop = FALSE]) %>%
    count() -> ncj
  
  if(anyNA(B)==TRUE){
    aa <- which(is.na(B))
    B <- na.omit(B)
    X <- X[,-aa]
  }
  
  g_t <- X %*% B
  p_ <- function(g_){exp(g_t)/(1+exp(g_t))}
  p_t <- p_(g_t)
  q_i <- 1 - p_t
  Logi <- sum((y * log(p_t) + (1 - y) * log(q_i)))
  n <- length(p_t)
  
  I <- p_t * q_i
  h <- as.vector(I)
  V <- diag(h, length(h))
  o <-(t(X) %*% V %*% X)
  varB <- solve(o)
  dimnames(varB)<- list(names(coef),names(coef))
  
  SEBj <- (diag(varB))^(1/2)
  names(SEBj) <- names(coef)
  
  out1 <- dim(nj)[2]
  
  if (is.factor(varclass) | is.character(varclass)){
    
    njj = unique(X)
    
    g_gj <- njj %*% B
    p2_ <- function(g_){exp(g_gj) / (1 + exp(g_gj))}
    p_gj <- p2_(g_gj)
    Lgj <- zj * g_gj - ncj[,"n"] * log(1 + exp(g_gj))
    
    cov <- varB[1,2]
    
    V_Logit <- 2*ncj[,1]*cov + ncj[,1]^2*SEBj[2]^2 + SEBj[1]^2
    
    SE_Logit <- sqrt(V_Logit)
    
    tab2 <- cbind(nj, zj, g_gj, p_gj, Lgj, V_Logit)
    colnames(tab2)[dim(nj)[2]:dim(tab2)[2]] <-c("nj","zj","gj","pj","Lj","Var.logit") 
    
  } else{
    
    njj = as.matrix(cbind(1,nj[,-out1])) 
    
    g_gj <- njj %*% B
    p2_ <- function(g_){exp(g_gj) / (1 + exp(g_gj))}
    p_gj <- p2_(g_gj)
    Lgj <- zj * g_gj - nj[,"n"] * log(1 + exp(g_gj))
    
    cov <- varB[1,2]
    
    V_Logit <- 2*nj[,1]*cov + nj[,1]^2*SEBj[2]^2 + SEBj[1]^2
    
    SE_Logit <- sqrt(V_Logit)
    
    tab2 <- cbind(nj, zj, g_gj, p_gj, Lgj, V_Logit)
    colnames(tab2)[dim(nj)[2]:dim(tab2)[2]] <-c("nj","zj","gj","pj","Lj","Var.logit") 
  }
  
  
  #W <- t(B) %*% o %*% B
  z <- (B/SEBj)^2
  #p_vz <- 2*pnorm(abs(z), lower.tail=FALSE)
  d_f <- (rep(1, length(z)))
  P_valor <- 1 - pchisq(z, d_f)
  exb <- exp(B)
  OR <- exp(coef[-1])
  odd <- p_t/(1 - p_t)
  #Comparativo de Modelos#
  
  #LogÃ­stico vs Completo#
  k <-1
  Dvu <- 2*(Com - Logi)
  gu <- (n-(k + 1))
  p_vu <- pchisq(c(Dvu), df=gu, lower.tail=FALSE)
  #Decisi1<-ifelse(p_val1<0.05,"Se rechaza H_0","No se rechaza H_0")
  
  #Nulo vs LogÃ­stico#
  Dvd <- 2*(Logi - Nul)
  p_vd <- pchisq(c(Dvd), df=k, lower.tail=FALSE)
  #Decisi<-ifelse(p_val<0.05,"Se rechaza H_0","No se rechaza H_0")
  
  #LogÃ­tico vs Saturado#
  
  Dvt <- 2*(Sat - Logi)
  gt <- (J-(k + 1))
  p_vt <- pchisq(c(Dvt), df=gt, lower.tail=FALSE)
  #Decisi2<-ifelse(p_val2<0.05,"Se rechaza H_0","No se rechaza H_0")
  
  Ela <- list(coefficients = B,
              coef = coef,
              Std.Error = SEBj,
              ExpB = exb,
              Wald = z,
              DF = d_f,
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
              odd   = odd,
              OR = OR,
              ########################################
              n_j = nj,
              z_j = zj, 
              p_j = pj, 
              L_p = Lp,
              Esm = tab1,
              Elm = tab2,
              ########################################
              mcov = varB,
              mcor = cor(varB)
              )
  
  Ela$call <- match.call()
  class(Ela) <- "lsm"
  return(Ela)
  
}

#' @export
 print.lsm <- function(x, ...)
{
  TB <- cbind(x$coefficients, x$Std.Error,  x$ExpB)
  colnames(TB) <- c("CoefB", "Std.Error", "ExpB")
  
  cat("\nCall:\n")
  print(x$call)
  
  cat("\nPopulations in Saturate Model: ", x$Populations, "\n", sep = "")
  
  cat("\nCoefficients: \n",  sep = "")
  
  if(anyNA(x$coef)==TRUE){
    cat("(", sum(is.na(x$coef)), " not defined because of singularities)\n", sep = "")
}
  
  print(TB, P.values=TRUE, has.Pvalue=TRUE)
  
  cat("\nLog_Likelihood: \n")
  LL <- cbind(x$Log_Lik_Complete, x$Log_Lik_Null, x$Log_Lik_Logit, x$Log_Lik_Saturate)
  dimnames(LL) <- list("Estimation", c("Complete", "Null", "Logit", "Saturate"))
  print(t(LL))
  
  if(anyNA(x$data)==TRUE){
    cat("(",nrow(x$data) - nrow(na.omit(x$data)) , " observations deleted due to missingness)\n", sep = "")
  }
}



