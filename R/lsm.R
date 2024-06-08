# lsm.R

#' Estimation of the log Likelihood of the Saturated Model

#' @title Estimation of the log Likelihood of the Saturated Model
#' @description When the values of the outcome variable \code{Y} are either 0 or 1, the function \code{lsm()} calculates the estimation of the log likelihood in the saturated model. This model is characterized by Llinas (2006, ISSN:2389-8976) in section 2.3 through the assumptions 1 and 2. If \code{Y} is dichotomous and the data are grouped in \code{J} populations, it is recommended to use the function \code{lsm()} because it works very well for all \code{K}.

#' @param formula An expression of the form y ~ model, where y is the outcome variable (binary or dichotomous: its values are 0 or 1).
#' @param family an optional funtion for example binomial.
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which \code{lsm()} is called.
#' @param ... further arguments passed to or from other methods.
#'
#' @return  \code{lsm} returns an object of class "\code{lsm}".
#'
#' An object of class "\code{lsm}" is a list containing at least the
#'  following components:
#'
#' \item{coefficients}{Vector of coefficients estimations (intercept and slopes).}
#'
#' \item{coef}{Vector of coefficients estimations (intercept and slopes).}
#'
#' \item{Std.Error}{Vector of the coefficients’s standard error (intercept and slopes).}
#'
#' \item{ExpB}{Vector with the exponential of the coefficients (intercept and slopes).}
#'
#' \item{Wald}{Value of the Wald statistic (with chi-squared distribution).}
#'
#' \item{DF}{Degree of freedom for the Chi-squared distribution.}
#'
#' \item{P.value}{P-value calculated with the Chi-squared distribution. }
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
#' \item{Dev_Logit_vs_Complete}{Value of the test statistic  (Hypothesis:  logistic vs complete models).}
#'
#' \item{Dev_Logit_vs_Saturate}{Value of the test statistic  (Hypothesis: logistic vs saturated models).}
#'
#' \item{Df_Null_vs_Logit }{Degree of freedom for the test statistic’s distribution (Hypothesis: null vs logistic models).}
#'
#' \item{Df_Logit_vs_Complete }{Degree of freedom for the test statistic’s distribution (Hypothesis: logistic vs saturated models).}
#'
#' \item{Df_Logit_vs_Saturate}{Degree of freedom for the test statistic’s distribution (Hypothesis: logistic vs saturated models).}
#'
#' \item{P.v_Null_vs_Logit}{P-value for the hypothesis test: null vs logistic models.}
#'
#' \item{P.v_Logit_vs_Complete }{P-value for the hypothesis test:  logistic vs complete models.}
#'
#' \item{P.v_Logit_vs_Saturate}{P-value for the hypothesis test: logistic vs saturated models.}
#'
#' \item{Logit}{Vector with the log-odds.}
#'
#' \item{p_hat_complete}{Vector with the probabilities that the outcome variable takes the value 1, given the \code{jth} population (estimated with the complete model and  without the logistic model).}
#'
#' \item{p_hat_null}{Vector with the probabilities that the outcome variable takes the value 1, given the \code{jth} population (estimated with the null model and  without the logistic model).}
#'
#' \item{p_j}{Vector with the probabilities that the outcome variable takes the value 1, given the \code{jth} population (estimated with the logistic model).}
#'
#' \item{odd}{Vector with the values of the odd in each \code{jth} population.}
#'
#' \item{OR}{Vector with the values of the odd ratio for each coefficient of the variables.}
#'
#' \item{z_j}{Vector with the values of each \code{Zj} (the sum of the observations in the \code{jth} population).}
#'
#' \item{n_j}{Vector with the \code{nj} (the number of the observations in each \code{jth} population).}
#'
#' \item{p_j_tilde}{Vector with the estimation of each \code{pj} (the probability of success in the \code{jth} population) in the saturated model (without estimate the logistic parameters).}
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
#' \item{Esm}{Data frame with estimates in the saturated model. It contains for each population \code{j}: the value of the explanatory variables, \code{nj}, \code{Zj}, \code{pj} and Log-Likelihood \code{Lj_tilde}.}
#'
#' \item{Elm}{Data frame with estimates in the logistic model. It contains for each population \code{j}: the value of the explanatory variables, \code{nj}, \code{Zj}, \code{pj}, Log-Likelihood \code{Lj}, \code{Logit_pj} and the variance of logit (\code{var.logit}).}
#'
#' \item{call}{It displays the original call that was used to fit the model lsm.}
#'
#' \item{data}{data envarironment.}
#'
#' \item{\dots}{Additional arguments to be passed to methods.}
#'
#' @encoding UTF-8
#' @details An expression of the form \code{y ~ model} is interpreted as a specification that the response \code{y} is modelled by a linear predictor specified symbolically by \code{model} (systematic component). Such a model consists of a series of terms separated by \code{+} operators. The terms themselves consist of variable and factor names separated by \code{:} operators. Such a term is interpreted as the interaction of all the variables and factors appearing in the term. Here, \code{y} is the outcome variable (binary or dichotomous: its values are 0 or 1).
#'
#' @references [1] LLinás, H. J. (2006). Precisiones en la teoría de los modelos logísticos. Revista Colombiana de Estadística, 29(2), 239–265. https://revistas.unal.edu.co/index.php/estad/article/view/29310
#' @references [2] Hosmer, D.W., Lemeshow, S. and Sturdivant, R.X. (2013). Applied Logistic Regression, 3rd ed., New York: Wiley.
#' @references [3] Chambers, J. M. and Hastie, T. J. (1992). Statistical Models in S. Wadsworth & Brooks/Cole.
#'
#' @author Dr. rer. nat. Humberto LLinás Solano [aut] (Universidad del Norte, Barranquilla-Colombia);  MSc. Omar Fábregas Cera [aut] (Universidad del Norte, Barranquilla-Colombia); MSc. Jorge Villalba Acevedo [cre, aut] (Universidad Tecnológica de Bolívar, Cartagena-Colombia).
#'
#' @examples
#' #library(lsm)
#'
#' #1. AGE and Coronary Heart Disease (CHD) Status of 20 subjects:
#'
#'    #AGE <- c(20,23,24,25,25,26,26,28,28,29,30,30,30,30,30,30,30,32,33,33)
#'    #CHD <- c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
#'    #data <- data.frame (CHD,  AGE )
#'    #lsm(CHD ~ AGE , data)
#'
#' #2.You can use the following notation:
#'
#'    #lsm(y~., data)
#'
#' #3. Other example:
#'
#'    #y <- c(1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1)
#'    #x1 <- c(2, 2, 2, 5, 5, 5, 5, 8, 8, 11, 11, 11)
#'    #data <- data.frame (y, x1)
#'    #ELAINYS <-lsm(y ~ x1, data)
#'    #summary(ELAINYS)
#'
#' #4. Other example:
#'
#'    #y <- as.factor(c(1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1))
#'    #x1 <- as.factor(c(2, 2, 2, 5, 5, 5, 5, 8, 8, 11, 11, 11))
#'    #data <- data.frame (y, x1)
#'    #ELAINYS1 <-lsm(y ~ x1, family=binomial, data)
#'    #summary(ELAINYS1)
#' @seealso
#' \code{\link{lsm}}
#' @export


lsm <- function(formula, family=binomial, data = environment(formula), ...)
{

  # Verificar si se han proporcionado argumentos adicionales
  if (length(list(...)) > 0) {
    stop("This function does not accept additional arguments. Please review the documentation.")
  }

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
    count() -> nn

  dat[,-1] %>%
    group_by(mf[, names(mf)[-1], drop = FALSE]) %>%
    summarize(across(everything(),list(z=sum, p=mean)), .groups = 'drop')  -> zz

  zj <- select(zz, contains("z"))
  zj <- as.matrix(zj[2])
  colnames(zj) <- " "
  pj <- select(zz, contains("p"))
  pj <- as.matrix(pj[2])
  colnames(pj) <- " "
  nj <- as.matrix(nn["n"])
  colnames(nj) <- " "
  nn <- as.matrix(nn)

  Lp = ifelse(zj == 0 | zj == nj, 0, zj*log(pj)+(nj-zj)*log(1-pj))
  Sat <- sum(Lp)
  J <- dim(nj)[1]

  tab1 <- cbind(nn,zj,pj,Lp)
  colnames(tab1)[dim(nn)[2]:dim(tab1)[2]] <-c("nj","zj","pj","Lp")
  tab1

  vj <- pj * (1 - pj)
  mj <- nj * pj
  Vj <- nj * vj
  V <- diag(vj)
  sp <- ( zj - nj * pj)/ vj
  sp[is.nan(sp)] <- 0
  ip <- diag(nj / vj)
  Zj <- (zj - nj * pj) / sqrt(nj * vj)
  Zj[is.nan(Zj)] <- 0
  ####################################################

  if(! is.factor(y)) y <- as.factor(y)
  y       <- unclass(y)
  ylevels <- levels(y)
  y   <- as.integer(y) - 1
  #names(y) <- names(mf)[1]

  Media <- mean(y)
  n1 <- length(mf[, 1])
  Nul <- n1 * (Media * log(Media) + (1 - Media) * log(1 - Media))
  Nul
  ####################################################
  Com  <- 0
  ####################################################
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
  ####################################################

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
              p_j = p_t,
              p_hat_complete = y,
              p_hat_null = mean(y) ,
              odd   = odd,
              OR = OR,
              ########################################
              n_j = nj,
              z_j = zj,
              p_j_tilde = pj,
              L_p = Lp,
              v_j = vj,
              m_j = mj,
              V_j = Vj,
              V  = V ,
              S_p = sp,
              I_p = ip,
              Zast_j = Zj,
              Esm = tab1,
              Elm = tab2,
              ########################################
              mcov = varB,
              mcor = cor(varB),
              data = as.data.frame(mf),
              formula = formula
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

  if(anyNA(unlist(x$data))==TRUE){
    cat("(",nrow(x$data) - nrow(na.omit(x$data)) , " observations deleted due to missingness)\n", sep = "")
  }
}



