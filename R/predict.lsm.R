#' Predict Method for lsm Fits
#' 
#' @title Predict  Method for \code{lsm}  Objects
#' 
#' @description Obtains predictions from a fitted \code{lsm} object. 
#' 
#' @param object A fitted object of class \code{lsm}.
#'
#' @param newdata Optionally, a data frame in which to look for variables with which to predict. 
#' If omitted, the fitted linear predictors are used.  
#' @param type The type of prediction required. The default is on the scale of the linear predictors. 
#' The alternative \code{response} gives the predicted probabilities. 
#' @param interval gives the
#' @param level  gives the
#' @param ... further arguments passed to or from other methods.
#'
#' @return A vector or matrix of predictions.
#'  following components:
#'
#' @rdname predict.lsm
#' @exportS3Method predict lsm
#'

predict.lsm <- function (object,
                         newdata, 
                         type = c("link", "response", "odd"), 
                         interval = c("none","confidence", "prediction", "odd"),
                         level = 0.95,...){
  
  if ((length(level) != 1L) || is.na(level) || (level <= 
                                                0) || (level >= 1)){
    stop("'conf.level' must be a single number between 0 and 1")
  }
  
  type <- match.arg(type)
  interval <- match.arg(interval)
  
  
  if ((missing(newdata) ||  is.null(newdata)) && interval == "none" ){
    pred <- switch(type, link =  object$Logit, response =  object$p_hat, odd = object$odd )
  }    
  
  if ((missing(newdata) ||  is.null(newdata)) && interval != "none" ){
    V_beta <- object$mcov[2,2] 
    V_delta  <- object$mcov[1,1]
    Cov_delta_beta <- object$mcov[1,2]
    alpha <- 1 - level
    Z <- qnorm(1 - alpha/2)
    
    V_Logit <- V_delta + ((object$Logit)^2)*V_beta + 2*(object$Logit)*Cov_delta_beta
    SE_Logit <- sqrt(V_Logit)
    
    CI_lower <- (object$Logit) - Z*SE_Logit
    CI_upper <- (object$Logit)  + Z*SE_Logit
    CI_Logit <- cbind(CI_lower, CI_upper)
    colnames(CI_Logit)  <- c("lower", "upper")
    
    V_Logit <- V_delta + ((object$p_hat )^2)*V_beta + 2*(object$p_hat )*Cov_delta_beta
    SE_Logit <- sqrt(V_Logit)
    
    CI_lower <- object$p_hat - Z*SE_Logit
    CI_upper <- object$p_hat + Z*SE_Logit
    CI_pro <- cbind(CI_lower, CI_upper)
    colnames(CI_pro)  <- c("lower", "upper")
    
    V_Logit <- V_delta + ((object$Logit)^2)*V_beta + 2*(object$Logit)*Cov_delta_beta
    SE_Logit <- sqrt(V_Logit)  
    CI_lowero <-  exp((object$Logit) - Z*SE_Logit)
    CI_uppero <-  exp((object$Logit)  + Z*SE_Logit)
    CI_Odds  <- cbind(CI_lowero, CI_uppero)
    colnames(CI_Odds)  <- c("lower", "upper")
    
    pred <-  switch(interval, confidence = CI_Logit,  prediction = CI_pro, odd = CI_Odds)
    
  }    
  
  
  if (!(missing(newdata) ||  is.null(newdata)) && interval == "none" ){
    ax <- object$coefficients[-1] %*% t(newdata)
    phat <- 1/(1+exp(-(object$coefficients[1]+ax)))
    pred <- switch(type, link = object$coefficients[1] + ax, response = phat, odd = phat/(1 - phat))
  }
  
  if (!(missing(newdata) ||  is.null(newdata)) && interval != "none" ){
    ax <- object$coefficients[-1] %*% t(newdata)
    
    V_beta <- object$mcov[2,2] 
    V_delta  <- object$mcov[1,1]
    Cov_delta_beta <- object$mcov[1,2]
    alpha <- 1 - level
    Z <- qnorm(1 - alpha/2)
    
    V_Logit <- V_delta + ((newdata)^2)*V_beta + 2*(newdata)*Cov_delta_beta
    SE_Logit <- sqrt(V_Logit)
    CI_lowerl <- object$coefficients[1]+ax - Z*SE_Logit
    CI_upperl <- object$coefficients[1]+ax + Z*SE_Logit
    CI_LogitD <- cbind(CI_lowerl, CI_upperl)
    colnames(CI_LogitD)  <- c("lower", "upper")
    
    CI_lowerp <- exp(CI_LogitD[1])/(1+ exp(CI_LogitD[1])) 
    CI_upperp <- exp(CI_LogitD[2])/(1+ exp(CI_LogitD[2]))
    CI_predict <- cbind(CI_lowerp, CI_upperp)
    colnames(CI_predict)  <- c("lower", "upper")
    
    CI_lowero <- exp(CI_LogitD[1])
    CI_uppero <- exp(CI_LogitD[2])
    CI_OddsD  <- cbind(CI_lowero, CI_uppero)
    colnames(CI_OddsD)  <- c("lower", "upper")
    
    pred <- switch(interval, confidence = CI_LogitD, prediction = CI_predict, odd = CI_OddsD)
    
  }
  
  
  if ((missing(newdata) ||  is.null(newdata))  && interval == "confidence" ){
    colnames(object$Logit) <- "Logit_hat"
    pred <- cbind(object$Logit, CI_Logit)
  }    
  
  if ( (missing(newdata) ||  is.null(newdata)) && type == "response" && interval == "prediction" ){
    colnames(object$p_hat) <- "p_hat"
    pred <- cbind(  object$p_hat, CI_pro)
  }   
  
  if ( (missing(newdata) ||  is.null(newdata)) && type == "odd" && interval == "odd" ){
    colnames(object$odd ) <- "odd_hat"
    pred <- cbind( object$odd, CI_Odds)
  }  
  
  
  if (!(missing(newdata) ||  is.null(newdata)) && type == "link" && interval == "confidence" ){
    Logit_hat <-object$coefficients[1] + ax
    #colnames(object$Logit) <- "Logit_hat"
    pred <- cbind(Logit_hat, CI_LogitD)
  }    
  
  if ( !(missing(newdata) ||  is.null(newdata)) && type == "response" && interval == "prediction" ){
    colnames(object$p_hat) <- "p_hat"
    pred <- cbind(  phat, CI_predict)
  }   
  
  if ( !(missing(newdata) ||  is.null(newdata)) && type == "odd" && interval == "odd" ){
    colnames(object$odd ) <- "odd_hat"
    pred <- cbind( phat/(1 - phat), CI_OddsD)
  }  
  
  res <-  as.matrix(pred)
  return(res)
  
  
}  





