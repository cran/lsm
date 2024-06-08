#' Predict Method for lsm Fits
#'
#' @title Predictions and Confidence intervals
#'
#' @description Obtains predictions  and confidence intervals from a fitted \code{lsm} object.
#'
#' @param object A fitted object of class \code{lsm}.
#'
#' @param newdata Optionally, a data frame in which to look for variables with which to predict.
#'
#' @param type The type of prediction required. The alternatives \code{response}, \code{link}, \code{odd} and \code{OR} give the predicted probabilities, logits, odds  and odds ratios, repectively.
#'
#' @param level  Confidence level to use (default is 0.95).
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @return The option \code{type =...} returns a matrix with one column containing the requested predictions. The option \code{interval =...} returns a matrix with 3 columns containing the lower and upper extremes of the requested interval and the corresponding predictions.
#'
#' @details If \code{newdata} is omitted, a matrix with the predictions for each observation is obtained. That is to say, the predictions are based on the data used for the fit. In that case how cases with missing values in the original fit is determined by the na.action argument of that fit. If na.action = na.omit omitted cases will not appear in the residuals, whereas if na.action = na.exclude they will appear (in predictions and standard errors), with residual value NA.
#' @encoding UTF-8
#' @details The saturated model is characterized by the assumptions 1 and 2 presented in section 2.3 by Llinas (2006, ISSN:2389-8976).
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
#' # library(lsm)
#' # library(tidyverse)
#' # datos <- lsm::chdage
#' # attach(datos)
#' # modelo <- lsm(CHD ~ AGE, data=datos)
#' # head(predict(modelo, type = "link"))
#' # predict(modelo,newdata=data.frame(AGE=35),type = "response")
#' # head(predict(modelo, type = "odd"))
#' # head(predict(modelo, type = "OR"))
#'
#' @rdname predict.lsm
#' @exportS3Method predict lsm
#'

predict.lsm <- function (object,
                     newdata,
                     type = c("link", "response", "odd", "OR"),
                     level = 0.95,...){

  # Verificar si se han proporcionado argumentos adicionales
  if (length(list(...)) > 0) {
    stop("This function does not accept additional arguments. Please review the documentation.")
  }

  if ((length(level) != 1L) || is.na(level) || (level <=
                                                0) || (level >= 1)){
    stop("'conf.level' must be a single number between 0 and 1")
  }

  type <- match.arg(type)

  if ((missing(newdata) ||  is.null(newdata)) ){

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
    colnames(CI_Logit)  <- c(paste0("lower ",100*(1-level)/2,"%"), paste0("upper ",100*(1-(1-level)/2),"%"))
    colnames(object$Logit) <- "Logit_hat"
    predL <- cbind(object$Logit, CI_Logit)
    colnames(predL)  <- c("Logit_hat", paste0("lower ",100*(1-level)/2,"%"), paste0("upper ",100*(1-(1-level)/2),"%"))

    CI_lower <- object$p_j - Z*SE_Logit
    CI_upper <- object$p_j + Z*SE_Logit
    CI_pro <- cbind(CI_lower, CI_upper)
    colnames(CI_pro)  <- c(paste0("lower ",100*(1-level)/2,"%"), paste0("upper ",100*(1-(1-level)/2),"%"))
    colnames(object$p_j) <- "p_j"
    predP <- cbind(object$p_j, CI_pro)
    colnames(predP)  <- c("p_j", paste0("lower ",100*(1-level)/2,"%"), paste0("upper ",100*(1-(1-level)/2),"%"))

    CI_lowero <-  exp((object$Logit) - Z*SE_Logit)
    CI_uppero <-  exp((object$Logit)  + Z*SE_Logit)
    CI_Odds  <- cbind(CI_lowero, CI_uppero)
    colnames(CI_Odds)  <- c(paste0("lower ",100*(1-level)/2,"%"), paste0("upper ",100*(1-(1-level)/2),"%"))
    colnames(object$odd ) <- "odd_hat"
    predodd <- cbind(object$odd, CI_Odds)
    colnames(predodd)  <- c("odd_hat", paste0("lower ",100*(1-level)/2,"%"), paste0("upper ",100*(1-(1-level)/2),"%"))

    beta <- object$coefficients[-1]
    ES_beta <- object$Std.Error[-1]

    CI_lower <- beta - Z*ES_beta
    CI_upper <- beta + Z*ES_beta
    CI_beta <- cbind(CI_lower, CI_upper)
    predOR <- cbind(object$OR, exp(CI_beta))
    colnames(predOR)  <- c("OR", paste0("lower ",100*(1-level)/2,"%"), paste0("upper ",100*(1-(1-level)/2),"%"))

    pred <- switch(type, link = predL, response =  predP, odd =  predodd, OR = predOR )

  }


  if (!(missing(newdata) ||  is.null(newdata)) ){

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
    #colnames(CI_LogitD)  <- c(paste0("lower ",100*(1-level)/2,"%"), paste0("upper ",100*(1-(1-level)/2),"%"))
    Logit_hat <- object$coefficients[1] + ax
    #colnames(Logit_hat) <- "Logit_hat"
    predL <- cbind(Logit_hat, CI_LogitD)

    CI_lowerp <- exp(CI_LogitD[1])/(1+ exp(CI_LogitD[1]))
    CI_upperp <- exp(CI_LogitD[2])/(1+ exp(CI_LogitD[2]))
    CI_response <- cbind(CI_lowerp, CI_upperp)
    colnames(CI_response)  <- c(paste0("lower ",100*(1-level)/2,"%"), paste0("upper ",100*(1-(1-level)/2),"%"))
    pj <- 1/(1+exp(-(object$coefficients[1]+ax)))
    colnames(pj) <- "p_j"
    predP <- cbind(pj, CI_response)
    colnames(predP)  <- c("p_j", paste0("lower ",100*(1-level)/2,"%"), paste0("upper ",100*(1-(1-level)/2),"%"))

    CI_lowero <- exp(CI_LogitD[1])
    CI_uppero <- exp(CI_LogitD[2])
    CI_OddsD  <- cbind(CI_lowero, CI_uppero)
    colnames(CI_OddsD)  <- c(paste0("lower ",100*(1-level)/2,"%"), paste0("upper ",100*(1-(1-level)/2),"%"))
    odd = pj/(1 - pj)
    colnames(odd ) <- "odd_hat"
    predodd <- cbind(odd, CI_OddsD)
    colnames(predodd)  <- c("odd_hat", paste0("lower ",100*(1-level)/2,"%"), paste0("upper ",100*(1-(1-level)/2),"%"))

    beta <- object$coefficients[-1]
    ES_beta <- object$Std.Error[-1]

    CI_lower <- beta - Z*ES_beta
    CI_upper <- beta + Z*ES_beta
    CI_beta <- cbind(CI_lower, CI_upper)
    predOR <- cbind(object$OR, exp(CI_beta))
    colnames(predOR)  <- c("OR", paste0("lower ",100*(1-level)/2,"%"), paste0("upper ",100*(1-(1-level)/2),"%"))

    pred <- switch(type, link = predL, response =  predP, odd =  predodd, OR = predOR )

  }

  res <-  as.matrix(pred)
  return(res)


}



