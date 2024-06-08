#' Gráfico de regresión logística
#'
#' @title Graphics  Method for \code{lsm}  Objects
#'
#' @description Obtains graphics from a fitted \code{lsm} object.

#' @param x The LSM model object.
#' @param type The type of plot to draw. Options are "scatter" for a scatter plot, "probability" for a probability plot, "Logit" for a plot related to logistic regression, and "odds" for a plot related to odds.
#' @param title The title of the plot.
#' @param xlab The label for the x-axis.
#' @param ylab The label for the y-axis.
#' @param color The color of the dots in the plot.
#' @param size The size of the dots in the plot.
#' @param shape The shape oof the dots in the plot.
#' @param ... Additional graphical arguments to be passed to ggplot.
#'
#' @return Un objeto ggplot.
#'  following components:
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
#' #library(lsm)
#'
#' #1. AGE and Coronary Heart Disease (CHD) Status of 100 subjects:
#'
#' # library(lsm)
#' # library(tidyverse)
#' # datos <- lsm::chdage
#' # attach(datos)
#' # modelo <- lsm(CHD ~ AGE, data=datos)
#' # plot(modelo, type = "scatter")
#' # plot(modelo, type = "scatter", title  = "Villalba-llinas lsm")
#' # plot(modelo, type = "probability", xlab = "Elainys")
#' # plot(modelo, type = "Logit", color = "blue")
#' # plot(modelo, type = "odds", size = 3)
#'
#' @rdname plot.lsm
#' @exportS3Method plot lsm

plot.lsm <- function(x, type = c("scatter", "probability", "Logit", "odds"), title = NULL, xlab = NULL, ylab = NULL, color = "red", size = 1.5, shape = 19, ...) {

  if (length(list(...)) > 0) {
    stop("This function does not accept additional arguments. Please review the documentation.")
  }

  type <- match.arg(type)

    formula <- x$formula
    v.i <- as.character(all.vars(formula)[-1])
    v.d <- as.character(all.vars(formula)[1])

  if(type == "scatter") {

    gl <- ggplot(x$data, aes_string( x = v.i, y = v.d )) +
      geom_point( size = size, shape = shape, color = color) +
      labs(x = ifelse(is.null(xlab),  paste(v.i), xlab),
           y = ifelse(is.null(ylab), paste(v.d), ylab),
           title = ifelse(is.null(title), paste("Scatterplot ", paste(v.d), "versus", paste(v.i)),title))

    return(gl)

  } else{

    if(type == "probability") {

      p <- x$p_j
      x$data$p <- p
      ynames <- bquote(~ hat(p)[j] == hat(P)(.(as.name(v.d)) == "1" ~ "|" ~ .(as.name(v.i)) == x[j]))
      yna <- bquote(~ hat(p)[j]  ~ "versus" ~ .(as.name(v.i)))
      gl <- ggplot(x$data, aes_string( x = v.i, y = p )) +
        geom_point( size = size, shape = shape, color = color) +
        labs(x = if (is.null(xlab)) paste(v.i) else xlab,
             y = if (is.null(ylab)) ynames else ylab,
             title = if(is.null(title)) yna else title)

      return(gl)

    } else{

      if(type == "Logit") {
        L <- x$Logit
        ynames <-  bquote(~ Logit(hat(p)[j]))
        yna <- bquote(~ Logit(hat(p)[j])  ~ "versus" ~ .(as.name(v.i)))

        x$data$L <- L

        gl <- ggplot(x$data, aes_string( x = v.i, y = L )) +
          geom_point( size = size, shape = shape, color = color) +
          labs(x = if (is.null(xlab)) paste(v.i) else xlab,
               y = if (is.null(ylab)) ynames else ylab,
               title = if(is.null(title)) yna else title)

        return(gl)

      } else{

        if(type == "odds") {
          O <- x$odd
          ynames <-  bquote(~ hat(O)[j] == frac( hat(p)[j], 1 - hat(p)[j]))
          yna <-  bquote(~ hat(O)[j] ~ "versus" ~ .(as.name(v.i)))
          x$data$O <- O

          gl <- ggplot(x$data, aes_string( x = v.i, y = O )) +
            geom_point( size = size, shape = shape, color = color) +
            labs(x = if (is.null(xlab)) paste(v.i) else xlab,
                 y = if (is.null(ylab)) ynames else ylab,
                 title = if(is.null(title)) yna else title)


          return(gl)

        } else {

          stop("Invalid plot type. Valid types are 'scatter', 'probability', 'Logit', and 'odds'.")

        }

      }

    }

  }

}
