[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/lsm)](https://cran.r-project.org/package=lsm)

Welcome to the *lsm* package!
=============================

When the values of the outcome variable Y are either 0 or 1, the function calculates the estimation of the log likelihood in the saturated model. This model is characterized by Llinas (2006, ISSN:2389-8976) in section 2.3 through the assumptions 1 and 2. If is dichotomous and the data are grouped in J populations, it is recommended to use the function because it works very well for all .

Details
-------

The saturated model is characterized by the assumptions 1 and 2 presented in section 2.3 by Llinas (2006, ISSN:2389-8976).

References
----------

\[1\] Humberto Jesus Llinas. (2006). Accuracies in the theory of the logistic models. Revista Colombiana De Estadistica,29(2), 242-244.

\[2\] Hosmer, D. (2013). Wiley Series in Probability and Statistics Ser. : Applied Logistic Regression (3). New York: John Wiley & Sons, Incorporated.

Author(s)
---------

Humberto Llinas Solano \[aut\], Universidad del Norte, Barranquilla-Colombia \\ Omar Fabregas Cera \[aut\], Universidad del Norte, Barranquilla-Colombia \\ Jorge Villalba Acevedo \[cre, aut\], Unicolombo, Cartagena-Colombia.

Installation
------------

``` r
library(devtools)
install_github("jlvia1191/lsm")

```

Example Usage
-------------

Hosmer, D. (2013) page 3: Age and coranary Heart Disease (CHD) Status of 20 subjects:

``` r
library(lsm)

  AGE <- c(20,23,24,25,25,26,26,28,28,29,30,30,30,30,30,30,30,32,33,33)
  CHD <- c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
  
  data <- data.frame (CHD,  AGE )
  lsm(CHD ~ AGE , data)
#> 
#> Call:
#> lsm(formula = CHD ~ AGE, data = data)
#> 
#> Log_Likelihood: 
#> [1] -4.257109
#> 
#> Populations: 
#> [1] 10
```

\# Other case.

``` r
 
 y <- c(0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1)
 x1 <- c(2, 2, 2, 2, 2, 5, 5, 5, 5, 6, 6, 6, 8, 8, 11, 11, 11, 1)
 x2 <- c(3, 3, 3, 3, 3, 6, 6, 6, 6, 8, 8, 8, 9, 9, 12,  12, 12, 12)
 x3 <- c(4, 4, 4, 4, 4, 7, 7, 7, 7, 9, 9, 9, 10, 10, 13, 13, 13,    13)
 x4 <- c(1, 1, 1, 1, 1, 9, 9, 9, 9, 10, 10, 10, 4, 4, 2, 2, 2, 2)
 x5 <- c(32, 32, 32, 32, 32, 20, 20, 20, 20, 21, 21, 21, 19, 19, 16, 16, 16, 16)
 x6 <- c(15, 15, 15, 15, 15, 18, 18, 18, 18, 16, 16, 16, 25, 25, 20, 20, 20, 20)
 x7 <- c(28, 28, 28, 28, 28, 23, 23, 23, 23, 32, 32, 32, 24, 24, 32, 32, 32, 32)
 x8 <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0)
 x9 <- c(6, 6, 6, 6, 6, 10, 10, 10, 10, 11, 11, 11, 7, 7, 21, 21, 21, 21)
 x10 <- c(5, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 7, 8, 8, 8, 8)
 
 data <- data.frame (y, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
 lsm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, data)
#> 
#> Call:
#> lsm(formula = y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + 
#>     x10, data = data)
#> 
#> Log_Likelihood: 
#> [1] -11.34303
#> 
#> Populations: 
#> [1] 6
 
## For more ease, use the following notation.
 
 lsm(y ~., data)
#> 
#> Call:
#> lsm(formula = y ~ ., data = data)
#> 
#> Log_Likelihood: 
#> [1] -11.34303
#> 
#> Populations: 
#> [1] 6
```

\# Other case.

``` r

y <- as.factor(c(1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1))
  x1 <- as.factor(c(2, 2, 2, 5, 5, 5, 5, 8, 8, 11, 11, 11))
  x2 <- as.factor(c(3, 3, 3, 6, 6, 6, 6, 9, 9, 12, 12, 12))
  x3 <- as.factor(c(4, 4, 4, 7, 7, 7, 7, 10, 10, 13, 13, 13))
  x4 <- as.factor(c(1, 1, 1, 9, 9, 9, 9, 4, 4, 2, 2, 2))
  x5 <- as.factor(c(5, 5, 5, 6, 6, 6, 6, 7, 7, 8, 8, 8))

   data <- data.frame (y, x1, x2, x3, x4, x5) 
  lsm(y ~ x1 + x2 + x3 + x4 + x5, data)
#> 
#> Call:
#> lsm(formula = y ~ x1 + x2 + x3 + x4 + x5, data = data)
#> 
#> Log_Likelihood: 
#> [1] -7.45472
#> 
#> Populations: 
#> [1] 4
  
## For more ease, use the following notation.
  
  lsm(y~., data)
#> 
#> Call:
#> lsm(formula = y ~ ., data = data)
#> 
#> Log_Likelihood: 
#> [1] -7.45472
#> 
#> Populations: 
#> [1] 4
```
