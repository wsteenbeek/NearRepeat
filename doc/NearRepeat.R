## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE--------------------------------------------------------
#  library(NearRepeat)

## ------------------------------------------------------------------------
mydata <- chicago_be[which(chicago_be$date < "2016-02-01"), ]
head(mydata)

## ----results = "hide"----------------------------------------------------
s_bands <- c(0, 0.1, 200, 400, 600)
t_bands <- c(0, 7, 14, 21)

set.seed(9489)
result <- NearRepeat(x = mydata$X, y = mydata$Y, time = mydata$date, 
                     sds = s_bands, tds = t_bands)

## ------------------------------------------------------------------------
result

## ---- fig.width=4.5, fig.height=4----------------------------------------
plot(result)

## ---- fig.width=4.5, fig.height=4, fig.show="hold"-----------------------
plot(result, pvalue_range = c(0, .01))

## ---- fig.width=4.5, fig.height=4, fig.show="hold"-----------------------
plot(result, pvalue_range = c(0, .01), minimum_perc = 50)

## ---- fig.width=4.5, fig.height=4, fig.show="hold"-----------------------
plot(result, text = "pvalues")

## ---- fig.width=4.5, fig.height=4, fig.show="hold"-----------------------
plot(result, text = "observed")

## ----results = "hide"----------------------------------------------------
# Not using parallel processing (the default)
set.seed(123)
result <- NearRepeat(x = mydata$X, y = mydata$Y, time = mydata$date, 
                     sds = s_bands, tds = t_bands)

# Parallel processing
library(future)
plan(multisession)
set.seed(123)
result <- NearRepeat(x = mydata$X, y = mydata$Y, time = mydata$date, 
                     sds = s_bands, tds = t_bands)

