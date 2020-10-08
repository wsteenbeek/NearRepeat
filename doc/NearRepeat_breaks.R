## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fig.height = 6, fig.width = 6--------------------------------------------
set.seed(10)
mydata <- data.frame(x = sample(x = 20, size = 20, replace = TRUE) * 5,
                     y = sample(x = 20, size = 20, replace = TRUE) * 5,
                     time = sort(sample(20, size = 20, replace = TRUE)))
mydata$date = as.Date(mydata$time, origin = "2018-01-01")

head(mydata)

# plot(mydata$x, mydata$y, type = "n", axes = FALSE, xlab = NA, ylab = NA)
plot(mydata$x, mydata$y, pch = 0, cex = 2,
     frame.plot = FALSE, xlab = "x", ylab = "y", xlim = c(0,100), ylim = c(0,100)) # , axes = FALSE, xlab = NA, ylab = NA)
text(mydata$x, mydata$y, 1:nrow(mydata), cex = .7)

## -----------------------------------------------------------------------------
library(NearRepeat)

set.seed(123)
myoutput <- NearRepeat(x = mydata$x, y = mydata$y,time = mydata$date,
                       sds = c(0,20,40,60), tds = c(0,2,4,6))

## ---- fig.width=4.5, fig.height=4---------------------------------------------
plot(myoutput, text = "observed")

## -----------------------------------------------------------------------------
set.seed(123)
ff <- NearRepeat(x = mydata$x, y = mydata$y, time = mydata$date,
                 sds = c(0,1,20,40,60), tds = c(0,1,2,3,4),
                 s_include.lowest = FALSE, s_right = FALSE, # the defaults
                 t_include.lowest = FALSE, t_right = FALSE) # the defaults

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(ff$observed)

## -----------------------------------------------------------------------------
set.seed(123)
ft <- NearRepeat(x = mydata$x, y = mydata$y, time = mydata$date,
                 sds = c(0,1,20,40,60), tds = c(0,1,2,3,4),
                 s_include.lowest = FALSE, s_right = TRUE,
                 t_include.lowest = FALSE, t_right = TRUE)

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(ft$observed)

## -----------------------------------------------------------------------------
set.seed(123)
tf <- NearRepeat(x = mydata$x, y = mydata$y, time = mydata$date,
                 sds = c(0,1,20,40,60), tds = c(0,1,2,3,4),
                 s_include.lowest = TRUE, s_right = FALSE,
                 t_include.lowest = TRUE, t_right = FALSE)

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(tf$observed)

## -----------------------------------------------------------------------------
set.seed(123)
tt <- NearRepeat(x = mydata$x, y = mydata$y, time = mydata$date,
                 sds = c(0,1,20,40,60), tds = c(0,1,2,3,4),
                 s_include.lowest = TRUE, s_right = TRUE,
                 t_include.lowest = TRUE, t_right = TRUE)

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(tt$observed)

