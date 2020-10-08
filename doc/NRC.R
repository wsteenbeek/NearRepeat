## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(NearRepeat)
mydata <- chicago_be[which(chicago_be$date < "2016-02-01"), ]

## ---- echo = FALSE, results='asis'--------------------------------------------
mysds <- c(0, 1, 100, 200, 300, Inf)
mytds <- c(0, 7, 14, 21, Inf)

# Create xy matrix
xy <- cbind(mydata$X, mydata$Y)

# Distances of the observed space-time pairs
s_dist <- dist(xy, method = "manhattan")
t_dist <- dist(mydata$date)

# Observed space-time pairs
observed <- table(cut(s_dist, mysds, include.lowest = FALSE, right = FALSE, dig.lab = 10),
                  cut(t_dist, mytds, include.lowest = FALSE, right = FALSE, dig.lab = 10))

mytable <- observed
dimnames(mytable)[[1]] <- c("Same location", "1 to 100", "101 to 200", "201 to 300", "More than 300")
dimnames(mytable)[[2]] <- c("0 to 7", "8 to 14", "15 to 21", "More than 21")

knitr::kable(mytable)

## -----------------------------------------------------------------------------
# Set up the bandwidths
mysds <- c(0, 1, 101, 201, 301, Inf)
mytds <- c(0, 8, 15, 22, Inf)

# The test can now be run (this takes some time):
set.seed(831)
result <- NearRepeat(x = mydata$X, y = mydata$Y, time = mydata$date,
                     sds = mysds, tds = mytds,
                     method = "manhattan", nrep = 20)

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(result$observed)

## -----------------------------------------------------------------------------
pathological_data <- data.frame(x = c(0, 0),
                                y = c(0, 100),
                                t = as.Date(c("2019/01/01", "2019/01/04")))

## ---- echo = FALSE, results='asis'--------------------------------------------
knitr::kable(pathological_data)

## ---- echo = FALSE, results='asis'--------------------------------------------
output <- as.table(
  matrix(
    data = c(rep(0,4),0,1,0,0,rep(0,4)), 
    nrow = 4, ncol = 3, 
    dimnames = list(c("Same location", "1 to 200", "201 to 400", "More than 400"),
                    c("0 to 3", "4 to 6", "More than 6"))))

knitr::kable(output)

## ---- echo = FALSE, results='asis'--------------------------------------------
output <- as.table(
  matrix(
    data = c(rep(0,4),0,0,1,0,rep(0,4)), 
    nrow = 4, ncol = 3, 
    dimnames = list(c("Same location", "1 to 100", "101 to 200", "More than 200"),
                    c("0 to 3", "4 to 6", "More than 6"))))

knitr::kable(output)

## -----------------------------------------------------------------------------
mysds <- c(0, 1, 101, 201, 301, Inf)
mytds <- c(0, 8, 15, 22, Inf)

set.seed(831)
result <- NearRepeat(x = mydata$X, y = mydata$Y, time = mydata$date,
                     sds = mysds, tds = mytds,
                     method = "manhattan", nrep = 20)

## ---- echo = FALSE, results='asis'--------------------------------------------
knitr::kable(result$observed)

## -----------------------------------------------------------------------------
mysds <- c(0, 1, 100, 200, 300, Inf)
mytds <- c(0, 7, 14, 21, Inf)

set.seed(831)
result <- NearRepeat(x = mydata$X, y = mydata$Y, time = mydata$date,
                     sds = mysds, tds = mytds,
                     method = "manhattan", nrep = 20)

## ---- echo = FALSE, results='asis'--------------------------------------------
knitr::kable(result$observed)

## -----------------------------------------------------------------------------
mysds <- c(0, 0.01, 201, 401, 601, Inf)
mytds <- c(0, 1, 2, 3, Inf)

set.seed(63)
result <- NearRepeat(x = chicago_arson$X, y = chicago_arson$Y, time = chicago_arson$date,
                     sds = mysds, tds = mytds,
                     method = "manhattan", nrep = 100)

## ---- warning = FALSE, fig.width=4.5, fig.height=4.5--------------------------
plot(result)

