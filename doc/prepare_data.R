## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
#  # This block is not run as it takes a while to download, but code should
#  # work on your computer
#  
#  # Note: Because the data in the Crime Open Database can be suject to change,
#  # running the code below *could* result in a different data.frame than used
#  # in this vignette. Therefore, the resulting datasets are included
#  # with the package NearRepeat. I.e., the below code need not be executed:
#  # after loading the package you can simply access the data object 'chicago_be'
#  # and 'chicago_arson'.
#  
#  library(crimedata)
#  chicago_df <- get_crime_data(years = 2016, cities = "Chicago", type = "core")
#  chicago_df <- chicago_df[which(chicago_df$offense_type %in% c("residential burglary/breaking & entering", "arson")), ]
#  
#  # convert to SpatialPointsDataFrame
#  library(sp)
#  library(rgdal)
#  wgs84_CRS <- CRS("+init=epsg:4326")
#  
#  chicago_sp <- SpatialPointsDataFrame(coords = cbind(chicago_df$longitude, chicago_df$latitude),
#                                       data = chicago_df[, c("uid", "date_single", "offense_type")],
#                                       proj4string = wgs84_CRS)
#  
#  # Re-project spatial data to EPSG:26971 projection (Illinois State Plane)
#  illinois_CRS <- CRS("+init=epsg:26971")
#  chicago_sp <- spTransform(chicago_sp, illinois_CRS)
#  
#  # add X and Y variables, and date
#  chicago_sp$X = coordinates(chicago_sp)[,1]
#  chicago_sp$Y = coordinates(chicago_sp)[,2]
#  chicago_sp$date <- as.Date(chicago_sp$date_single)
#  
#  chicago_df <- as.data.frame(chicago_sp@data)

## ----eval = FALSE-------------------------------------------------------------
#  chicago_df$dates <- paste(substr(as.character(chicago_df$date), 6, 7),
#                            substr(as.character(chicago_df$date), 9, 10),
#                            substr(as.character(chicago_df$date), 1, 4),
#                            sep = "/")

## ----eval = FALSE-------------------------------------------------------------
#  # save as data.frame
#  chicago_arson <- chicago_df[which(chicago_df$offense_type == "arson"), c("X", "Y", "date")]
#  
#  # save as data.frame
#  chicago_be <- chicago_df[which(chicago_df$offense_type == "residential burglary/breaking & entering"), c("X", "Y", "date")]
#  
#  # Select first month of data, and save as .csv file
#  # Adjust file name (and include path) accordingly
#  chicago_be_month1 <- chicago_be[which(chicago_be$date < "2016-02-01"), ]
#  
#  write.table(chicago_be_month1,
#              file = "inst/extdata/chicago_be_month1.csv",
#              sep = ",",
#              quote = FALSE,
#              row.names = FALSE, col.names = FALSE)
#  
#  write.table(chicago_arson,
#              file = "inst/extdata/chicago_arson.csv",
#              sep = ",",
#              quote = FALSE,
#              row.names = FALSE, col.names = FALSE)

## ----echo = FALSE, eval = FALSE-----------------------------------------------
#  usethis::use_data(chicago_be, overwrite = TRUE)
#  usethis::use_data(chicago_arson, overwrite = TRUE)

