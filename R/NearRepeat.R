#' @import future.apply
NULL

#' Near Repeat calculator using the Knox test
#'
#' This function uses the Knox test for space-time clustering to quantify the spatio-temporal
#' association between events.
#'
#' (Please note that any cases with missing x, y, or time will be
#' removed, i.e. this crime event will not be included in the analysis.)
#'
#' Standard \href{https://en.wikipedia.org/wiki/Interval_(mathematics)}{interval notation}
#' is used to indicate whether break points of that interval are included or excluded from the interval.
#'
#' Use set.seed() before the function call to guarantee reproducibility. See helpfile and vignette 'NearRepeat' for details.
#'
#' @param x                  a vector of x coordinates
#' @param y                  a vector of y coordinates
#' @param time               a vector of time. This can be of type integer, numeric, or date
#' @param sds                A vector of break points of the spatial intervals. For example c(0,50,120,300) to specify
#'                           spatial intervals from 0-50, 50-120, 120-300 meters. Or c(0,50,100,Inf) to specify
#'                           spatial intervals from 0-50, 50-100, and 100-Inf meters. (More accurately,
#'                           on the scale of the provided x and y coordinates. For example, data may be
#'                           projected in feet and thus the distances refer to feet instead of meters).
#' @param tds                A vector of break points of the temporal intervals. For example c(0,2,4,Inf) to specify temporal intervals
#'                           from 0-2, 2-4, 4-Inf days.
#' @param s_include.lowest   the descriptions above are ambiguous on how exactly the spatial break points are handled. For example,
#'                           does c(0,100,200) refer to 0-100, 101-200? Or to 0-99 and 100-199?
#'                           s_include.lowest follows the arguments of cut (see ?cut). Logical, indicating if a spatial
#'                           distance equal to the lowest (or highest, for right = FALSE) ‘breaks’ value should be included.
#'                           Default = FALSE. See vignette("NearRepeat_breaks") for details.
#' @param s_right            logical, indicating if the spatial intervals should be closed on the right (and open on
#'                           the left) or vice versa. Default = FALSE. See vignette("NearRepeat_breaks") for details.
#' @param t_include.lowest   t_include.lowest follows the arguments of cut (see ?cut). Logical, indicating if a temporal
#'                           distance equal to the lowest (or highest, for right = FALSE) ‘breaks’ value should be included.
#'                           Default = FALSE. See vignette("NearRepeat_breaks") for details.
#' @param t_right            logical, indicating if the temporal intervals should be closed on the right (and open on
#'                           the left) or vice versa. Default = FALSE. See vignette("NearRepeat_breaks") for details.
#' @param method             The method to calculate the spatial distances between crime events. Methods possible
#'                           as in the 'dist' function (see ?dist). Default is 'manhattan', which seems to be a fair
#'                           approximation of the distance travelled by a road network. Alternatively, the user can
#'                           specify 'euclidean' to get the 'as the crow flies' distance.
#' @param nrep               The number of replications of the Monte Carlo simulation (default = 999).
#' @param saveSimulations    Should all simulated contingency tables be saved as a 3-dimensional array? Default = FALSE
#' @param future.seed        A logical or an integer (of length one or seven), or a list of length(X) with pre-generated
#'                           random seeds. Default = TRUE. See R package future.apply for details.
#' @param ...                (optional) Additional arguments passed to future_lapply()
#' @return                   An object of type "knox", i.e. a list with four tables. For each spatial and temporal distance combination,
#'                           (1) The counts of observed crime pairs, (2) The Knox ratios based on the mean of the
#'                           simulations, (3) The Knox ratios based on the median of the simulations, (4) p-values.
#' @examples
#'
#' # Generate example data. Suppose x and y refer to meters distance.
#' set.seed(10)
#' (mydata <- data.frame(x = sample(x = 20, size = 20, replace = TRUE) * 20,
#'                      y = sample(x = 20, size = 20, replace = TRUE) * 20,
#'                      date = as.Date(sort(sample(20, size = 20, replace = TRUE)), origin = "2018-01-01")
#'                      ))
#'
#' # Near Repeat calculation using 0-100 meters and 100-Inf meters, and three temporal intervals of 2 days
#' set.seed(38673)
#' NearRepeat(x = mydata$x, y = mydata$y, time = mydata$date,
#'            sds = c(0,100,Inf), tds = c(0,2,4))
#'
#' # Add a 'same repeat' spatial interval of 0.001 meters, and use Euclidean distance
#' set.seed(38673)
#' NearRepeat(x = mydata$x, y = mydata$y, time = mydata$date,
#'            sds = c(0,0.001,100,Inf), tds = c(0,2,4),
#'            method = "euclidean")
#'
#' # Only do 99 replications
#' set.seed(38673)
#' NearRepeat(x = mydata$x, y = mydata$y, time = mydata$date,
#'            sds = c(0,0.001,100,Inf), tds = c(0,2,4),
#'            method = "euclidean", nrep = 99)
#'
#'
#' # The plot() function can be used to plot a Heat Map of Near Repeat results based on p-values
#' set.seed(4622)
#' myoutput <- NearRepeat(x = mydata$x, y = mydata$y, time = mydata$date,
#'                        sds = c(0,100,200,300,400), td = c(0,1,2,3,4,5))
#' plot(myoutput)
#'
#' # The default range of p-values that will be highlighted (0-.05) can be adjusted using
#' # the 'pvalue_range' parameter. By default the Knox ratios are printed in the cells,
#' # but this can be adjusted using the 'text' parameter. The default is "knox_ratio".
#' # Possible values are "observed", "knox_ratio", "knox_ratio_median", "pvalues", or NA.
#'
#' # For more information, see vignette("NearRepeat")
#'
#' plot(myoutput, pvalue_range = c(0, .1), text = "observed")
#' plot(myoutput, pvalue_range = c(0, .1), text = "pvalues")
#'
#' @export
NearRepeat <- function(x, y, time,
                       sds, tds,
                       s_include.lowest = FALSE, s_right = FALSE,
                       t_include.lowest = FALSE, t_right = FALSE,
                       method = "manhattan",
                       nrep = 999,
                       saveSimulations = FALSE,
                       future.seed = TRUE,
                       ...
){

  # check for at least 2 replications
  if (nrep < 2) stop("At least 2 replications are needed. Increase 'nrep'")

  # check for a vector of spatial and temporal intervals
  if (length(sds) < 2 | length(tds) < 2) stop("Supply a vector of spatial and temporal intervals")

  # Create data.frame
  mydf <- data.frame(x = x, y = y, time = time)

  # Missing data?
  if (sum(!complete.cases(mydf)) > 0){
    # Print information on deleted cases
    print(paste0(sum(!complete.cases(mydf))," events are removed due to missing x or y or time"))
    # Remove missing cases
    mydf <- mydf[complete.cases(mydf),]
  }

  # Create xy matrix
  xy <- cbind(mydf$x, mydf$y)

  # Distances of the observed space-time pairs
  s_dist <- dist(xy, method = method)
  t_dist <- dist(mydf$time)

  # Observed space-time pairs
  observed <- table(cut(s_dist, sds, include.lowest = s_include.lowest, right = s_right, dig.lab = 10),
                    cut(t_dist, tds, include.lowest = t_include.lowest, right = t_right, dig.lab = 10))

  ########
  #
  # MC Simulation to compute the null distribution
  #
  ########

  # Spatial cuts done once here (to reuse many times below)
  spat_cut <- cut(s_dist, sds, include.lowest = s_include.lowest, right = s_right, dig.lab = 10)

  #### Run permutations, calculate pairs, and save to list
  mylist <- future_lapply(seq_len(nrep), function(i){
    t_dist_perm <- dist(sample(mydf$time))
    return(table(spat_cut,
                 cut(t_dist_perm, tds, include.lowest = t_include.lowest, right = t_right, dig.lab = 10)))
  }, future.seed = future.seed, ...)

  # Create 3-dimensional output array
  array_Knox <- array(data = as.numeric(unlist(mylist)), dim = c(dim(observed), nrep))

  ########
  #
  # Create output
  #
  ########

  knox_ratio <- observed / apply(array_Knox, 1:2, mean)
  knox_ratio_median <- observed / apply(array_Knox, 1:2, median)

  pvalues <- observed
  for (i in seq_len(nrow(pvalues))){
    for (j in seq_len(ncol(pvalues))){
      pvalues[i,j] <- (sum(array_Knox[i,j,] >= observed[i,j]) + 1) / (nrep + 1)
    }
  }

  # Create output list
  result <- list(observed = observed,
                 knox_ratio = knox_ratio,
                 knox_ratio_median = knox_ratio_median,
                 pvalues = pvalues,
                 array_Knox = array_Knox)
  # remove array_Knox if saveSimulations == FALSE
  if(!saveSimulations) result$array_Knox <- NULL

  class(result) <- "knox"

  return(result)
}
