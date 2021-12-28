#' Calculate near-repeats using the Knox test with tidy inputs and outputs
#'
#' A wrapper around \code{\link{NearRepeat}} that accepts inputs and produces
#' outputs that meet the definition of
#' \href{https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html}{tidy data}
#' and work within a pipeline of functions.
#'
#' This function uses default values for spatial and temporal distance bands
#' that are likely to be reasonable in many circumstances \strong{if
#' co-ordinates are given in metres}, as well as warning the user when the
#' values they have provided may produce unexpected results.
#'
#' @param data A data frame, tibble or Simple Features (SF) object in which each
#'   row represent a single event.
#' @param x,y The names of the numeric columns in \code{data} that contain the
#'   \code{x} (longitude, easting, etc.) and \code{y} (latitude, northing, etc.)
#'   co-ordinates for each event. If \code{x} and \code{y} are not specified and
#'   \code{data} is an SF object, the function will attempt to extract
#'   co-ordinates from the geometry column.
#' @param time The bare name of the column in \code{data} containing the date
#'   of each event. This column must contain Date values. If not given and the
#'   data contains only one column of the type Date, the function will use this
#'   column automatically.
#' @param sds A numeric vector of distances to be used as the breaks between
#'   distance bands over which to test for near-repeat events. These distances
#'   should be expressed in the same units (metres, feet, etc.) as used to
#'   specify co-ordinates in the \code{x} and \code{y} columns.
#' @param tds A numeric vector of numbers of days to be used as the breaks
#'   between temporal bands over which to test for near-repeat events.
#' @param ... Optional arguments passed to \code{\link{NearRepeat}}
#'
#' @return A tibble containing observed counts and Knox test results for each
#'   combination of distance and time bands.
#'
#' @export


tidy_near_repeat <- function (
  data,
  x = NULL,
  y = NULL,
  time = NULL,
  sds = c(0, 200, 400, 600, 800, 1000),
  tds = c(0, 7, 14, 21, 28),
  ...
) {

  # Check that required packages are installed
  rlang::check_installed("tibble", reason = "to use `tidy_near_repeat()`")
  if (inherits(data, "sf")) {
    rlang::check_installed(
      "sf",
      reason = "to use `tidy_near_repeat()' with SF objects"
    )
  }

  # Process column names
  columns <- rlang::set_names(
    x = rlang::enquos(x, y, time, .ignore_empty = "all"),
    nm = c("x", "y", "time")
  )
  columns <- lapply(columns, function (x) {
    if (rlang::quo_is_null(x)) {
      FALSE
    } else {
      rlang::as_name(x)
    }
  })

  # Check inputs
  if (inherits(data, "sf")) {
    if (!all(sf::st_is(data, "POINT"))) {
      stop(
        "The `data` argument to `tidy_near_repeat()` must be a data frame, ",
        "tibble or SF object containing points"
      )
    }
  } else if (!is.data.frame(data)) {
    stop(
      "The `data` argument to `tidy_near_repeat()` must be a data frame, ",
      "tibble or SF object containing points"
    )
  }
  if (
    (!columns$x %in% names(data) & !isFALSE(columns$x)) |
    (!columns$y %in% names(data) & !isFALSE(columns$y)) |
    (!columns$time %in% names(data) & !isFALSE(columns$time))
  )
    stop(
      "The `x`, `y` and `time` arguments to `tidy_near_repeat()` must be NULL ",
      "or the unquoted names of columns in the data frame supplied using the ",
      "`data` argument"
    )
  if (!is.numeric(sds) & !is.null(sds))
    stop(
      "The `sds` argument to `tidy_near_repeat()` must be NULL or a numeric ",
      "vector"
    )
  if (!is.numeric(tds) & !is.null(tds))
    stop(
      "The `tds` argument to `tidy_near_repeat()` must be NULL or a numeric ",
      "vector"
    )
  if (is.numeric(sds)) {
    if (length(unique(sds)) < 2) stop(
      "If the `sds` argument to `tidy_near_repeat()` is specified, it must ",
      "include at least two unique values"
    )
  }
  if (is.numeric(tds)) {
    if (length(unique(tds)) < 2) stop(
      "If the `tds` argument to `tidy_near_repeat()` is specified, it must ",
      "include at least two unique values"
    )
  }

  # Find date column automatically if not specified
  if (isFALSE(columns$time)) {
    date_cols <- which(unlist(lapply(data, rlang::inherits_any, c("Date", "POSIXt"))))
    if (length(date_cols) > 1) {
      stop(
        "More than one column in the `data` data frame contains date values. ",
        "Please specify which date column should be used by giving the column ",
        "name in the `time` argument."
      )
    } else if (length(date_cols) == 0) {
      stop(
        "No columns in the `data` data frame contain date values. Check the ",
        "contents of each column."
      )
    } else {
      columns$time <- names(data)[date_cols[1]]
    }
  }

  # Check if date column contains dates
  if (!rlang::inherits_any(data[[columns$time]], c("Date", "POSIXt")))
    stop(
      "The `", columns$time, "` column in the `data` data frame does not ",
      "contain date values. Please specify which date column should be used ",
      "by giving the column name in the `time` argument."
    )

  # Extract co-ordinates from geometry if column names not specified and data is
  # an SF object
  if (
    (isFALSE(columns$x) & !isFALSE(columns$y)) |
    (!isFALSE(columns$x) & isFALSE(columns$y))
  ) {
    stop("Specify either `x` and `y` arguments or neither")
  } else if (isFALSE(columns$x) & isFALSE(columns$y) & !inherits(data, "sf")) {
    stop(
      "If `x` and `y` arguments are not specified, `data` must be an SF object"
    )
  } else if (isFALSE(columns$x) & isFALSE(columns$y) & inherits(data, "sf")) {
    if (sf::st_is_longlat(data))
      warning(
        "The co-ordinates in the `data` argument are specified using latitude ",
        "and longitude, so the distance-bands returned will be treated as ",
        "being in decimal degrees. Transform `data` to a projected CRS to ",
        "use units such as metres or feet."
      )
    data <- cbind(sf::st_drop_geometry(data), sf::st_coordinates(data))
    columns$x <- "X"
    columns$y <- "Y"
  }

  # Remove rows with missing values
  data <- ggplot2::remove_missing(data[, unlist(columns)])

  # Calculate Knox test
  nr_values <- NearRepeat(
    x = data[[columns$x]],
    y = data[[columns$y]],
    time = data[[columns$time]],
    sds = sds,
    tds = tds,
    ...
  )

  # Convert results from a list of matrices to a tibble
  result <- tibble::as_tibble(cbind(
    # Get distance and time bands from the first element in the results list
    rlang::set_names(
      x = as.data.frame(nr_values$observed)[, 1:2],
      nm = c("distance", "time")
    ),
    # Get values from each of the elements in the list
    rlang::exec(cbind, !!!lapply(nr_values, function (x) as.data.frame(x)[, 3]))
  ))

  # Convert distance/time bands to ordered factors
  result$distance <- as.ordered(result$distance)
  result$time <- as.ordered(result$time)

  # Replace p-values corresponding with NaN Knox ratios with NA
  result$pvalues <- ifelse(is.nan(result$knox_ratio), NA, result$pvalues)

  # Issue warnings if the number of spatial or temporal bands may be inadequate
  if (
    any(
      result[result$distance == result$distance[[length(sds) - 1]], "pvalues"] < 0.05,
      na.rm = TRUE
    ) &
    max(sds) < Inf
  ) {
    warning(
      "Significant near-repeats detected at the maximum distance specified. ",
      "Consider increasing the maximum distance to check if near-repeats are ",
      "signficiant at greater distances."
    )
  }
  if (
    any(
      result[result$time == result$time[[length(tds) - 1]], "pvalues"] < 0.05,
      na.rm = TRUE
    ) &
    max(tds) < Inf
  ) {
    warning(
      "Signficiant near-repeats detected at the maximum time period ",
      "specified. Consider increasing the maximum time period to check if ",
      "near-repeats are significant after longer time periods."
    )
  }

  # Issue a message if some Knox ratios could not be calculated
  if (any(is.nan(result$knox_ratio)))
    message(
      "Knox ratios cannot be calculated for combinations of time and distance ",
      "in which no events occurred. NaN values inserted instead."
    )

  # Return result
  result

}
