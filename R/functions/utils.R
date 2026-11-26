## =============================================================================
## Checkers
## =============================================================================

#' Check that an object is a numeric vector
#'
#' Verifies that an object is a numeric vector and optionally allows missing
#' values.
#'
#' @param x Object to be checked.
#' @param name Character scalar. Name of the object, used in error messages.
#' @param allow_na Logical. Should missing values be allowed? Defaults to
#'   \code{FALSE}.
#'
#' @return Invisibly returns \code{TRUE} if the check passes.
check_numeric_vector = function(x, name, allow_na = FALSE) {
  if (!is.numeric(x)) {
    stop(sprintf("`%s` must be a numeric vector.", name), call. = FALSE)
  }
  if (!allow_na && anyNA(x)) {
    stop(sprintf("`%s` must not contain NA values.", name), call. = FALSE)
  }
}

#' Check that an object is a numeric scalar
#'
#' Verifies that an object is a numeric vector of length one and optionally
#' allows missing values.
#'
#' @param x Object to be checked.
#' @param name Character scalar. Name of the object, used in error messages.
#' @param allow_na Logical. Should missing values be allowed? Defaults to
#'   \code{FALSE}.
#'
#' @return Invisibly returns \code{TRUE} if the check passes.
check_numeric_value = function(x, name, allow_na = FALSE) {
  if (!is.numeric(x) || length(x) != 1) {
    stop(sprintf("`%s` must be a numeric scalar.", name), call. = FALSE)
  }
  if (!allow_na && anyNA(x)) {
    stop(sprintf("`%s` must not contain NA values.", name), call. = FALSE)
  }
}

#' Check that an object is a character vector
#'
#' Verifies that an object is a character vector and optionally allows missing
#' values.
#'
#' @param x Object to be checked.
#' @param name Character scalar. Name of the object, used in error messages.
#' @param allow_na Logical. Should missing values be allowed? Defaults to
#'   \code{FALSE}.
#'
#' @return Invisibly returns \code{TRUE} if the check passes.
check_character_vector = function(x, name, allow_na = FALSE) {
  if (!is.character(x)) {
    stop(sprintf("`%s` must be a character value", name), call. = FALSE)
  }
  if (!allow_na && anyNA(x)) {
    stop(sprintf("`%s` must not contain NA values.", name), call. = FALSE)
  }
}

#' Check that an object is a character scalar
#'
#' Verifies that an object is a character vector of length one and optionally
#' allows missing values.
#'
#' @param x Object to be checked.
#' @param name Character scalar. Name of the object, used in error messages.
#' @param allow_na Logical. Should missing values be allowed? Defaults to
#'   \code{FALSE}.
#'
#' @return Invisibly returns \code{TRUE} if the check passes.
check_character_value = function(x, name, allow_na = FALSE) {
  if (!is.character(x) || length(x) != 1) {
    stop(sprintf("`%s` must be a character", name), call. = FALSE)
  }
  if (!allow_na && anyNA(x)) {
    stop(sprintf("`%s` must not contain NA values.", name), call. = FALSE)
  }
}

#' Check that an object is a logical scalar
#'
#' Verifies that an object is a logical vector of length one.
#'
#' @param x Object to be checked.
#' @param name Character scalar. Name of the object, used in error messages.
#' @param allow_na Logical. Should missing values be allowed? Defaults to
#'   \code{FALSE}.
#'
#' @return Invisibly returns \code{TRUE} if the check passes.
check_logical_value = function(x, name) {
  if (!is.logical(x) || length(x) != 1) {
    stop(sprintf("`%s` must be a logical", name), call. = FALSE)
  }
}

#' Check that two objects have the same length
#'
#' Verifies that two vectors have the same length.
#'
#' @param x First object.
#' @param y Second object.
#' @param name_x Character scalar. Name of the first object.
#' @param name_y Character scalar. Name of the second object.
#'
#' @return Invisibly returns \code{TRUE} if the check passes.
check_same_length = function(x, y, name_x, name_y) {
  if (length(x) != length(y)) {
    stop(sprintf(
      "`%s` and `%s` must have the same length.", name_x, name_y
    ), call. = FALSE)
  }
}

#' Check that a numeric value lies within given bounds
#'
#' Verifies that a numeric scalar lies within a specified interval.
#'
#' @param x Numeric scalar to be checked.
#' @param name Character scalar. Name of the object, used in error messages.
#' @param limits Numeric vector of length 2 giving lower and upper bounds.
#'
#' @return Invisibly returns \code{TRUE} if the check passes.
check_value_bounds = function(x, name, limits){
  if (x < limits[1]) {
    stop(sprintf(
      "`%s` must be greater than `%s`.", name, limits[1]
    ), call. = FALSE)
  }
  else if (x > limits[2]) {
    stop(sprintf(
      "`%s` must be lower than `%s`.", name, limits[2]
    ), call. = FALSE)
  }
}

#' Check that an object is a data frame
#'
#' Verifies that an object is a data frame.
#'
#' @param x Object to be checked.
#' @param name Character scalar. Name of the object, used in error messages.
#'
#' @return Invisibly returns \code{TRUE} if the check passes.
check_data_frame = function(x, name) {
  if (!is.data.frame(x)) {
    stop(sprintf("`%s` must be a data.frame.", name), call. = FALSE)
  }
}

#' Check that a directory exists
#'
#' Verifies that a directory exists at the specified path.
#'
#' @param path Character scalar. Path to a directory.
#'
#' @return Invisibly returns \code{TRUE} if the check passes.
check_data_frame_path = function(path) {
  if (!dir.exists(path)) {
    stop(sprintf("Directory `%s` does not exist.", path),
         call. = FALSE)
  }
}