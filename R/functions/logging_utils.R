################################################################################
## Logging utilities
## Small helpers to make long-running scripts (the validation analysis can
## take hours at full precision/grid) legible while they run: timestamped
## step messages and a progress/ETA line.
################################################################################

#' Print a timestamped log message
#'
#' @param msg Character scalar. Message to print.
#' @param indent Integer. Number of 2-space indents to prefix (used to show
#'   sub-steps nested under a parent step). Defaults to 0.
#'
#' @return Invisibly returns NULL. Called for its side effect (printing).
log_step = function(msg, indent = 0) {
  prefix = paste0(strrep("  ", indent), sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")))
  cat(prefix, msg, "\n", sep = "")
  flush(stdout())
  invisible(NULL)
}

#' Format a duration in seconds as a short human-readable string
#'
#' @param seconds Numeric scalar, duration in seconds.
#'
#' @return Character scalar, e.g. "42s", "3.2min", "1.05h".
format_duration = function(seconds) {
  if (length(seconds) == 0 || is.na(seconds) || !is.finite(seconds)) return("--")
  if (seconds < 60) return(sprintf("%.0fs", seconds))
  if (seconds < 3600) return(sprintf("%.1fmin", seconds / 60))
  sprintf("%.2fh", seconds / 3600)
}

#' Print a "current/total (pct%) | elapsed | ETA" progress line
#'
#' ETA is extrapolated linearly from the average time per unit so far, so
#' it settles down after the first few calls; it is necessarily rough right
#' after `start_time` (based on a single data point).
#'
#' @param current Integer. Number of units completed so far (>= 1).
#' @param total Integer. Total number of units expected.
#' @param start_time A POSIXct timestamp (from Sys.time()) marking when the
#'   whole loop started.
#' @param label Character scalar. Optional label prefixed to the line.
#'
#' @return Invisibly returns NULL. Called for its side effect (printing).
log_progress = function(current, total, start_time, label = "") {
  elapsed = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  rate = elapsed / max(current, 1)
  remaining = rate * (total - current)
  pct = 100 * current / total
  log_step(sprintf("%s%d/%d (%.1f%%) | elapsed %s | ETA %s",
                   if (nzchar(label)) paste0(label, ": ") else "",
                   current, total, pct,
                   format_duration(elapsed), format_duration(remaining)))
  invisible(NULL)
}
