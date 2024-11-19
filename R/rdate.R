# ==================================================================== #
# TITLE                                                                #
# cleaner: Fast and Easy Data Cleaning                                 #
#                                                                      #
# SOURCE                                                               #
# https://github.com/msberends/cleaner                                 #
#                                                                      #
# LICENCE                                                              #
# 2019-2024 Berends MS (m.s.berends@umcg.nl)                           #
#                                                                      #
# This R package is free software; you can freely use and distribute   #
# it for both personal and commercial purposes under the terms of the  #
# GNU General Public License version 2.0 (GNU GPL-2), as published by  #
# the Free Software Foundation.                                        #
#                                                                      #
# This R package was publicly released in the hope that it will be     #
# useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ==================================================================== #

#' Generate random dates
#' 
#' This function provides random date generation with a specified range, that defaults to the beginning and end of the current year.
#' @inheritParams stats::runif
#' @param min,max lower and upper limits of the distribution. Must be (coercible to) valid dates.
#' @param ... parameters given to \code{as.Date()} for coercing the values of \code{min} and \code{max}
#' @export
#' @examples 
#' # generate a million random dates and check the distribution
#' hist(rdate(1000000), breaks = "months")
rdate <- function(n,
                  min = paste0(format(Sys.Date(), "%Y"), "-01-01"),
                  max = paste0(format(Sys.Date(), "%Y"), "-12-31"),
                  ...) {
  tryCatch({
    min <- as.Date(min, ...)
    max <- as.Date(max, ...)
  }, error = function(e) {
    stop("Both 'min' and 'max' must be coercible to valid dates. Note: ", e$message)
  })
  dates <- seq.Date(min, max, by = "day")
  dates_numeric <- as.double(dates)
  out <- sample(dates_numeric,
                size = ifelse(length(n) == 1, n, length(n)),
                replace = TRUE)
  as.Date(out, origin = "1970-01-01")
}
