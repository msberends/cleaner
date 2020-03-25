# ==================================================================== #
# TITLE                                                                #
# Fast and Easy Data Cleaning                                          #
#                                                                      #
# SOURCE                                                               #
# https://github.com/msberends/cleaner                                 #
#                                                                      #
# LICENCE                                                              #
# (c) 2020 Berends MS (m.s.berends@umcg.nl)                            #
#                                                                      #
# This R package is free software; you can freely use and distribute   #
# it for both personal and commercial purposes under the terms of the  #
# GNU General Public License version 2.0 (GNU GPL-2), as published by  #
# the Free Software Foundation.                                        #
#                                                                      #
# This R package was publicly released in the hope that it will be     #
# useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ==================================================================== #

# throws warning on error, so invalid regex will still run, but then set as fixed value
grepl_warn_on_error <- function(pattern, x, ignore.case = FALSE, perl = FALSE,
                                fixed = FALSE, useBytes = FALSE) {
  tryCatch(expr = base::grepl(pattern = pattern, x = x, ignore.case = ignore.case, perl = perl,
                              fixed = fixed, useBytes = useBytes),
           error = function(e) {
             warning(paste0(e$message, " - now interpreting as fixed value"), call. = FALSE)
             return(base::grepl(pattern = pattern, x = x,
                                fixed = TRUE, useBytes = useBytes))
           })
}
gsub_warn_on_error <- function(pattern, replacement, x, ignore.case = FALSE, perl = FALSE,
                               fixed = FALSE, useBytes = FALSE) {
  tryCatch(expr = base::gsub(pattern = pattern, replacement = replacement, x = x, 
                             ignore.case = ignore.case, perl = perl,
                             fixed = fixed, useBytes = useBytes),
           error = function(e) {
             warning(paste0(e$message, " - now interpreting as fixed value"), call. = FALSE)
             return(base::gsub(pattern = pattern, replacement = replacement, x = x, 
                               fixed = TRUE, useBytes = useBytes))
           })
}

# works exactly like round(), but rounds `round(44.55, 1)` as 44.6 instead of 44.5
# and adds decimal zeroes until `digits` is reached when force_zero = TRUE
round2 <- function(x, digits = 0, force_zero = TRUE) {
  x <- as.double(x)
  # https://stackoverflow.com/a/12688836/4575331
  val <- (trunc((abs(x) * 10 ^ digits) + 0.5) / 10 ^ digits) * sign(x)
  if (digits > 0 & force_zero == TRUE) {
    values_trans <- val[val != as.integer(val) & !is.na(val)]
    val[val != as.integer(val) & !is.na(val)] <- paste0(values_trans,
                                                        strrep("0", 
                                                               max(0, 
                                                                   digits - nchar(
                                                                     format(
                                                                       as.double(
                                                                         gsub(".*[.](.*)$", 
                                                                              "\\1",
                                                                              values_trans)),
                                                                       scientific = FALSE)))))
  }
  as.double(val)
}

getdecimalplaces <- function(x, minimum = 0, maximum = 3) {
  if (maximum < minimum) {
    maximum <- minimum
  }
  if (minimum > maximum) {
    minimum <- maximum
  }
  max_places <- max(unlist(lapply(strsplit(sub("0+$", "", 
                                               as.character(x * 100)), ".", fixed = TRUE),
                                  function(y) ifelse(length(y) == 2, nchar(y[2]), 0))), na.rm = TRUE)
  max(min(max_places,
          maximum, na.rm = TRUE),
      minimum, na.rm = TRUE)
}

# Coefficient of variation (CV)
cv <- function(x, na.rm = TRUE) {
  stats::sd(x, na.rm = na.rm) / base::abs(base::mean(x, na.rm = na.rm))
}

# Coefficient of dispersion, or coefficient of quartile variation (CQV).
# (Bonett et al., 2006: Confidence interval for a coefficient of quartile variation).
cqv <- function(x, na.rm = TRUE) {
  # using type 6:
  # m = p. p[k] = k / (n + 1). Thus p[k] = E[F(x[k])]. This is used by Minitab and by SPSS.
  quartiles <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = na.rm, type = 6)
  (quartiles[2] - quartiles[1]) / (quartiles[2] + quartiles[1])
}

# on checks if the input is a valid date type. It supports all date types, including \code{Date}, \code{POSIXct} and \code{POSIXlt}.
is.Date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}

year <- function(x) {
  as.POSIXlt(x, tz = tz(x))$year + 1900
}

tz <- function(x) {
  # from pkg lubridate
  tzone <- attr(x, "tzone")[[1]]
  if (is.null(tzone) && !inherits(x, "POSIXt")) 
    return("UTC")
  if (is.character(tzone) && nzchar(tzone)) 
    return(tzone)
  tzone <- attr(as.POSIXlt(x[1]), "tzone")[[1]]
  if (is.null(tzone)) 
    return("UTC")
  tzone
}
