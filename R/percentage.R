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

#' Transform to percentage
#' 
#' Transform input to a percentage. The actual values are numeric, but will be printed as formatted percentages.
#' @param x input
#' @param ... other parameters passed on to methods
#' @param digits how many digits should be printed. It defaults to printing all decimals available in the data after transforming to a percentage, with a minimum of 0 and a maximum of 3.
#' @details Printing percentages will always have a percentage symbol and is never written in scientific format (like 2.5e+04\%).
#' 
#' The function \code{percentage} is a wrapper around \code{format(as.percentage(...))} with automatic determination of the number of digits, varying between 0 and 1. It also, unlike \R, rounds according to basic math rules: \code{percentage(0.4455)} returns \code{"44.6\%"} and not \code{"44.5\%"}. This function always returns a character, and can also be used in plotting, see Examples.
#' @rdname percentage
#' @name percentage
#' @export
#' @examples 
#' proportion <- as.percentage(c(0.25, 2.5, 0.0025))
#' proportion
#' sum(proportion)
#' max(proportion)
#' mean(proportion)
#' 
#' as.percentage(2.5e-14)
#' 
#' as.percentage(pi)
#' format(as.percentage(pi))
#' format(as.percentage(pi), digits = 6)
#' 
#' round(0.4455 * 100, 1) # mind the rounding
#' percentage(0.4455) # does not round to 44.5%
#' 
#' if (require("ggplot2")) {
#'   ggplot(iris) +
#'     geom_col(aes(Species, Sepal.Length / sum(Sepal.Length)),
#'              position = "stack") +
#'     # add percentage as function to the labels:
#'     scale_y_continuous(labels = percentage)
#' }
as.percentage <- function(x, ...) {
  if (is.percentage(x)) {
    return(x)
  }
  if (!is.numeric(x) & any(grepl("%", x), na.rm = TRUE)) {
    clean_percentage(x, ...)
  } else {
    structure(.Data = as.double(x, ...),
              class = c("percentage", "numeric"))
  }
}

#' @noRd
#' @method as.double percentage
#' @export
as.double.percentage <- function(x, ...) {
  as.double(structure(x, class = "numeric"))
}

#' @rdname percentage
#' @export
is.percentage <- function(x) {
  identical(class(x), c("percentage", "numeric"))
}

#' @method [ percentage
#' @export
#' @noRd
"[.percentage" <- function(x, ...) {
  y <- NextMethod()
  attributes(y) <- attributes(x)
  y
}
#' @method [<- percentage
#' @export
#' @noRd
"[<-.percentage" <- function(value) {
  y <- NextMethod()
  attributes(y) <- attributes(value)
  y
}
#' @method [[ percentage
#' @export
#' @noRd
"[[.percentage" <- function(x, ...) {
  y <- NextMethod()
  attributes(y) <- attributes(x)
  y
}
#' @method [[<- percentage
#' @export
#' @noRd
"[[<-.percentage" <- function(value) {
  y <- NextMethod()
  attributes(y) <- attributes(value)
  y
}
#' @method c percentage
#' @export
#' @noRd
c.percentage <- function(x, ...) {
  y <- NextMethod()
  attributes(y) <- attributes(x)
  y
}

#' @rdname percentage
#' @method print percentage
#' @export
print.percentage <- function(x, ...) {
  print(format(x), quote = FALSE)
}

#' @rdname percentage
#' @method format percentage
#' @export
format.percentage <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- getdecimalplaces(x)
  }
  # round right: percentage(0.4455) and format(as.percentage(0.4455), 1) should return "44.6%", not "44.5%"
  x <- round2(x = as.double(x) * 100, digits = digits)
  x_formatted <- format(x, scientific = FALSE, ...)
  x_formatted[!is.na(x_formatted)] <- paste0(x_formatted[!is.na(x_formatted)], "%")
  x_formatted
}

#' @noRd
#' @method sum percentage
#' @export
sum.percentage <- function(x, ...) {
  as.percentage(sum(as.double(x), ...))
}

#' @noRd
#' @method min percentage
#' @export
min.percentage <- function(x, ...) {
  as.percentage(min(as.double(x), ...))
}

#' @noRd
#' @method max percentage
#' @export
max.percentage <- function(x, ...) {
  as.percentage(max(as.double(x), ...))
}

#' @noRd
#' @method mean percentage
#' @export
mean.percentage <- function(x, ...) {
  as.percentage(mean(as.double(x), ...))
}

#' @noRd
#' @method median percentage
#' @export
median.percentage <- function(x, ...) {
  as.percentage(median(as.double(x), ...))
}

#' @noRd
#' @method summary percentage
#' @export
summary.percentage <- function(object, ...) {
  c("Class" = "percentage",
    "<NA>" = length(object[is.na(object)]),
    "Min." = format(min(object)),
    "Mean" = format(mean(object)),
    "Max." = format(max(object)))
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.percentage <- function(x, ...) {
  "pct"
}

#' @importFrom vctrs vec_ptype_full
#' @export
vec_ptype_full.percentage <- function(x, ...) {
  "percentage"
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.percentage <- function(x, ...) {
  pillar_shaft(as.numeric(x) * 100, ...)
}


#' @rdname percentage
#' @export
percentage <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- getdecimalplaces(as.double(x), minimum = 0, maximum = 1)
  }
  trimws(format(as.percentage(x), digits = digits))
}
