# ==================================================================== #
# TITLE                                                                #
# Fast and Easy Data Cleaning                                          #
#                                                                      #
# SOURCE                                                               #
# https://github.com/msberends/cleaner                                 #
#                                                                      #
# LICENCE                                                              #
# (c) 2022 Berends MS (m.s.berends@umcg.nl)                            #
#                                                                      #
# This R package is free software; you can freely use and distribute   #
# it for both personal and commercial purposes under the terms of the  #
# GNU General Public License version 2.0 (GNU GPL-2), as published by  #
# the Free Software Foundation.                                        #
#                                                                      #
# This R package was publicly released in the hope that it will be     #
# useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ==================================================================== #

#' Replace NA values
#' 
#' This is a generic function to replace NA values in data. It takes most data types as input and is extendible by other packages.
#' @param x any vector, data.frame, matrix or list with values of which \code{NA} must be replaced
#' @param ... When \code{x} is a \code{data.frame}: columns of \code{x} to affect. This supports tidy evaluation without the need to quote the columns, see Examples.
#' @param replacement value to replace \code{NA} with. This is at default: \code{0} for numeric values and class \link{matrix}, \code{FALSE} for class \link{logical}, today for class \code{Date}, and \code{""} otherwise. Can also be a vector with the length of the number of NAs of \code{x} (\code{sum(is.na(x))}). When \code{x} is a \code{data.frame}, this can be a vector with the length of the number of columns to be affected, see Examples.
#' @details All functions preserve attributes. Within a \code{list} or \code{data.frame}, all attributes per index/item/column are also preserved.
#' @export
#' @rdname na_replace
#' @examples
#' mtrx <- matrix(c(1, 2, NA, 3), nrow = 2)
#' mtrx
#' na_replace(mtrx)
#' 
#' na_replace(c(1, 2, NA, NA))
#' na_replace(c(1, 2, NA, NA), replacement = -1)
#' na_replace(c(1, 2, NA, NA), replacement = c(0, -1))
#' 
#' na_replace(c(Sys.Date(), NA)) # replacement defaults to 'today'
#' 
#' na_replace(c(TRUE, FALSE, NA))
#' na_replace(c(TRUE, FALSE, NA), replacement = TRUE)
#' 
#' # we're flexible, the class only remains the same if
#' # the replacement value allows it
#' na_replace(c(1, 2, 3, NA), replacement = "-")
#' 
#' # data.frame is a special case
#' mtcars[1:6, c("mpg", "hp")] <- NA
#' head(mtcars)
#' head(na_replace(mtcars, mpg, hp)) # no need to quote columns (but you can)
#' head(na_replace(mtcars, mpg, hp, replacement = c(999, 123)))
#' 
#' \dontrun{
#' # practical way using tidyverse
#' library(dplyr)
#' starwars %>% 
#'   na_replace()
#' 
#' # even maintains groups
#' starwars %>%
#'   group_by(hair_color) %>%
#'   na_replace(hair_color, replacement = "TEST!") %>% 
#'   summarise(n = n())
#' }
na_replace <- function(x, ...) {
  UseMethod("na_replace")
}

na_replace_exec <- function(x, replacement) {
  if (length(replacement) == 1) {
    replacement <- rep(replacement, sum(is.na(x)))
  } else if (length(replacement) != sum(is.na(x))) {
    stop("`replacement` must be length 1 or ", sum(is.na(x)), " (the number of NAs in x)", call. = FALSE)
  }
  attrbt <- attributes(x) # keep attributes
  x[is.na(x)] <- replacement
  attributes(x) <- attrbt
  x
}

#' @method na_replace default
#' @export
#' @rdname na_replace
na_replace.default <- function(x, replacement = "", ...) {
  na_replace_exec(x, replacement)
}

#' @method na_replace data.frame
#' @importFrom rlang enquos eval_tidy as_name
#' @rdname na_replace
#' @export
na_replace.data.frame <- function(x, ..., replacement = NULL) {
  # save attributes
  attrbt <- attributes(x)
  
  user_exprs <- enquos(...)
  if (length(user_exprs) == 0) {
    # affect all columns if none is set, but don't include lists
    user_exprs <- as.list(colnames(x[, !sapply(x, is.list)]))
  }
  if (!is.null(replacement)) {
    if (length(replacement) == 1) {
      replacement <- rep(replacement, length(user_exprs))
    }
    if (length(replacement) != length(user_exprs)) {
      stop("`replacement` must be length 1 or ", length(user_exprs), call. = FALSE)
    }
  }
  
  y <- lapply(user_exprs,
              function(user_expr, 
                       data = as.data.frame(x, stringsAsFactors = FALSE)) {
                ev <- eval_tidy(expr = user_expr, data = data)
                if (identical(ev, as.character(as_name(user_expr)))) {
                  ev <- data[, ev, drop = TRUE]
                }
                ev
              })
  names(y) <- as.character(sapply(user_exprs, as_name))
  
  for (col in seq_len(length(names(y)))) {
    vctr <- y[[col]]
    vctr_colname <- names(y)[col]
    attr_vctr <- attributes(x[, vctr_colname])
    if (is.null(replacement)) {
      replace_val <- ifelse(is.numeric(vctr), 0,
                            ifelse(is.logical(vctr), FALSE,
                                   ifelse(is.Date(vctr), Sys.Date(),
                                          "")))
    } else {
      replace_val <- replacement[col]
    }
    vctr[is.na(vctr)] <- replace_val
    x[, vctr_colname] <- vctr
    attributes(x[, vctr_colname]) <- attr_vctr
    # special need for dplyr grouping: support it without the need to be dependent on dplyr :-)
    if (!is.null(attrbt$groups)) {
      if (vctr_colname %in% colnames(attrbt$groups)) {
        attrbt$groups[which(is.na(attrbt$groups[, vctr_colname, drop = TRUE])), vctr_colname] <- replace_val
        # groups are always ordered on alphabet, so order it again with the new replacement value
        attrbt$groups <- attrbt$groups[order(attrbt$groups[, vctr_colname]), ]
      }
    }
  }
  
  attributes(x) <- attrbt
  x
}

#' @method na_replace matrix
#' @rdname na_replace
#' @export
na_replace.matrix <- function(x, replacement = 0, ...) {
  dims <- dim(x)
  attrbt <- attributes(x) # like dimnames and other things
  mtrx <- matrix(
    sapply(x, function(x) {
    x[is.na(x)] <- replacement
    x
  }), nrow = dims[1], ncol = dims[2])
  attributes(mtrx) <- attrbt
  mtrx
}

#' @method na_replace list
#' @rdname na_replace
#' @export
na_replace.list <- function(x, replacement = NULL, ...) {
  attrbt <- attributes(x) # like dimnames and other things
  lst <- lapply(x, function(vctr) {
    attr_vctr <- attributes(vctr)
    if (is.null(replacement)) {
      replace_val <- ifelse(is.numeric(vctr), 0,
                            ifelse(is.logical(vctr), FALSE,
                                   ifelse(is.Date(vctr), Sys.Date(),
                                          "")))
    } else {
      replace_val <- replacement[1L]
    }
    vctr[is.na(vctr)] <- replace_val
    attributes(vctr) <- attr_vctr
    vctr
  })
  attributes(lst) <- attrbt
  lst
}

#' @method na_replace character
#' @noRd
#' @export
na_replace.character <- function(x, replacement = "", ...) {
  na_replace_exec(x, replacement)
}

#' @method na_replace numeric
#' @rdname na_replace
#' @export
na_replace.numeric <- function(x, replacement = 0, ...) {
  na_replace_exec(x, replacement)
}

#' @method na_replace integer
#' @noRd
#' @export
na_replace.integer <- function(x, replacement = 0, ...) {
  na_replace_exec(x, replacement)
}

#' @method na_replace Date
#' @rdname na_replace
#' @export
na_replace.Date <- function(x, replacement = Sys.Date(), ...) {
  na_replace_exec(x, replacement)
}

#' @method na_replace logical
#' @rdname na_replace
#' @export
na_replace.logical <- function(x, replacement = FALSE, ...) {
  na_replace_exec(x, replacement)
}
