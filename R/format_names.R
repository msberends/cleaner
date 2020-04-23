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

#' Cleaning column names
#' 
#' @param x a \code{data.frame}, \code{list} or character vector
#' @param ... when \code{x} is a \code{data.frame}: new column names to set, which can be named (in the form \code{old = "new"}). The original column names do not need to be quoted, see Examples.
#' @param snake_case logical to indicate whether the column names must be in \href{https://en.wikipedia.org/wiki/Snake_case}{snake case}. This will have no effect on manually set column names.
#' @param camelCase logical to indicate whether the column names must be in \href{https://en.wikipedia.org/wiki/camelCase}{camel case}. This will have no effect on manually set column names.
#' @param tolower,toupper logical to indicate whether the column names must be lower/upper case. This will have no effect on manually set column names.
#' @export
#' @examples 
#' df <- data.frame(Name.341ABC = "value", 
#'                  name_123def = "value",
#'                  This.is.a.column = "value")
#'                  
#' format_names(df, snake_case = TRUE)
#' 
#' format_names(df, camelCase = TRUE)
#' 
#' format_names(df, letters[1:3])
#' 
#' format_names(df, This.is.a.column = "a_new_colname")
#' 
#' rownames(mtcars) <- format_names(rownames(mtcars), snake_case = TRUE)
#' mtcars[, 1:5]
#' 
#' format_names(list(a = 1, b = 2), c("new_1", "new_2"))
#' 
#' \dontrun{
#' library(dplyr)
#' starwars %>%
#'   format_names(camelCase = TRUE) %>%            # new column names
#'   mutate(name = format_names(name, 
#'                               snake_case = TRUE)) # new values in column
#' }
format_names <- function(x,
                         ...,
                         snake_case = FALSE, 
                         camelCase = FALSE,
                         tolower = FALSE, 
                         toupper = FALSE) {
  if (is.data.frame(x)) {
    current <- colnames(x)
  } else if (is.list(x)) {
    current <- names(x)
  } else {
    current <- x
  }
  
  if (isTRUE(camelCase)) {
    current <- gsub("[^a-zA-Z0-9]+([a-zA-Z])", "\\U\\1", current, perl = TRUE)
    current <- gsub("[^a-zA-Z0-9]+", "", current)
    current <- gsub("^(.)", "\\L\\1", current, perl = TRUE)
  } else if (isTRUE(snake_case)) {
    current <- tolower(gsub("[^a-zA-Z0-9]+", "_", current))
  }
  
  if (isTRUE(toupper)) {
    current <- toupper(current)
  } else if (isTRUE(tolower)) {
    current <- tolower(current)
  }
  
  # new names as set by user
  new <- list(...)
  if (length(new) > 0) {
    if (length(new) == 1) {
      # could be set_colnames(df, c("a", "b")), so make every item a list index
      new_names <- names(new)
      new <- as.list(new[[1]])
      if (is.null(names(new))) {
        names(new) <- new_names
      }
    }
    if (length(new) > length(x)) {
      warning("ignoring ", length(new) - length(x), " values of `new` that exceed the number of columns of `x`")
      new <- new[1:length(x)]
    }
    for (n in seq_len(length(new))) {
      new_val <- new[[n]]
      names(new_val) <- names(new)[n]
      if (!is.null(names(new_val))) {
        current[current == names(new_val)] <- new_val
      } else {
        current[n] <- new_val
      }
    }
  }
  
  if (is.data.frame(x)) {
    colnames(x) <- current
  } else if (is.list(x)) {
    names(x) <- current
  } else {
    x <- current
  }
  x
}
