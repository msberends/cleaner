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

#' Transform to currency
#' 
#' Transform input to a currency. The actual values are numeric, but will be printed as formatted currency values.
#' @param x input
#' @param currency_symbol the currency symbol to use, which defaults to the current system locale setting (see \code{\link{Sys.localeconv}})
#' @param decimal.mark symbol to use as a decimal separator, defaults to \code{\link{getOption}("OutDec")}
#' @param big.mark symbol to use as a thousands separator, defaults to a dot if \code{decimal.mark} is a comma, and a comma otherwise 
#' @param as_symbol try to format and print using currency symbols instead of text
#' @param ... other parameters passed on to methods
#' @details Printing currency will always have a currency symbol followed by a space, 2 decimal places and is never written in scientific format (like 2.5e+04).
#' @rdname currency
#' @name currency
#' @export
#' @examples 
#' money <- as.currency(c(0.25, 2.5, 25, 25000))
#' money
#' sum(money)
#' max(money)
#' mean(money)
#' 
#' format(money, currency_symbol = "USD")
#' format(money, currency_symbol = "EUR", decimal.mark = ",")
#' format(money, currency_symbol = "EUR", as_symbol = FALSE)
#' 
#' as.currency(2.5e+04)
as.currency <- function(x, currency_symbol = Sys.localeconv()["int_curr_symbol"], ...) {
  structure(.Data = as.double(x, ...),
            class = c("currency", "numeric"),
            currency_symbol = toupper(unname(currency_symbol)))
}

#' @rdname currency
#' @export
is.currency <- function(x) {
  identical(class(x), c("currency", "numeric"))
}

#' @method [ currency
#' @export
#' @noRd
"[.currency" <- function(x, ...) {
  y <- NextMethod()
  attributes(y) <- attributes(x)
  y
}
#' @method [<- currency
#' @export
#' @noRd
"[<-.currency" <- function(value) {
  y <- NextMethod()
  attributes(y) <- attributes(value)
  y
}
#' @method [[ currency
#' @export
#' @noRd
"[[.currency" <- function(x, ...) {
  y <- NextMethod()
  attributes(y) <- attributes(x)
  y
}
#' @method [[<- currency
#' @export
#' @noRd
"[[<-.currency" <- function(value) {
  y <- NextMethod()
  attributes(y) <- attributes(value)
  y
}
#' @method c currency
#' @export
#' @noRd
c.currency <- function(x, ...) {
  y <- NextMethod()
  attributes(y) <- attributes(x)
  y
}

txt2symb <- function(txt) {
  switch(txt,
         "USD" = "\u0024",
         "EUR" = "\u20ac",
         "JPY" = "\u00a5",
         "GBP" = "\u00a3",
         "CNY" = "\u5143",
         "KRW" = "\u20a9", 
         txt)
}

#' @rdname currency
#' @method print currency
#' @export
print.currency <- function(x, 
                           decimal.mark = getOption("OutDec"),
                           big.mark = ifelse(decimal.mark == ",", ".", ","),
                           as_symbol = TRUE,
                           ...) {
  currency_symbol <- toupper(trimws(attributes(x)$currency_symbol))
  if (isTRUE(as_symbol)) {
    currency_symbol <- txt2symb(currency_symbol)
  }
  print(paste0("`", trimws(paste0(currency_symbol, " ",
                                  trimws(format(as.numeric(x), 
                                                decimal.mark = decimal.mark, 
                                                big.mark = big.mark,
                                                big.interval = 3L,
                                                nsmall = 2L,
                                                digits = 2L,
                                                scientific = FALSE)))),
               "`"),
        quote = FALSE)
}

#' @rdname currency
#' @method format currency
#' @export
format.currency <- function(x, 
                            currency_symbol = attributes(x)$currency_symbol,
                            decimal.mark = getOption("OutDec"),
                            big.mark = ifelse(decimal.mark == ",", ".", ","),
                            as_symbol = TRUE,
                            ...) {
  currency_symbol <- toupper(trimws(currency_symbol))
  if (isTRUE(as_symbol)) {
    currency_symbol <- txt2symb(currency_symbol)
  }
  trimws(paste0(currency_symbol, " ",
                trimws(format(as.numeric(x), 
                              decimal.mark = decimal.mark, 
                              big.mark = big.mark,
                              big.interval = 3L,
                              nsmall = 2L,
                              digits = 2L,
                              scientific = FALSE))))
}

#' @noRd
#' @method sum currency
#' @export
sum.currency <- function(x, ...) {
  as.currency(sum(as.numeric(x), ...), currency_symbol = attributes(x)$currency_symbol)
}

#' @noRd
#' @method min currency
#' @export
min.currency <- function(x, ...) {
  as.currency(min(as.numeric(x), ...), currency_symbol = attributes(x)$currency_symbol)
}

#' @noRd
#' @method max currency
#' @export
max.currency <- function(x, ...) {
  as.currency(max(as.numeric(x), ...), currency_symbol = attributes(x)$currency_symbol)
}

#' @noRd
#' @method mean currency
#' @export
mean.currency <- function(x, ...) {
  as.currency(mean(as.numeric(x), ...), currency_symbol = attributes(x)$currency_symbol)
}

#' @noRd
#' @method median currency
#' @importFrom stats median
#' @export
median.currency <- function(x, ...) {
  as.currency(median(as.numeric(x), ...), currency_symbol = attributes(x)$currency_symbol)
}

#' @noRd
#' @method summary currency
#' @export
summary.currency <- function(object, ...) {
  c("Class" = paste0("currency", txt2symb(trimws(attributes(object)$currency_symbol))),
    "<NA>" = length(object[is.na(object)]),
    "Min." = format(min(object)),
    "Mean" = format(mean(object)),
    "Max." = format(max(object)))
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.currency <- function(x, ...) {
  paste0("crncy/", txt2symb(trimws(attributes(x)$currency_symbol)))
}

#' @importFrom vctrs vec_ptype_full
#' @export
vec_ptype_full.currency <- function(x, ...) {
  paste0("currency/", txt2symb(trimws(attributes(x)$currency_symbol)))
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.currency <- function(x, ...) {
  # format without currency symbol - it will already print in the tibble header
  out <- format(x, currency_symbol = "")
  out[is.na(x)] <- pillar::style_na(NA)
  pillar::new_pillar_shaft_simple(out, align = "right", min_width = 5, ...)
}
