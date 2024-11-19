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

#' Format p values (APA guideline)
#' 
#' This function will round p values according to the APA guideline. It will try to round to two decimals where possible, and will try to avoid printing the value of \code{alpha}, see Examples.
#' @param p p value(s) to transform
#' @param alpha the value of alpha, defaults to 0.05
#' @param prepend_p a logical to indicate whether "p =" should be prepended to the result
#' @export
#' @return A character
#' @examples 
#' format_p_value(0.345678)
#' format_p_value(0.05125)
#' 
#' # this must not be "0.05", but is not "0.049" either,
#' # so it will add as many decimals as needed:
#' format_p_value(0.04993) 
#' 
#' format_p_value(c(0.123, 0.00000001))
#' format_p_value(c(0.123, 0.00000001), prepend_p = TRUE)
format_p_value <- function(p, alpha = 0.05, prepend_p = FALSE) {
  
  p <- as.double(p)
  p_new <- rep(NA_character_, length(p))
  
  p_new[as.double(round(p, 2)) == alpha] <- round(p[as.double(round(p, 2)) == alpha], 3)
  p_new[as.double(round(p, 3)) == alpha] <- round(p[as.double(round(p, 3)) == alpha], 4)
  
  p_new[p < 0.001 & alpha > 0.001] <- "< 0.001"
  
  p[p < 0.01 & p == p_new] <- round(p[p < 0.01 & p == p_new], 3)
  p_new[is.na(p_new) & !is.na(p)] <- round(p[is.na(p_new) & !is.na(p)], 2)
  
  if (prepend_p == TRUE) {
    gsub("= <", "<", paste0("p = ", p_new), fixed = TRUE)
  } else {
    p_new
  }
}
