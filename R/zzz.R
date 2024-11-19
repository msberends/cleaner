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

pkg_env <- new.env(hash = FALSE)
.onLoad <- function(libname, pkgname) {
  backports::import(pkgname)
  # read symbols from external file to prevent CRAN errors
  symbols <- tryCatch(utils::read.delim(system.file("symbols.txt", package = "cleaner"), sep = ","), error = function(e) NULL)
  if (!is.null(symbols)) {
    pkg_env$symbols <- stats::setNames(symbols$symbol, symbols$txt)
  } else {
    pkg_env$symbols <- character(0)
  }
}
