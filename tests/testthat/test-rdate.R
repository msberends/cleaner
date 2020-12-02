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

context("rdate.R")

test_that("Date checking works", {
  randomised <- rdate(5000)
  expect_identical(
    sort(as.vector(freq(randomised))), # as.vector.freq contains a date check
    sort(randomised)
  )
})

test_that("random date generation works", {
  expect_error(rdate(42, -1))
  expect_length(rdate(42), 42)
  expect_length(rdate(c(42, 42)), 2)
})
