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

context("freq.R")

test_that("frequency table works", {
  expect_equal(nrow(freq(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5))), 5)
  
  nrs <- as.integer(runif(2000, min = 0, max = 20))

  expect_output(print(freq(nrs)))
  expect_output(print(freq(nrs, nmax = 5)))
  expect_output(print(freq(nrs, nmax = Inf, markdown = FALSE)))
  expect_output(print(freq(nrs, nmax = Inf)))
  expect_output(print(freq(nrs, nmax = NA)))
  expect_output(print(freq(nrs, nmax = NULL)))
  expect_output(print(freq(nrs, sort.count = FALSE)))
  expect_output(print(freq(nrs, markdown = TRUE)))
  expect_output(print(freq(nrs, markdown = TRUE), markdown = FALSE))
  expect_output(print(freq(nrs, markdown = TRUE), markdown = TRUE))
  expect_output(print(freq(nrs, quote = TRUE)))
  
  # character
  expect_output(print(freq(unclean$gender)))
  
  # with weights
  df <- data.frame(x = sample(letters, size = 100, replace = TRUE),
                   y = round(runif(100) * 100))
  expect_gt(sum(freq(df, wt = y)$count),
            sum(freq(df)$count))
})
