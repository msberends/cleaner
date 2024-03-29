# ==================================================================== #
# TITLE                                                                #
# cleaner: Fast and Easy Data Cleaning                                 #
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

context("clean.R")

test_that("cleaning works", {
  skip_on_cran()

  expect_equal(clean_logical(c("Yes", "No", "Invalid", "Unknown")),
               c(TRUE, FALSE, NA, NA))
  expect_equal(clean_logical(x = c("Positive", "Negative", "Unknown", "Some value"),
                             true = "pos",
                             false = "neg"),
               c(TRUE, FALSE, NA, NA))
  
  gender_age <- c("male 0-50", "male 50+", "female 0-50", "female 50+")
  expect_equal(clean_factor(gender_age, levels = c("M", "F")),
               factor(c("M", "M", "F", "F"), levels = c("M", "F")))
  expect_equal(clean_factor(gender_age, levels = c("Male", "Female")),
               factor(c("Male", "Male", "Female", "Female"), levels = c("Male", "Female")))
  
  expect_equal(clean_factor(gender_age, levels = c("0-50", "50+"), ordered = TRUE),
               factor(c("0-50", "50+", "0-50", "50+"), levels = c("0-50", "50+"), ordered = TRUE))
  
  values <- c("no5538", "no929", "yes2390", "no841", "no2610")
  expect_equal(clean_logical(values),
               c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(clean_character(values),
               c("no", "no", "yes", "no", "no"))
  expect_equal(clean_numeric(values),
               c(5538, 929, 2390, 841, 2610))
  expect_equal(clean_double(values),
               c(5538, 929, 2390, 841, 2610))
  
  expect_equal(clean_numeric(values) * 0.25,
               c(1384.50, 232.25, 597.50, 210.25, 652.50))
  expect_equal(clean_integer(clean_numeric(values) * 0.25),
               c(1384,    232,    597,    210,    652))
  
  expect_equal(clean_Date("2020-11-12 12:24:12"),
               as.Date("2020-11-12"))
  
  expect_equal(
    suppressMessages(
      suppressWarnings(
        clean_Date(c("01-02-50", "01-02-50"),
                   format = c("dd-mm-yy", "dd-mm-yyyy"),
                   max_date = c("2020-01-01", "2070-01-01"))
      )),
    as.Date(c("1950-02-01", "2050-02-01")))
  
  expect_equal(
    as.Date(
      suppressMessages(
        suppressWarnings(
          clean_POSIXct(c("01-02-50", "01-02-50"),
                        format = c("dd-mm-yy", "dd-mm-yyyy"),
                        max_date = c("2020-01-01", "2070-01-01"), tz = "UTC")
        ))),
    as.Date(c("1950-02-01", "2050-02-01"), tz = "UTC"))
  
})
