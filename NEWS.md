# cleaner 1.5.3

* CRAN fix for macOS (r-release-macos-arm64)


# cleaner 1.5.2

* Fix for latest R-devel, that does not allow `digits = 0` for `format()`
* `clean_Date()` now supports month-year format for which it sets the day as 1:
  ```r
  clean_Date("March")
  #> (assuming format 'mmmm')
  #> [1] "2021-03-01"
  clean_Date("March 2020")
  #> (assuming format 'mmmm yyyy')
 #> [1] "2020-03-01"
  ```
* `freq()` now contains a `wt` argument to set the weights. The default (`NULL`) yields the old behaviour.
* Fixed a bug in `clean_POSIXct()` that led to the warning `Incompatible methods ("Ops.POSIXt", "Ops.Date") for ">"`

# cleaner 1.5.1

* New function `format_p_value()` to format raw p values according to the APA guideline
* `clean_Date()` now works with POSIX standards:
  ```r
  clean_Date("2020-11-12 12:24:12")
  clean_Date(c("2020-11-12 12:24:12", "2020-11-13"), guess_each = TRUE)
  ```
* Currency now prints and formats without symbols as default, use `as_symbol = TRUE` to print/format with currency symbols
* Support for older versions of R (v3.2)

# cleaner 1.5.0

* New function `format_names()` to quickly and easily change names of `data.frame` columns, `list`s or `character` vectors.
  ```r
  df <- data.frame(old.name = "test1", value = "test2")
  format_names(df, snake_case = TRUE)
  format_names(df, camelCase = TRUE)
  format_names(df, c(old.name = "new_name", value = "measurement"))
  
  library(dplyr)
  starwars %>% 
    format_names(camelCase = TRUE) %>% # column names
    mutate(name = name %>% 
             format_names(snake_case = TRUE)) # values in column
  ```
  
* New generic function `na_replace()` to replace `NA` values in any data type. Its default replacement value is dependent on the data type that is given as input: `0` for numeric values and class `matrix`, `FALSE` for class `logical`, today for class `Date`, and `""` otherwise.
  ```r
  na_replace(c(1, 2, NA, NA))
  #> [1] 1 2 0 0
  na_replace(c(1, 2, NA, NA), replacement = -1)
  #> [1]  1  2 -1 -1
  
  library(dplyr)
  starwars %>% 
    na_replace(hair_color) # only replace NAs in this column
    
  starwars %>% 
    na_replace() # replace NAs in all columns ("" for hair_color and 0 for birth_year)
  ```
* Support for the upcoming R 4.1.0

# cleaner 1.4.0

* New function `rdate()` to generate random dates (in analogy to e.g. `runif()`)
* Frequency tables (`freq()`):
  * Added availability of data to header
  * Fix for using `na.rm`
  * Fix for transforming to a visual histogram with `hist()`
  * New method for using `format()` on a frequency table
  * New method for transforming the values of a frequency table to a vector with `as.vector()`, which also supports dates
    ```r
    library(dplyr)
    library(cleaner)
    data.frame(dates = rdate(100)) %>% 
      freq(dates) %>% 
      as.vector()
    ```
* Fix for `clean_Date()` not accepting already `POSIX` or `Date` input 
* When using `clean_Date(..., guess_each = TRUE)` it now accepts the `format` parameter as a vector of options to let it choose from
* `clean_Date()` and `clean_POSIXct` gained a parameter `max_date` (that defaults to today), so that they will never return years beyond a specified date:
  ```r
  # old
  clean_Date("23-01-67")
  #> [1] "2067-01-23"
  
  # new
  clean_Date("23-01-67")
  #> [1] "1967-01-23"
  #> Warning: Some years were decreased by 100 to not exceed today.
  #>          Use clean_Date(..., max_date = Inf) to prevent this.
  clean_Date("23-01-67", max_date = Inf)
  #> [1] "2067-01-23"
  ```
* Cleaned all code using the `lintr` package

# cleaner 1.3.1

* Fixed a bug when using a `percentage` class into the `percentage()` function, i.e. `percentage(as.percentage(1))` would fail
* Fixed extremely small percentages, like `as.percentage(2.5e-14)`

# cleaner 1.3.0

* Added functions `clean_double()` and `clean_integer()`
* Added a method for `median()` in percentages
* Fixed a bug where `NA` in percentages would not be formatted correctly
* Fixed a bug in frequency tables where sometimes the number of digits used for percentages would be astronomical

# cleaner 1.2.0

* **DUE TO CRAN POLICY: RENAMED TO PACKAGE TO `cleaner`**
* Added support for percentages as a new class: `as.percentage()` and `clean_percentage()`. They also come with 'S3 methods' for `print`, `format`, `sum`, `min` and `max`.
* More robust coercing of dates
* Support for negative values for `clean_numeric()`, `clean_percentage()` and `clean_currency()`
* Fix for `clean_character()` on R v3.5 and lower
* Fix for digits in frequency tables for numeric values

# clean 1.1.0

* Added support for currency as a new class: `as.currency()` and `clean_currency()`. They also come with 'S3 methods' for `print`, `format`, `sum`, `min` and `max`.
* Added `clean_POSIXct()` to clean date/time objects
* `top_freq()` now correctly selects bottoms items using negative a number for *n*
* `freq.default()` is now exported for use in other packages
* All numeric calculation in the header of frequency tables now use the same algorithm as used by Minitab and SPSS (see 'Type 6' on `stats::quantile()`)
* More robust results for `clean_character()`, it also keeps in-between spaces now
* `clean_numeric()` now supports currency
* Fix for `freq()` where the precentage of NAs in the header was not calculated right

# clean 1.0.0

* First release
