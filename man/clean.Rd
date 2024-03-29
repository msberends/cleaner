% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{clean}
\alias{clean}
\alias{clean.data.frame}
\alias{clean_logical}
\alias{clean_factor}
\alias{clean_numeric}
\alias{clean_double}
\alias{clean_integer}
\alias{clean_character}
\alias{clean_currency}
\alias{clean_percentage}
\alias{clean_Date}
\alias{clean_POSIXct}
\title{Clean column data to a class}
\source{
\href{https://www.bis.org/publ/rpfx16fx.pdf}{Triennial Central Bank Survey Foreign exchange turnover in April 2016} (PDF). Bank for International Settlements. 11 December 2016. p. 10.
}
\usage{
clean(x)

\method{clean}{data.frame}(x)

clean_logical(
  x,
  true = regex_true(),
  false = regex_false(),
  na = NULL,
  fixed = FALSE,
  ignore.case = TRUE
)

clean_factor(
  x,
  levels = unique(x),
  ordered = FALSE,
  droplevels = FALSE,
  fixed = FALSE,
  ignore.case = TRUE
)

clean_numeric(x, remove = "[^0-9.,-]", fixed = FALSE)

clean_double(x, remove = "[^0-9.,-]", fixed = FALSE)

clean_integer(x, remove = "[^0-9.,-]", fixed = FALSE)

clean_character(
  x,
  remove = "[^a-z \\t\\r\\n]",
  fixed = FALSE,
  ignore.case = TRUE,
  trim = TRUE
)

clean_currency(x, currency_symbol = NULL, remove = "[^0-9.,-]", fixed = FALSE)

clean_percentage(x, remove = "[^0-9.,-]", fixed = FALSE)

clean_Date(x, format = NULL, guess_each = FALSE, max_date = Sys.Date(), ...)

clean_POSIXct(
  x,
  tz = "",
  remove = "[^.0-9 :/-]",
  fixed = FALSE,
  max_date = Sys.Date(),
  ...
)
}
\arguments{
\item{x}{data to clean}

\item{true}{\link[base]{regex} to interpret values as \code{TRUE} (which defaults to \code{\link{regex_true}}), see Details}

\item{false}{\link[base]{regex} to interpret values as \code{FALSE} (which defaults to \code{\link{regex_false}}), see Details}

\item{na}{\link[base]{regex} to force interpret values as \code{NA}, i.e. not as \code{TRUE} or \code{FALSE}}

\item{fixed}{logical to indicate whether regular expressions should be turned off}

\item{ignore.case}{logical to indicate whether matching should be case-insensitive}

\item{levels}{new factor levels, may be named with regular expressions to match existing values, see Details}

\item{ordered}{logical to indicate whether the factor levels should be ordered}

\item{droplevels}{logical to indicate whether non-existing factor levels should be dropped}

\item{remove}{\link[base]{regex} to define the character(s) that should be removed, see Details}

\item{trim}{logical to indicate whether the result should be trimmed with \code{\link{trimws}(..., which = "both")}}

\item{currency_symbol}{the currency symbol to use, which will be guessed based on the input and otherwise defaults to the current system locale setting (see \code{\link{Sys.localeconv}})}

\item{format}{character string giving a date-time format as used by \link[base]{strptime}. 

For \code{clean_Date(..., guess_each = TRUE)}, this can be a vector of values to be used for guessing, see Examples.}

\item{guess_each}{logical to indicate whether all items of \code{x} should be guessed one by one, see Examples}

\item{max_date}{date (coercible with [as.Date()]) to indicate to maximum allowed of \code{x}, which defaults to today. This is to prevent that \code{clean_Date("23-03-47")} will return 23 March 2047 and instead returns 23 March 1947 with a warning.}

\item{...}{for \code{clean_Date} and \code{clean_POSIXct}: other parameters passed on these functions}

\item{tz}{time zone specification to be used for the conversion,
    \emph{if one is required}.  System-specific (see \link[base]{time zones}),
    but \code{""} is the current time zone, and \code{"GMT"} is UTC
    (Universal Time, Coordinated).  Invalid values are most commonly
    treated as UTC, on some platforms with a warning.}
}
\value{
The \code{clean_*} functions \strong{always} return the class from the function name:
\itemize{
  \item{\code{clean_logical()}: class \code{logical}}
  \item{\code{clean_factor()}: class \code{factor}}
  \item{\code{clean_numeric()} and \code{clean_double()}: class \code{numeric}}
  \item{\code{clean_integer()}: class \code{integer}}
  \item{\code{clean_character()}: class \code{character}}
  \item{\code{clean_percentage()}: class \code{percentage}}
  \item{\code{clean_currency()}: class \code{currency}}
  \item{\code{clean_Date()}: class \code{Date}}
  \item{\code{clean_POSIXct()}: classes \code{POSIXct/POSIXt}}
}
}
\description{
Use any of these functions to quickly clean columns in your data set. Use \code{clean()} to pick the functions that return the least relative number of \code{NA}s. They \strong{always} return the class from the function name (e.g. \code{clean_Date()} always returns class \code{Date}).
}
\details{
Using \code{clean()} on a vector will guess a cleaning function based on the potential number of \code{NAs} it returns. Using \code{clean()} on a data.frame to apply this guessed cleaning over all columns.

Info about the different functions:

\itemize{
  \item{\code{clean_logical()}:\cr}{Use parameters \code{true} and \code{false} to match values using case-insensitive regular expressions (\link[base]{regex}). Unmatched values are considered \code{NA}. At default, values are matched with \code{\link{regex_true}} and \code{\link{regex_false}}. This allows support for values "Yes" and "No" in the following languages: Arabic, Bengali, Chinese (Mandarin), Dutch, English, French, German, Hindi, Indonesian, Japanese, Malay, Portuguese, Russian, Spanish, Telugu, Turkish and Urdu. Use parameter \code{na} to override values as \code{NA} that would else be matched with \code{true} or \code{false}. See Examples.}
  \item{\code{clean_factor()}:\cr}{Use parameter \code{levels} to set new factor levels. They can be case-insensitive regular expressions to match existing values of \code{x}. For matching, new values for \code{levels} are internally temporary sorted descending on text length. See Examples.}
  \item{\code{clean_numeric()}, \code{clean_double()}, \code{clean_integer()} and \code{clean_character()}:\cr}{Use parameter \code{remove} to match values that must be removed from the input, using regular expressions (\link[base]{regex}). In case of \code{clean_numeric()}, comma's will be read as dots and only the last dot will be kept. Function \code{clean_character()} will keep middle spaces at default. See Examples.}
  \item{\code{clean_percentage()}:\cr}{This new class works like \code{clean_numeric()}, but transforms it with \code{\link{as.percentage}}, which will retain the original values, but will print them as percentages. See Examples.} 
  \item{\code{clean_currency()}:\cr}{This new class works like \code{clean_numeric()}, but transforms it with \code{\link{as.currency}}. The currency symbol is guessed based on the most traded currencies by value (see Source): the United States dollar, Euro, Japanese yen, Pound sterling, Swiss franc, Renminbi, Swedish krona, Mexican peso, South Korean won, Turkish lira, Russian ruble, Indian rupee and the South African rand. See Examples.}
  \item{\code{clean_Date()}:\cr}{Use parameter \code{format} to define a date format, or leave it empty to have the format guessed. Use \code{"Excel"} to read values as Microsoft Excel dates. The \code{format} parameter will be evaluated with \code{\link{format_datetime}}, which means that a format like \code{"d-mmm-yy"} with be translated internally to \code{"\%e-\%b-\%y"} for convenience. See Examples.}
  \item{\code{clean_POSIXct()}:\cr}{Use parameter \code{remove} to match values that must be removed from the input, using regular expressions (\link[base]{regex}). The resulting string will be coerced to a date/time element with class \code{POSIXct}, using \code{\link{as.POSIXct}()}. See Examples.}
}

The use of invalid regular expressions in any of the above functions will not return an error (like in base R), but will instead interpret the expression as a fixed value and will throw a warning.
}
\examples{
clean_logical(c("Yes", "No"))   # English
clean_logical(c("Oui", "Non"))  # French
clean_logical(c("ya", "tidak")) # Indonesian
clean_logical(x = c("Positive", "Negative", "Unknown", "Some value"),
              true = "pos", false = "neg")

gender_age <- c("male 0-50", "male 50+", "female 0-50", "female 50+")
clean_factor(gender_age, c("M", "F"))
clean_factor(gender_age, c("Male", "Female"))
clean_factor(gender_age, c("0-50", "50+"), ordered = TRUE)

clean_Date("13jul18", "ddmmmyy")
clean_Date("12 August 2010")
clean_Date("12 06 2012")
clean_Date("October 1st 2012")
clean_Date("43658")
clean_Date("14526", "Excel")
clean_Date(c("1 Oct 13", "October 1st 2012")) # could not be fitted in 1 format
clean_Date(c("1 Oct 13", "October 1st 2012"), guess_each = TRUE)
clean_Date(c("12-14-13", "1 Oct 2012"), 
           guess_each = TRUE,
           format = c("d mmm yyyy", "mm-yy-dd")) # only these formats will be tried

clean_POSIXct("Created log on 2020/02/11 11:23 by user Joe")
clean_POSIXct("Created log on 2020.02.11 11:23 by user Joe", tz = "UTC")

clean_numeric("qwerty123456")
clean_numeric("Positive (0.143)")
clean_numeric("0,143")
clean_numeric("minus 12 degrees")

clean_percentage("PCT: 0.143")
clean_percentage(c("Total of -12.3\%", "Total of +4.5\%"))

clean_character("qwerty123456")
clean_character("Positive (0.143)")

clean_currency(c("Received 25", "Received 31.40"))
clean_currency(c("Jack sent £ 25", "Bill sent £ 31.40"))

df <- data.frame(A = c("2 Apr 2016", "5 Feb 2020"), 
                 B = c("yes", "no"),
                 C = c("Total of -12.3\%", "Total of +4.5\%"),
                 D = c("Marker: 0.4513 mmol/l", "Marker: 0.2732 mmol/l"))
df
clean(df)
}
