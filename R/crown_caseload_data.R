#' Data integrity checks for table C1 (Crown Court caseload)
#' @param x Table C1.
#' @return If the data is correct, nothing is returned.
#' @examples
#' crown_caseload_data(CREST_2016)
#' @export

crown_caseload_data <- function(x){

  message('Initiating crown_caseload_data.
\n\nExpects a data.frame with three columns: period, measure, value, where
measure is one of disposals, receipts or outstanding cases. The data.frame
should include historical data, which is used for checks on the quality of
this year\'s data, and for producing tables and plots. More information on
the format expected is given by ?crown_caseload_data().')

  message('\n*** Running integrity checks on input dataframe (x):')
  message('\nChecking input is properly formatted...')
  message('Checking x is a data.frame...')

  if (!is.data.frame(x)) stop("x must be a data.frame")

  message('Checking x has correct columns...')
  if (length(colnames(x)) != 3) stop("x must have three columns: period, variable and value")

  message('Checking x contains a period column...')
  if (!'period' %in% colnames(x)) stop("x must contain period column")

  message('Checking x contains a variable column...')
  if (!'variable' %in% colnames(x)) stop("x must contain variable column")

  message('Checking x does not contain missing values...')
  if (anyNA(x)) stop("x cannot contain any missing values")

  message('Checking for the correct number of rows...')
  if (nrow(x) != length(unique(x$variable)) * length(unique(x$period))) {
            warning("x does not appear to be well formed. nrow(x) should equal
            length(unique(x$variable)) * length(unique(x$period)). Check the of x.")
    }


  message('\n***Running statistical checks on input dataframe (x)...\n
  These tests are implemented using the package assertr see:
  https://cran.r-project.org/web/packages/assertr for more details.')

  # Check sensible range for year

  message('Checking years in a sensible range (2000:2020)...')

  x$year <- as.numeric(stringr::str_sub(x$period, 1, 4))
  assertr::assert(x, assertr::in_set(2000:2020), year)
  x <- select(x, - year)

  # Check that the correct levels are in variables

  message('Checking variables are correct...')

  # Save variables name lookup for use later

  variable_set <- c(
    "disposals"    = "Disposals (all)",
    "receipts"    = "Receipts (all)",
    "outstanding"     = "Outstanding cases (all)"
  )

  assertr::assert_(x, assertr::in_set(names(variable_set)), ~variable, error_fun = raise_issue)

  message('...passed')

  # Check for outliers ----

    # Check for simple outliers in the value column for each variable,
    # over the entire timeseries. Outliers are detected using
    # mean +- 3 * standard deviations, implemented in the
    # assertr::within_n_stds() function.

    message('Checking for outliers in each variable (within 3 standard deviations of the mean)...')

  x %>%
    assertr::insist(assertr::within_n_sds(3), value) %>%
    group_by(variable) %>%
    summarise(avg.value=mean(value))

  message('...passed')

  # Define the class here ----

  structure(
    list(
      df = x,
      colnames = colnames(x),
      type = colnames(x)[!colnames(x) %in% c('period','variable')],
      variable_set = variable_set,
      periods = unique(x$period)
    ),
    class = "crown_caseload_data")
  }
