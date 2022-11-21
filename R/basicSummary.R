#' Basic Summary
#'
#' This function receives as arguments the target dataframe. It returns a very
#' basic descriptive of the dataframe: the number of observations, the number
#' of variables, how many numeric, categoric and factor variables there are and the
#' percentage of complete cases (rows without NAs) and complete variables
#' (columns without NA).
#'
#'
#' @param df A dataframe.
#' @return NULL.
#' @export
basicSummary <- function(df) {
  # We start by calculating the number of rows and columns of the dataset:
  rows <- nrow(df)
  cols <- ncol(df)

  # Then, we count the number of numeric, categoric and factor columns:
  numCols <- length(names(df)[sapply(df, is.numeric)])
  charCols <- length(names(df)[sapply(df, is.character)])
  factCols <- length(names(df)[sapply(df, is.factor)])

  # Next, we calculate the number of complete cases (number of rows without NAs)
  # and complete variables (number of variables without NAs). We print the number
  # of these cases and the percentage.
  compCases <- nrow(df %>% filter(complete.cases(.)))
  compCasesPct <- paste(compCases, '/', rows, ' = ', round(((compCases/rows) * 100), 2), '%')
  comVars <- df %>% select_if(~ all(!is.na(.))) %>% length()
  comVarsPct <- paste(comVars, '/', cols, ' = ', round(((comVars/cols) * 100), 2), '%')

  # Setting of the column and values of the dataframe:
  description <- c("Number of rows", "Number of variables", "Number of numeric variables", "Number of categorical variables", "Number of factor variables","Percentage of complete cases (rows without NAs)", "Percentage of complete variables (variables without NAs)")
  values <- c(rows, cols, numCols, charCols, factCols, compCasesPct, comVarsPct)

  # Creation of the dataframe and generation of the table:
  summary <- data.frame(description, values)
  fmtSum <- kable(summary, booktabs = TRUE, caption = "Basic description of the Dataframe", format = "html", align= "c", col.names = c("Description", "Value")) %>%
    row_spec(0, bold = T, color = "black", background = "#dcecf5") %>%
    column_spec(1, border_right = T) %>%
    kable_paper("hover", font_size = 18, fixed_thead = T,
                html_font =  "\"Trebuchet MS\", verdana, sans-serif")

  print(fmtSum)

}
