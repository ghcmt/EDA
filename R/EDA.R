#' Exploratory Data Analysis (EDA)
#'
#' This function receives several arguments: a dataframe, a vector with the
#' name of the uninformative columns that we want to remove, a vector with
#' the name of the desired grouping variables, a boolean to clean the variables
#' names and their levels with Janitor and an option to export the results
#' to a PDF file. It is the main function of the package and it will summon
#' basicSummary and summaryByVariable. Mainly, it will generate tables
#' for categoric and numeric variables (together or one table for each,
#' depending on the argument fullTable) with the desired metrics and its output
#' will be a formatted table with all these data. Then, it will summon the
#' summaryByVariable to obtain the plots and summaries for each grouping
#' variables (if needed). There are also additional options as to show
#' p-values with 2-levels grouping variables (argument statistics) and it
#' can also perform transformation of a variable of vector of variables to
#' natural logarithm (argument transf).
#'
#'
#' @param df A dataframe.
#' @param rmCol Vector of columns to remove of the dataframe.
#' @param groupingVar Vector of variables to perform summaries.
#' @param janitor Boolean. R-Friendly adaptation of variables and their levels.
#' @param fullTable Option to display a table with all variables (num and cat) or two tables (num and cat)
#' @param statistics p-Value columns in numerical summaries by grouping variable.
#' @param transf NatLog Transform of a numeric variable (or vector of numeric variables)
#' @param export Option to export the results to a PDF. Only name, without ".pdf".
#' @param exportAll Option to export plots by grouping variable to PDF.
#' @return NULL.
#' @importFrom purrr set_names map_df
#' @importFrom dplyr filter select_if select relocate n_distinct na_if group_by summarise
#' @importFrom qpdf pdf_combine
#' @importFrom janitor clean_names make_clean_names
#' @importFrom tidyr drop_na all_of
#' @import kableExtra ggplot2 gridExtra ggpubr
#' @export
EDA <- function(df, rmCol = NULL, groupingVar = NULL, janitor = TRUE, fullTable = TRUE,
                statistics = TRUE, transf = NULL, export = NULL, exportAll = FALSE){

  cat('\n## EDA \n')

  # Removal of desired columns:
  df <- df %>% select(!all_of(rmCol))

  # If the argument janitor is TRUE, we mutate the variables' names to R-friendly names.
  # We perform the same cleaning to the given variables in the 'groupingVar' and
  # 'transf' arguments.
  if (isTRUE(janitor)) {
    df <- df %>% clean_names()
    groupingVar <- groupingVar %>% make_clean_names()
    transf <- transf %>% make_clean_names()
  }

  # We start by calling the basicSummary function to perform an initial exploration
  # of the dataset:
  cat('\n### Initial descriptive of Dataframe \n')
  basicSummary(df)

  # Then, if there are some variables to transform, we perform the change to
  # natural logarithm and we store these data in new columns. These columns
  # will be placed after the original non-transformed column.
  if(!is.null(transf)) {
    for (var in transf) {
      transVar <- paste("log_", var, sep = "")
      df[transVar] <- log(df[var])
      df <- df %>% dplyr::relocate(transVar, .after=var)
    }
  }

  # We generate the numerical summary of the dataset:
  cat('\n### Numerical summary \n')
  varTable <- c()
  varCatTable <- c()
  varNumTable <- c()

  # Iteration through the dataframe. The metrics will change depending on the
  # type of the variable:
  for (i in names(df)) {
    var <- names(df[i])
    varType <- class(df[[i]])
    sample <- sum(!is.na(df[i]))
    NAs <- sum(is.na(df[i]))
    unique <- n_distinct(df[i])

    if (varType == "character" | varType == "factor") {
      varCat <- var
      varNum <- NA
      if (isTRUE(janitor)) {
        df[[i]] <- sapply(df[[i]], make_clean_names, USE.NAMES = F)
        df[[i]] <- na_if(df[[i]], 'na')
      }
      meanSD <- ""
      med <- ""
      fmtFreq <- ""
      minV <- ""
      maxV <- ""

      # If the number of unique values is too high (arbitrarily set at more than
      # 10), we don't print the frequency list in the table (as it will unnecessarily
      # enlarge the table)
      if (unique <= 10) {
        values <- table(df[i])
        roundV <- round(prop.table(values)*100, 2)
        listFreq <- list(roundV)
        freq <- gsub("^c\\(|\\)$", "", listFreq)
        fmtFreq <- gsub(", |$", "% \r \n ", freq)
      }

    } else {
      varNum <- var
      varCat <- NA
      fmtFreq <- ""
      meanSD <- paste(round(sapply(df[i], mean, na.rm = TRUE), 2), "±",
                     round(sapply(df[i], sd, na.rm = TRUE), 2))
      med <- round(sapply(df[i], median, na.rm = TRUE), 2)
      minV <- round(min(df[i], na.rm = TRUE), 2)
      maxV <- round(max(df[i], na.rm = TRUE), 2)

      # If the number of unique values of a given numeric variable is two,
      # we do not calculate some of the metrics and we treat these variables
      # as categoric by showing the frequency of their two values.
      if (unique == 2) {
        meanSD <- ""
        med <- ""
        values <- table(df[i])
        roundV <- round(prop.table(values)*100, 2)
        listFreq <- list(roundV)
        freq <- gsub("^c\\(|`|\\)$", "", listFreq)
        fmtFreq <- gsub(", |$", "% \r \n ", freq)
      }
    }

    # We generate the tables: varTable will contain all variables, varCatTable
    # only the categoric variables and varNumTable the numeric variables:
    varTable <- rbind(varTable,c(var, sample, NAs, meanSD, med, minV, maxV, fmtFreq))
    varCatTable <- rbind(varCatTable, c(varCat, sample, NAs, fmtFreq))
    varNumTable <- rbind(varNumTable, c(varNum, sample, NAs, meanSD, med, minV, maxV))

  }

  # Then, we transform these tables to dataframes to operate with them:
  varDF <- varTable %>% as.data.frame() %>% setNames(c("Variable", "N", "NAs[note]", "Mean ± SD", "Median", "Min", "Max", "Frequency"))
  varCatDF <- varCatTable %>% as.data.frame() %>% setNames(c("Variable", "N", "NAs[note]", "Frequency"))
  varNumDF <- varNumTable %>% as.data.frame() %>% setNames(c("Variable", "N", "NAs[note]", "Mean ± SD", "Median", "Min", "Max"))

  # Removal of rows without variable name:
  varCatDF <- varCatDF %>% drop_na(Variable)
  varNumDF <- varNumDF %>% drop_na(Variable)

  # We arbitrarily set the NA threshold at 10% of maximum observations.
  thrNA <- 0.10 * max(as.numeric(varDF$N))

  # And we print a table depending on the value of the fullTable argument.
  if (isTRUE(fullTable)) {
    fmtTable <- kable(varDF, col.names = c("Variable", "N", "NAs[note]", "Mean ± SD", "Median", "Min", "Max", "Frequency"), caption = "General Numerical Summary", format = "html", align= "c") %>%
      row_spec(0, bold = T, color = "black", background = "#dcecf5") %>%
      column_spec(1, bold = T, border_right = T) %>%
      column_spec(3, color = ifelse(as.numeric(varDF$NAs) > thrNA, "red", "black")) %>%
      add_footnote(c("Footnote 1: NAs are marked in red if their number is higher than 10% of total observations"), notation = "symbol") %>%
      kable_paper("hover", font_size = 18, fixed_thead = T, html_font =  "\"Trebuchet MS\", verdana, sans-serif")
    print(fmtTable)
  } else {
    # Categoric variables' table:
    catTable <- kable(varCatDF, col.names = c("Variable", "N", "NAs[note]", "Frequency"), caption = "Numerical Summary of Categoric Variables", format = "html", align= "c") %>%
      row_spec(0, bold = T, color = "black", background = "#dcecf5") %>%
      column_spec(1, bold = T, border_right = T) %>%
      column_spec(3, color = ifelse(as.numeric(varCatDF$NAs) > thrNA, "red", "black")) %>%
      add_footnote(c("Footnote 1: NAs are marked in red if their number is higher than 10% of total observations"), notation = "symbol") %>%
      kable_paper("hover", font_size = 18, fixed_thead = T, html_font =  "\"Trebuchet MS\", verdana, sans-serif")
    print(catTable)

    # Numeric variables' table:
    numTable <- kable(varNumDF, col.names = c("Variable", "N", "NAs[note]", "Mean ± SD", "Median", "Min", "Max"), caption = "Numerical Summary of Numeric Variables", format = "html", align= "c") %>%
      row_spec(0, bold = T, color = "black", background = "#dcecf5") %>%
      column_spec(1, bold = T, border_right = T) %>%
      column_spec(3, color = ifelse(as.numeric(varNumDF$NAs) > thrNA, "red", "black")) %>%
      add_footnote(c("Footnote 1: NAs are marked in red if their number is higher than 10% of total observations"), notation = "symbol") %>%
      kable_paper("hover", font_size = 18, fixed_thead = T, html_font =  "\"Trebuchet MS\", verdana, sans-serif")
    print(numTable)
  }

  # Next, we call the plotEDA function to generate the desired plots:
  plotEDA(df, variable = NULL, export, exportAll = FALSE)

  # Finally, if the groupingVar argument is not null, we also call the
  # summaryByVariable table.
  if (!is.null(groupingVar)) {
    for (var in groupingVar){
      summaryByVariable(df, var, export, exportAll, statistics)
    }
  }

}
