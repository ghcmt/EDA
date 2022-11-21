#' Summary by Variable
#'
#' This function receives as arguments a dataframe and a desired grouping variable.
#' The function will obtain the numeric and categoric variables of the dataframe
#' and will summon the statsByGroup function to create a table with the summary
#' numeric variables based on the grouping variable given as an argument and the
#' value of the statistics argument. Then, it will use Kable to correctly format
#' the table. Finally, it will summon the plotEDA function to generate the
#' graphic summary, also providing the desired arguments regarding the export
#' of plots to PDF files.
#'
#' @param df A dataframe.
#' @param variable A grouping variable.
#' @param export Option to export first exploratory plots to PDF.
#' @param exportAll Option to export plots by grouping variables to PDF.
#' @param statistics Addition of columns with p-Values for the levels of the variable.
#' @return NULL.
#' @export
summaryByVariable <- function(df, variable, export = NULL, exportAll = FALSE, statistics = TRUE){
  cat('\n')
  cat('\n### Numerical summary by grouping variable: ', variable, '\n')

  # First, we obtain the numeric variables of the dataset:
  nums <- names(df)[sapply(df, is.numeric)]

  # Then, we pass these variables to the statsByGroup function to perform
  # a numerical summary of all of them.
  groupedSt <- sort(nums) %>%
    set_names() %>%
    map_df(~ statsByGroup(df, variable, .x, statistics), .id = 'Variable')

  # Next, we get the number of levels of the grouping variable and we transform
  # the table to a dataframe. The number of levels is important because we will
  # use it to automatically pack the rows of the table.
  packIndex <- rep(nums, each = n_distinct(df[variable]))
  statsDf <- as.data.frame(groupedSt)

  # If the dataframe has six columns, it will have the p-Value columns. In this
  # case, we transform these two columns to numeric:
  if (ncol(statsDf) == 6) {
    statsDf <- transform(statsDf, p.Value = as.numeric(p.Value),
                         p.Value2 = as.numeric(p.Value2))
  }

  # Then, the table is formatted depending on the number of columns of the DF
  # (that, in turn, depends on the levels of the variable and the 'statistics' argument):
  if (ncol(statsDf) == 6) {
    groupedT <- kable(statsDf, col.names = c("Variable", variable, "Sample (NA)", "Mean ± SD", "p.Value[note]", "p.Value2[note]"), booktabs = TRUE, caption = "Numerical Summary by Grouping Variable",
                      format = "html", align= "c") %>%
      row_spec(0, bold = T, color = "black", background = "#dcecf5") %>%
      column_spec(1, border_right = T) %>%
      kable_paper("hover", font_size = 18, fixed_thead = T,
                  html_font =  "\"Trebuchet MS\", verdana, sans-serif") %>%
      column_spec(5, color = ifelse(statsDf$p.Value < 0.05 & !is.na(statsDf$p.Value),
                                    "red", "black")) %>%
      column_spec(6, color = ifelse(statsDf$p.Value2 < 0.05 & !is.na(statsDf$p.Value2),
                                    "red", "black")) %>%
      add_footnote(c("Student's t-test were performed to assess statistical significance (p.Value). The p-value is NA if one of the groups has a sample of N = 0",
                     "P.Value2 refers to the p-value obtained using the Mann-Whitney non-parametric test. NA refers to the outcome of assessing statistical significance with a group N = 0"),
                   notation = "symbol") %>%
      pack_rows(index = table(packIndex), background = "#fde9b7")
  } else {
    groupedT <- kable(statsDf, col.names = c("Variable", variable, "Sample (NA)", "Mean ± SD"), booktabs = TRUE, caption = "Numerical Summary by Grouping Variable", format = "html", align= "c") %>%
      row_spec(0, bold = T, color = "black", background = "#dcecf5") %>%
      column_spec(1, border_right = T) %>%
      kable_paper("hover", font_size = 18, fixed_thead = T, html_font =  "\"Trebuchet MS\", verdana, sans-serif") %>%
      pack_rows(index = table(packIndex), background = "#fde9b7")
  }

  # We print the generated table:
  print(groupedT)

  # Finally, we call the plotEDA function to perform the graphical part of the EDA:
  plotEDA(df, variable, export, exportAll)

}
