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

  # If the dataframe has seven columns, it will have the p-Value columns. In this
  # case, we transform the last two columns to numeric:
  if (ncol(statsDf) == 7) {
    statsDf[, 6:7] <- sapply(statsDf[, 6:7], as.numeric)
  }

  # Then, we evaluate if the provided dataframe has 7 columns (to identify
  # if it contains p-values that we want to highlight if lower than 0.05) and
  # we also indirectly assess the levels of the variables. If the column
  # 'p.value.tSt' is present in the dataframe, then the group variable has two
  # levels and we will show in red p-values lower than 0.05 resulting from the
  # Student's t test and the Mann-Whitney test. If the dataframe has 7 columns
  # but it contains a p.Value associated with ANOVA, we will proceed otherwise.
  # The footnote also changes depending on the applied test.
  # Formatem la taula en funció dels nivells de la variable:
  if (ncol(statsDf) == 7 & "p.Value.tSt" %in% colnames(statsDf)) {
    groupedT <- kableExtra::kable(statsDf, col.names = c("Variable", variable, "Sample (NA)", "Mean ± SD", "Median", "p.Value[note]", "p.Value2[note]"), booktabs = TRUE, caption = "Numerical Summary by Grouping Variable",
                                  format = "html", align= "c") %>%
      kableExtra::row_spec(0, bold = T, color = "black", background = "#dcecf5") %>%
      kableExtra::column_spec(1, border_right = T) %>%
      kableExtra::kable_paper("hover", font_size = 18, fixed_thead = T,
                              html_font =  "\"Trebuchet MS\", verdana, sans-serif") %>%
      kableExtra::column_spec(6, color = ifelse(statsDf$p.Value.tSt < 0.05 & !is.na(statsDf$p.Value.tSt),
                                                "red", "black")) %>%
      kableExtra::column_spec(7, color = ifelse(statsDf$p.Value.MW < 0.05 & !is.na(statsDf$p.Value.MW),
                                                "red", "black")) %>%
      kableExtra::add_footnote(c("Student's t-test were performed to assess statistical significance (p.Value). The p-value is NA if one of the groups has a sample of N = 0",
                                 "P.Value2 refers to the p-value obtained using the Mann-Whitney non-parametric test. NA refers to the outcome of assessing statistical significance with a group N = 0"),
                               notation = "symbol") %>%
      kableExtra::pack_rows(index = table(packIndex), background = "#fde9b7")

    # Grouping variable with more than two levels:
  } else if(ncol(statsDf) == 7 & "p.Value.AOV" %in% colnames(statsDf)) {
    groupedT <- kableExtra::kable(statsDf, col.names = c("Variable", variable, "Sample (NA)", "Mean ± SD", "Median", "p.Value[note]", "p.Value2[note]"), booktabs = TRUE, caption = "Numerical Summary by Grouping Variable",
                                  format = "html", align= "c") %>%
      kableExtra::row_spec(0, bold = T, color = "black", background = "#dcecf5") %>%
      kableExtra::column_spec(1, border_right = T) %>%
      kableExtra::kable_paper("hover", font_size = 18, fixed_thead = T,
                              html_font =  "\"Trebuchet MS\", verdana, sans-serif") %>%
      kableExtra::column_spec(6, color = ifelse(statsDf$p.Value.AOV < 0.05 & !is.na(statsDf$p.Value.AOV),
                                                "red", "black")) %>%
      kableExtra::column_spec(7, color = ifelse(statsDf$p.Value.KW < 0.05 & !is.na(statsDf$p.Value.KW),
                                                "red", "black")) %>%
      kableExtra::add_footnote(c("An ANOVA test was performed to assess statistical significance (p.Value). The p-value is NA if one of the groups has a sample of N = 0. A p-value lower than 0.05 indicates that there are significative differences between groups; however, a post-hoc analysis is required to identify the specific differences between the tested groups.",
                                 "The second p-value was obtained using the Kruskal-Wallis non-parametric test. NA refers to the outcome of assessing statistical significance with a group N = 0. A p-value lower than 0.05 indicates that there are significative differences between groups; however, a post-hoc analysis is required to identify the specific differences between the tested groups."),
                               notation = "symbol") %>%
      kableExtra::pack_rows(index = table(packIndex), background = "#fde9b7")

    # Without p-values:
  } else {
    groupedT <- kableExtra::kable(statsDf, col.names = c("Variable", variable, "Sample (NA)", "Mean ± SD", "Median"), booktabs = TRUE, caption = "Numerical Summary by Grouping Variable", format = "html", align= "c") %>%
      kableExtra::row_spec(0, bold = T, color = "black", background = "#dcecf5") %>%
      kableExtra::column_spec(1, border_right = T) %>%
      kableExtra::kable_paper("hover", font_size = 18, fixed_thead = T, html_font =  "\"Trebuchet MS\", verdana, sans-serif") %>%
      kableExtra::pack_rows(index = table(packIndex), background = "#fde9b7")
  }

  # We print the generated table:
  print(groupedT)

  # Finally, we call the plotEDA function to perform the graphical part of the EDA:
  plotEDA(df, variable, export, exportAll)

}
