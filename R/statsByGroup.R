#' Statistics by Grouping Variable
#'
#' This function receives as arguments a dataframe, a desired grouping variable
#' and a variable. It returns a summary of the main statistic metrics of the
#' desired variable for each level of the grouping variable. Depending on the
#' levels of the grouping variable and the argument statistics, it will generate
#' a table with or without p-values (Student's t test and Mann-Whitney
#' non-parametric test)
#'
#'
#' @param df A dataframe.
#' @param groupV A grouping variable.
#' @param var Dataframe variable in which metrics we are interested.
#' @param statistics Columns with p-value in numerical summaries.
#' @return Grouped statistics by variable.
#' @export
statsByGroup <- function(df, groupV, var, statistics = TRUE) {
  # We start by transforming these variables to symbol to ease their manipulation
  # inside the function (this prevents issues related to quotation marks)
  groupV <- sym(groupV)
  var <- sym(var)

  # Then, we calculate the levels of each grouping variable:
  levels <- dplyr::n_distinct(df$groupV)

  # Next, depending on the levels that we have just calculated and the value of the
  # argument 'statistics', we will generate a table with p-values or without them.
  # If the argument 'statistics' is true and the variable has two levels, we will
  # perform two statistical tests: the parametric Student's t test and the non-
  # parametric Mann-Whitney test. These two tests are contained within try-catch
  # because otherwise they would generate an error if all the values of one of the
  # levels are NAs. In this case, we will print NA as a result of the test.
  if (levels == 2 & isTRUE(statistics)) {
    groupedStats <- df %>%
      dplyr::group_by(!! groupV) %>%
      dplyr::summarise("Sample (NAs)" = paste(sum(!is.na(!! var)), " (", sum(is.na(!! var)), ")", sep = ""),
                       "Mean ± SD" = paste(round(mean(!! var, na.rm = TRUE), 2), "±", round(sd(!! var, na.rm = TRUE), 2)),
                       p.Value = format(tryCatch({t.test(!! var ~ !! groupV, data = df, na.action = na.omit)$p.value}, error = function(e){NA}), digits = 3),
                       p.Value2 = format(tryCatch({wilcox.test(!! var ~ !! groupV, data = df, na.action = na.omit)$p.value}, error = function(e){NA}), digits = 3))

  } else {
    groupedStats <- df %>%
      dplyr::group_by(!! groupV) %>%
      dplyr::summarise("Sample (NAs)" = paste(sum(!is.na(!! var)), " (", sum(is.na(!! var)), ")", sep = ""),
                       "Mean ± SD" = paste(round(mean(!! var, na.rm = TRUE), 2), "±", round(sd(!! var, na.rm = TRUE), 2)))
  }

  # Finally, the function will return a groupedStats table:
  return(groupedStats)
}
