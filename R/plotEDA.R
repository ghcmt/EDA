#' Plots Exploratory Data Analysis
#'
#' This function receives as arguments a dataframe, a desired grouping variable
#' and an export option. It performs the graphical summary of the dataframe.
#' Depending on the argument variable, the output will be a summary of all the
#' variables of the dataframe (if 'variable' is null) or a summary focused
#' on one target variable. The type of the plots will change depending on
#' the 'variable' argument. The results of the function can be exported to a
#' PDF file, which name has to be specified when calling the function
#' (i.e., "Results", without ".pdf"). It does not plot variables if their number of
#' levels is higher than 10 (for categoric variables) or lower/equal to two
#' (for numeric variables). If 'variable' is not null but the exporting of plots to
#' a file is desired, changing the argument 'exportAll' to TRUE will generate
#' a PDF file for each grouping variable with the name of the original export
#' and the variable (i.e, "Results_variable.pdf").
#'
#'
#' @param df A dataframe.
#' @param variable A grouping variable.
#' @param export Option to export the results to a PDF.
#' @param exportAll Option to export plots by grouping variable to PDF.
#' @return NULL.
#' @export
plotEDA <- function(df, variable = NULL, export = NULL, exportAll = FALSE) {
  # We create a 'Results' folder inside the working directory.
  dir.create(file.path(getwd(), "results"))
  resultsDir <- file.path(getwd(), "results")

  if(!is.null(export) & !isTRUE(exportAll)) {
    cat('\n### Graphical summary \n')
    cat("Plots are stored at ", resultsDir, " in the file ", export, ".pdf", sep = "")
  }

  if (is.null(variable)) {
    if (is.null(export)) {
      cat('\n')
      cat('\n### Graphical summary \n')
      cat('\n#### Categorical variables \n')
    }
  } else if (isTRUE(exportAll)) {
    cat('\n')
    cat('\n### Graphical summary by grouping variable: ', variable, ' \n')
    cat("\n Plots are stored at ", resultsDir, " in the file ", export, "_", variable, ".pdf", sep = "")
  }
  else {
    cat('\n')
    cat('\n### Graphical summary by grouping variable: ', variable, ' \n')
    cat('\n#### Categorical variables \n')
  }

  # We define the numeric and categoric (or factor) variables with sapply:
  nums <- names(df)[sapply(df, is.numeric)]
  chars <- names(df)[sapply(df, is.character)]

  # Establishment of a common theme to all plots:
  plotTheme <- theme(axis.title.x = element_blank(),
                     text = element_text(size = 15), axis.line = element_line(),
                     panel.background = element_blank(),
                     panel.grid = element_blank(),
                     legend.text = element_text(size = 15),
                     legend.title = element_text(size = 15),
                     legend.key.size = unit(1.5, 'cm'))

  # We only want to plot categoric variables with less than 10 unique values,
  # as a high number of unique values would interfere with data interpretation:
  manyVal <- c()
  chars2Plot <- c()
  for (char in chars) {
    unique <- n_distinct(df[char])
    if (unique > 10) {
      manyVal <- append(manyVal, char)
    } else {
      chars2Plot <- append(chars2Plot, char)
    }
  }

  # We create a vector to save the plots of categoric variables:
  pChar <- vector("list", length = length(chars2Plot))

  # The plots created will depend on the argument 'variable':
  if (is.null(variable)) {
    for (i in seq_along(chars2Plot)) {
      g <- ggplot(df, aes_string(chars2Plot[i])) +
        geom_bar(color = "black", fill = "#c3e3f7", size = 1) + plotTheme + ggtitle(chars2Plot[i])
      pChar[[i]] <- g
    }
  } else {
    for (i in seq_along(chars2Plot)) {
      g <- ggplot(df, aes_string(chars2Plot[i], fill = variable)) +
        geom_bar(color = "black", size = 1, position = position_dodge()) +
        plotTheme + scale_fill_brewer(palette="Pastel1") + ggtitle(chars2Plot[i])
      pChar[[i]] <- g
    }
  }

  # If we don't want to export the plots to a pdf file, we will print the newly
  # generated plots; else, we will store these plots in the results folder:
  if(!is.null(export) & is.null(variable)) {
    plotlist <- gridExtra::marrangeGrob(pChar, ncol = 2, nrow = 6, top = grid::textGrob('\n Categorical variables \n', gp=grid::gpar(fontsize=30)), bottom = grid::textGrob(paste("\n The following variables were not plotted due to their high number (> 10) of levels:", paste(manyVal, collapse=", ")), '\n'))
    ggsave(file.path(resultsDir, "charVarPlots.pdf"), plotlist, width = 15, height = 25)
  } else if (!is.null(export) & isTRUE(exportAll) & !is.null(variable)) {
    fig <- ggarrange(plotlist = pChar, ncol=2, nrow = 6, common.legend = TRUE, legend="top")
    ggexport(fig, filename = file.path(resultsDir, paste("charVarPlots_", variable, ".pdf", sep = "")), width = 15, height = 25, verbose = FALSE)
  } else {
    print(ggarrange(plotlist = pChar, ncol=2, nrow = 6, common.legend = TRUE, legend="top"))
  }


  if (is.null(variable) & !is.null(manyVal) & is.null(export)) {
    cat('\n')
    cat("\n The following variables were not plotted due to their high number
        (> 10) of levels: ", paste(manyVal, collapse=", "), ".")
  }

  if (is.null(variable)) {
    if (is.null(export)) {
      cat('\n')
      cat('\n#### Numeric variables \n')
    }
  } else if (isTRUE(exportAll)) {
    cat('\n')
  }
  else {
    cat('\n')
    cat('\n#### Numeric variables \n')
  }

  # We will exclude numeric variables with two or less unique values, as these
  # variables are bound to be uninformative.
  binVar <- c()
  nums2Plot <- c()
  for (num in nums) {
    unique <- n_distinct(df[num])
    if (unique <= 2) {
      binVar <- append(binVar, num)
    } else {
      nums2Plot <- append(nums2Plot, num)
    }
  }

  pNum <- vector("list", length = length(nums2Plot))

  # And, as we did with categoric variables, the generated plots will change
  # depending on the value of the variable argument:
  if (is.null(variable)) {
    for (i in seq_along(nums2Plot)) {
      g <- ggplot(df, aes_string(nums2Plot[i])) + geom_density(alpha = 0.2, fill = "red", color = "darkred") + plotTheme + ggtitle(nums2Plot[i])
      pNum[[i]] <- g
    }
  } else {
    for (i in seq_along(nums2Plot)) {
      g <- ggplot(df, aes_string(nums2Plot[i], x = variable, y = nums2Plot[i], fill = variable)) +
        geom_boxplot(outlier.shape=NA) +
        geom_jitter(color = "black", width = 0.1, size = 0.6, alpha = 0.8) +
        plotTheme + scale_fill_brewer(palette="Pastel1") + ggtitle(nums2Plot[i])
      pNum[[i]] <- g
    }
  }

  # Finally, if we want to export the plots to a PDF file, we will merge the plots
  # of categoric and numeric variables into one single file. Then, we will remove
  # the other PDF files.
  if(!is.null(export) & is.null(variable) & !isTRUE(exportAll)) {
    plotlist <- gridExtra::marrangeGrob(pNum, ncol = 2, nrow = 6, top = grid::textGrob('\n Numerical variables \n', gp=grid::gpar(fontsize=30)), bottom = grid::textGrob(paste("\n The following variables were not plotted due to their binary distribution:", paste(binVar, collapse=", ")), '\n'))
    ggsave(file.path(resultsDir, "numVarPlots.pdf"), plotlist, width = 15, height = 25)
    qpdf::pdf_combine(input = c(file.path(resultsDir, "charVarPlots.pdf"), file.path(resultsDir, "numVarPlots.pdf")), output = c(file.path(resultsDir, paste(export, ".pdf", sep = ""))))
    file.remove(c(file.path(resultsDir, "charVarPlots.pdf"), file.path(resultsDir, "numVarPlots.pdf")))
  } else if (!is.null(export) & isTRUE(exportAll) & !is.null(variable)) {
    fig <- ggarrange(plotlist = pNum, ncol=2, nrow = 6, common.legend = TRUE, legend="top")
    ggexport(fig, filename = file.path(resultsDir, paste("numVarPlots_", variable, ".pdf", sep = "")), width = 15, height = 25, verbose = FALSE)
    qpdf::pdf_combine(input = c(file.path(resultsDir, paste("charVarPlots_", variable, ".pdf", sep = "")), file.path(resultsDir, paste("numVarPlots_", variable, ".pdf", sep = ""))), output = c(file.path(resultsDir, paste(export, "_", variable, ".pdf", sep = ""))))
    file.remove(c(file.path(resultsDir, paste("numVarPlots_", variable, ".pdf", sep = "")), file.path(resultsDir, paste("charVarPlots_", variable, ".pdf", sep = ""))))
  } else {
    print(ggarrange(plotlist = pNum, ncol=2, nrow = 6, common.legend = TRUE, legend="top"))
  }


  if (is.null(variable) & !is.null(binVar) & is.null(export) & !isTRUE(exportAll)) {
    cat('\n')
    cat("\n The following variables were not plotted due to its binary
        distribution: ", paste(binVar, collapse=", "), ".")
  }

}
