#' Makes a codebook from a data frame
#'
#' Describes features of variables in a data frame relevant for synthesis.
#'
#' @param data a data frame with a data set to be synthesised.
#' @param maxlevs the number of factor levels above which separate tables with
#'   all labels are returned as part of \code{labs} component.
#'
#' @return  a list with two components:
#' - **tab**: a data frame with the following information about each variable:
#'   - **name**: variable name
#'   - **class**: class of variable
#'   - **nmiss**: number of missing values (`NA`)
#'   - **perctmiss**: percentage of missing values
#'   - **ndistinct**: number of distinct values (excluding missing values)
#'   - **details**: range for numeric variables, maximum length for character
#'     variables, labels for factors with <= maxlevs levels
#'
#' - **labs**: a list of extra tables with labels for each factor with a
#'   number of levels greater than `maxlevs`.
#'
#' @examples
#' codebook.syn(SD2011)
#' @export
codebook.syn <- function(data, maxlevs = 3) {
  if (!(is.data.frame(data))) {
    stop("codebook.syn() requires a data frame as a parameter.\n", call. = FALSE)
  }
  n <- dim(data)[[1]]
  p <- dim(data)[[2]]

  # calculate number and % of missing and non-missing values
  nmiss <- sapply(data, function(x) length(x[is.na(x)]))
  perctmiss <- round(nmiss / n * 100, 2)
  nok <- sapply(data, function(x) length(x[!is.na(x)]))
  ndistinct <- sapply(data, function(x) length(table(x)))
  dfclass <- sapply(data, class)

  fortab2 <- details <- rep("", length(nmiss))

  for (i in 1:p) {
    if (any(class(data[, i]) == "character")) {
      details[i] <- paste("Max length: ",
        max(nchar(data[, i])),
        sep = ""
      )
    }
    if (any(class(data[, i]) == "numeric")) {
      details[i] <- paste("Range: ",
        min(data[!is.na(data[, i]), i]), " - ", max(data[!is.na(data[, i]), i]),
        sep = ""
      )
    }
    if (any(class(data[, i]) == "factor") & ndistinct[i] > maxlevs) {
      details[i] <- "See table in labs"
      fortab2[i] <- paste("'", paste(names(table(data[, i])), collapse = "' '"),
        "'",
        sep = ""
      )
    }
    if (any(class(data[, i]) == "factor") & ndistinct[i] <= maxlevs) {
      details[i] <-
        paste("'", paste(names(table(data[, i])), collapse = "' '"), "'", sep = "")
    }
  }

  if (any(grepl("factor", sapply(data, class)) & ndistinct > maxlevs)) {
    vnum <- (1:p)[grepl("factor", sapply(data, class)) & ndistinct > maxlevs]
    tabs2 <- vector("list", length(vnum))
    names(tabs2) <- names(data)[vnum]
    for (i in 1:length(vnum)) {
      tabs2[[i]] <- data.frame(label = names(table(data[, vnum[i]])))
    }
  } else {
    tabs2 <- NULL
  }

  result <- data.frame(
    variable = names(data), class = sapply(dfclass, paste, collapse = "-"),
    nmiss = nmiss, perctmiss = perctmiss,
    ndistinct = ndistinct, details = details
  )
  rownames(result) <- 1:p
  list(tab = result, labs = tabs2)
}

# usethis::use_test("codebook_syn")
