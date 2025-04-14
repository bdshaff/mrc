#' Replace Variables in a Formula
#'
#' This function replaces variables in a given formula with new variable names.
#'
#' @param formula A formula object where variables need to be replaced.
#' @param old_vars A character vector of variable names to be replaced.
#' @param new_vars A character vector of new variable names to replace the old ones.
#'
#' @return A formula object with the specified variables replaced.
#' @details The function uses regular expressions to match the variable names in the formula.


replace_variables_in_formula <- function(formula, old_vars, new_vars) {
  if (length(old_vars) != length(new_vars)) {
    stop("The lengths of old_vars and new_vars must be the same.")
  }
  updated_formula <- deparse(formula)
  for (i in seq_along(old_vars)) {
    updated_formula <- gsub(paste0("\\b", old_vars[i], "\\b"), new_vars[i], updated_formula)
  }
  as.formula(updated_formula)
}
