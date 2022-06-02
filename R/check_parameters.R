#' Validate parameters without knitting a Rmd file
#'
#' @param rmarkdown_document path to a Rmarkdown document (i.e. \code{*.Rmd}) to be inspected.
#' @param params a named list of arguments to use for the report. Names must be a subset of the declarations in the yaml header of Rmarkdown report supplied.
#' @param allow_NULL Should \code{NULL} as a parameter value be accepted for parameters? This takes precedence over how check functions may handle \code{NULL} values.
#' @param allow_NA Should \code{NA} as a parameter value be accepted for parameters? This takes precedence over how check functions may handle \code{NA} values.
#' @param verbose Set \code{TRUE} to get verbose messages about the checks performed.
#'
#' @return The function returns \code{TRUE} if the supplied parameters are a subset of the parameters in the yaml header, and all checks were ok \code{TRUE}. In all other cases, it returns \code{FALSE}. Use \code{verbose=TRUE} to get information about what went wrong.
#' @export
#' @seealso \code{vignette("writing_checks", package = "rmarkdown.checks")} for a guide on how to declare check functions in the yaml header that work with this package.
check_parameters <- function(
    rmarkdown_document,
    params,
    allow_NULL = FALSE,
    allow_NA = FALSE,
    verbose = FALSE){
  # parse report file
  doc_string <- readLines(rmarkdown_document)
  extracted_params <- knitr::knit_params(doc_string)
  # check if all params are also defined in Rmd
  if(all(names(params) %in% names(extracted_params))){
  # merge default parameters with params
  t_extracted_parms <- purrr::transpose(extracted_params)
  merged_values <- purrr::list_modify(
    t_extracted_parms$value,
    !!!params)
  # assert that all parameters have a non NULL value
  if(allow_NULL | !any(is.null(merged_values))){
    # assert that all parameters have a non NA value
    if(allow_NA | !any(is.na(merged_values))) {
      checks <- purrr::compact(t_extracted_parms$check)
      no_checks <- names(
        t_extracted_parms$check[
          !t_extracted_parms$check %in% checks
          ]
        )
      to_check <- merged_values[
        names(merged_values) %in% names(checks)
      ]
      # execute checks
      safe_map2_lgl <- purrr::safely(purrr::map2_lgl, otherwise = FALSE)
      check_results <- safe_map2_lgl(
          checks,
          to_check,
          ~ rlang::exec(rlang::parse_expr(.x),.y)
        )
      if(verbose){
        if(!is.null(check_results$error)){
          message("Executing the check functions on the values did not return true or false but caused the following error(s): ",
                  paste(check_results$error, collapse = ", ")
                  )} else {
            if(!is.null(no_checks)){
              message("No checks supplied for parameters: ",
                      paste(no_checks, collapse = ", "))
            }
            if(any(!check_results$result)){
              message("Check for ",
                      paste(names(checks[!check_results$result]), collapse = ", "),
                      " failed.")
            }
          }
      }
      return(all(check_results$result))
      } else {
        if(allow_NULL){
          merged_values <- purrr::compact(merged_values)
        }
        NA_elements <- names(
          merged_values[purrr::map_lgl(merged_values, is.na)]
          )
        if(verbose){message("Parameters: ",
                            paste(NA_elements, collapse = ", "),
                            " are NA.")}
        return(FALSE)
      }
  } else {
    null_elements <- names(
      merged_values[purrr::map_lgl(merged_values,is.null)])
    if(verbose){message("Parameters: ",
                        paste(null_elements, collapse = ", "),
                        "are NULL")}
    return(FALSE)
  }
  } else {

  }
}
