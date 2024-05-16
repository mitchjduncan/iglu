#' Calculate GMI
#'
#' @description
#' The function gmi produces GMI values in a tibble object.
#'
#' @usage
#' gmi(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' updated description of return.
#' @return If a data.frame object is passed, then a tibble object with
#' three columns: subject id, corresponding GMI and unit of glucose measure 
#' is returned. If a vector of glucose values is passed, then a tibble object 
#' with the GMI value and unit of glucose measure is returned.
#' as.numeric() can be wrapped around the latter to output just a numeric value.
#'
#' @export
#'
#' updated description of return and GMI calculation
#' @details
#' A tibble object with 1 row for each subject, a column for subject id, 
#' a column for GMI values, and unit of glucose measure is returned.
#' NA glucose values are omitted from the calculation of the GMI.
#'
#' GMI score is calculated depending on the metric that glucose is recorded in. 
#' The glucose unit (mg/dL or mmol/L) must be specified as part of the function. 
#' GMI is calculated by either: 
#' \eqn{3.31 + (.02392*mean(G))}
#' where G is the vector of Glucose Measurements (mg/dL).
#' 
#' Or 
#' 
#' \eqn{12.71 + (4.70587*mean(G))}
#' where G is the vector of Glucose Measurements (mmol/L). 
#' 
#'
#'
#' @references
#' Bergenstal (2018) Glucose Management Indicator (GMI): A New Term for
#' Estimating A1C From Continuous Glucose Monitoring
#' \emph{Hormone and Metabolic Research} \strong{41} .2275-2280,
#' \doi{10.2337/dc18-1581}.
#'
#' @examples
#'
#' #changes propose using specifying the unit the data is recorded in the function.
#' data(example_data_1_subject)
#' gmi(example_data_1_subject, "mg")
#'
#' data(example_data_5_subject)
#' gmi(example_data_5_subject, "mg")
#'

# change to function specifies the unit data is recorded in to specify approach 
# to calculating GMI 
gmi <- function(data, unit){ # change requires unit of glucose recorded to be specified
  gl = id = NULL
  rm(list = c("gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")
  
  if (unit == "mg") {  # calculation of gmi baed on recorded units in mg
    out = data %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(
        GMI = 3.31 + (.02392*mean(gl, na.rm = TRUE) )
      )
  } else if (unit == "mmol") { # calculation of gmi baed on recorded units in mmol
    out = data %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(
        GMI = 12.71 + (4.70587*mean(gl, na.rm = TRUE) )
      )
  } else {
# added error notice of failure to specify unit
    stop("Invalid unit specified. Use 'mg' or 'mmol' to specify the unit glucose is recorded in.")
  }
# output now specifies the unit that glucose is recorded in.   
  out$unit = unit
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
