#' Set a total constraint function
#'
#' This function creates a constraint function that ensures the sum of the elements in a vector equals a specified total.
#' @param total A numeric value representing the desired total sum of the elements in the vector.
#' @return A function that takes a numeric vector as input and returns a numeric vector of constraints.
#'
#' @export

set_total_constraint = function(total){
  ttl = total
  function(x){
    constr = numeric(1)
    constr[1] = sum(x) - ttl
    return(constr)
  }
}
