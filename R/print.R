

#' @title Alternate Print Method for a Simon's Two-Stage Design
#' 
#' @description
#' An alternate \link[base]{print} method for \link[clinfun]{ph2simon} object.
#' 
#' @param x a \link[clinfun]{ph2simon} object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns
#' Function [print_ph2simon()] does not have a returned value.
#' 
#' @note
#' We do not overwrite function `clinfun:::print.ph2simon`.
#' 
#' @keywords internal
#' @export
print_ph2simon <- function(x, ...) {
  x |>
    ph2simon4(type = c('minimax', 'optimal', 'n1', 'maximax')) |>
    show()
}







