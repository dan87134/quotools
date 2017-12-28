# my_negate_om implementation


#' Example using NSE for arithmetic expresion
#'
#' @param expr an arithmetic expression
#'
#' @return
#' @export
#'
#' @examples
qtls_my_negate_om <- function(expr) {
	qexpr <- rlang::enquo(expr)
	expr_tbl <- qtls_make_rlang_model(qexpr)
	# find all leaf nodes that are in position 1
	# these are the functions in the expression
	fcns <- dplyr::filter(expr_tbl, leaf, position == 1, id != 2)
	# this function only handles arithmetic opertors so all the
	# position 1 symbols must be one of them
	stopifnot(nrow(dplyr::filter(fcns, expr_text %in% c("+", "-", "*", "/", "("))) == nrow(fcns))
	fcns

}

