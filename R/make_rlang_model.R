

#' Wrap the rlang object model for a language object in a table
#'
#' @param e a language object like an expr or quosure
#' @param parent parent of expression (used by recursion only)
#' @param context  environment used to pass table being constructed
#'   by reference
#' @param order position relative to siblings
#' @param path path in e to object
#'
#' @return A table where each row has, among other things a path
#'   to the object for that row
#' @export
#'
#' @examples
qtls_make_rlang_model <- function(
	e,
	parent = 0L,
	context = new.env() ,
	order = 1L,
	path = vector(mode = "integer")) {
	if (is.null(context$tbl)) {
		context$tbl <-
			tibble::tribble(
			)
		# pass is incremented after each row is added to tbl
		context$pass <- 1L
		context$expr_id <- 0L
	}
	depth <- length(path)
	context$tbl <- dplyr::bind_rows(context$tbl, tibble::tibble(
		id = c(context$pass),
		parent = c(parent),
		path = list(path),
		depth = c(depth),
		expr_type = c(typeof(e)),
		expr_class = c(stringr::str_c(class(e), collapse = ", ")),
		order = c(order),
		expr_text = rlang::expr_text(e),
		label = stringr::str_c(rlang::expr_text(e), "\n", order),
		what_is_expr = list(qtls_what_is_it(e)),
	)
	)
	parent <- context$pass
	context$pass <- context$pass + 1L
	if (length(e) > 1) {
		for (index in 1:length(e)) {
			qtls_make_rlang_model(e[[index]], order = index,
														context = context,
														parent = parent,
														path = c(path, index))

		}

	}
	tibble::rowid_to_column(context$tbl)
}




