# factory to make function that formats a row of table

#' Title
#'
#' @param tbl A table with a key column in it
#' @param key_column The column that serves as a key
#' @param ... Columns used to make info string for row.
#'   Values must be convertable to character.
#'
#' @return A function that returns a string
#'   with values (<column name>:<value>, )* for the row
#'   specified by the key passed into it
#' @export
#'
#' @examples
qtls_make_info_function <- function(tbl, key_column, ...) {
	qkey <- rlang::enquo(key_column)
	qkey_expr <- rlang::get_expr(qkey)
	qkey_text <- rlang::expr_text(qkey_expr)
	q <- quos(...)
	columns <- purrr::map_chr(q, function(q) {
		expr <- rlang::get_expr(q)
		rlang::expr_text(expr)
	})
	function(key) {
		values <- purrr::map_chr(columns, function(column) {
			stringr::str_c(column, ":", as.character(
				tbl[tbl[qkey_text] == key, column][[1]], collapse = ""))
		})
		stringr::str_c(values, collapse = ", ")
	}
}
