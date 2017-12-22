# function factory to format rows of table



qtls_make_info_function <- function(tbl, key_column, ...) {
	qkey <- rlang::enquo(key_column)
	qkey_expr <- rlang::get_expr(qkey)
	qkey_text <- rlang::expr_text(qkey_expr)
	q <- quos(...)
	columns <- purrr::map_chr(q, function(q) {
		rhs <- rlang::get_expr(q)
		rlang::expr_text(rhs)
	})
	function(tbl, key) {
		values <- purrr::map_chr(columns, function(column, tbl, id) {
			stringr::str_c(column, ":", as.character(
				tbl[tbl[qkey_text] == key, column][[1]], collapse = ""))
		}, tbl, id)
		stringr::str_c(values, collapse = ", ")
	}
}
