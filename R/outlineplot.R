# outline plot
#' Makes character based outline plot of tbl
#'
#' @param tbl a table with an id and parent column
#' @param key_column  key column for table (typically id)
#' @param ... columns to be included in output
#'
#' @return
#' @export
#'
#' @examples
qtls_make_outline_plot <- function(tbl, key_column, ...) {
	qkey <- rlang::enquo(key_column)
	qcolumns <- rlang::quos(...)
	# example of using `!!` and `!!!` functions
	info_function <- qtls_make_info_function(tbl, !!qkey, !!!qcolumns)
	oenv <- new.env()
	oenv$output <- vector(mode = "character")
	traverse_depth_first <- function(tbl, depth = 0, current_id = NULL, oenv)
	{
		if (is.null(current_id)) {
			current_id <- dplyr::filter(tbl, parent == 0)$id
		}
		info <- info_function(current_id)
		# make one line
		info <- stringr::str_replace_all(info, "\\n", " ")
		oenv$output <- c(oenv$output,
						stringr::str_c(stringr::str_dup("--", depth), info, collapse = ""))
		children <- dplyr::filter(tbl, parent == current_id)
		purrr::walk(children$id, function(child_id, depth) {
			traverse_depth_first(tbl, current_id = child_id, depth = depth + 1, oenv)
		}, depth)
	}
	traverse_depth_first(tbl, oenv = oenv)
	oenv$output
}
