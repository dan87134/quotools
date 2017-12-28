


#' Wrap the rlang object model for an object into a table.
#' This is a recursive function
#'
#' @param e a language object like an expr or atomic or
#'   quosure (which mimics an expression)
#' @param parent parent of expression (used by recursion only)
#' @param context  environment used to pass table being constructed
#'   by reference (used by recursion only)
#' @param position position relative to siblings (used by recursion only)
#' @param path path in e in object (used by recursion only)
#'
#' @return A table where each row has, among other things a path
#'   to the object for that object
#' @export
#'
#' @examples
qtls_make_rlang_model <- function(e,
																	parent = 0L,
																	context = new.env() ,
																	position = 1L,
																	path = vector(mode = "integer")) {
	# begin initialization
	# table needs to be passed by reference to put into its own environment
	if (is.null(context$tbl)) {
		context$tbl <-
			tibble::tribble()
		# pass is incremented after each row is added to tbl
		context$pass <- 1L
	}
	# end initialization
	# begin processing
	# depth is useful when making an outline plot
	depth <- length(path)
	leaf <- length(e) == 1
	# this add a new row to the table that will be returned by qtls_make_rlang_model
	context$tbl <- dplyr::bind_rows(
		context$tbl,
		tibble::tibble(
			# context$pass is just a running number used for id, which must be a key
			id = c(context$pass),
			# parent of this object, see more below
			parent = c(parent),
			# this path can be used to locate object
			path = list(path),
			depth = c(depth),
			# handy info about e
			expr_type = c(typeof(e)),
			# handy info about e
			expr_class = c(stringr::str_c(class(e), collapse = ", ")),
			# position in relation to other siblings. Insures that things like
			# a - b are not interpreted as b - a
			position = c(position),
			# handy info about e
			expr_text = rlang::expr_text(e),
			# hand info about e. Useful for DiagrammeR plot
			label = stringr::str_c(rlang::expr_text(e), "\n", id, ":", position),
			leaf = c(leaf),
			# handy info about what kinds of objects e mimics
			what_is_expr = list(qtls_what_is_it(e)),
		)
	)
	parent <- context$pass
	context$pass <- context$pass + 1L
	#end processing
	# test for completion
	if (length(e) > 1) {
		#end test for completion
		# begin recursion
		# if length > 1 then object has children
		# which are recursively processed
		for (index in 1:length(e)) {
			# add object to table
			qtls_make_rlang_model(
				e[[index]],
				position = index,
				context = context,
				parent = parent,
				path = c(path, index)
			)
		}
		# end recursion
	}
	# just in case add rowid column
	tibble::rowid_to_column(context$tbl)
}
