label_fix <- function(label) {
	if (length(label) == 1) {
		label
	} else {
		if (rlang::is_pairlist(label)) {
			"pairlist"
		} else {
			if (rlang::is_callable(label)) {
				"function"
			} else {
				stringr::str_c(label, collapse = "")
			}
		}
	}
}


#' Title
#' build a table of parent child relationship based
#' on car/cdr's from a quosure
#' @param q
#' @param level
#' @param tbl
#' @param parent
#'
#' @return
#' @export
#'
#' @examples
#' @export
qtls_make_ast_table <-
	function(q_or_expr,
					 tbl = new.env() ,
					 parent = 0L,
					 order = 1L) {
		if (is.null(tbl$tbl)) {
			tbl$tbl <-
				tibble::tribble(
					~ expr_property,
					~ id,
					~ parent,
					~ expr_id,
					~ label,
					~ position,
					~ expression,
					~ typeof_carcdr,
					~ carcdr_mimics
				)
			# pass is incremented after each row is added to tbl
			tbl$pass <- 1L
			tbl$expr_id <- 0L
		}
		# q might be a quosure or an expression
		#
		# If it is a quosure get its expression from
		# its right hand side. Note that a quosure
		# mimics a formula
		#
		# otherwise you can assume that it is an expression
		if (rlang::is_quosure(q_or_expr)) {
			e <- rlang::f_rhs(q_or_expr)
		} else {
			e <- q_or_expr
		}
		tbl$expr_id <- tbl$expr_id + 1L
		# an expression has a car and a cdr, get them
		cdr <- rlang::node_cdr(e)
		car <- rlang::node_car(e)
		#  length(car) == 1 is the typical car. It means it is a function
		# and the cdr is a list of its arguments
		if (length(car) == 1) {
			# add the function to the table
			label <- label_fix(rlang::get_expr(car))
			tbl$tbl <-
				dplyr::bind_rows(
					tbl$tbl,
					tibble::tibble(
						expr_property = c("car"),
						id = c(tbl$pass),
						parent = c(parent),
						expr_id = c(tbl$expr_id),
						label = c(glue::glue("{label}:{order}")),
						position = c(1L),
						expression = c(e),
						typeof_carcdr = c(typeof(car)),
						carcdr_mimics = list(qtls_what_is_it(car))
					)
				)
			# this function is the parent of the cdr that follows
			parent <- as.integer(tbl$pass)
			# increment pass because row was added to table
			tbl$pass <- tbl$pass + 1L
		} else {
			# if we get to here it means the car has more than one item in its list
			label <- label_fix(rlang::get_expr(car))
			tbl$tbl <-
				dplyr::bind_rows(
					tbl$tbl,
					tibble::tibble(
						expr_property = c("car"),
						id = c(tbl$pass),
						parent = c(parent),
						expr_id = c(tbl$expr_id),
						label = c(glue::glue("{label}:{order}")),
						position = c(1L),
						expression = c(e),
						typeof_carcdr = c(typeof(car)),
						carcdr_mimics = list(qtls_what_is_it(car))
					)
				)
			parent <- as.integer(tbl$pass)
			tbl$pass <- tbl$pass + 1L

			for (index in 1L:length(car)) {
				if (rlang::is_lang(car[[index]])) {
					qtls_make_ast_table(car[[index]], tbl, parent, order = index)
				} else {
					label <- label_fix(rlang::get_expr(car[[index]]))
					tbl$tbl <-
						dplyr::bind_rows(
							tbl$tbl,
							tibble::tibble(
								expr_property = c("car"),
								id = c(tbl$pass),
								parent = c(parent),
								expr_id = c(tbl$expr_id),
								label = c(glue::glue("{label}:{index}")),
								position = c(index - 1),
								expression = c(e),
								typeof_carcdr = c(typeof(car)),
								carcdr_mimics = list(qtls_what_is_it(car))
							)
						)
					tbl$pass <- tbl$pass + 1L
				}
			}

			tbl$tbl <-
				dplyr::bind_rows(
					tbl$tbl,
					tibble::tibble(
						expr_property = c("car"),
						id = c(tbl$pass),
						parent = c(parent),
						expr_id = c(tbl$expr_id),
						label = c(glue::glue(":{length(car)}")),
						position = c(0L),
						expression = c(e),
						typeof_carcdr = c(typeof(car)),
						carcdr_mimics = list(qtls_what_is_it(car))
					)
				)
			parent <- as.integer(tbl$pass)
			tbl$pass <- tbl$pass + 1L

		}
		if (length(cdr) > 0) {
			for (index in 1L:length(cdr)) {
				if (rlang::is_lang(cdr[[index]])) {
					qtls_make_ast_table(cdr[[index]], tbl, parent, order = index)
				} else {
					cdr_child <- cdr[[index]]
					label <- label_fix(rlang::get_expr(cdr_child))
					tbl$tbl <-
						dplyr::bind_rows(
							tbl$tbl,
							tibble::tibble(
								expr_property = c("cdr"),
								id = c(tbl$pass),
								parent = c(parent),
								expr_id = c(tbl$expr_id),
								label = c(glue::glue("{label}:{index}")),
								position = c(index),
								expression = c(e),
								typeof_carcdr = c(typeof(cdr_child)),
								carcdr_mimics = list(qtls_what_is_it(cdr_child))
							)
						)
					tbl$pass <- tbl$pass + 1L
				}
			}
		}
		tbl$tbl
	}
