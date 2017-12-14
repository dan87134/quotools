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
qtls_walk_carcdr <-
	function(q_or_expr,
					 tbl = new.env() ,
					 parent = 0,
					 order = 1) {
		if (is.null(tbl$tbl)) {
			tbl$tbl <- tibble::tribble(~ node, ~ id, ~ parent,
																 ~ label, ~ position, ~ expression,
																 ~ typeof_carcdr, ~ carcdr_mimics)
			# pass is incremented after each row is added to tbl
			tbl$pass <- 1
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
		# an expression has a car and a cdr, get them
		cdr <- rlang::node_cdr(e)
		car <- rlang::node_car(e)
		# length(car) == 1 is the typical car. It means it is a function
		# and the cdr is a list of its arguments
		if (length(car) == 1) {
			# add the function to the table
			label <- rlang::get_expr(car)
			tbl$tbl <-
				dplyr::bind_rows( tbl$tbl,
													tibble::tibble(
														node = c("car"),
														id = c(tbl$pass),
														parent = c(parent),
														label = c(glue::glue("{label}:{order}")),
														position = c(1),
														expression = c(e),
														typeof_carcdr = c(typeof(car)),
														carcdr_mimics = list(qtls_what_is_it(car))
													))
			# this function is the parent of the cdr that follows
			parent <- tbl$pass
			# increment pass because row was added to table
			tbl$pass <- tbl$pass + 1
		} else {
			# if we get to here it means the car has more than one item in its list
			expr <- rlang::get_expr(car)
			tbl$tbl <-
				dplyr::bind_rows( tbl$tbl,
													tibble::tibble(
														node = c("car"),
														id = c(tbl$pass),
														parent = c(parent),
														label = c(glue::glue("{label}:{order}")),
														position = c(1),
														expression = c(e),
														typeof_carcdr = c(typeof(car)),
														carcdr_mimics = list(qtls_what_is_it(car))
													))
			parent <- tbl$pass
			tbl$pass <- tbl$pass + 1

			for (index in 2:length(car)) {
				if (rlang::is_lang(car[[index]])) {
					qtls_walk_carcdr(car[[index]], tbl, parent, order = index)
				} else {
					label <- rlang::get_expr(car[[index]])
						tbl$tbl <-
						dplyr::bind_rows( tbl$tbl,
															tibble::tibble(
																node = c("car"),
																id = c(tbl$pass),
																parent = c(parent),
																label = c(glue::glue("{label}:{index - 1}")),
																position = c(index - 1),
																expression = c(e),
																typeof_carcdr = c(typeof(car)),
																carcdr_mimics = list(qtls_what_is_it(car))
															))
					tbl$pass <- tbl$pass + 1
				}
			}

			tbl$tbl <-
				dplyr::bind_rows( tbl$tbl,
													tibble::tibble(
														node = c("car"),
														id = c(tbl$pass),
														parent = c(parent),
														label = c(stringr_str_c(":{length(car)}")),
														position = c(0),
														expression = c(e),
														typeof_carcdr = c(typeof(car)),
														carcdr_mimics = list(qtls_what_is_it(car))
													))
			parent <- tbl$pass
			tbl$pass <- tbl$pass + 1

		}
		if (length(cdr) > 0) {
			for (index in 1:length(cdr)) {
				if (rlang::is_lang(cdr[[index]])) {
					qtls_walk_carcdr(cdr[[index]], tbl, parent, order = index)
				} else {
					cdr_child <- cdr[[index]]
					label <- rlang::get_expr(cdr_child)
					tbl$tbl <-
						dplyr::bind_rows(tbl$tbl,
														 tibble::tibble(
														 	node = c("cdr"),
														 	id = c(tbl$pass),
														 	parent = c(parent),
														 	label = c(glue::glue("{label}:{index}")),
														 	position = c(index),
														 	expression = c(e),
														 	typeof_carcdr = c(typeof(cdr_child)),
														 	carcdr_mimics = list(qtls_what_is_it(cdr_child))
														 ))
					tbl$pass <- tbl$pass + 1
				}
			}
		}
		tbl$tbl
	}
