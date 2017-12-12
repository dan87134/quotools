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
	function(q,
					 tbl = new.env() ,
					 parent = 0,
					 order = 1) {
		if (is.null(tbl$tbl)) {
			tbl$tbl <- tibble::tribble(~ type, ~ id, ~ parent,
																 ~ atom, ~ position, ~ expression)
			# pass is incremented after each row is added to tbl
			tbl$pass <- 1
		}
		if (rlang::is_formula(q)) {
			e <- rlang::f_rhs(q)
		} else {
			e <- q
		}
		# get car/cdr, head/tail
		cdr <- rlang::node_cdr(e)
		car <- rlang::node_car(e)
		# length(car) == 1 is the typical car. It means it is a function
		# and the cdr is a list of its arguments
		if (length(car) == 1) {
			# add the function to the table
			tbl$tbl <-
				dplyr::bind_rows( tbl$tbl,
													tibble::tibble(
														type = c("car"),
														id = c(tbl$pass),
														parent = c(parent),
														atom = c(glue::glue("{rlang::get_expr(car)}:{order}")),
														position = c(1),
														expression = c(e)
													))
			# this function is the parent of the cdr that follows
			parent <- tbl$pass
			# increment pass because row was added to table
			tbl$pass <- tbl$pass + 1
		} else {
			# if we get to here it means the car has more than one item in its list
			tbl$tbl <-
				dplyr::bind_rows( tbl$tbl,
													tibble::tibble(
														type = c("car"),
														id = c(tbl$pass),
														parent = c(parent),
														atom = c(glue::glue("{rlang::get_expr(car)}:{order}")),
														position = c(1),
														expression = c(e)
													))
			parent <- tbl$pass
			tbl$pass <- tbl$pass + 1

			for (index in 2:length(car)) {
				if (rlang::is_lang(car[[index]])) {
					qtls_walk_carcdr(car[[index]], tbl, parent, order = index)
				} else {
					tbl$tbl <-
						dplyr::bind_rows( tbl$tbl,
															tibble::tibble(
																type = c("car"),
																id = c(tbl$pass),
																parent = c(parent),
																atom = c(glue::glue("{rlang::get_expr(car[[index]]}:{index - 1}")),
																position = c(index - 1),
																expression = c(e)
															))
					tbl$pass <- tbl$pass + 1
				}
			}
			#parent <- tbl$pass

			tbl$tbl <-
				dplyr::bind_rows( tbl$tbl,
													tibble::tibble(
														type = c("car"),
														id = c(tbl$pass),
														parent = c(parent),
														atom = c(stringr_str_c(":{length(car)}")),
														position = c(0),
														expression = c(e)
													))
			parent <- tbl$pass
			tbl$pass <- tbl$pass + 1

		}
		if (length(cdr) > 0) {
			for (index in 1:length(cdr)) {
				if (rlang::is_lang(cdr[[index]])) {
					qtls_walk_carcdr(cdr[[index]], tbl, parent, order = index)
				} else {

					tbl$tbl <-
						dplyr::bind_rows(tbl$tbl,
														 tibble::tibble(
														 	type = c("cdr"),
														 	id = c(tbl$pass),
														 	parent = c(parent),
														 	atom = c(glue::glue("{rlang::get_expr(cdr[[index]])}:{index}")),
														 	position = c(index),
														 	expression = c(e)
														 ))
					tbl$pass <- tbl$pass + 1
				}
			}
		}
		tbl$tbl
	}
