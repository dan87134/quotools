#' Apply to quosue tree
#' Applies a functions to car/cdr elements in quosure tree
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
qtls_apply_carcdr <- function(q,
															car_solo_apply = NULL,
															car_multi_apply = NULL,
															cdr_apply = NULL,
															context = NULL) {
	if (is.null(context)) {
		context <- new.env()
		if (!is.null(car_solo_apply)) {
			context$car_solo_apply <- car_solo_apply
		} else {
			context$car_solo_apply <- function(expr, car) {
			}
		}
		if (!is.null(car_multi_apply)) {
			context$car_multi_apply <- car_multi_apply
		} else {
			context$car_multi_apply <- function(expr, car) {
			}
		}
		if (!is.null(cdr_apply)) {
			context$cdr_apply <- cdr_apply
		} else {
			context$cdr_apply <- function(expr, car, cdr) {
			}
		}
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
		# apply_solo_car

		context$car_solo_apply(e, car)
	} else {
		# apply_multi_car
		context$car_multi_apply(e, car)
	}
	# apply_cdr
	if (!is.null(cdr)) {
		context$cdr_apply(e, car, cdr)
		for (index in 1:length(cdr)) {
			next_cdr <- cdr[[index]]
			if (rlang::is_lang(next_cdr)) {
				qtls_apply_carcdr(next_cdr, context = context)
			}
		}
	}
}
