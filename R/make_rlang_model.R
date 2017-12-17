# make rlang model table

qtls_make_rlang_table <- function(
	q_or_expr,
	parent = 0L,
	context = new.env() ,
	order = 1L,
	source = "root") {
	if (is.null(context$tbl)) {
		context$tbl <-
			tibble::tribble(
			)
		# pass is incremented after each row is added to tbl
		context$pass <- 1L
		context$expr_id <- 0L
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
	fcn_def <- inherits(e, "srcref")
	dictionaryish <- rlang::is_dictionaryish(e)
	context$tbl <- dplyr::bind_rows(context$tbl, tibble::tibble(
		id = c(context$pass),
		parent = c(parent),
		source = c(source),
		address = c(pryr::address(e)),
		expression = if (!fcn_def & !dictionaryish)  list(e) else  c(NA),
		expr_type = c(typeof(e)),
		expr_class = c(class(e)),
		order = c(order),
		expr_text = rlang::expr_text(e),
		label =  if (!fcn_def) stringr::str_c(
				label_fix(rlang::expr_text(e)), "\n", order, "/", source,
			collapse = "") else c(stringr::str_c("srcdef\n", order, "/", source)),
		what_is_expr = list(qtls_what_is_it(e))
		)
	)
	parent <- context$pass
	context$pass <- context$pass + 1
	if (rlang::is_lang(e)) {
		car <- rlang::node_car(e)
		car_length <- length(car)
		if (car_length == 1) {
			qtls_make_rlang_table(car, order = 1,
														context = context,
														parent = parent,
														source = "car")
		} else {
			for (index in 1:length(car)) {
				qtls_make_rlang_table(car[[index]],
															order = index,
															context = context,
															parent = parent,
															source = "car")
			}
		}
		cdr <- rlang::node_cdr(e)
		cdr_length <- length(cdr)
		if (cdr_length == 1) {
			qtls_make_rlang_table(cdr, order = 1 + car_length,
														context = context,
														parent = parent,
														source = "cdr")
		} else {
			for (index in 1:cdr_length) {
				qtls_make_rlang_table(cdr[[index]],
															order = index + car_length,
															context = context,
															parent = parent,
															source = "cdr")
			}
		}

	}
	tibble::rowid_to_column(context$tbl)
	}