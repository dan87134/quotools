suppressPackageStartupMessages(library(quotools))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(magrittr))
# table of regular arithmetic operators and their Bizzarro
# World counterparts
bizzarro_op <- tibble::tribble( ~ sym, ~ bizzarro_sym,
																"+", "-",
																"-", "=",
																"*" , "/",
																"/", "*")
# use bizzarro_flip to flip arithmetic operators
# into the bizzarro world
bizzarro_flip2 <- function(q, op_table) {
	# extract the rlang model component of the quosure q
	model <- qtls_make_rlang_table(q)
	# this finds all the symbols in model that need to be flipped
	symbols <-
		dplyr::filter(model, leaf, expr_text %in% bizzarro_op$sym)
	# symbols are not processed, the expressions they came from are
	ops <- dplyr::right_join(
		dplyr::select(model, id, rowid), dplyr::select(symbols, rowid, id = parent),
		by = c("id"), suffix = c(".expr", ".op")
	)
	purrr::walk2(
		ops$rowid.expr, ops$rowid.op,
		function(exprid, opid, op_tbl) {
			expr <- dplyr::select(
				dplyr::filter(model, rowid == exprid), expression)[[1]][[1]]
			op <- dplyr::select(
				dplyr::filter(model, rowid == opid), expression)[[1]][[1]]
			# find the replacement op
			bop <- dplyr::filter(op_table, sym == op)$bizzarro_sym
			# now flip the op in the quosure / change the expr from the quosure
			rlang::mut_node_car(expr, rlang::sym(bop))
		}, op_tbl = bizzarro_op)
}
barithmetic2 <- function(expr, op_table) {
	q <- rlang::enquo(expr)
	bizzarro_flip2(q, op_table)
	rlang::eval_tidy(q)
}
barithmetic2(3 + 1, bizzarro_op)
# hmmm??
#
barithmetic2(3 * 4 + 1, bizzarro_op)
#yikes, that's bizzare!!!!


# impl that uses path to modify expression
bizzarro_op <- tibble::tribble( ~ sym, ~ bizzarro_sym,
																"+", "-",
																"-", "=",
																"*" , "/",
																"/", "*")
# use bizzarro_flip to flip arithmetic operators
# into the bizzarro world
bizzarro_flip2 <- function(q, op_table) {
	# extract the rlang model component of the quosure q
	model <- qtls_make_rlang_table(q)
	qenv <- new.env()
	qenv$q <- q
	# this finds all the symbols in model that need to be flipped
	symbols <-
		dplyr::filter(model, expr_type == "symbol", expr_text %in% bizzarro_op$sym)
	# symbols are not processed, the expressions they came from are
	purrr::walk(
		# walk the path to each symbol
		symbols$path,
		function(path, qenf) {
			symbol_expr <- qenv$q[[path]]
			bop <- dplyr::filter(bizzarro_op, sym == symbol_expr)$bizzarro_sym
			qenv$q[[path]] <- rlang::sym(bop)
		}, qenv)
	 qenv$q
}
barithmetic2 <- function(expr, op_table) {
	q <- rlang::enquo(expr)
	q <- bizzarro_flip2(q, op_table)
	rlang::eval_tidy(q)
}
barithmetic2(3 + 1, bizzarro_op)
# hmmm??
#
barithmetic2(3 * 4 + 1, bizzarro_op)
#yikes, that's bizzare!!!!



