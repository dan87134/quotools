#Bizzarro table base implementation
# working table based implementation
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
	symbols <-
		dplyr::filter(model, expr_type == "symbol", expr_text %in% bizzarro_op$sym)
	ops <- dplyr::right_join(
		dplyr::select(model, id, rowid),
		dplyr::select(symbols, rowid, id = parent),
		by = c("id"),
		suffix = c(".expr", ".op")
	)
	purrr::walk2(
		ops$rowid.expr, ops$rowid.op,
		function(exprid, opid, op_tbl) {
			expr <- dplyr::select(
				dplyr::filter(model, rowid == exprid), expression)[[1]][[1]]
			op <- dplyr::select(
				dplyr::filter(model, rowid == opid), expression)[[1]][[1]]
			bop <- dplyr::filter(op_table, sym == op)$bizzarro_sym
			rlang::mut_node_car(expr, rlang::sym(bop))
		}, op_tbl = bizzarro_op)
}
barithmetic2 <- function(expr) {
	q <- rlang::enquo(expr)
	bizzarro_flip2(q, bizzarro_op)
	rlang::eval_tidy(q)
}
# hmmm??
barithmetic2(3 + 1)
#yikes, that's bizzare!!!!
barithmetic2(3 * 4 + 1)

