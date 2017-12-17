# working example  working example working example working example working example

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
bizzarro_flip <- function(q) {
	# extract the rlang model component of the quosure q
	model <- qtls_make_rlang_table(q)
	# walk through all the expressions in the model
	purrr::walk(model$expression, function(expr) {
		if (!rlang::is_atomic(expr)) {
			car <- rlang::node_car(expr)
			# if the expression is a symbol you need to check it to see if
			# it is an arithmetic operator
			if (rlang::is_symbol(car)) {
				# see if there is a Bizzarro World counterpart for operator
				flip <- dplyr::filter(bizzarro_op, sym == car)$bizzarro_sym
				# if you find an arithemtic operator flip it into the bizzarro world
				if (length(flip) > 0) {
					# this replaces the existing operator with it's bizzarro version
					# note that this modifies the q argument in place, not copy on write
					rlang::mut_node_car(expr, rlang::sym(flip))
				}
			}
		}
	})
}
barithmetic <- function(expr) {
	q <- rlang::enquo(expr)
	bizzarro_flip(q)
	rlang::eval_tidy(q)
}
# hmmm??
barithmetic(3 + 1)
#yikes, that's bizzare!!!!
barithmetic(3 * 4 + 1)


