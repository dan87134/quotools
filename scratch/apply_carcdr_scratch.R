q <- rlang::quo(x <- a + b * (c + d * h) + e * f)



qtls_apply_carcdr(q, cdr_apply =
										function(e, car, cdr) { print(glue::glue("cdr_apply {car}:{cdr}"))},
									car_solo_apply = function(e, car) {print(glue::glue("car_solo_apply {car}"))},
									car_multi_apply = function(e, car) {print(glue::glue("car_multi_apply {car}"))})

# change + to -
qtls_apply_carcdr(q, car_solo_apply = function(expr, car) {
										if (rlang::is_symbol(car) & car == rlang::sym("+")) {
										rlang::mut_node_car(expr, rlang::sym("-"))
										}
									})
q

# this does not work
rhs <- rlang::f_rhs(q)
car <- rlang::node_car(rhs)
if (rlang::is_symbol(car)) {
 rlang::mut_node_car(car, rlang::sym("-"))
}

suppressPackageStartupMessages(library(tidyverse))
q <- rlang::quo(a + b)
# what q looks like to start with
q
# note q is setup for addition
q
a <- 2
b <- 3
# and evaluating it produces 5, as expected
rlang::eval_tidy(q)


## this works and changed + to -
change_to_minus <- function(q) {
	#q2 <- q[[2]]
	q2 <- rlang::f_rhs(q)
	car <- rlang::node_car(q2)
	cdr <- rlang::node_cdr(q2)
	mod_car <- function(car) {
		if (rlang::is_symbol(car)) {
			if (car == rlang::sym("+")) {
				rlang::mut_node_car(q2, rlang::sym("-"))
			}
		}
	}
	mod_car(car)
}
change_to_minus(q)
# note that the '+' is now a '-' as expected
q
# and evaluating it produces -1, as expected
rlang::eval_tidy(q)

suppressPackageStartupMessages(library(tidyverse))
# reset to try again with code that fails
q <- rlang::quo(a + b)
# what q looks like to start with
# this does not work, why ???
rhs <- rlang::f_rhs(q)
car <- rlang::node_car(rhs)
if (rlang::is_symbol(car)) {
	rlang::mut_node_car(rhs, rlang::sym("-"))
}

# note that garbage has been inserted into q
# also because the rlang:node functions do
# not do any checking this result will eventually
# crash the R session because something, no
# doubt, has been corrupted
q



