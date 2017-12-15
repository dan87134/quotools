# scratch for quopotl rmd


#quosures can only be built out of expression, not a program script
# this produces an error
q <- rlang::quo(a <- 8; b <- 9)

q2 <- rlang::quo(a * b + c * d)

mt <- qtls_make_rlang_table(q2)


suppressPackageStartupMessages(library(quotools))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(magrittr))
q4 <- rlang::quo(a * b + c * d)
ast <- qtls_make_ast_table(q4)

flip_table <- tibble::tribble(
	~ sym, ~ bizzarro_sym,
	"+", "-",
	"-", "=",
	"*" , "/",
	"/", "*"
)


bizzarro_flip <- function(q) {
	t2 <- qtls_make_ast_table(q) %>%
		dplyr::filter(expr_property == "car")
	purrr::walk(t2$expression, function(expr) {
		car <- rlang::node_car(expr)
		if (rlang::is_symbol(car)) {
			flip <- dplyr::filter(flip_table, sym == car)$bizzarro_sym
			if (flip != "") {
				rlang::mut_node_car(expr, rlang::sym(flip))
			}
		}
	})
}

q8 <- rlang::quo(a * b + c * d)
bizzarro_flip(q8)


barithmetic <- function(expr) {
	q <- rlang::enquo(expr)
	# get  rlang's model of q
	bizzarro_flip(q)
	rlang::eval_tidy(q)
}


flip_table <- tibble::tribble(
	~ sym, ~ bizzarro_sym,
	"+", "-",
	"-", "=",
	"*" , "/",
	"/", "*"
)


bizzarro_flip <- function(expr) {
	q <- rlang::enquo(expr)
	t2 <- qtls_make_ast_table(q) %>%
		dplyr::filter(expr_property == "car")
	purrr::walk(t2$expression, function(expr) {
		car <- rlang::node_car(expr)
		if (rlang::is_symbol(car)) {
			flip <- dplyr::filter(flip_table, sym == car)$bizzarro_sym
			if (flip != "") {
				rlang::mut_node_car(expr, rlang::sym(flip))
			}
		}
	})
	q
}


barithmetic <- function(expr) {
	q <- rlang::enquo(expr)
	# get  rlang's model of q
	bizzarro_flip(q)
	rlang::eval_tidy(q)
}

q7 <- rlang::quo(a + b)

q8 <- bizzarro_flip(q7)

barithmetic(3 + 2)



op_table <- tibble::tribble(
	~ op, ~ bizzarro_op,
	"+", "-",
	"-", "=",
	"*" , "/",
	"/", "*"
)

bizzarro_flip <- function(q) {
	t2 <- qtls_make_ast_table(q) %>%
		dplyr::filter(expr_property == "car")
	purrr::walk(t2$expression, function(expr) {
		car <- rlang::node_car(expr)
		if (rlang::is_symbol(car)) {
			bizzarro_op <- dplyr::filter(op_table, op == car)$bizzarro_op
			if (bizzarro_op != "") {
				rlang::mut_node_car(expr, rlang::sym(bizzarro_op))
			}
		}
	})
}

#-----------------------------------
# model table implementation

q10 <- rlang::quo(a * b + c * d)
model <- qtls_make_rlang_table(q10)



ex <- model[[1, "expr"]]
car <- rlang::node_car(ex)
bop <- dplyr::filter(op_table, op == "+")$bizzarro_op

rlang::mut_node_car(model[[1, "expr"]], rlang::sym(bop))

purrr::walk(1:nrow(model), function(index) {
	expr <- model[[index,"expr"]]
})


dplyr::filter(model, expr_type == "symbol")

op_table <- tibble::tribble(
	~ op, ~ bizzarro_op,
	"+", "-",
	"-", "=",
	"*" , "/",
	"/", "*"
)

bizzarro_flip <- function(q) {
	t2 <- qtls_make_rlang_table(q) %>%
		dplyr::filter(expr_type == "symbol")
	purrr::walk(1:nrow(t2), function(index) {
#		car <- rlang::node_car(row)
		expr <- t2[[index, "expr"]]
		if (rlang::is_symbol(expr)) {
			bizzarro_op <- dplyr::filter(op_table, op == t2[[index, "expr_text"]])$bizzarro_op
			print(t2[[index, "expr_text"]])
			if (length(bizzarro_op) != 0) {
				print(bizzarro_op)

				rlang::mut_node_car(t2[[index, "expr"]], rlang::sym(bizzarro_op[1]))
			}
			print("-------")
		}
	})
}


q11 <- rlang::quo(a * b + c * d)
bizzarro_flip(q11)

