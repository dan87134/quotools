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
	t2 <- qtls_make_rlang_table(q)
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

barithmetic <- function(expr) {
	q <- rlang::enquo(expr)
	bizzarro_flip(q)
	rlang::eval_tidy(q)
}

# hmmm??
barithmetic(3 + 1)
# yikes!!
barithmetic(3 * 4 + 1)



q8 <- rlang::quo(a * b + c * d)
t8 <- qtls_make_rlang_table(q8)

bizzarro_flip(q8)

q9 <- rlang::quo(3 + 2)
t9 <- qtls_make_rlang_table(q9)
bizzarro_flip(q9)

q10 <- rlang::quo(2)
ex <- rlang::get_expr(q10)
rlang::node_car(ex)

expr_text = rlang::expr_text(ex)


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

suppressPackageStartupMessages(library(quotools))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(DiagrammeR))



q10 <- rlang::quo(a * b + c * d)
model <- qtls_make_rlang_table(q10)
g <- qtls_plot_model(model)
DiagrammeR::render_graph(g, layout="tree")

q12 <- rlang::quo(2 + 3)
ex <- rlang::get_expr(q12)
rlang::is_atomic(ex)
rlang::is_lang(ex)
rlang::expr_label(ex)
qtls_what_is_it(ex)

t9 <- qtls_make_rlang_table(q12)
g <- qtls_plot_model(t9)
DiagrammeR::render_graph(g, layout="tree")

sym_model <- dplyr::filter(model, expr_type == "symbol")



bop <- dplyr::filter(op_table, op == "+")$bizzarro_op

s <- rlang::sym(bop)

rlang::mut_node_car(expr, s)

purrr::walk(1:nrow(sym_model), function(index) {
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
	t2 <- qtls_make_rlang_table(q)
	purrr::walk(t2$expr, function(expr) {
#		car <- rlang::node_car(row)
#		expr <- t2[[index, "expr"]]
		if (rlang::is_symbol(expr)) {
			bizzarro_op <- dplyr::filter(op_table, op == rlang::expr_text(expr))$bizzarro_op
			if (length(bizzarro_op) != 0) {
				bizzarro_sym <- rlang::sym(bizzarro_op)
				rlang::mut_node_car(expr, rlang::sym(bizzarro_sym))
			}
		}
	})
}



op_table <- tibble::tribble(
	~ op, ~ bizzarro_op,
	"+", "-",
	"-", "=",
	"*" , "/",
	"/", "*"
)


q11 <- rlang::quo(a * b + c * d)
bizzarro_flip(q11)


bizzarro_flip <- function(q) {
	model <- qtls_make_rlang_table(q) %>%
		dplyr::filter(source == "car")
	purrr::walk(model$expression, function(expr) {
		#car <- rlang::node_car(ex)
		#if (typeof(ex) == "symbol") {
			print("sym")
			bizzarro_op <- dplyr::filter(op_table, op == car)$bizzarro_op
			if (length(bizzarro_op) > 0) {
				rlang::mut_node_car(expr, rlang::sym(bizzarro_op))
			}
		#}
	})
}


#------------------------

bizzarro_op <- tibble::tribble(
	~ sym, ~ bizzarro_sym,
	"+", "-",
	"-", "=",
	"*" , "/",
	"/", "*"
)

# doesn't work
# uses op_table to flip arithmetic operators
# in the bizzarro world
bizzarro_flip <- function(q) {
	t2 <- qtls_make_rlang_table(q)
	dplyr::select(t2, expression) %>%
	purrr::walk( function(expr) {
		car <- rlang::node_car(expr)
		if (rlang::is_symbol(car)) {
			flip <- dplyr::filter(bizzarro_op, sym == car)$bizzarro_sym
			if (flip != "") {
				rlang::mut_node_car(expr, rlang::sym(flip))
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
# yikes!!
barithmetic(3 * 4 + 1)



