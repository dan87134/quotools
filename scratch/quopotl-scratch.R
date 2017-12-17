# scratch for quopotl rmd


#quosures can only be built out of expression, not a program script
# this produces an error
q <- rlang::quo(a <- 8, b <- 9)

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
# working example
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

#-------------------------------


q1 <- rlang::quo(a * b + c * d)
q2 <- rlang::quo(function(x){x})
q3 <- rlang::quo(function(x){x}(a))
q4 <- rlang::quo(function(x = 9){x + b}(a + 1))
q5 <- rlang::quo(f <- function(x = 9){x + b}(function(y){b + 1})(p + 8))
q6 <- rlang::quo(1 + 2)
m <- qtls_make_rlang_table(q6)
rhs <- rlang::f_rhs(q6)
qtls_what_is_it(rhs)
typeof(rhs)
class(rhs)
qtls_what_is_it(rhs)

ex <- m[[3, "expression"]]
c <- rlang::node_car(ex)
qtls_what_is_it(ex)



rlang::is_symbol(ex)


car <- rlang::node_car(rhs)
cdr <- rlang::node_cdr(rhs)
length(car)
length(cdr)
typeof(car)
car
rlang::mut_node_car(rhs, rlang::sym("-"))


rhs1 <- rlang::f_rhs(q1)
rhs2 <- rlang::f_rhs(q2)

typeof(rhs1)
typeof(rhs2)
class(rhs1)
class(rhs2)
qtls_what_is_it(rhs1)
qtls_what_is_it(rhs2)

model <- qtls_make_rlang_table(q1)
model <- qtls_make_rlang_table(q2)
model <- qtls_make_rlang_table(q3)
model <- qtls_make_rlang_table(q4)
model <- qtls_make_rlang_table(q5)
model <- qtls_make_rlang_table(q6)

l <- model[[4, "what_is_expr"]]

f3 <- rlang::eval_tidy(q5)
x <- 9
b <- 2
a <- 3
p <- 4

f3(0)

g <- qtls_plot_model(model)
DiagrammeR::render_graph(g, layout="tree")

model[[7, "expr_text"]]
e <- model[[7, "expression"]]

f <- rlang::eval_tidy(q)
f(9)
qtls_what_is_it(q)
rhs <- rlang::f_rhs(q)
rlang::expr_text(rhs)
rlang::expr_label(rhs)
typeof(rhs)
class(rhs)

qtls_what_is_it(rhs)
car <- rlang::node_car(rhs)
qtls_what_is_it(car)
class(car)
typeof(car)
length(car)
cdr <- rlang::node_cdr(rhs)
qtls_what_is_it(cdr)
typeof(cdr)
cdr1 <- cdr[[1]]
typeof(cdr1)
qtls_what_is_it(cdr1)
cdr2 <- rlang::node_cdr(cdr[[2]])
qtls_what_is_it(cdr2)
rlang::node_tag(cdr2)

q10 <- rlang::quo(2 + 3)
ex <- rlang::f_rhs(q6)
rlang::is_symbol(ex)
q6

expr <- model[[2, "expression"]]
qtls_what_is_it(expr)
car <- rlang::node_car(expr)
cdr <- rlang::node_cdr(expr)

rlang::mut_node_cdr(expr, rlang::sym("-"))

rlang::is_symbol(expr)
print(expr)

rlang::is_symbol(NULL)
#---------------------------------------
q2 <- rlang::quo(1 + 2)
t2 <- qtls_make_rlang_table(q2)
t3 <- dplyr::filter(t2, expr_type == "symbol")

ex1 <- t3[[1, "expression"]]
pryr::address(ex1)

purrr::walk(1:nrow(t3), function(index, tbl) {
ex <- tbl[[index, "expression"]]
print(pryr::address(ex))


	}, t3)

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




#---------------------------------------


q10 <- rlang::quo(3 + 2)
t2 <- qtls_make_rlang_table(q10)
t3 <- dplyr::filter(t2, expr_type == "symbol", expr_text %in% bizzarro_op$sym)
t4 <- dplyr::select(t3, sym = expression,
											 parent = t2[[id == parent, "expression"]])
t5 <- dplyr::right_join(dplyr::select(t2, id, rowid),
												dplyr::select(t3, rowid, id = parent), by = c("id"),
												suffix = c(".lang", ".op"))
expr <- dplyr::select(dplyr::filter(t2,
																		rowid == 2), expression)[[1]]


purrr::walk2(t5$expression.lang, t5$expression.op, function(expr, op) {
	print(expr)
	print("---")
	print(op)
})

s <- rlang::sym("+")

bop <- dplyr::filter(bizzarro_op, sym == s)$bizzarro_sym



rlang::expr_text(t5[[1, "expression.lang"]])
qtls_what_is_it(t5[[1, "expression.lang"]])




rhs <- rlang::f_rhs(q10)
car <- rlang::node_car(rhs)
qtls_what_is_it(car)
car2 <- rlang::node_car(car)


s <- rlang::sym("-")
str(s)
rlang::is_symbol(s)
qtls_what_is_it(s)


rlang::mut_node_car(t2[2,]$expression, rlang::sym("-"))


t2 <- qtls_make_rlang_table(q10)


ex <- t2[t2$rowid == 2,]$expression[[1]]
typeof(ex)
nrow(t3)
t3[[1, "expression"]]
f <- dplyr::filter(bizzarro_op, sym == "+" | sym == "-")$bizzarro_sym
typeof(f)

e <- rlang::expr(1 + 2)

qtls_what_is_it(e)

rlang::eval_tidy(e)


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






