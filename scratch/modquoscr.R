# mod quosure scratch


q <- rlang::quo(a + b)
head <- rlang::lang_head(q)
tail <- rlang::lang_tail(q)

rlang::node_car( rlang::get_expr(q))

ge <- rlang::get_expr(q)
qtls_what_is_it(ge)
car <- rlang::node_car(ge)
cdr <- rlang::node_cdr(ge)
n <- rlang::node(car, cdr)

q2 <- rlang::quo(a - b)
ge2 <- rlang::get_expr(q2)
car2 <- rlang::node_car(ge2)

n2 <- rlang::node(car2, cdr)

rlang::mut_node_car(car, n2)
q

car2
cc <- c(1, b = 2, 3)
rlang::modify(cc, 4, b = "foo")

x <- list(a = 1, b = 2)
y <- list(b = 3, c = 4)
modify(x, splice(y))



a <- 2
b <- 3

rlang::eval_tidy(n2)
eval(n2)


qtls_what_is_it(head)


rlang::mut_node_car(head, rlang::node(rlang::sym("-"), rlang::node_car(tail)))
q
rlang::node_car(head)
rlang::node_car(tail)
rlang::node_cdr(tail)


q

lang <- quote(foo(bar))
copy <- lang
lang[[1]]
# This modifies + operator
#1
q <- rlang::quo(a + b)
q[[1]]
#2
q2 <- q[[2]]

car <- rlang::node_car(q2)
typeof(car)
car == rlang::sym("+")
#3
rlang::mut_node_car(q2, rlang::sym("-"))

a <- 2
b <- 3

rlang::eval_tidy(q)

# this works too
change_to_minus <- function(q) {
	q2 <- q[[2]]
	car <- rlang::node_car(q2)
	if (car == rlang::sym("+")) {
			rlang::mut_node_car(q2, rlang::sym("-"))
	}
	q2
}

q <- rlang::quo(a + b)
q <- change_to_minus(q)
rlang::eval_tidy(q)






q

