# quotes and closures
a <- 3
qt <- quote(a)
qtb <- quote(bbbbb)




eval(qt)

qtls_what_is_it(qt)
qtls_what_is_it(q)

q3 <- rlang::quo(a + b)
b <- 8

n <- rlang::node(rlang::sym("+"), rlang::lang_tail(q3))
qtls_what_is_it(n)
q3
qtls_what_is_it(q3)
tail <- rlang::lang_tail(q3)
head <- rlang::lang_head(q3)

qtls_what_is_it(tail)
qtls_what_is_it(head)

s <- rlang::sym("-")
q3 <- rlang::quo(a + b)

q4 <- rlang::quo(a - b)
car4 <- rlang::node_car(q4)
car3 <- rlang::node_car(q3)
head <- rlang::lang_head(q3)
rlang::mut_node_car(head, node(s)
head


qtls_what_is_it(head)


rlang::lang_head
rlang::lang_tail
rlang::mut_node_car(q3, s)



str(q3)

q3


qtls_what_is_it(s)

eval(tail)


rlang::eval_tidy(n)
eval(n)
car <- rlang::node_car(n)
cdr <- rlang::node_cdr(n)
typeof(cdr)
typeof(car)

args1 <- rlang::as_pairlist(list(rlang::sym("a"), rlang::sym("b")))
typeof(args1)


a <- 4

q <- rlang::quo(a)

rlang::eval_tidy(q)
rlang::eval_tidy(qt)

f1 <- function(x)
{
	function(y) {x * y}
}

f2 <- f1(2)

f2(10)

f1q <- function(x) {
	q <- rlang::enquo(x)
	function(y) {
		rlang::eval_tidy(q) * y
	}
}

f10 <- function(x) {
	qt <- quote(x)
	function(y) {
		eval(qt) * y
	}
}

f11 <- f10(a)


f3 <- function(x) {
	f2q(x)
}

f4 <- function(x) {
	a <- 100
	f2q(x)
}

f5 <- function(x) {
	a <- 100
	f11(x)
}



f3(10)

f4(10)

f5(10)


f2q <- f1q(a)

f2q(10)

a <- 2




a <- 3


bquote(a == a)
quote(a == a)

bquote(a == .(a))
substitute(a == A, list(A = a))

plot(1:10, a*(1:10), main = bquote(a == .(a)))



