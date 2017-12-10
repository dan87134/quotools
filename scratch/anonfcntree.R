# tree for f(x)(y)


q <- rlang::quo(f(x)())

head1 <- rlang::lang_head(q)
tail1 <- rlang::lang_tail(q)

length(head1)

head1_1 <- head1[1]
head1_2 <- head1[2]

rlang::get_expr(head1_1)



qtls_what_is_it(head1_1)
qtls_what_is_it(head1_2)


lobstr::ast(f(x)(f(y)())())
lobstr::ast(a + f(x)(y)(z) + c)

f <- function(a) {
	function(z, a) {
		a + b
	}
}

lobstr::ast(a + f() + g(a))


f1 <- function(a) {a}

2
z <- 9
f(2) %>% .(7)




2 %>%
