# mod quosure scratch


qa <- rlang::quo(a)
qtls_what_is_it(qa)
ea <- rlang::get_expr(qa)
qtls_what_is_it(ea)

q1 <- rlang::quo(1L)
e1 <- rlang::get_expr(q1)
qtls_what_is_it(e1)

q2 <- rlang::quo("DFSDF")
e2 <- rlang::get_expr(q2)
qtls_what_is_it(e2)

q3 <- rlang::quo(a + b)
e3 <- rlang::get_expr(q3)
qtls_what_is_it(e3)

q4 <- rlang::quo(a + b + c)
e4 <- rlang::get_expr(q4)
qtls_what_is_it(e4)






walk_cons2 <-
	function(q,
					 level = 1,
					 tbl = new.env() ,
					 parent = 0) {
		if (is.null(tbl$tbl)) {
			tbl$tbl <- tibble::tribble( ~ id, ~ parent, ~ atom)
			tbl$pass <- 1
		}

		print(glue::glue("pass: {tbl$pass}  ------------------------------"))
		print(glue::glue("q{level}: {qtls_what_is_it(q)}"))
		if (level > 5)
			return()
		if (!rlang::is_node(q)) {
			if (rlang::is_formula(q)) {
				e <- rlang::f_rhs(q)
			} else {
				e <- rlang::get_expr(q)
			}
			print(glue::glue("e{level}: {e}"))
			print(glue::glue("e{level}: {qtls_what_is_it(e)}"))
			cdr <- rlang::node_cdr(e)
			car <- rlang::node_car(e)
			print(glue::glue("cdr {level}: {cdr}"))
			print(glue::glue("cdrw{level} {length(cdr)}: {qtls_what_is_it(cdr)}"))
			print(glue::glue("car{level} {car}"))
			print(glue::glue("car{level}: {qtls_what_is_it(car)}"))
			for (index in 1:length(cdr)) {
				print(glue::glue("cd_child {index} {qtls_what_is_it(cdr[[index]])}"))
			}
			car <- rlang::node_car(e)
			tbl$tbl <-
				dplyr::bind_rows(tbl$tbl,
												 tibble::tibble(
												 	id = list(tbl$pass),
												 	parent = list(parent),
												 	atom = list(car)
												 ))
			print(glue::glue("car{level}: {qtls_what_is_it(car)}"))
			parent = tbl$pass
			for (index in 1:length(cdr)) {
				tbl$pass <- tbl$pass + 1
				if (rlang::is_lang(cdr[[index]])) {
					walk_cons(cdr[[index]], level + 1, tbl, parent)
				} else {
					tbl$tbl <-
						dplyr::bind_rows(tbl$tbl,
														 tibble::tibble(
														 	id = list(tbl$pass),
														 	parent = list(parent),
														 	atom = list(cdr[[index]])
														 ))
					tbl$pass <- tbl$pass + 1
					print(glue::glue("leaf: {cdr[[index]]} type: {typeof(cdr[[index]])}"))
				}
			}
		}
		tbl$tbl
	}

q <- rlang::quo(a + b * c + d)
t <- walk_cons(q)



tbl <- new.env();
tbl$tbl <- tibble::tribble(~id, ~parent, ~atom)

dplyr::bind_rows(tbl$tbl, list(id=1, parent="asdf", atom=list("a", "n")))



p <- rlang::parse_quosure("a + b * c + d")

qtls_what_is_it(p)


rlang::get_expr

node_walk <- function(.x, .f, ...) {
	cur <- .x
	while (!is.null(cur)) {
		.f(cur, ...)
		cur <- rlang::node_cdr(cur)
	}
	NULL
}



rlang::node_car


rlang::lang_tail

q <- rlang::quo(a + b * c + d)
e <- rlang::get_expr(q)
cdr <- rlang::node_cdr(e)
car <- rlang::node_car(e)

walk_cons(cdr)


node_walk(car, print)


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
}


change_to_minus2 <- function(q) {
	expr <- rlang::get_expr(q)
	#expr <- q[[2]]
	car <- rlang::node_car(expr)
	if (car == rlang::sym("+")) {
		rlang::mut_node_car(expr, rlang::sym("-"))
	}
}
# working, walks entire expression
change_to_minus3 <- function(q) {
	expr <- rlang::get_expr(q)
	if(!rlang::is_lang(expr) & !rlang::is_node(expr)) return()
	car <- rlang::node_car(expr)
	if (car == rlang::sym("+")) {
		rlang::mut_node_car(expr, rlang::sym("-"))
	}
	cdr <- rlang::node_cdr(expr)
	carn <- rlang::node_car(cdr)
	cdrn <- rlang::node_cdr(cdr)
	change_to_minus3(carn)
}


change_to_minus4 <- function(q) {
	expr <- rlang::get_expr(q)
	if(!rlang::is_lang(expr) & !rlang::is_node(expr)) return()
	car <- rlang::node_car(expr)
	if (car == rlang::sym("+")) {
		rlang::mut_node_car(expr, rlang::sym("-"))
	}
	cdr <- rlang::node_cdr(expr)
	carn <- rlang::node_car(cdr)
	cdrn <- rlang::node_cdr(cdr)
	for (index in 1:length(cdr))
	{
		change_to_minus4(carn[[index]])
	}
}



q7 <- rlang::quo(a + (b * c) - d)
change_to_minus4(q7)

qtls_walk_outline  (q7)


a <- 2
b <- 3
c <- 4
d <- 5
q <- rlang::quo(a + (b * c) / d)
#change_to_minus3(q)

# this sequence traverses tree
expr <- rlang::get_expr(q)
#expr <- q[[2]]
car <- rlang::node_car(expr)
if (car == rlang::sym("+")) {
	rlang::mut_node_car(expr, rlang::sym("-"))
}
cdr <- rlang::node_cdr(expr)
carn <- rlang::node_car(cdr)
length(carn)
cdrn <- rlang::node_cdr(cdr)
length(cdrn)
qtls_what_is_it(carn)
qtls_what_is_it(cdrn)

carn[[3]]

expr <- rlang::get_expr(carn)
rlang::is_lang(expr)
car <- rlang::node_car(expr)
rlang::mut_node_car(expr, rlang::sym("-"))
rlang::is_lang(expr)

cdr <- rlang::node_cdr(expr)
car <- rlang::node_car(expr)
carn <- rlang::node_car(cdr)
cdrn <- rlang::node_cdr(cdr)

expr <- rlang::get_expr(cdrn)
qtls_what_is_it(expr)
rlang::is_lang(expr)
car <- rlang::node_car(expr)
cdr <- rlang::node_cdr(expr)
#rlang::mut_node_car(expr, rlang::sym("-"))

expr <- rlang::get_expr(car)
rlang::is_lang(expr)
car <- rlang::node_car(expr)
cdr <- rlang::node_cdr(expr)
#rlang::mut_node_car(expr, rlang::sym("-"))

expr <- rlang::get_expr(cdr)
rlang::is_node(expr)
#rlang::mut_node_car(expr, rlang::sym("-"))
cdr <- rlang::node_car(expr)


q



qtls_what_is_it(expr)
rlang::is_lang(expr)



expr <- rlang::get_expr(carn)

qtls_what_is_it(car)
rlang::is_lang(expr)
expr <- rlang::get_expr(cdrn)
rlang::is_lang(expr)
typeof(expr)


expr <- rlang::get_expr(carn)
#test for completion
rlang::is_lang(expr)

rlang::is_symbol(expr)
car <- rlang::node_car(expr)

q


cdr <- rlang::node_car(car)
carn <- rlang::node_car(cdr)
change_to_minus2(carn)



change_to_minus2(q)
rlang::eval_tidy(q)

expr <- rlang::get_expr(q)
cdr <- rlang::node_cdr(expr)
cdr1 <- cdr[[1]]
cdr2 <- cdr[[2]]

carn <- rlang::node_car(cdr)
cdrn <- rlang::node_cdr(cdr)
typeof(cdrn)
x2 <- rlang::get_expr(carn)
car2 <- rlang::node_car(x2)
cdr2 <- rlang::node_cdr(x2)

q2 <- q[[2]]
typeof(q2)
q2e <- rlang::get_expr(q2)
typeof(q2e)
identical(q2, q2e)
pryr::address(q2)
pryr::address(q2e)

car <- rlang::node_car(q[[2]])
length(q[[2]])
length(car)
q2 <- q[[2]]


q <- rlang::quo(a * b + c * d)
x <- rlang::get_expr(q)
 x[[1]]
 x[[2]]
 x[[3]]

rlang::node_car(x)
rlang::node_cdr(x)

length(x)

q2[[1]]
q2_2 <- q2[[2]]
typeof(q2_2)

q2[[3]]



str(car)


q_1 <- q[[1]]
q_2 <- q[[2]]
q_3 <- q[[3]]

qtls_walk <- function(quosure) {
	head <- rlang::lang_head(quosure)
	tail <- rlang::lang_tail(quosure)
	#context$head_processing(head)
	for (index in 1:length(tail)) {
		item <- tail[[index]]
		if (rlang::is_lang(item)) {
			node <- qtls_walk(rlang::quo(!!item))
			#context$lang_processing(item)
		} else {
			#context$expr_processing(item)
		}
		#context$loop_end()
	}
	#context$post_loop()
}


q

