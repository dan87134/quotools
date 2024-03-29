# scratch for make_rlang_model
q <- rlang::quo((a))
rhs <- rlang::f_rhs(q)
qtls_what_is_it(rhs)
typeof(rhs)


qa <- rlang::quo(a)
rhsa <- rlang::f_rhs(qa)
qtls_what_is_it(rhsa)
typeof(rhsa)
rlang::node_car(rhsa)

`+`(1,`+`(2, `+`(3, `+`(4, 5))))

a <- 1
b <- 2
q <- rlang::quo(a + b)
`!!`(q)

identity(!!!q)

f <- function(expr) {
	rlang::enquo(expr)
}

get_arith_ops <- function(expr) {
	rlang::enquo(expr) %>%
		qtls_make_rlang_model() %>%
		dplyr::filter(position == 1, expr_text %in% c("+", "-", "*", "/"))
}
q <- rlang::quo(a + b * c)
get_arith_ops(!!q)

identity(!!(q))
UQE

q
rlang::quo(!! q)
f <- function(x) { x}
f(!!q)

f3 <- function(...) {
	tbl <- tibble::tribble (
		~x, ~y, ~z,
		1,2,3,
		4,5,6
	)
	dplyr::select(tbl, ...)
}

tbl <- tibble::tribble (
	~x, ~y, ~z,
	1,2,3,
	4,5,6
)

dplyr::select(tbl, x, z)

columns <- list("x", "z")

dplyr::select(tbl, !!! columns)


f3(x,z)

columns <- list("x", "z")

f3(!!! columns)



f4 <- function(cols) {
	qs <- quos(cls)
	tbl <- tibble::tribble (
		~x, ~y, ~z,
		1,2,3,
		4,5,6
	)
	dplyr::select(tbl, !!!qs)
}

f4(list("x", "y"))

# tribble built with text column names
q <- list(call("~", "a"), call("~", "b"), 1, 2, 3, 4)

q3 <- rlang::quo(tibble::tribble(rlang::UQS(q)))

rlang::eval_tidy(q3)





t1 <- tibble::as_tibble(list(a="asdf", b=3, c=5))

t2 <- tibble::as_tibble(list(a="eeg", b=7, c=6))

l11 <- list(t1, t2)

dplyr::bind_rows(!!!l11)

dplyr::bind_rows(!!! l11)



f3(x,y)

rlang::quos(c(x,y))
f3(c(x, y))

qs <- quos(list("x", "y"))


#try it out here
expr <- rlang::get_expr(q)
get_arith_ops(UQE(q))
UQ
identity

args <- list(1:10, na.rm = TRUE)
quo(mean( !!!args ))



f5 <- function(q) {
	rlang::get_expr(q)
}


q2 <- rlang::quo(expr)


`!!`(3)

!!(q)

`!!!`


qtls_what_is_it(q)

 f(!!q)

 f2 <- function(...) {
 	q <- rlang::quos(...)
 	f3(!!!(q))
 }

  f3(1, 2, 3)

 f3 <- function(...) { sum(...)}


get_arith_ops <- function(expr) {

}


get_ops <- function(expr) {
	ops <- rlang::enquo(expr) %>%
		qtls_make_rlang_model() %>%
		dplyr::filter(position == 1, expr_text %in% c("+", "-", "*", "/"))
}
nrow(get_ops(a + b - c * d))


get_ops <- function(expr) {
	q <- rlang::enquo(expr)
	ops <- q %>%
		qtls_make_rlang_model() %>%
		dplyr::filter(position == 1, expr_text %in% c("+", "-", "*", "/"))
}
nrow(get_ops(a + b - c * d))





qa <- rlang::quo("a")
rhsa <- rlang::f_rhs(qa)
l <- rlang::expr_label(rhsa)


qa <- rlang::quo(a)
tbl <- qtls_make_rlang_table(qa)

qa <- rlang::quo(a + b)
tbl <- qtls_make_rlang_table(qa)

s <- "\"b\""

q <- rlang::quo(a + b + c)
tbl <- qtls_make_rlang_model(q)

flip_add <- function(expr) {
	q <- rlang::enquo(expr)
	tree_table <- qtls_make_rlang_model(q)
	# this finds all the + operators in the expression
	plus_ops <- dplyr::filter(tree_table, expr_type == "symbol", expr_text == "+")
	# for this to work q has to be passed by reference
	qenv <- new.env()
	qenv$q <- q
	# this changes all of them
	purrr::walk(plus_ops$path, function(path) {
		qenv$q[[path]] <- rlang::sym("-")})
	eval_tidy(qenv$q)
}

flip_add(1 + 2 + 3)
flip_add(1 + 2 * 3)


q <- rlang::quo(a + b + c + d + e + f + g + h + j + k)
tree_table <- qtls_make_rlang_model(q)
g <- qtls_plot_model(tree_table)
DiagrammeR::render_graph(g, layout = "tree")

dplyr::select(tree_table, id, parent, position, expr_text)


e <- rlang::parse_expr("1 + a")
typeof(e)
class(e)
qtls_what_is_it(e)

qa <- rlang::quo(a + b + c)
tbl <- qtls_make_rlang_model(qa)
g <- qtls_plot_model(tbl)
DiagrammeR::render_graph(g, layout="tree")

s <- rlang::sym("a")
q <- rlang::quo(s)
qtls_what_is_it(s)

rlang::quo(+p)



q3 <- rlang::enquo(a + 1)
tbl3 <- qtls_make_rlang_model(q3)
QQ3 <- q3[[2:3]]

rlang::new_quosure

rlang::new_formula

rlang::lang

rlang:::rlang_interp


qtls_what_is_it(QQ3)


qtls_what_is_it(qa)

l <- c("asdfasdf", "ooo")
"asfasdf"  == class(l)
typeof(l)

s1 <- rlang::sym("asdf")
tbl <- qtls_make_rlang_model(":afsd")

qtls_what_is_it( 9)
e <- rlang::parse_expr("9")
qtls_what_is_it(e)

class(e)
typeof(e)

x <- function(z){z}(9)()
x(8)


ql <- rlang::quo(some <- mtcars %>% split(.$cyl) %>% map(select, vars_keep) %>% map(cor))
tbll <- qtls_make_rlang_model(ql)
g <- qtls_plot_model(tbll)
DiagrammeR::render_graph(g, layout="tree")

ql <- rlang::quo(mtcars %>% split(.$cyl))
tbll <- qtls_make_rlang_model(ql)
g <- qtls_plot_model(tbll)
DiagrammeR::render_graph(g, layout="tree")

`$`


ql <- rlang::quo(f1(select, vars_keep))
tbll <- qtls_make_rlang_model(ql)
g <- qtls_make_outline_plot(tbll)
DiagrammeR::render_graph(g, layout="tree")

length(ql)

t <- ql[[2]]
length(t)


qt <- rlang::qup


qf <- rlang::quo(function(x){}(z)())
tbl <- qtls_make_rlang_model(qf)
g <- qtls_plot_model(tbl)
DiagrammeR::render_graph(g, layout = "tree")
l <- qtls_make_outline_plot(tbl, id, id, position, parent,  expr_type, expr_text, path)
writeLines(l)

q <- rlang::quo(a + b + c)
tbl <- qtls_make_rlang_model(q)
l <- qtls_make_outline_plot(tbl, id, id, position, parent,  expr_type, expr_text, path)
writeLines(l)
A <-9
tbl <- qtls_make_rlang_model(rlang::parse_expr("A + b"))
l <- qtls_make_outline_plot(tbl, id, id, parent,  expr_type, expr_text, path)
writeLines(l)

qtls_what_is_it(A)


t <- rlang::expr_text( qa[[2]])
typeof(t)
str(t)
stringr::str_length(rlang::expr_text(rlang::get_expr(qa)))
qtls_what_is_it(qa)

q8 <- quo(a)
tbl <- qtls_make_rlang_model(q8)
l <- qtls_make_outline_plot(tbl, id, id, parent,  expr_type, a + b,  expr_text, path)
writeLines(l)

q8[[2]]

q9 <- rlang::quo(a + b)
tbl9 <- qtls_make_rlang_model(q9)
l <- qtls_make_outline_plot(tbl9, id, id, parent, expr_type, expr_text, path)
writeLines(l)

q10 <- rlang::quo(9)
tbl10 <- qtls_make_rlang_model(q10)
l <- qtls_make_outline_plot(tbl10, id, id, parent, expr_type, expr_text, what_is_expr)
writeLines(l)



q9[[c(2,1)]]

qb <- qa[[2]]

qc <- `!!`(qb)
tbl <- qtls_make_rlang_model(qc)
g <- qtls_plot_model(tbl)
DiagrammeR::render_graph(g, layout="tree")

typeof(qc)
qtls_what_is_it(qc)

qa <- rlang::quo(a * "b" + 7 * d - lim(x + 3))
tbl <- qtls_make_rlang_model(qa)
g <- qtls_plot_ast(tbl)
DiagrammeR::render_graph(g, layout="tree")

b


qa <- rlang::quo(a * b * 7 * d * "sdfSADF")
tbl <- qtls_make_rlang_model(qa)
g <- qtls_plot_model(tbl)
DiagrammeR::render_graph(g, layout="tree")
l <- qtls_make_outline_plot(tbl, id, id, parent, depth, expr_type, expr_text, what_is_expr)
writeLines(l)



qc <- rlang::parse_expr("3 * 4")
typeof(qc)
class(qc)
tab2 <- qtls_make_rlang_model(qc)
rlang::eval_tidy(qc)


qc <-  rlang::parse_exprs("a + b;c+ d;g + 9")
rlang::parse_quosure
qtls_what_is_it(qc)
typeof(qc)
class(qc)

rlang::is_formula(qc[[1]])



qa <- rlang::quo(a * f1(b + c * d) * 7 * (d + 9))
tbl <- qtls_make_rlang_model(qa)
g <- qtls_plot_model(tbl)
DiagrammeR::render_graph(g)
l <- qtls_make_outline_plot(tbl, id, id, parent, depth, expr_type, expr_text)
writeLines(l)



qa <- rlang::quo(function(x) { function (y = x) {y}}(8)())
tbl <- qtls_make_rlang_model(qa)
l <- qtls_make_outline_plot(tbl, id, id, parent, depth, expr_type, expr_text)
writeLines(l)
g <- qtls_plot_model(tbl)
DiagrammeR::render_graph(g, layout="nicely")


g <- qtls_plot_model(tbl)
DiagrammeR::render_graph(g, layout="tree")

x <- qa[[c(2,3,1)]]
ty
qa <- rlang::quo(1:3 + a)
tbl <- qtls_make_rlang_table(qa)
g <- qtls_plot_ast(tbl)
DiagrammeR::render_graph(g, layout="tree")

