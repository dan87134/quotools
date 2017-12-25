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


q <- rlang::quo(a + b + c)
tree_table <- qtls_make_rlang_model(q)
g <- qtls_plot_model(tree_table)
DiagrammeR::render_graph(g, layout = "tree")

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

