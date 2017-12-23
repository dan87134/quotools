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



qa <- rlang::quo(a * "b" + 7 * d)
tbl <- qtls_make_rlang_model(qa)
g <- qtls_plot_model(tbl)
DiagrammeR::render_graph(g, layout="tree")

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

