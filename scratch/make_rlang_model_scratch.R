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
tbl <- qtls_make_rlang_table(qa)
g <- qtls_plot_ast(tbl)
DiagrammeR::render_graph(g, layout="tree")



qa <- rlang::quo(a * b + 7 * d)
tbl <- qtls_make_rlang_table(qa)
g <- qtls_plot_ast(tbl)
DiagrammeR::render_graph(g, layout="tree")



qa <- rlang::quo(function(x) { x }(9))
tbl <- qtls_make_rlang_table(qa)
g <- qtls_plot_ast(tbl)
DiagrammeR::render_graph(g, layout="tree")

qa <- rlang::quo(1:3 + a)
tbl <- qtls_make_rlang_table(qa)
g <- qtls_plot_ast(tbl)
DiagrammeR::render_graph(g, layout="tree")

