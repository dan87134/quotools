# my negate om scratch

q <- rlang::quo(a + 2 + 3L + 4.9/ "ddd" - d)

tbl <- qtls_make_rlang_model(q)
g <- qtls_plot_model(tbl)
DiagrammeR::render_graph(g)
ol <- qtls_make_outline_plot(tbl, id, id, expr_text, expr_type, what_is_expr)
writeLines(ol)

fcns <- dplyr::filter(tbl, position == 1, leaf, id != 2)
fcn_parents <- dplyr::filter(tbl, rowid %in% dplyr::left_join( fcns, tbl, by = c("parent" = "id"))$rowid.y)

fcn_parents <- dplyr::bind_cols(fcn_parents, v = as.list(rep(NA, length.ou)))

c(NULL, NULL)

rlang::eval_tidy(2)

ops <- dplyr::filter(tbl, any(id %in% fcns$parent))

dplyr::filter(fcns, expr_type %in% c("+", "*", "/", "-"))

"a" %in% c("a", "b")

rep(NA, 10)


q1 <- rlang::quo(a)
tbl1 <- qtls_make_rlang_model(q1)


s <- rlang::sym("sdf")
s[1]
length(2)

fcn_parents <- dplyr::bind_cols(fcn_parents, v = rep(NA, nrow(fcn_parents)))





qtls_my_negate_om(a + b / c - d)

any(c("a", "b", "g", "h") %in% c("d", "a", "f"))
