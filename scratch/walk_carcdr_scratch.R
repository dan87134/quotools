# walk_carcdr scratch

# example of ysing table produced by qtls_walk_carcdr
# to modify quosure
q <- rlang::quo(a * b + 2 * d + (e + f * (x - y)))
ast_tbl1 <- qtls_walk_carcdr(q)
g <- qtls_plot_parent_child(ast_tbl1)
DiagrammeR::render_graph(g, layout = "tree")


q <- rlang::quo(function(x,y) { x + y}())
ast_tbl1 <- qtls_walk_carcdr(q)
g <- qtls_plot_parent_child(ast_tbl1)
DiagrammeR::render_graph(g, layout = "tree")

c <- c("a", "b")
glue::glue("test {stringr::str_c(c, collapse='')}")


q <- rlang::quo(a * b + c * d)
qtls_walk_carcdr(q)

qtls_what_is_it(q)
rhs <- rlang::f_rhs(q)
# qtls_what_is_it(rhs)
e <- rlang::get_expr(rhs)
car <- rlang::node_car(e)
care <- rlang::get_expr(car)
cdr <- rlang::node_cdr(e)
cdre <- rlang::get_expr(cdr)

typeof(care)
typeof(cdre)


q <- rlang::quo(7 + 1)
ast_tbl1 <- qtls_walk_carcdr(q)


cars  <- dplyr::filter(ast_tbl1, node=="car")
q

car_tbl1 <- dplyr::filter(ast_tbl1, type == "car")
car_tbl1$expression


purrr::walk(car_tbl1$expression, function(expr) {
	car <- rlang::node_car(expr)
	if (rlang::is_symbol(car) & car == rlang::sym("+")) {
		rlang::mut_node_car(expr, rlang::sym("-"))
	}
})

q
t <- tibble::tibble(
	c1 = 1:3,
	c2 = list(c("a", "b"), c("c"), c("as", "fwer", "rwear"))

)

list(c("awe", "wAAER"), c("frea", "fewar"))

t <- tibble::tibble(
	c1 = 1,
	c2 = list(c("a", "b"))

)



