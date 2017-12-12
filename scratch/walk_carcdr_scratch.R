# walk_carcdr scratch

# example of ysing table produced by qtls_walk_carcdr
# to modify quosure
q <- rlang::quo(a * b + c * d)
ast_tbl1 <- qtls_walk_carcdr(q)
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
