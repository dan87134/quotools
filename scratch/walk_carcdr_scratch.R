# walk_carcdr scratch


# example of ysing table produced by qtls_walk_carcdr
# to modify quosure
q <- rlang::quo(a * b + 2 * d )
ast_tbl1 <- qtls_make_ast_table(q)
g <- qtls_plot_ast(ast_tbl1)
DiagrammeR::render_graph(g, layout = "tree")

q <- rlang::quo(a * b + 2 * d )
t2 <- qtls_make_ast_table(q) %>%
dplyr::filter(expr_property == "car")

purrr::walk(t2$expression, function(expr) {
	car <- rlang::node_car(expr)
	if(rlang::is_symbol(car)) {
		if(car == rlang::sym("*")) {
			rlang::mut_node_car(expr, rlang::sym("/"))
		}
	}
})

flip_table <- tibble::tribble (
	~ sym, ~ bizzarro_sym,
	"+", "-",
	"-", "=",
	"*" , "/",
	"/", "*"
)

dplyr::filter(flip_table, sym == rlang::sym("%"))$bizzarro_sym



q <- rlang::quo(a * b + 2 * d )
bizzarro_flip <- function(q) {
t2 <- qtls_make_ast_table(q) %>%
	dplyr::filter(expr_property == "car")

purrr::walk(t2$expression, function(expr) {
	car <- rlang::node_car(expr)
	if (rlang::is_symbol(car)) {
		flip <- dplyr::filter(flip_table, sym == car)$bizzarro_sym
		if (flip != "") {
			rlang::mut_node_car(expr, rlang::sym(flip))
		}
	}
}
)
}


dplyr::filter(flip_table, sym == rlang::sym("%"))$bizzarro_sym


flip_table <- tibble::tribble(
	~ sym, ~ bizzarro_sym,
	"+", "-",
	"-", "=",
	"*" , "/",
	"/", "*"
)


bizzarro_flip <- function(q) {
	t2 <- qtls_make_ast_table(q) %>%
		dplyr::filter(expr_property == "car")
	purrr::walk(t2$expression, function(expr) {
		car <- rlang::node_car(expr)
		if (rlang::is_symbol(car)) {
			flip <- dplyr::filter(flip_table, sym == car)$bizzarro_sym
			if (flip != "") {
				rlang::mut_node_car(expr, rlang::sym(flip))
			}
		}
	})
}

q <- rlang::quo(a * b + 2 * d)

bizzarro_flip(q)

t2[1,]$expression

purrr::walk(dplyr::select(t2, expression), function(row) {
	print(row)
	print("----")
})


purrr::walk(function(row) {
	expr <- row
	car <- rlang::node_car(expr)
	if (rlang::is_symbol(car)) {
		if (car == rlang::sym("*")) {
		rlang::mut_node_car(expr, rlang::sym("/"))
			return()
	}
}
}
)






q <- rlang::quo(f <- function(x,y) { x + y}(1,2))
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



