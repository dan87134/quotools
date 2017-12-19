# outline plot scratch
q <- rlang::quo(a * b + c * d)
tbl <- qtls_make_rlang_table(q)
g <- qtls_plot_model(tbl)
DiagrammeR::render_graph(g, layout = "tree")

# + d=0 pos=1
# == * d=1 pos=1
# ==== a d=2 pos=1
# ==== b d=2 pos= 1
# == * d=1 pos=2
# ==== c d=2 pos=1
# ==== d d=2 pos=2

root <- rlang:filter(tbl, parent == 0)
tbl


f1 <- function(tbl, key_column, ...) {
	qkey <- rlang::quo(key_column)
	q <- quos(...)
	list(qkey, q)
}

f1 (tbl, id, expr_text, id, parent)

make_info_function <- function(tbl, key_column, ...) {
	qkey <- rlang::enquo(key_column)
	qkey_rhs <- rlang::f_rhs(qkey)
	qkey_text <- rlang::expr_text(qkey_rhs)
	q <- quos(...)
	columns <- purrr::map_chr(q, function(q) {
		rhs <- rlang::f_rhs(q)
		rlang::expr_text(rhs)
	})
	function(tbl, key) {
		values <- map_chr(columns, function(column, tbl, id) {
			stringr::str_c(column, ":", as.character(
				tbl[tbl[qkey_text] == key, column][[1]], collapse = ""))
		}, tbl, id)
		stringr::str_c(values, collapse = ", ")
	}
}

info <- qtls_make_info_function(tbl, key_column = id, expr_text, id, parent)

info(tbl, 5)
s <- as.character(tbl[[1, "id"]])

typeof(s)
tbl[tbl["id"] == 2, "parent"][[1]]

make_info_function(tbl, id, parent)



node_info <- function(tbl, current_id) {
	row <- dplyr::filter(tbl, id == current_id)
	mimics <- stringr::str_c(row$what_is_expr, collapse = ", ")
	glue::glue("{row$expr_text} type:{row$expr_type} id:{row$id} p:{row$parent} {row$expr_text} src:{row$source} class:{row$expr_class}")
}


make_outline_plot <- function(tbl, key_column, ...) {
	qkey <- rlang::enquo(key_column)
	qcolumns <- rlang::quos(...)
	info_function <- qtls_make_info_function(tbl, !!qkey, !!!qcolumns)
	oenv <- new.env()
	oenv$output <- vector(mode = "character")
	traverse_depth_first <- function(tbl, depth = 0, current_id = NULL,
																	 oenv)
	 {
		if (is.null(current_id)) {
			current_id <- dplyr::filter(tbl, parent == 0)$id
		}
		info <- info_function(tbl, current_id)
		oenv$output <- c(oenv$output,
		stringr::str_c(stringr::str_dup("--", depth), info, collapse = ""))
		children <- dplyr::filter(tbl, parent == current_id)
		purrr::walk(children$id, function(child_id, depth) {
			 traverse_depth_first(tbl, current_id = child_id, depth = depth + 1, oenv)
		}, depth)
	}
	traverse_depth_first(tbl, oenv = oenv)
	oenv$output
}
otree <- qtls_make_outline_plot(tbl, id, expr_text, id, source, expr_type)

purrr::walk(otree, function(line) {
	print(line)
})


paste, "\n", "b")

stringr::str_c(otree, collapse="", separate="\n")

print(otree, justify="left")




a <- c("adsa", "vrear")

print(stringr::str_c(a, collapse  = "\n"))

print(a, )


purrr::map_chr(a, ~ print(.x))


glue::glue( sep="  ")
q3 <- rlang::quo(a * b + c * d)
rhs <- rlang::f_rhs(q3)
ex <- rlang::get_expr(q3)

typeof(rhs)
typeof(ex)

qtls_what_is_it(rhs)
qtls_what_is_it(ex)

identical(rhs, ex)

make_outline_plot(tbl, id, expr_text, id, parent)

walk_tbl_depth_first <- function(tbl,
																 current_id = NULL,
																 depth = 0
) {
	if (is.null(current_id)) {
		current_id <- dplyr::filter(tbl, parent == 0)$id
	}
	#print(current_id)
	f(current_id, depth)
	children <- dplyr::filter(tbl, parent == current_id)
	purrr::walk(children$id, function(id, depth) {
		walk_tbl_depth_first(tbl, current_id = id, depth = depth + 1)
	}, depth)
}

row1 <- tbl[1,]
row1$id

walk_tbl_depth_first(tbl)

s <- stringr::str_dup("==", 0)
str(s)

typeof(s)


qapb <- rlang::quo(a * b + c * d)
ex <- rlang::get_expr(qapb)
ex1 <- ex[1]
ex2 <- ex[2]
ex3 <- ex[3]

ex3a <- rlang::get_expr(ex3)
ex3a[2]



ex2[2]


rlang::get_expr(ex2)
q3 <- rlang::quo(1 + 2)
rlang::quo(+)

length(q3)
 x1 <- q3[1]
 x2 <- q3[2]
x <- rlang::get_expr(x2)

rlang::expr_text(x)
 x <- rlang::get_expr(q3)
 rlang::expr_name(x2)

 length(x)
x1 <- x[1]
nx <- rlang::set_expr(x1, rlang::quo(sym("-")))
q3

length(qapb)
qapb[1]
t1 <- qapb[2]
ex <- rlang::get_expr(qapb)

length(ex)

x1 <- ex[1]

ex[2]
x3 <- ex[3]
qtls_what_is_it(x3)

length(x3)
x3[1]




