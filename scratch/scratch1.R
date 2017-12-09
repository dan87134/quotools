# scratch

q <- rlang::quo(a * b + c * d)
qtls_what_is_it(q)


tf <- function(arg) {
	q <- rlang::enquo(arg)
	rhs <- rlang::f_rhs(q)
	typeof(rhs)

}

tf(quo(a + b))

tf(a + b)


q <- rlang::quo(a + b * c + d)
l <- qtls_walk_outline(q)
l1 <- qtls_walk(q, qtls_outline_context())

g <- qtls_quo_tree(rlang::quo(a * b + c * d))
DiagrammeR::render_graph(g, layout = "tree")

g <- qtls_expr_tree(a * b + c * d)
DiagrammeR::render_graph(g, layout = "tree")

g <- qtls_quo_tree(rlang::quo(a * b + c * d))
DiagrammeR::render_graph(g, layout="tree")




suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(DiagrammeR))
g <- qtls_node_tree(rlang::quo(a * fn(b, c, 1:3) + c * (d + w)))
DiagrammeR::render_graph(g, layout = "tree" )


q2 <- rlang::quo(a * f1(b + c) + 34 + d)
l1 <- qtls_walk(q2, qtls_outline_context())

g1 <- DiagrammeR::create_graph(
	directed = FALSE
)

g1 <- DiagrammeR::add_node(g1)

DiagrammeR::render_graph(g1)

aaa <- `gggg`
fff <- function(expr) {
	q <- rlang::enquo(expr)
	h <- rlang::lang_head(q)
	t <- rlang::expr_label(h)
	typeof(t)
}
fff(a + b)


typeof(l)
l[[1]]
str(l)
as.integer("10")



suppressPackageStartupMessages(library(DiagrammeR))
a_graph <-
	create_graph(
		directed = FALSE
	) %>%
	add_node(label = "+") %>%
	add_node(label = "b") %>%
	add_node(label = "c") %>%
	add_edge(
		from = 1,
		to = 2,
		rel = "interacted_with"  ) %>%
	add_edge(from = 1, to = 3)
set_node_attrs(a_graph, node_attr = color, values = "red")
a_graph %>% render_graph(layout = "tree")

remove.packages(DiagrammeR)


rm(list = ls())


q <- rlang::quo(a + (b * c) + d)

q <- rlang::quo(a + (b * f1(c, x) + y))

g <- qtls_expr_tree(a + (b * f1(c, x) + y * 2) + d)
DiagrammeR::render_graph(g, layout = "tree")


q <- rlang::quo(a + (b * f1(c, x) + y * 2 / uu + q * b + 0) + d)
q <- rlang::quo(a + (b * f1(c, x) + y))
q <- rlang::quo(a + b / c + d)
t <- qtls_walk_carcdr(q)
g <- qtls_plot_parent_child(t)
DiagrammeR::render_graph(g, layout = "tree")

t <- qtls_graph_car_cdr_for_expression(a + b * c + d)


g <- qtls_graph_car_cdr_for_expression(a + b / c + d)
DiagrammeR::render_graph(g, layout = "tree")

g <- qtls_graph_car_cdr_for_expression(a + (b * f1(c, x, x1, x, x2) + y * 2 / uu + q * b + 0) + d)
DiagrammeR::render_graph(g, layout = "tree")

q <- rlang::quo(a + b * c + d)
t <- qtls_walk_table(q)

DiagrammeR::render_graph(g)

c <- dplyr::filter(t, parent == 22220)

f <- ~ d + b
f

1:0

qa <- rlang::quo(a)
q1 <- rlang::quo(1)
ex <- rlang::get_expr(qa)
qtls_what_is_it(ex)

rlang::get_expr(q1)
rlang::lang_head(qa)
rlang::is_lang

qab <- rlang::quo(a + b)
qtls_what_is_it(qab)
ex1 <- rlang::get_expr(qab)
qtls_what_is_it(ex1)

h <- rlang::lang_head(qab)
rlang::get_expr(h)


typeof(qa)
qtls_what_is_it(qa)
qtls_what_is_it(q1)
rlang::lang_tail(qa)
rlang::is_lang


tibble::tr
nrow(t)
t[1,]$atom

gr <- DiagrammeR::create_graph(directed = TRUE)
gr$last_node
gr <- DiagrammeR::add_node(gr,
										 label = "atom")

