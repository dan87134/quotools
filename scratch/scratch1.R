# scratch

q <- rlang::quo(a * b + c * d)
l <- qtls_walk_outline(q)
l1 <- qtls_walk(q, qtls_outline_context())

g <- qtls_node_tree(rlang::quo(a * b + c * d))
DiagrammeR::render_graph(g)


q2 <- rlang::quo(a * f1(b + c) + 34 + d)
l1 <- qtls_walk(q2, qtls_outline_context())



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

set_node_attrs

