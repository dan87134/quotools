# scratch

q <- rlang::quo(a * b + c * d)
l <- qtls_walk_outline(q)
l1 <- qtls_walk(q, qtls_outline_context())

g <- qtls_node_tree(rlang::quo(a * b + c * d))
DiagrammeR::render_graph(g)

g <- qtls_node_tree(rlang::quo(a * b + c * d))
DiagrammeR::render_graph(g, layout="tree")

g <- qtls_node_tree(rlang::quo(a * fn(b, c, 1:3) + c * (d + w)))
DiagrammeR::render_graph(g, layout = "tree")



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

set_node_attrs

														 																																								 																																			 																									1468065900, 1468065900, 1468065900), class = c("POSIXct", "POSIXt"
															 																																								 																																			 																									), tzone = "UTC"), days = c(1042L, 1042L, 554L, 554L, 554L, 554L
															 																																								 																																			 																									), Start_Date = structure(c(1420217880, 1420217880, 1420218180,
															 																																								 																																			 																																							1420218180, 1420218180, 1420218180), class = c("POSIXct", "POSIXt"
															 																																								 																																			 																																							), tzone = "UTC"), Stop_Date = structure(c(NA, NA, 1468065900,
															 																																								 																																			 																																																												 1468065900, 1468065900, 1468065900), class = c("POSIXct", "POSIXt"
															 																																								 																																			 																																																												 ), tzone = "UTC"), group = c("A", "B", "C", "D", "E", "F")), row.names = c(NA,
															 																																								 																																			 																																																												 																																					 -6L), class = c("tbl_df", "tbl", "data.frame"), .Names = c("y",
															 																																								 																																			 																																																												 																																					 																													 "Reg_Date", "Del_Date", "days", "Start_Date", "Stop_Date", "group"
															 																																								 																																			 																																																												 																																					 ))



