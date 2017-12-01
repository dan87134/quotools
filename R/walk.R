# for walking AST

next_sibling <- function(level) {
	re <- "\\d+$"
	s <- as.integer(stringr::str_extract(level, re)) + 1L

	stringr::str_replace(level, re, as.character(s))
}

next_level <- function(level) {
	stringr::str_c(level, ".1")
}

qtls_walk_outline <-
	function(quosure,
					 qtbl = tibble::tibble(),
					 slevel = "1") {
		class(qtbl) <- c("qtls_outline", class(qtbl))
		head <- rlang::lang_head(quosure)
		tail <- rlang::lang_tail(quosure)
		qtbl <- dplyr::bind_rows(
			qtbl,
			data.frame(
				outline = slevel,
				expr = rlang::expr_label(head),
				stringsAsFactors = FALSE
			)
		)
		slevel <- stringr::str_c(slevel, ".1")
		for (index in 1:length(tail)) {
			item <- tail[[index]]
			if (rlang::is_lang(item)) {
				qtbl <- qtls_walk_outline(rlang::quo(!!item), qtbl, slevel)
			} else {
				qtbl <- dplyr::bind_rows(
					qtbl,
					data.frame(
						outline = slevel,
						expr = rlang::expr_label(item),
						stringsAsFactors = FALSE
					)
				)
			}
			slevel <- next_sibling(slevel)
		}
		qtbl
	}

qtls_context <- function() {
	context <- new.env()
	context$head_processing <- function(head) {

	}
	context$expr_processing <- function(expr) {

	}
	context$lang_processing <- function(item)	{

	}
	context$post_loop <- function() {
		NA
	}
	context$loop_start <- function() {

	}
	context$loop_end <- function() {

	}
	context
}


qtls_outline_context <- function() {
	outline <- qtls_context()
	outline$tbl <- tibble::tibble()
	class(outline$tbl) <-
		c("qtls_outline", "qtls_tbl", class(outline$tbl))

	next_sibling <- function(level) {
		re <- "\\d+$"
		s <- as.integer(stringr::str_extract(level, re)) + 1L
		stringr::str_replace(level, re, as.character(s))
	}
	outline$tbl <- tibble::tibble()
	class(outline$tbl) <-
		c("qtls_outline", "qtls_tbl", class(outline$tbl))
	outline$head_processing <- function(head) {
		outline$tbl <-
			dplyr::bind_rows(
				outline$tbl,
				data.frame(
					outline = outline$slevel,
					expr = rlang::expr_label(head),
					stringsAsFactors = FALSE
				)
			)
		outline$slevel <- stringr::str_c(outline$slevel, ".1")
	}
	outline$expr_processing <- function(expr) {
		outline$tbl <- dplyr::bind_rows(
			outline$tbl,
			data.frame(
				outline = outline$slevel,
				expr = rlang::expr_label(expr),
				stringsAsFactors = FALSE
			)
		)
	}
	outline$lang_processing <- function(item) {
		save <- outline$slevel
		outline$tbl <- qtls_walk(rlang::quo(!!item), outline)
		outline$slevel <- save
	}
	outline$loop_end <- function() {
		outline$slevel <-  next_sibling(outline$slevel)
	}
	outline$post_loop <- function() {
		outline$tbl
	}
	outline$slevel <- "1"
	outline
}

qtls_walk <- function(quosure, context) {
	head <- rlang::lang_head(quosure)
	tail <- rlang::lang_tail(quosure)
	context$head_processing(head)
	for (index in 1:length(tail)) {
		item <- tail[[index]]
		if (rlang::is_lang(item)) {
			node <- qtls_walk(rlang::quo(!!item), context)
			context$lang_processing(item)
		} else {
			context$expr_processing(item)
		}
		context$loop_end()
	}
	context$post_loop()
}


qtls_node_tree_context <- function()
{
	node_context <- qtls_context()
	node_context$heads <- vector()
	node_context$graph <- DiagrammeR::create_graph(directed = TRUE)
	node_context$head_processing <- function(head) {
		name <- rlang::expr_label(head)
		node_context$graph <-
			DiagrammeR::add_node(node_context$graph, label = name)
		node_context$heads <-
			c(node_context$heads,
				DiagrammeR::get_last_nodes_created(node_context$graph))
	}
	node_context$loop_end <- function() {

	}
	node_context$post_loop <- function() {
		len <- length(node_context$heads)
		if (len > 1) {
			node_context$graph <- DiagrammeR::add_edge(node_context$graph,
																								 from = node_context$heads[[len]],
																								 to = node_context$heads[[len - 1]])
		}
		node_context$heads <- node_context$heads[-len]
		node_context$graph

	}
	node_context$expr_processing <- function(expr) {
		name <- rlang::expr_label(expr)
		node_context$node_count <- node_context$node_context + 1
		node_context$graph <-
			DiagrammeR::add_node(node_context$graph, label = name)
		node_added <-
			DiagrammeR::get_last_nodes_created(node_context$graph)[[1]]
		node_context$graph <-
			DiagrammeR::add_edge(node_context$graph,
													 from = node_added,
													 to = node_context$heads[length(node_context$heads)])
		node_context
	}
	node_context
}


qtls_node_tree2 <- function(quosure) {
	qtls_walk(quosure, qtls_node_tree_context())
}

qtls_node_tree <- function(quosure) {
	heads <- vector()

	graph <- DiagrammeR::create_graph(directed = TRUE)
	gcontext <- new.env()
	gcontext$graph <- graph
	build_nodes <- function(quosure, ctx, parent = NA) {
		head <- rlang::lang_head(quosure)
		tail <- rlang::lang_tail(quosure)
		ctx$graph <-
			DiagrammeR::add_node(ctx$graph, color = "green", type = typeof(head), label = rlang::expr_label(head))
		if (!is.na(parent)) {
			ctx$graph <- DiagrammeR::add_edge(ctx$graph,
																				from = parent,
																				to = ctx$graph$last_node)
		}
		parent <- ctx$graph$last_node
		for (index in 1:length(tail)) {
			item <- tail[[index]]
			if (rlang::is_lang(item)) {
				node <- build_nodes(rlang::quo(!!item), ctx, parent)
			} else {
				ctx$graph <-
					DiagrammeR::add_node(ctx$graph, label = rlang::expr_label(item))
				child <- ctx$graph$last_node
				ctx$graph <-
					DiagrammeR::add_edge(ctx$graph, to = child, from = parent)
			}
		}
		ctx$graph
	}
	build_nodes(quosure, gcontext)
}
