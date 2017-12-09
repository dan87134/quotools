# for walking AST

next_sibling <- function(level) {
	re <- "\\d+$"
	s <- as.integer(stringr::str_extract(level, re)) + 1L

	stringr::str_replace(level, re, as.character(s))
}

next_level <- function(level) {
	stringr::str_c(level, ".1")
}

qtls_walk_outline <- function(quosure,
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

qtls_quo_tree2 <- function(quosure) {
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
					DiagrammeR::add_node(ctx$graph,  label = rlang::expr_label(item))
				child <- ctx$graph$last_node
				ctx$graph <-
					DiagrammeR::add_edge(ctx$graph, to = child, from = parent)
			}
		}
		ctx$graph
	}
	build_nodes(quosure, gcontext)
}



qtls_expr_tree <- function(expr) {
	q <- rlang::enquo(expr)
	qtls_quo_tree(q)
}

qtls_walk_carcdr <- function(q, level = 1, tbl = new.env() ,  parent = 0) {
	if (is.null(tbl$tbl)) {
		tbl$tbl <- tibble::tribble(~id, ~parent, ~atom)
		tbl$pass <- 1
	}
	if (!rlang::is_node(q)) {
		if (rlang::is_formula(q)) {
			e <- rlang::f_rhs(q)
		} else {
			e <- rlang::get_expr(q)
		}
		cdr <- rlang::node_cdr(e)
		car <- rlang::node_car(e)
		tbl$tbl <- dplyr::bind_rows(tbl$tbl, tibble::tibble(id = list(tbl$pass),
																												parent = list(parent),
																												atom=list(car)))
		parent = tbl$pass
		for (index in 1:length(cdr)) {
			tbl$pass <- tbl$pass + 1
			if (rlang::is_lang(cdr[[index]])) {
				qtls_walk_carcdr(cdr[[index]], level + 1, tbl, parent)
			} else {
				tbl$tbl <- dplyr::bind_rows(tbl$tbl, tibble::tibble( id = list(tbl$pass),
																														 parent = list(parent),
																														 atom = list(cdr[[index]])))
				tbl$pass <- tbl$pass + 1
			}
		}
	} else {
		print(glue::glue("end {q}"))
	}
	tbl$tbl
}


qtls_plot_parent_child(tbl) {
	current <= 0

}


walk_carcdr_old <- function(q, level = 1, tbl = new.env() ,  parent = 0) {
	if(is.null(tbl$tbl)) {
		tbl$tbl <- tibble::tribble(~id, ~parent, ~atom)
		tbl$pass <- 1
	}

	print(glue::glue("pass: {tbl$pass}  ------------------------------"))
	print(glue::glue("q{level}: {qtls_what_is_it(q)}"))
	if (level > 5)
		return()
	if (!rlang::is_node(q)) {
		if (rlang::is_formula(q)) {
			e <- rlang::f_rhs(q)
		} else {
			e <- rlang::get_expr(q)
		}
		print(glue::glue("e{level}: {e}"))
		print(glue::glue("e{level}: {qtls_what_is_it(e)}"))
		cdr <- rlang::node_cdr(e)
		car <- rlang::node_car(e)
		print(glue::glue("cdr {level}: {cdr}"))
		print(glue::glue("cdrw{level} {length(cdr)}: {qtls_what_is_it(cdr)}"))
		print(glue::glue("car{level} {car}"))
		print(glue::glue("car{level}: {qtls_what_is_it(car)}"))
		for (index in 1:length(cdr)) {
			print(glue::glue("cd_child {index} {qtls_what_is_it(cdr[[index]])}"))
		}
		car <- rlang::node_car(e)
		tbl$tbl <- dplyr::bind_rows(tbl$tbl, tibble::tibble(id = list(tbl$pass),
																												parent = list(parent),
																												atom=list(car)))
		print(glue::glue("car{level}: {qtls_what_is_it(car)}"))
		parent = tbl$pass
		for (index in 1:length(cdr)) {
			tbl$pass <- tbl$pass + 1
			if (rlang::is_lang(cdr[[index]])) {
				walk_cons(cdr[[index]], level + 1, tbl, parent)
			} else {
				tbl$tbl <- dplyr::bind_rows(tbl$tbl, tibble::tibble( id = list(tbl$pass),
																														 parent = list(parent),
																														 atom = list(cdr[[index]])))
				tbl$pass <- tbl$pass + 1
				print(glue::glue("leaf: {cdr[[index]]} type: {typeof(cdr[[index]])}"))
			}
		}
	}
	tbl$tbl
}



