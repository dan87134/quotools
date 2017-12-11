# for walking AST


# produces parse tree
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
		}
		qtbl
	}

# qtls_context <- function() {
# 	context <- new.env()
# 	context$head_processing <- function(head) {
#
# 	}
# 	context$expr_processing <- function(expr) {
#
# 	}
# 	context$lang_processing <- function(item)	{
#
# 	}
# 	context$post_loop <- function() {
# 		NA
# 	}
# 	context$loop_start <- function() {
#
# 	}
# 	context$loop_end <- function() {
#
# 	}
# 	context
# }


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


#' Title
#' Builds a graph object for DiagrammeR from an expression
#' @param quosure
#'
#' @return
#' @export
#'
#' @examples
qtls_node_tree <- function(quosure) {
	qtls_walk(quosure, qtls_node_tree_context())
}

#' Title
#' Builds a graph object for DiagrammeR from a quosure
#' @param quosure
#'
#' @return
#' @export
#'
#' @examples
qtls_quo_tree <- function(quosure) {
	heads <- vector()

	graph <- DiagrammeR::create_graph(directed = TRUE)
	gcontext <- new.env()
	gcontext$graph <- graph
	build_nodes <- function(quosure, ctx, parent = NA) {
		head <- rlang::lang_head(quosure)
		tail <- rlang::lang_tail(quosure)
		ctx$graph <-
			DiagrammeR::add_node(ctx$graph, color = "green",
													 type = typeof(head),
													 label = rlang::expr_label(head))
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

#' Title
#' build a table of parent child relationship based
#' on car/cdr's from a closure
#' @param q
#' @param level
#' @param tbl
#' @param parent
#'
#' @return
#' @export
#'
#' @examples
#' @export
# qtls_walk_carcdr <- function(q, level = 1, tbl = new.env() ,  parent = 0, order=1) {
# 	if (is.null(tbl$tbl)) {
# 		tbl$tbl <- tibble::tribble(~id, ~parent, ~atom)
# 		tbl$pass <- 1
# 	}
# 	if (!rlang::is_node(q)) {
# 		if (rlang::is_formula(q)) {
# 			e <- rlang::f_rhs(q)
# 		} else {
# 			e <- rlang::get_expr(q)
# 		}
# 		cdr <- rlang::node_cdr(e)
# 		car <- rlang::node_car(e)
# 		tbl$tbl <- dplyr::bind_rows(tbl$tbl, tibble::tibble(id = list(tbl$pass),
# 																												parent = list(parent),
# 																												atom=list(glue::glue("{car}:{order}"))))
# 		parent = tbl$pass
# 		for (index in 1:length(cdr)) {
# 			tbl$pass <- tbl$pass + 1
# 			if (rlang::is_lang(cdr[[index]])) {
# 				qtls_walk_carcdr(cdr[[index]], level + 1, tbl, parent, order=index)
#
# 			} else {
# 				tbl$tbl <- dplyr::bind_rows(tbl$tbl, tibble::tibble( id = list(tbl$pass),
# 																														 parent = list(parent),
# 																														 atom = list(
# 																														 	glue::glue("{cdr[[index]]}:{index}"))))
# 				tbl$pass <- tbl$pass + 1
# 			}
# 		}
# 	} else {
# 		print(glue::glue("end {q}"))
# 	}
# 	tbl$tbl
# }
#


#' Title
#' build a table of parent child relationship based
#' on car/cdr's from a quosure
#' @param q
#' @param level
#' @param tbl
#' @param parent
#'
#' @return
#' @export
#'
#' @examples
#' @export
qtls_walk_carcdr <-
	function(q,
					 tbl = new.env() ,
					 parent = 0,
					 order = 1) {
		print("start")
		if (is.null(tbl$tbl)) {
			tbl$tbl <- tibble::tribble(~ type, ~ id, ~ parent,
																 ~ atom, ~ position)
			# pass is incremented after each row is added to tbl
			tbl$pass <- 1
		}
		if (rlang::is_formula(q)) {
			e <- rlang::f_rhs(q)
		} else {
			e <- q
		}
		# get car/cdr, head/tail
		cdr <- rlang::node_cdr(e)
		car <- rlang::node_car(e)
		# length(car) == 1 is the typical car. It means it is a function
		# and the cdr is a list of its arguments
		if (length(car) == 1) {
			print("car one...")
			# add the function to the table
			tbl$tbl <-
				dplyr::bind_rows( tbl$tbl,
												 tibble::tibble(
												 	type = c("car"),
												 	id = c(tbl$pass),
												 	parent = c(parent),
												 	atom = c(glue::glue("{rlang::get_expr(car)}:{order}")),
												 	position = c(1)
												 ))
			print(glue::glue("pass1 {tbl$pass}"))
			# this function is the parent of the cdr that follows
			parent <- tbl$pass
			# increment pass because row was added to table
			tbl$pass <- tbl$pass + 1
		} else {
			# if we get to here it means the car has more than one item in its list
			print("car multiple...")

			tbl$tbl <-
				dplyr::bind_rows( tbl$tbl,
												 tibble::tibble(
												 	type = c("car"),
												 	id = c(tbl$pass),
												 	parent = c(parent),
												 	atom = c(glue::glue("{rlang::get_expr(car)}:{order}")),
												 	position = c(1)
												 ))
			print(glue::glue("pass1 {tbl$pass}"))
			parent <- tbl$pass
			tbl$pass <- tbl$pass + 1

			for (index in 2:length(car)) {
				if (rlang::is_lang(car[[index]])) {
					qtls_walk_carcdr(car[[index]], tbl, parent, order = index)
				} else {
					print("car mul2 ...")
					tbl$tbl <-
						dplyr::bind_rows( tbl$tbl,
														 tibble::tibble(
														 	type = c("car"),
														 	id = c(tbl$pass),
														 	parent = c(parent),
														 	atom = c(glue::glue("{rlang::get_expr(car[[index]]}:{index - 1}")),
														 	position = c(index - 1)
														 ))
					print(glue::glue("pass1 {tbl$pass}"))
					#parent <- tbl$pass
					tbl$pass <- tbl$pass + 1
				}
			}
			#parent <- tbl$pass
			print("car...")

			tbl$tbl <-
				dplyr::bind_rows( tbl$tbl,
													tibble::tibble(
														type = c("car"),
														id = c(tbl$pass),
												 	parent = c(parent),
												 	atom = c(stringr_str_c(":{length(car)}")),
												 	position = c(0)
												 ))
			print(glue::glue("pass1 {tbl$pass}"))
			parent <- tbl$pass
			tbl$pass <- tbl$pass + 1

		}
		if (length(cdr) > 0) {
			for (index in 1:length(cdr)) {
				if (rlang::is_lang(cdr[[index]])) {
					qtls_walk_carcdr(cdr[[index]], tbl, parent, order = index)
				} else {
					print("cdr...")

					tbl$tbl <-
						dplyr::bind_rows(tbl$tbl,
														 tibble::tibble(
														 	type = c("cdr"),
														 	id = c(tbl$pass),
														 	parent = c(parent),
														 	atom = c(glue::glue("{rlang::get_expr(cdr[[index]])}:{index}")),
														 	position = c(index)

														 ))
					print(glue::glue("pass1 {tbl$pass}"))
					tbl$pass <- tbl$pass + 1
				}
			}
		}
		tbl$tbl
	}

#' Title
#' Builds a graph object using DiagrammeR for a tbl
#' that list obj id's and their parent.
#'
#' Table at least an id, parent, and atom column
#'
#' The id column contains unique numeric identities for each row.
#'
#' The parent column contains the id of the node of that row's parent.
#'
#' The atom column contains text that tag's the nodes of the tree that
#' DiagrameR will use.
#'
#' @param tbl
#'
#' @param root_node
#'
#' @return
#' @export
#'
#' @examples
#'
qtls_plot_parent_child <- function(tbl, root_node = 1) {
	graph <- DiagrammeR::create_graph(directed = TRUE)
	context <- new.env()
	context$graph <- graph
	context$tbl <- tbl
	build_graph <-
		function(context,
						 current_id = root_node,
						 parent_graph_id = 0) {
			current_row <- dplyr::filter(context$tbl, id == current_id)
			atom <- current_row$atom
			# add a node for the row we are currently working on
			child_rows <- dplyr::filter(context$tbl, parent == current_id)
			color = "blue"
			if(nrow(child_rows) == 0) {
				color = "green"
			}
			context$graph <-
				DiagrammeR::add_node(context$graph, label = atom, color = color)
			# hang onto the id of the DiagrammeR node we just created in case
			# the current row has children
			current_graph_id <- context$graph$last_node
			# root node does not have parent
			if (parent_graph_id != 0) {
				# add visual connection from parent to child
				context$graph <-
					DiagrammeR::add_edge(context$graph, to =
															 	current_graph_id, from = parent_graph_id)
			}
			# if current row has children process those next
			if (nrow(child_rows) != 0) {
				for (index in 1:nrow(child_rows)) {
					build_graph(context,
											as.numeric(child_rows[index,]$id),
											current_graph_id)
				}
			}
		}
	build_graph(context)
	context$graph

}

qtls_graph_car_cdr_for_expression <- function(expr) {
	q <- rlang::enquo(expr)
	print(rlang::f_rhs(q))
	t <- qtls_walk_carcdr(q)
	qtls_plot_parent_child(t)
}

#' Title
#' Walks tree by following head/tail and produces tribble
#' @param quosure
#' @param parent_id
#' @param order
#' @param context
#'
#' @return
#' @export
#'
#' @examples
qtls_walk_table <- function(quosure,
														parent_id = 0,
														order = 1,
														context = NA) {
	if (is.na(context)) {
		context = new.env()
		context$pass = 1
		context$qtbl <-
			tibble::tribble(~id, ~parent, ~atom)
		class(context$qtbl) <- c("qtls_outline", class(context$qtbl))
	}
	expr <- rlang::get_expr(quosure)
	id <- context$pass
	if (!rlang::is_lang(expr)) {
		context$qtbl <- dplyr::bind_rows(
			context$qtbl,
			data.frame(
				parent = parent_id,
				id = id,
				atom = glue::glue("{expr}:{order}"),
				stringsAsFactors = FALSE
			)
		)
		context$pass <- context$pass + 1
	} else {
		head <- rlang::lang_head(quosure)
		context$qtbl <- dplyr::bind_rows(
			context$qtbl,
			data.frame(
				parent = parent_id,
				id = id,
				atom = glue::glue("{head}:{order}"),
				stringsAsFactors = FALSE
			)
		)
		context$pass <- context$pass + 1
		tail <- rlang::lang_tail(quosure)
		for (index in 1:length(tail)) {
			item <- tail[[index]]
			qtls_walk_table(rlang::quo(!!item),
											id ,
											index,
											context)
		}
	}
	context$qtbl
}

#' Apply to quosue tree
#' Applies a functions to car/cdr elements in quosure tree
#' on car/cdr's from a quosure
#' @param q
#' @param level
#' @param tbl
#' @param parent
#'
#' @return
#' @export
#'
#' @examples
#' @export
qtls_apply_carcdr <- function(q,
															car_solo_apply = NA,
															car_multi_apply = NA,
															cdr_apply = NA,
															context = NULL) {
	print("=====================")
	if (is.null(context)) {
		print("make env")
		context <- new.env()
		if (!is.na(car_solo_apply)) {
			context$car_solo_apply <- car_solo_apply
		} else {
			context$car_solo_apply <- function(car) {
			}
		}
		if (!is.na(car_multi_apply)) {
			context$car_multi_apply <- car_multi_apply
		} else {
			context$car_multi_apply <- function(car) {
			}
		}
		if (!is.na(cdr_apply)) {
			context$cdr_apply <- cdr_apply
		} else {
			context$cdr_apply <- function(car, cdr) {
			}
		}
	}
	if (rlang::is_formula(q)) {
		e <- rlang::f_rhs(q)
	} else {
		e <- q
	}
	print(glue::glue("start {e}"))
	# get car/cdr, head/tail
	cdr <- rlang::node_cdr(e)
	car <- rlang::node_car(e)
	# length(car) == 1 is the typical car. It means it is a function
	# and the cdr is a list of its arguments
	if (length(car) == 1) {
		# apply_solo_car
		context$car_solo_apply(car)
	} else {
		# apply_multi_car
		context$car_multi_apply(car)
	}
	# apply_cdr
	if (!is.null(cdr)) {
		context$cdr_apply(car, cdr)
		for (index in 1:length(cdr)) {
			next_cdr <- cdr[[index]]
			if (rlang::is_lang(next_cdr)) {
				qtls_apply_carcdr(next_cdr, context = context)
			}
		}
	}
}
