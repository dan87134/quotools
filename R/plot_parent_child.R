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
qtls_plot_parent_childx <- function(tbl, root_node = 1) {
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
