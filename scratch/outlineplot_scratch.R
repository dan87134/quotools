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

walk_tbl_depth_first <-
	function(tbl,
					 .f = function(id, depth) {
					 	print(glue::glue("id:{id} depth{depth}"))
					 },
					 current_id = NULL,
					 depth = 0,
) {
		if (is.null(current_id)) {
			current_id <- dplyr::filter(tbl, parent == 0)$id
		}
		#print(current_id)
		f(current_id, depth)
		children <- dplyr::filter(tbl, parent == current_id)
		purrr::walk(children$id, function(id, depth) {
			walk_tbl_depth_first(tbl, id, depth = depth + 1)
		}, depth)
	}

row1 <- tbl[1,]
row1$id

walk_tbl_depth_first(tbl)

