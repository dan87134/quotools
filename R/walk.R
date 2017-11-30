# for walking AST

next_sibling <- function(level) {
	re <- "\\d+$"
	s <- as.integer(stringr::str_extract(level, re)) + 1L;
	stringr::str_replace(level, re, as.character(s))
}

next_level <- function(level) {
	stringr::str_c(level, ".1")
}

qtls_walk_outline <- function(quosure, qtbl = tibble::tibble(), slevel = "1") {
	class(qtbl) <- c("qtls_outline", class(qtbl))
	head <- rlang::lang_head(quosure)
	tail <- rlang::lang_tail(quosure)
	qtbl <- dplyr::bind_rows(qtbl, data.frame(outline = slevel,
																						expr = rlang::expr_label(head),
																						stringsAsFactors = FALSE))
	slevel <- stringr::str_c(slevel, ".1")
	for (index in 1:length(tail)) {
		item <- tail[[index]]
		if (rlang::is_lang(item)) {
		  qtbl <- qtls_walk_outline(rlang::quo(!! item), qtbl, slevel)
		} else {
			qtbl <- dplyr::bind_rows(qtbl, data.frame( outline = slevel,
															 expr = rlang::expr_label(item),
															 stringsAsFactors = FALSE))
		}
		slevel <- next_sibling(slevel)
	}
	qtbl
}




qtls_outline_context <- function() {

	outline <- new.env()
	outline$next_sibling <- function(level) {
		re <- "\\d+$"
		s <- as.integer(stringr::str_extract(level, re)) + 1L;
		stringr::str_replace(level, re, as.character(s))
	}
	outline$tbl <- tibble::tibble()
	class(outline$tbl) <- c("qtls_outline", "qtls_tbl", class(outline$tbl))
	outline$head_processing <- function(head) {outline$tbl <-
		dplyr::bind_rows(outline$tbl, data.frame(outline = outline$slevel,
																			expr = rlang::expr_label(head),
																			stringsAsFactors = FALSE))
		outline$slevel <- stringr::str_c(outline$slevel, ".1")
	}
	outline$expr_processing <- function(expr) {
		outline$tbl <- dplyr::bind_rows(outline$tbl, data.frame( outline = outline$slevel,
																							 expr = rlang::expr_label(expr),
																							 stringsAsFactors = FALSE))}
	outline$lang_processing <- function(item) {
		save <- outline$slevel
		outline$tbl <- qtls_walk(rlang::quo(!! item), outline)
		outline$slevel <- save
	}
	outline$post_loop <- function() {
	}
	outline$loop_start <- function() {

	}
	outline$loop_end <- function() {
		outline$slevel <-  next_sibling(outline$slevel)
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
			context$lang_processing(item)
		} else {
			context$expr_processing(item)
		}
		context$loop_end()
	}
	context$tbl
}
