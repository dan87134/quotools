# for walking AST

next_sibling <- function(level) {
	re <- "\\d+$"
	s <- as.integer(stringr::str_extract(level, re)) + 1L;
	stringr::str_replace(level, re, as.character(s))
}

next_level <- function(level) {
	stringr::str_c(level, ".1")
}

qtls_walk <- function(quosure, qtbl = tibble::tibble(), level = "1", pass = 1) {
	message("r1")
	head <- rlang::lang_head(quosure)
	tail <- rlang::lang_tail(quosure)
	qtbl <- dplyr::bind_rows(qtbl, data.frame(quosure = rlang::expr_label(head), id = level, stringsAsFactors = FALSE))
	pass <- pass + 1
	message(level)
	slevel <- stringr::str_c(level, ".1")
	for (index in 1:length(tail)) {
		item <- tail[[index]]
		if (rlang::is_lang(item)) {
		  qtbl <- qtls_walk(rlang::quo(!! item), qtbl, slevel)
		} else {
			qtbl <- dplyr::bind_rows(qtbl, data.frame(quosure = rlang::expr_label(item), id = slevel, stringsAsFactors = FALSE))
		}
		slevel <- next_sibling(slevel)
	}
	qtbl
}
