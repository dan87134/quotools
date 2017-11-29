# for walking AST

next_sibling <- function(level) {
	re <- "\\d+$"
	s <- as.integer(stringr::str_extract(level, re)) + 1L;
	stringr::str_replace(level, re, as.character(s))
}

next_level <- function(level) {
	stringr::str_c(level, ".1")
}

qtls_walk <- function(quosure, qlist = list(), level = "1", pass = 1) {
	message("r1")
	head <- rlang::lang_head(quosure)
	tail <- rlang::lang_tail(quosure)
	qlist <- c(qlist, c(head, level))
	#qlist[pass] <- c(qlist, c(head, level))
	pass <- pass + 1
	message(level)
	slevel <- stringr::str_c(level, ".1")
	for (index in 1:length(tail)) {
		item <- tail[[index]]
		if (rlang::is_lang(item)) {
		  qlist <- qtls_walk(rlang::quo(!! item), qlist, slevel)
		} else {
			#qlist[pass] <- c(item, slevel)
			qlist <- c(qlist, c(item, slevel))
		}
		slevel <- next_sibling(slevel)
	}
	qlist
}
