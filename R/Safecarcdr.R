# safe car/cdr access


qtls_mut_node_car <- function(lang, obj) {
		if (rlang::is_lang(lang)) {
			car <- rlang::node_car(lang)
			if (typeof(car) == typeof(obj)) {
				rlang::mut_node_car(lang, obj)
			}
		}
}


