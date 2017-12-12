q <- rlang::quo(a * b + c * d)

qtls_apply_carcdr(q, cdr_apply =
										function(car, cdr) { print(glue::glue("cdr_apply {car}:{cdr}"))},
									car_solo_apply = function(car) {print(glue::glue("car_solo_apply {car}"))},
									car_multi_apply = function(car) {print(glue::glue("car_multi_apply {car}"))})

