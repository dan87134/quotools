# the table of the "is" functions that test for a type
is_functions <- tibble::tribble(
	~ test,
	~ title,
	rlang::is_atomic, "atomic",
	rlang::is_bare_atomic, "bare_atomic",
	rlang::is_bare_bytes, "bare_bytes",
	rlang::is_bare_character, "bare_character",
	rlang::is_bare_double, "bare_double",
	rlang::is_bare_env, "bare_env",
	rlang::is_bare_formula, "bare_formula",
	rlang::is_bare_integer, "bare_integer",
	rlang::is_bare_integerish, "bare_integerish",
	rlang::is_bare_list, "bare_list",
	rlang::is_bare_logical, "bare_logical",
	rlang::is_bare_numeric, "bare_numeric",
	rlang::is_bare_raw, "bare_raw",
	rlang::is_bare_string, "bare_string",
	rlang::is_bare_vector, "bare_vector",
	rlang::is_binary_lang, "binary_lang",
	rlang::is_bytes, "bytes",
	rlang::is_call_stack, "call_stack",
	rlang::is_callable, "callable",
	rlang::is_character, "character",
	rlang::is_chr_na, "chr_na",
	rlang::is_closure, "closure",
	rlang::is_copyable, "copyable",
	rlang::is_cpl_na, "cpl_na",
	rlang::is_dbl_na, "dbl_na",
	rlang::is_definition, "definition",
	rlang::is_dictionary, "dictionary",
	rlang::is_dictionaryish, "dictionaryish",
	rlang::is_double, "double",
	rlang::is_empty, "empty",
	rlang::is_env, "env",
	rlang::is_eval_stack, "eval_stack",
	rlang::is_expr, "expr",
	rlang::is_false, "false",
	rlang::is_formula, "formula",
	rlang::is_formulaish, "formulaish",
	rlang::is_frame, "frame",
	rlang::is_function, "function",
	rlang::is_int_na, "int_na",
	rlang::is_integer, "integer",
	rlang::is_integerish, "integerish",
	rlang::is_lang, "language",
	rlang::is_lgl_na, "lgl_na",
	rlang::is_list, "list",
	rlang::is_logical, "logical",
	rlang::is_na, "na",
	rlang::is_named, "named",
	rlang::is_node, "node",
	rlang::is_null, "null",
	rlang::is_pairlist, "pairlist",
	rlang::is_primitive, "primitive",
	rlang::is_primitive_eager, "primitive_eager",
	rlang::is_primitive_lazy, "primitive_lazy",
	rlang::is_quosure, "quosure",
	rlang::is_quosureish, "quosureish",
	rlang::is_quosures, "quosures",
	rlang::is_raw, "raw",
	rlang::is_scalar_atomic, "scalar_atomic",
	rlang::is_scalar_bytes, "scalar_bytes",
	rlang::is_scalar_character, "scalar_character",
	rlang::is_scalar_double, "scalar_double",
	rlang::is_scalar_integer, "scalar_integer",
	rlang::is_scalar_integerish, "scalar_integerish",
	rlang::is_scalar_list, "scalar_list",
	rlang::is_scalar_logical, "scalar_logical",
	rlang::is_scalar_raw, "scalar_raw",
	rlang::is_scalar_vector, "scalar_vector",
	rlang::is_stack, "stack",
	rlang::is_string, "string",
	rlang::is_symbol, "symbol",
	rlang::is_symbolic, "symbolic",
	rlang::is_syntactic_literal, "syntactic_literal",
	rlang::is_true, "true",
	rlang::is_vector, "vector"
)

#' What does object mimic
#'
#' @param obj check obj to see what it mimics
#'
#' @return
#' @export
#'
#'@importFrom magrittr %>%
#' @examples
qtls_what_is_it <- function(obj) {
	dplyr::select(is_functions, title) %>%
		dplyr::filter(purrr::map_lgl(is_functions$test, ~ .(obj))) %>%
		purrr::flatten_chr()
}
