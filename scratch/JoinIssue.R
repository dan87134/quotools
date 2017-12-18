# walk2 quosure issue
suppressPackageStartupMessages(library(rlang))

# make a quosure
qwalk <- rlang::quo(1 + 3)
# the rhs is an expression
# remember that 1 + 3 is 4
expr <- rlang::f_rhs(qwalk)
# put the expression into a table
tbl1 <- tibble::tribble (
	~ id, ~ expr,
	1, expr
)
#
# notice that the type of the expr column is langauge
tbl1
#
# use walk2 to walk through table values
purrr::walk2(tbl1$expr, tbl1$id, function(expr, id) {
	# just print out value of expr
	# notice that expr is 4 and its type is double
	# this means the expression has been evaluated
	print(expr)
	print(typeof(expr))
})
#
# use walk to to walk though a table value
purrr::walk(tbl1$expr, function(expr) {
	# notice that the value of expr 3 + 1 is the inital expression
	# and the type is langauge, i.e. the type of an expression
	print(expr)
	print(typeof(expr))
	})

