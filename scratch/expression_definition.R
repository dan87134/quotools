# support for definition of an expression

q <- rlang::quo(a * b + c * d)
typeof(q)
qtls_what_is_it(q)
qcar <- rlang::node_car(q)
qcdr <- rlang::node_cdr(q)
qtls_what_is_it(qcdr)

qcar
length(qcdr)
qcdr[[1]]
qrhs <- rlang::f_rhs(q)
typeof(qrhs)


qtls_what_is_it(qrhs)
rhs <- rlang::f_rhs(q)
qtls_what_is_it(rhs)
typeof(rhs)

car <- rlang::node_car(rhs)
length(car)
cdr <- rlang::node_cdr(rhs)
length(cdr)


qf <- rlang::quo(function(x)
	{x }(6) )
qftab <- qtls_make_rlang_table(qf)
g <- qtls_plot_model(qftab)
DiagrammeR::render_graph(g, layout="tree")
qfrhs <- rlang::f_rhs(qf)
qfcar <- rlang::node_car(qfrhs)
length(qfcar)
qfcdr <- rlang::node_cdr(qfrhs)
length(qfcdr)

qf <- rlang::quo(function(x){x}(6) )
qfcar <- rlang::node_car(qfrhs)
n2 <- qfcdr[[2]]
n2car <- rlang::node_car(n2)
length(n2)

qtls_what_is_it(n2)

n2car <- rlang::node_car(n2)
length(n2)
qtls_what_is_it(n1)
