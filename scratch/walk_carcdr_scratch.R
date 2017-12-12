# walk_carcdr scratch
q <- rlang::quo(a * b + c * d)
ast_tbl1 <- qtls_walk_carcdr(q)
q
