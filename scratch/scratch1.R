# scratch

q <- rlang::quo(a * b + c * d)
l <- qtls_walk(q)
typeof(l)
l[[1]]
str(l)
as.integer("10")
