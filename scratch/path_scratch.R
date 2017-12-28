# path scratch
a <- 1
a[[1]]
a[2] <- 3
a[5] <- 5


pryr::address(a)

q <- rlang::quo(a + b + c)
pryr::address(q)

q2 <- q

q[[c(2,2)]] <- rlang::sym("aa")

q[[c(2,4)]] <- 90

rlang::eval_tidy(q)
 tbl <- qtls_make_rlang_model(q)

tbl <- qtls_make_rlang_model(q)

pryr::address(q)

q <- rlang::quo(1 + 2)
q[[c(2,1)]] <- NULL
q[[c(2,2)]] <- q[[c(2,1)]]
q[[c(2,1)]] <- 2
q <- rlang::quo(NULL)


dplyr::select(tbl, id, path)


rlang::eval_tidy(q)

car <- rlang::node_car(q[[c(2,1)]])

qp<- rlang::sym("-")
qp[[1]]


q[[c(2,1)]] <- rlang::sym("-")
rlang::node_car(qp)


q
