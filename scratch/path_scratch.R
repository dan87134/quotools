# path scratch
a <- 1
a[[1]]
a[2] <- 3
a[5] <- 5

`+`(9, 1, 2)

pryr::address(a)

nm <- 1:10
# get it address
pryr::address(nm)
# now change a location in it
nm[[2]] <- -2
pryr::address(nm)




a <- 9
a <- 10

q <- rlang::quo(a + b + c)
obj1 <- q[[2]]
obj2 <- obj1[[1]]
obj2


q <- rlang::quo(a + b + c)
pryr::address(q)

node4 <- q[c(2,1)]
node4
q[[c(1,1)]]

is.leaf(node4)
dnode4 <- as.dendrogram(node4)
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
