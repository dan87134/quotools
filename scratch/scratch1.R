# scratch

q <- rlang::quo(a * b + c * d)
qtls_what_is_it(q)


tf <- function(arg) {
	q <- rlang::enquo(arg)
	rhs <- rlang::f_rhs(q)
	typeof(rhs)

}

q <- rlang::quo(a)
rlang::is_lang(q)
cdr <- rlang::node_cdr(q)
rlang::is_lang(cdr)


glue::glue("{stringr::str_c(1:2, collapse='')}")

q <- rlang::quo(a + b)
rhs <- rlang::f_rhs(q)
rlang::get_expr(rhs)
length(rhs)

tibble::tibble(a = 1:2, b = c(1))

tf(quo(a + b))

tf(a + b)


qtls_apply_carcdr(q, cdr_apply = function(car, cdr) { print(glue::glue("cdr_apply {car}:{cdr}"))})


rlang::is_lang

q <- rlang::quo(a + b * c + d)
expr <- rlang::get_expr(q)

qs <- rlang::lang_standardise(expr)
qs

l <- qtls_walk_outline(q)
l1 <- qtls_walk(q, qtls_outline_context())

rlang::eval_tidy

rlang:::overscope_eval_next

lobstr::ast(f(x)(y))

f <- function(a) {function(b) { -b*a}}

f(3)(4)


lobstr::ast(a + (b * c + fn(x, y, f2(a)) + (d + 2.01/3)))

str(a + (b * c) + d)

g <- qtls_quo_tree(rlang::quo(a + b * c + d))
DiagrammeR::render_graph(g, layout = "tree")


g <- qtls_quo_tree(rlang::quo(a * b + c * d))
DiagrammeR::render_graph(g, layout = "tree")

g <- qtls_expr_tree(a * b + c * d)
DiagrammeR::render_graph(g, layout = "tree")

g <- qtls_quo_tree(rlang::quo(a * fn(b, c, 1:3) + c * (d + w)))
DiagrammeR::render_graph(g, layout="tree")

q <- rlang::quo(a + b * c + d)
e <- rlang::get_expr(q)
i <- rlang::expr_interp(e)


suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(DiagrammeR))
g <- qtls_node_tree(rlang::quo(a * fn(b, c, 1:3) + c * (d + w)))
DiagrammeR::render_graph(g, layout = "tree" )


q2 <- rlang::quo(a * f1(b + c) + 34 + d)
l1 <- qtls_walk(q2, qtls_outline_context())

g1 <- DiagrammeR::create_graph(
	directed = FALSE
)

g1 <- DiagrammeR::add_node(g1)

DiagrammeR::render_graph(g1)

aaa <- `gggg`
fff <- function(expr) {
	q <- rlang::enquo(expr)
	h <- rlang::lang_head(q)
	t <- rlang::expr_label(h)
	typeof(t)
}
fff(a + b)


typeof(l)
l[[1]]
str(l)
as.integer("10")



suppressPackageStartupMessages(library(DiagrammeR))
a_graph <-
	create_graph(
		directed = FALSE
	) %>%
	add_node(label = "+") %>%
	add_node(label = "b") %>%
	add_node(label = "c") %>%
	add_edge(
		from = 1,
		to = 2,
		rel = "interacted_with"  ) %>%
	add_edge(from = 1, to = 3)
set_node_attrs(a_graph, node_attr = color, values = "red")
a_graph %>% render_graph(layout = "tree")

remove.packages(DiagrammeR)


rm(list = ls())

q <- rlang::quo(a + b * c + d)
g <- qtls_quo_tree(q)
DiagrammeR::render_graph(g, layout="tree")

tx <- tibble::tibble(
	c = 1:3,
	d = c("asdf", "asdfrr", "rwea")
)

tx[1,]$c
tx[1,"c"]



q <- rlang::quo(a + (b * c) + d)
t1 <- qtls_walk_outline(q)

tail <- rlang::lang_tail(q)
head <- rlang::lang_head(q)

qtls_what_is_it(head)
qtls_what_is_it(tail)

g <- qtls_expr_tree(f(x)(y))
DiagrammeR::render_graph(g, layout="tree")

g <- qtls_graph_car_cdr_for_expression(f(x)(y))
DiagrammeR::render_graph(g)

q <- rlang::quo(f(x)(y))
t3 <- qtls_walk_carcdr(q)

q <- rlang::quo(f(x)(y))
t4 <- qtls_walk_carcdr(q)

q <- rlang::quo(f(x)())
t4 <- qtls_walk_carcdr(q)



lobstr::ast(f(x)())

lobstr::ast(h(f(x)(y)))



head2 <- rlang::lang_head(head)
tail2 <- rlang::lang_tail(head)

q <- rlang::quo(a + (b * f1(c, x) + y))

g <- qtls_expr_tree(a + (b * f1(c, x) + y * 2) + d)
DiagrammeR::render_graph(g, layout = "tree")


q <- rlang::quo(a + (b * f1(c, x) + y * 2 / uu + q * b + 0) + d)
q <- rlang::quo(a + (b * f1(c, x) + y))
q <- rlang::quo(a * b + c * d)
t <- qtls_walk_carcdr(q)
g <- qtls_plot_parent_child(t)
DiagrammeR::render_graph(g, layout = "tree")

q <- rlang::quo(f(a,z)(b,c))
q <- rlang::quo(f(a,z)(b,c) + a)
q <- rlang::quo(x * f(a,z)(b,c) + a)
q <- rlang::quo(x + f(a,z)(b,c) * a)
	q <- rlang::quo(a * b + c * d /f3(f2(a)(), x))
	t <- qtls_walk_carcdr(q)
	g <- qtls_plot_parent_child(t)
	DiagrammeR::render_graph(g, layout = "tree")

	q <- rlang::quo(foo <- a * b + c * d /f3(f2(a)(), x))
	q <- rlang::quo(function(a,b) { a + b})

lobstr::ast(function(a,b) { a + b})

lobstr::ast(a * b + c * d /f3(f2(a)(), x))

lobstr::ast(foo <- a * b + c * d /f3(f2(a)(), x))

q1 <- rlang::quo(a * b + c * d)
t1 <- qtls_walk_carcdr(q1)
row <- t1[2,]

car <- t1[2,]$car
cdr <- t1[2,]$cdr
position <- t1[2,]$position
rlang::mut_node_car(car, rlang::node(rlang::sym("-"), cdr))



q1
qtls_what_is_it(q)

rhs <- rlang::f_rhs(q)

car <- rlang::node_car(rhs)
cdr <- rlang::node_cdr(rhs)

length(car)

c

qtls_what_is_it(q)


rhs <- rlang::f_rhs(q)
head <- rlang::lang_head(rhs)
tail <- rlang::lang_tail(rhs)

length(head)


car <- rlang::node_car(q)
cdr <- rlang::node_cdr(q)

length(car)


head

g <- qtls_graph_car_cdr_for_expression(a * b + c * d)


q <- rlang::quo(a * b +  c * d)
t5 <- qtls_walk_carcdr(q)
g <- qtls_plot_parent_child(t5)
DiagrammeR::render_graph(g, layout = "tree")



g <- qtls_graph_car_cdr_for_expression(a + b / c + d)
DiagrammeR::render_graph(g, layout = "tree")

g <- qtls_graph_car_cdr_for_expression(a + (b * f1(c, x, x1, x, x2) + y * 2 / uu + q * b + 0) + d)
DiagrammeR::render_graph(g, layout = "tree")


g <- qtls_graph_car_cdr_for_expression(f(a)(b))
DiagrammeR::render_graph(g, layout = "tree")

q <- rlang::quo(a + b * c + d)
t <- qtls_walk_table(q)

DiagrammeR::render_graph(g)

c <- dplyr::filter(t, parent == 22220)

f <- ~ d + b
f

1:0

qa <- rlang::quo(a)
q1 <- rlang::quo(1)
ex <- rlang::get_expr(qa)
qtls_what_is_it(ex)

rlang::get_expr(q1)
rlang::lang_head(qa)
rlang::is_lang

qab <- rlang::quo(a + b)
qtls_what_is_it(qab)
ex1 <- rlang::get_expr(qab)
qtls_what_is_it(ex1)

h <- rlang::lang_head(qab)
rlang::get_expr(h)


typeof(qa)
qtls_what_is_it(qa)
qtls_what_is_it(q1)
rlang::lang_tail(qa)
rlang::is_lang


tibble::tr
nrow(t)
t[1,]$atom

gr <- DiagrammeR::create_graph(directed = TRUE)
gr$last_node
gr <- DiagrammeR::add_node(gr,
										 label = "atom")

q1 <- rlang::quo(a + b)
t1 <- qtls_walk_carcdr(q1)

car <- t1[[1,"car"]]
cdr <- t1[[1,"cdr"]]
str(car)
typeof(car)


lobstr::ast(car)

cdr <- t1[1,]$cdr
str(cdr)


rlang::is_symbol(car)
qtls_what_is_it(car)

0:0



rlang::mut_node_car(car, rlang::sym("-"))


q3 <- rlang::quo(a + b)
## this works ????? why????
change_to_minus <- function(q) {
	#q2 <- q[[2]]
	q2 <- rlang::f_rhs(q)
	car <- rlang::node_car(q2)
	cdr <- rlang::node_cdr(q2)
	mod_car <- function(car) {
		if (rlang::is_symbol(car)) {
			if (car == rlang::sym("+")) {
				rlang::mut_node_car(q2, rlang::sym("-"))
			}
		}
	}
	mod_car(car)
}
change_to_minus(q3)

q3 <- rlang::quo(a + b)
rhs <- rlang::f_rhs(q3)
car <- rlang::node_car(rhs)
cc <- c(car = car)

identical(car, cc$car)
pryr::address(car)
x <- cc$car
pryr::address(x)
ec <- new.env()
ec$car <- car
rlang::mut_node_car(car, rlang::sym("-"))


ttt <- tibble::tribble(
	~car,
	car
)
ttt
cc <- ttt[[1, "car"]]

rlang::mut_node_car(cc, rlang::sym("-"))

change_to_minus(q3)
a <- 2
b <- 3

rlang::eval_tidy(q3)

suppressPackageStartupMessages(library(tidyverse))
df <- data.frame(col1 = c("a", "b", "c"), col2 = c("d", "a", "e"),
								 stringsAsFactors = F)
df <- dplyr::mutate_if(df,
								is.character,
								stringr::str_replace_all, pattern = "a", replacement = "x")
df


df <- data.frame(col1 = c("a", "b", "c"), col2 = c("d", "a", "e"), stringsAsFactors = F)

df <-stringr::str_replace_all(as.list(df), "a", "x")

class(df)
typeof(df)

