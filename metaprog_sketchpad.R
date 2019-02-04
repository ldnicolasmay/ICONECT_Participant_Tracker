
library(dplyr)

# Goal create a list that looks like this ...

target <- list(
  a = c("foo"),
  b = c("bar"),
  c1 = c("baz"),
  c2 = c("baz"),
  c3 = c("baz")
)
target

# ... from component pieces ...

a_foo <- list(a = c("foo"))
b_bar <- list(b = c("bar"))
c_baz <- list(c1 = c("baz"), 
              c2 = c("baz"),
              c3 = c("baz"))
a_foo
b_bar
c_baz
c(a_foo, b_bar, c_baz)

identical(target, c(a_foo, b_bar, c_baz))

# ... the last of which is generated via metaprogramming 

# So how do we metaprogram the last list?

# some function that passes "c", 1:3, and c("baz")

# some_fxn <- function(prefix, suffix_vctr, value) {
#   return_list <- list()
#   
#   return_list <- c(return_list, list(c1 = value))
#   return_list <- c(return_list, list(c2 = value))
#   return_list <- c(return_list, list(c3 = value))
#   
#   return_list
# }

some_fxn <- function(prefix = "", infix_vctr, suffix = "", value) {
  
  return_list <- list()
  
  for (i in seq_along(infix_vctr)) {
    return_list <- 
      c(return_list,
        list2(!!sym(paste0(prefix, infix_vctr[i], suffix)) := value))
  }
  
  return_list
}

# some_fxn(c, 1:3, c("baz", "qux"))
some_fxn("c", 1:3, value = c("baz", "qux"))
some_fxn("vctr_", letters[1:10], value = c("foo", "bar", "baz"))
some_fxn("thing_", LETTERS[1:5], "_blah", value = c("qux"))

# some_fxn_r <- function(vctr, value) {
#   # base case
#   if (length(vctr) == 1L) {
#     return(c(paste0(vctr, value)))
#   } 
#   c(paste0(vctr[1], value), some_fxn_r(vctr[2:length(vctr)], value))
# }
# some_fxn_r(1:4, "r")

some_fxn_r <- function(prefix = "", infix_vctr, suffix = "", value) {
  # base case
  if (length(infix_vctr) == 1L) {
    return(list2(!!sym(paste0(prefix, infix_vctr, suffix)) := value))
  }
  
  c(list2(!!sym(paste0(prefix, infix_vctr[1], suffix)) := value),
    some_fxn_r(prefix, infix_vctr[2:length(infix_vctr)], suffix, value))
}
some_fxn_r("c", 1:3, value = c("baz", "qux"))
some_fxn_r(infix_vctr = letters[1:10], 
           suffix = "_vctr", 
           value = c("foo", "bar", "baz"))
some_fxn_r(prefix = "thing_", 
           infix_vctr = LETTERS[1:5], 
           suffix = "_blah", 
           value = c("qux"))




library(rlang)
library(lobstr)


expr(mean(x, na.rm = TRUE))
expr(10 + 100 + 1000)

capture_it <- function(x) { expr(x) }
capture_it(a + b + c)

capture_it <- function(x) { enexpr(x) }
capture_it(a + b + c)

# capture_it <- function(x) { 
#   # a = 1; b = 2; c = 3
#   eval_tidy(enexpr(x))
# }
# a <- 1; b <- 2; c <- 3;
# capture_it(a + b + c)

f <- expr(f(x = 1, y = 2))
f
f$z <- 3
f
f[[2]] <- NULL
f

ast(f(a, "b"))

ast(f1(f2(a, b), f3(1, f4(2))))

ast(list(a = c("foo")))
ast(list(a = c("foo"), b = c("bar")))

c(list(a = c("foo")), 
  list(b = c("bar")), 
  list(c1 = c("baz"), 
       c2 = c("baz")))
ast(c(list(a = c("foo")), 
      list(b = c("bar")), 
      list(c1 = c("baz"), 
           c2 = c("baz"))))

(1 + 2 * 3)
ast(1 + 2 * 3)

call2("f", 1, 2, 3)
call2("+", 1, call2("*", 2, 3))

cv <- function(var) {
  var <- enexpr(var)
  expr(sd(!!var) / mean(!!var))
}
cv(x)
cv(x + y)



















