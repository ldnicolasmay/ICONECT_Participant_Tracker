
library(dplyr)
library(rlang)

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

microbenchmark::microbenchmark(
  blah_f <- some_fxn(prefix = "prefix_",
                     infix_vctr = 1:2000,
                     suffix = "_suffix",
                     value = 1:100),
  blah_r <- some_fxn_r(prefix = "prefix_",
                       infix_vctr = 1:2000,
                       suffix = "_suffix",
                       value = 1:100),
  times = 50
)

paste0("0", 1:20)
some_fxn <- function(vctr, length = max(nchar(vctr)), filler = "") {
  print(length)
  paste0("0", vctr)
}
some_fxn(1:20)
some_fxn(1:20, 3L)
some_fxn(1:20, 4L, "0")

strrep("0", max(nchar(1:20))-nchar(1:20))
paste0(strrep("0", max(nchar(1:20))-nchar(1:20)), 1:20)

# paste0("UM", 
#        strrep("0", 8L - nchar(1:20)), 
#        "UDS_ID")
strrep("ABC", 2)
strrep(c("A", "B", "C"), 1:3)
strrep("c", 1:5)

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

paste0(
  strrep("0", max(nchar(week_vctr))-nchar(week_vctr)), 
  week_vctr
)

build_distilled_row <- function(uniq_id, df) {
  
  ts_sub_id = uniq_id
  
  week_vctr = 1:10
  week_vctr = 
    paste0(
      strrep("0", max(nchar(week_vctr))-nchar(week_vctr)), 
      week_vctr
    )
  
  # Left Hand SideS
  lhss = paste0("w", week_vctr, "_tel_date")
  syms_lhss = syms(lhss)
  # print(eval_tidy(syms(lhss)))
  
  # Right Hand Side ArgumentS
  rhss_args = paste0("w", week_vctr, "_tel_arm_1")
  syms_rhss_args = syms(rhss_args)
  # print(eval_tidy(syms(rhss_args)))
  
  # for (i in week_vctr) {
  #   cat(paste0("w", i, "tel_date", 
  #              " = ",
  #              "date_fields_complete(",
  #              "uniq_id", ", ",
  #              "\"w", i, "_tel_arm_1\"", ", ",
  #              "df",
  #              ")", 
  #              "\n"))
  # }
  
  # for (i in week_vctr) {
  #   
  # }
  
  list2(
    !!syms_lhss := date_fields_complete(ts_sub_id, "w01_tel_arm_1", df)
  )
}

build_distilled_row(uniq_ids[1], data_slct_fltr_cln)

















