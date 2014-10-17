library(XML)
library(stringr)

theurl <- "http://hyperpolyglot.org/scripting"
tables <- readHTMLTable(theurl, header=FALSE)
names(tables) <- c(1,2,3,4,5,6,7,8,9)
ptable <- tables$"1"

# 1行目が空行でもなく、２〜５行目がすべて空行でないものの真理値ベクトル
is.valid.row.vec <- !apply(is.na(as.matrix(ptable[,2:5])),c(1),all) & !(as.vector(ptable[,1]) == "")
# その行を取り出す
ptable2 <- ptable[is.valid.row.vec,]

findCategory <- function(tb, query){
  ptable3 <- tb[,2:5]
  flag.mat <- matrix(str_detect(as.matrix(ptable3), fixed(query)), nrow=nrow(ptable3), ncol=ncol(ptable3))
  as.character(tb[apply(flag.mat,c(1), any),1])
}

array_funcs = c(
  "inspect",
  "to_s",
  "to_a",
  "to_h",
  "to_ary",
  "frozen?",
  "==",
  "  eql?",
  "hash",
  "[]",
  "[]=",
  "  at",
  "fetch",
  "first",
  "last",
  "concat",
  "<<",
  "push",
  "pop",
  "shift",
  "unshift",
  "insert",
  "each",
  "each_index",
  "reverse_each",
  "length",
  "size",
  "empty?",
  "find_index",
  "index",
  "rindex",
  "join",
  "reverse",
  "reverse!",
  "rotate",
  "rotate!",
  "sort",
  "sort!",
  "sort_by!",
  "collect",
  "collect!",
  "map",
  "map!",
  "select",
  "select!",
  "keep_if",
  "values_at",
  "delete",
  "delete_at",
  "delete_if",
  "reject",
  "reject!",
  "zip",
  "transpose",
  "replace",
  "clear",
  "fill",
  "include?",
  "<=>",
  "slice",
  "slice!",
  "assoc",
  "rassoc",
  "+",
  "*",
  "-",
  "&",
  "|",
  "uniq",
  "uniq!",
  "compact",
  "compact!",
  "flatten",
  "flatten!",
  "count",
  "shuffle!",
  "shuffle",
  "sample",
  "cycle",
  "permutation",
  "combination",
  "repeated_permutation",
  "repeated_combination",
  "product",
  "take",
  "take_while",
  "drop",
  "drop_while",
  "bsearch",
  "pack"
)

for( i in seq(length(array_funcs))){
  print("========")
  print(array_funcs[i])
  print(findCategory(ptable2, array_funcs[i]))
}

# as.matrix(str_detect(as.matrix(ptable2[,2:5]), "none"), nrow=nrow(ptable2[,2:5]), ncol=ncol(ptable2[,2:5]))
findCategory(ptable2, "none")
