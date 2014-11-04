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
# 1行目 2列目以降は言語名が入っているのでテーブルの列名とする
colnames(ptable2) <- c("category", as.vector(as.matrix(ptable[1,2:ncol(ptable)])))

# 大カテゴリを取り出す
big.category.rows <- ptable[apply(is.na(as.matrix(ptable[,2:5])),c(1),all),]
big.categories <- as.vector(big.category.rows$V1)

# 小カテゴリを取り出す
small.cateogories <- as.vector(ptable2[,1])

findCategoryFromAll <- function(tb, query){
  # カテゴリ以外の部分を取り出す  
  ptable3 <- tb[,2:ncol(tb)]
  # クエリが存在するセルの論理値行列
  flag.mat <- matrix(str_detect(as.matrix(ptable3), fixed(query)), nrow=nrow(ptable3), ncol=ncol(ptable3))
  # 対応するカテゴリを取り出す
  as.character(tb[apply(flag.mat,c(1), any),1])
}

findCategory <- function(tb, lang, query){
  if(lang %in% colnames(ptable2)){
    # 言語の列を取り出す
    lang.col <- as.vector(tb[,colnames(tb) == lang])
    # クエリが存在するセルの論理値ベクトル
    flag.vec <- str_detect(lang.col, fixed(query))
    # 対応するカテゴリを取り出す
    return(as.character(tb[flag.vec,1]))
  }else{
    cat("lang is not found in tb")
    return(c())
  }
}

findCategoryFlag <- function(tb, lang, query){
  if(lang %in% colnames(ptable2)){
    # 言語の列を取り出す
    lang.col <- as.vector(tb[,colnames(tb) == lang])
    # クエリが存在するセルの論理値ベクトル
    flag.vec <- str_detect(lang.col, fixed(query))
    # 対応するカテゴリを取り出す
    return(flag.vec)
  }else{
    cat("lang is not found in tb")
    return(c())
  }
}


array_funcs = c(
  "inspect",
  "to_s",
  "to_a",
  "to_h",
  "to_ary",
  "frozen?",
  "==",
  "eql?",
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

#count <- 0
#for( i in seq(length(array_funcs))){
#  cat("========","\n")
#  cat(array_funcs[i],"\n")
#  result <- findCategory(ptable2, "ruby" ,array_funcs[i])
#  if(length(result)!=0){
#    print(which(small.cateogories==result))
#    count <- count + 1
#  }
#}

count <- 0
names.col <- c()
category.ids.col <- c()
for( func in array_funcs){
  cat("========","\n")
  cat(func,"\n")
  result <- findCategoryFlag(ptable2, "ruby" ,func)
  if(length(result)!=0){
    category.ids <- which(result)
    for( id in category.ids){
      print(as.character(ptable2[id,1]))
      names.col <- c(names.col, func)
      category.ids.col <- c(category.ids.col, id)
    } 
    count <- count + 1
  }
}
data.frame(name=names.col, category_id=category.ids.col)
count / length(array_funcs)

# as.matrix(str_detect(as.matrix(ptable2[,2:5]), "none"), nrow=nrow(ptable2[,2:5]), ncol=ncol(ptable2[,2:5]))
findCategory(ptable2, "ruby","uniq")

library(RSQLite)
dbname="~/Dropbox/Projects/rubyProjects/programming_thesaurus/db/development.sqlite3"
driver=dbDriver("SQLite")
con=dbConnect(driver,dbname)
tbl=dbGetQuery(con,"SELECT * from tokens;")
