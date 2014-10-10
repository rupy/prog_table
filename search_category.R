library(XML)
theurl <- "http://hyperpolyglot.org/scripting"
tables <- readHTMLTable(theurl, header=FALSE)
names(tables) <- c(1,2,3,4,5,6,7,8,9)
ptable <- tables$"1"

# 1行目が空行でもなく、２〜５行目がすべて空行でないものの真理値ベクトル
is.valid.row.vec <- !apply(is.na(as.matrix(ptable[,2:5])),c(1),all) & !(as.vector(ptable[,1]) == "")
# その行を取り出す
ptable2 <- ptable[is.valid.row.vec,]

findCategory <- function(tb, key){
  ptable3 <- tb[,2:5]
  as.character(tb[apply(ptable3 == "none", c(1), any),1])
}

findCategory(ptable2, "none")
