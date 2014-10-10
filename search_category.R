library(XML)
theurl <- "http://hyperpolyglot.org/scripting"
tables <- readHTMLTable(theurl, header=FALSE)
names(tables) <- c(1,2,3,4,5,6,7,8,9)
ptable <- tables$"1"
#as.vector(ptable[1,2:5])
#as.vector(ptable[1,2:5]) == "javascript"
#any(as.vector(ptable[1,2:5]) == "javascript")
#all(as.vector(ptables[1,2:5]) == "javascript")

#as.matrix(ptable[1:10,2:5])
#is.na(as.matrix(ptable[1:10,2:5]))
# 2列目から5列目がすべてNAの行をTRUE、それ以外をFALSE
#apply(is.na(as.matrix(ptable[1:10,2:5])),c(1),all)
# その逆
#!apply(is.na(as.matrix(ptable[1:10,2:5])),c(1),all)

#c(1,2,3,4) == c(1,2,5,4)
#as.vector(ptable[1:10,1]) == ""
#!(as.vector(ptable[1:10,1]) == "")

# 1行目が空行でもなく、２〜５行目がすべて空行でないものの真理値ベクトル
is.valid.row.vec <- !apply(is.na(as.matrix(ptable[,2:5])),c(1),all) & !(as.vector(ptable[,1]) == "")
# その行を取り出す
ptable2 <- ptable[is.valid.row.vec,]


findCategory <- function(tb, key){
  ptable3 <- tb[,2:5]
  as.character(ptable2[apply(ptable3 == "none", c(1), any),1])
}

findCategory(ptable2, "none")
