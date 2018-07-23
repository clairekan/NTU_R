library(tm)
library(tmcn)
library(factoextra)
library(Matrix)
library(NLP)
library(wordcloud)
library(dplyr)

music.corpus <- Corpus( DirSource("./musical") )
#DirSource是Directory Source顧名思義就是指定一個資料夾作為資料來源所以參數是要放一個資料夾的路徑
#開始清理資料

toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)

music.corpus <- tm_map(music.corpus, toSpace, "you")
music.corpus <- tm_map(music.corpus, toSpace, "and")
music.corpus <- tm_map(music.corpus, toSpace, "her")
music.corpus <- tm_map(music.corpus, toSpace, "all")
music.corpus <- tm_map(music.corpus, toSpace, "that")
music.corpus <- tm_map(music.corpus, toSpace, "let")
music.corpus <- tm_map(music.corpus, toSpace, "with")
music.corpus <- tm_map(music.corpus, toSpace, "for")
music.corpus <- tm_map(music.corpus, toSpace, "say")
music.corpus <- tm_map(music.corpus, toSpace, "the")

music.corpus <- tm_map(music.corpus, removePunctuation)
music.corpus<- tm_map(music.corpus, content_transformer(tolower))
music.tdm <- TermDocumentMatrix(music.corpus, control = list())
inspect(music.tdm)

docs.tf <- apply(as.matrix(music.tdm), 2, function(word) { word/sum(word) })
idf.function <- function(word_doc) { log2( (length(word_doc)+1) / nnzero(word_doc) ) }
docs.idf <- apply(as.matrix(music.tdm), 1, idf.function)
docs.tfidf <- docs.tf * docs.idf

cdocs.tfidf<-as.data.frame(docs.tfidf)
arrange(cdocs.tfidf,desc(CHRISTINE.r.txt))

#查詢字頻
query.tfidf <- function(q){
  q.position <- which(rownames(docs.tfidf) %in% q)
  q.tfidf <- docs.tfidf[q.position, ]
  return (q.tfidf)
}
query.tfidf(c("angel","christine","phantom","music"))


f <- sort(rowSums(docs.tfidf), decreasing = T)
docs.df <- data.frame(
  word = names(f),
  freq = f
)
wordcloud(docs.df$word, docs.df$freq, scale = c(2.9, 0.2),max.words = 100, colors=brewer.pal(8, "Dark2"))

docs.pca <- prcomp(docs.tfidf, scale = T)


fviz_eig(docs.pca)

fviz_pca_ind(docs.pca, geom.ind = c("point"), col.ind = "cos2")

fviz_pca_var(docs.pca, col.var = "contrib")

fviz_pca_biplot(docs.pca, geom.ind = "point")

docs.eig <- get_eig(docs.pca)
docs.var <- get_pca_var(docs.pca)
docs.ind <- get_pca_ind(docs.pca)

ind.coord2 <- docs.ind$coord[, 1:2]
wss <- c()
for (i in 1:10) { wss[i] <- kmeans(ind.coord2, i)$tot.withinss }
plot(wss, type = "b")

km <- kmeans(ind.coord2, 3)
plot(ind.coord2, col = km$cluster)
points(km$centers, col = 1:3, pch = 8, cex = 2)

