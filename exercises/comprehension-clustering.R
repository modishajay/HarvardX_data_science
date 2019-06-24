rm(list = ls())

#Question 1________________________________________

library(dslabs)
data("tissue_gene_expression")

d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))

#Question 2________________________________________

h <- hclust(d)
#export the dendogram to a pdf for better viewing
pdf("/Users/Kal/h.pdf", width=40, height=15)
plot(h, pch=20, col=cutree(h,3))
dev.off()

#Question 3________________________________________

x_0 <- tissue_gene_expression$x
x_0[is.na(x_0)] <- 0

k <- kmeans(x_0, centers = 7)
groups <- k$cluster
split(names(groups), groups)

#Question 4________________________________________

library(RColorBrewer)

sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)

