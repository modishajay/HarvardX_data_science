library(dslabs)
data("tissue_gene_expression")

dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

#Question 1____________________________
d <- dist(tissue_gene_expression$x)

#Question 2____________________________
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

#Question 3____________________________
image(as.matrix(d))
