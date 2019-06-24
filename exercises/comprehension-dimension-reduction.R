#Question 1____________________________________________________

data("tissue_gene_expression")
dim(tissue_gene_expression$x)

dat <- as.data.frame(tissue_gene_expression$x, tissue_gene_expression$y)

pca <- prcomp(tissue_gene_expression$x)

data.frame(pca$x[,1:2], tissue = tissue_gene_expression$y) %>%
  ggplot(aes(PC1, PC2, fill = tissue)) +
  geom_point(cex = 3, pch = 21) +
  coord_fixed(ratio = 1)

#Question 2____________________________________________________

avg <- rowMeans(tissue_gene_expression$x)

data.frame(PC1 = pca$x[,1], avg = avg, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avg, PC1, color = tissue)) +
  geom_point()

cor(pca$x[,1], avg)

#Question 3____________________________________________________

x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#Question 4____________________________________________________

pca <- prcomp(tissue_gene_expression$x)

data.frame(pca = pca$x[,1:10], tissue = tissue_gene_expression$y) %>%
  boxplot(pca ~ tissue)

data.frame(pc7 = pca$x[,7], tissue = tissue_gene_expression$y) %>%
  ggplot(aes(y=pc7, x=tissue)) + geom_boxplot()

#Question 5____________________________________________________

library(tidyr)
library(stringr)

importance_df <- data.frame(summary(pca)$importance)
importance_df <- importance_df[2,] %>% 
  gather(key = pc, value = importance)
importance_df <- importance_df %>% mutate(pc_index = as.integer(str_remove(importance_df$pc, "PC")))
importance_df$pc <- factor(importance_df$pc, levels = importance_df$pc[order(importance_df$pc_index)])
importance_df <- importance_df %>% mutate(cum_sum = cumsum(importance))

importance_df %>% 
  filter(pc_index < 20) %>% 
  arrange(pc_index, cum_sum) %>% 
  ggplot(aes(x = pc, y = cum_sum, fill=pc)) +
  geom_col() +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  theme_grey()

