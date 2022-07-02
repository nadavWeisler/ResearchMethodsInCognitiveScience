#install.packages("dyplr")
library("dplyr")
library("ggplot2")

wine_df <- read.csv("wine_quality.csv")

#Question 1

#A
part_wine_df <- wine_df[1: 25,]
part_wine_df <- part_wine_df[c("citric.acid", "quality")]

#B
colnames(part_wine_df) <- c("x", "quality")

part_wine_df$x2 <- part_wine_df$x ** 2
part_wine_df$x3 <- part_wine_df$x ** 3
part_wine_df$x4 <- part_wine_df$x ** 4
part_wine_df$x5 <- part_wine_df$x ** 5

#C - Create linear regression models, add p-value and effect size(R2)

# simple linear regression using x for quality
reg1 <- lm(part_wine_df$quality ~ x, part_wine_df)
summary(reg1)

# multiple linear regression using x, x2 for quality
reg2 <- lm(part_wine_df$quality ~ x + x2, part_wine_df)
summary(reg2)

# multiple linear regression using x, x2, x3 for quality
reg3 <- lm(part_wine_df$quality ~ x + x2 + x3, part_wine_df)
summary(reg3)
# multiple linear regression using x, x2, x3, x4 for quality
reg4 <- lm(part_wine_df$quality ~ x + x2 + x3 + x4, part_wine_df)
summary(reg4)
# multiple linear regression using x, x2, x3, x4, x5 for quality
reg5 <- lm(part_wine_df$quality ~ x + x2 + x3 + x4 + x5, part_wine_df)
summary(reg5)

#D - Calculate SSE for each model and report the lowest model
sse1 <- sum((part_wine_df$quality - predict(reg1)) ^ 2)
sse2 <- sum((part_wine_df$quality - predict(reg2)) ^ 2)
sse3 <- sum((part_wine_df$quality - predict(reg3)) ^ 2)
sse4 <- sum((part_wine_df$quality - predict(reg4)) ^ 2)
sse5 <- sum((part_wine_df$quality - predict(reg5)) ^ 2)

# ANSWER - Lowest model is 5.

#E - Create scatter plot for the relation between x and quality, add  
#regression line. (5 graphs differ only in the regression line)

p1 <- ggplot(part_wine_df, aes(y = quality, x = x)) +
  ylab("Quality") + 
  xlab("x value")+ 
  geom_point() + 
  ggtitle("x vs quality with regression of x")
p1 + geom_smooth(method=lm, formula = y ~ poly(x, 1))

p2 <- ggplot(part_wine_df, aes(y = quality, x = x)) + 
  ylab("Quality") + 
  xlab("x value")+ 
  geom_point()+ 
  ggtitle("x vs quality with regression of x, x2")
p2 + geom_smooth(method=lm, formula = y ~ poly(x, 2))

p3 <- ggplot(part_wine_df, aes(y = quality, x = x)) + 
  ylab("Quality") + 
  xlab("x value")+ 
  geom_point() + 
  ggtitle("x vs quality with regression of x, x2, x3")
p3 + geom_smooth(method=lm, formula = y ~ poly(x, 3))

p4 <- ggplot(part_wine_df, aes(y = quality, x = x)) +
  ylab("Quality") +
  xlab("x value")+ 
  geom_point() +
  ggtitle("x vs quality with regression of x, x2, x3, x4")
p4 + geom_smooth(method=lm, formula = y ~ poly(x, 4))

p5 <- ggplot(part_wine_df, aes(y = quality, x = x)) + 
  ylab("Quality") +
  xlab("x value")+ 
  geom_point()+
  ggtitle("x vs quality with regression of x, x2, x3, x4, x5")
p5 + geom_smooth(method=lm, formula = y ~ poly(x, 5))

#F - what is the poly degree which fit best, its wit for D question?

# ANSWER - 5, fit to question D.

#Question 2
wine_df <- read.csv("wine_quality.csv")
only_red_wines = wine_df[wine_df$type == "red", ]
only_white_wines = wine_df[wine_df$type == "white", ]

#A - Create scatter plot for quality VS alcohol %
plot(only_red_wines$quality, only_red_wines$alcohol, 
     main="Red wine - quality VS alcohol %",
     xlab="Wine Quality", ylab="Alcohol %", pch=19)

plot(only_white_wines$quality, only_white_wines$alcohol, 
     main="White wine - quality VS alcohol %",
     xlab="Wine Quality", ylab="Alcohol %", pch=19)

#B - For each wine type, calculate spearman between alcohol and quality
cor.test(only_red_wines$quality, 
         only_red_wines$alcohol, method = "spearman")
cor.test(only_white_wines$quality, 
         only_white_wines$alcohol, method = "spearman")


#C - Create bootstrap sampling for alcohol and quality pairs. calculate spearman
boot_white_sample_size = nrow(only_white_wines)
white_wine_df_boot = data.frame(matrix(NA, ncol = 2,
                                       nrow = boot_white_sample_size))
for (i in c(1:boot_white_sample_size)) {
  indexes <- sample(x=1:nrow(only_white_wines), size=nrow(only_white_wines),
                    replace=TRUE)
  white_wine_df_boot[i, ] <- c(only_white_wines[indexes, "alcohol"], 
                              only_white_wines[indexes, "quality"])
}
colnames(white_wine_df_boot) <- c("alcohol", "quality")
only_white_wine_spearman <- cor(white_wine_df_boot$quality,
                                white_wine_df_boot$alcohol, method = "spearman")

boot_red_sample_size = nrow(only_red_wines)
red_wine_df_boot = data.frame(matrix(NA, ncol = 2, nrow = boot_red_sample_size))
for (i in c(1:boot_red_sample_size)) {
  indexes <- sample(x=1:nrow(only_red_wines), size=nrow(only_red_wines),
                    replace=TRUE)
  red_wine_df_boot[i, ] <- c(only_red_wines[indexes, "alcohol"], 
                               only_red_wines[indexes, "quality"])
}
colnames(red_wine_df_boot) <- c("alcohol", "quality")
only_red_wine_spearman <- cor(x=red_wine_df_boot$quality,
                              y=red_wine_df_boot$alcohol, method = "spearman")

#D - Do C 10000 times
red_wine_boot_spearman <- c()
white_wine_boot_spearman <- c()

for (i in 1:10000){
  print(i)
  white_wine_df_boot = data.frame(matrix(NA, ncol = 2, nrow = boot_white_sample_size))
  red_wine_df_boot = data.frame(matrix(NA, ncol = 2, nrow = boot_red_sample_size))
  for (i in c(1:boot_white_sample_size)) {
    indexes <- sample(x=1:nrow(only_white_wines), size=nrow(only_white_wines),
                      replace=TRUE)
    white_wine_df_boot[i, ] <- c(only_white_wines[indexes, "alcohol"], 
                                 only_white_wines[indexes, "quality"])
  }
  
  colnames(white_wine_df_boot) <- c("alcohol", "quality")
  white_wine_boot_spearman <- c(white_wine_boot_spearman, 
                                cor(white_wine_df_boot$quality, white_wine_df_boot$alcohol, method = "spearman"))
  
  for (i in c(1:boot_red_sample_size)) {
      indexes <- sample(x=1:nrow(only_red_wines), size=nrow(only_red_wines),
                        replace=TRUE)
      red_wine_df_boot[i, ] <- c(only_red_wines[indexes, "alcohol"], 
                                 only_red_wines[indexes, "quality"])
  }
  
  colnames(red_wine_df_boot) <- c("alcohol", "quality")
  red_wine_boot_spearman <- c(red_wine_boot_spearman, 
                              cor(red_wine_df_boot$quality, red_wine_df_boot$alcohol, method = "spearman"))
}


#E - For each wine type calculate confidence interval
white_wine_quantiles <- quantile(x = white_wine_boot_spearman, probs = c(0.025, 0.975), na.rm = TRUE)
white_wine_quantiles

red_wine_quantile <- quantile(x = red_wine_boot_spearman, probs = c(0.025, 0.975), na.rm = TRUE)
red_wine_quantile

#F - For each wine type create histogram for D
hist(white_wine_boot_spearman, 
     main = "Spearman white wine bootstrap sample histogram",
     xlab="Spearman value")
abline(v = -0.02831828, col="red", lwd=3, lty=2)
abline(v = 0.02769062, col="red", lwd=3, lty=2)

red_wine_boot_spearman
#F - For each wine type create histogram for D
hist(red_wine_boot_spearman, 
     main = "Spearman red wine bootstrap sample histogram",
     xlab="Spearman value")
abline(v = -0.04777338, col="red", lwd=3, lty=2)
abline(v = 0.04938137, col="red", lwd=3, lty=2)



#G - Which wine type get the bigger confidence interval? 
#how can we explain that?

#H - BONUS - Create another graph,
#which the confidence interval will be error bar.

spearman_df = data.frame(SpearmanValue = c(only_white_wine_spearman, 
                                           only_red_wine_spearman),
                         WineType = c("white", "red"),
                         min_confidence_interval = c(-0.02831828, -0.04777338),
                         max_confidence_interval = c(0.02769062, 0.04938137))
ggplot(spearman_df, aes(x = WineType, y = SpearmanValue)) +
  geom_point(size = 4, color = "blue") + 
  ggtitle("Spearman value vs wine type with error bars for confidence interval") + 
  ylab("Spearman value") +
  xlab("Wine type") + 
  geom_errorbar(aes(ymax = max_confidence_interval,
                    ymin = min_confidence_interval),
                width = 0.3, size = 0.5) +
  scale_color_manual(name = "Legend", values = c("#666666", "#1B9E77"))

#Question 3

#A - Create violin / box (for each wine) or jitter (for both)
#plot for wine type (type) and wine quality (quality)
p <- ggplot(wine_df, aes(type, quality)) +
  ggtitle("jitter plot: wine type vs quality")
p + geom_jitter()

#B - Use Mann Whitney test for difference in the quality between red and 
# white wines. Output as W and p-value
res <- wilcox.test(quality ~ type, data = wine_df)
res

#C - Calculate U1 and U2 from B, which one reported in B?
ranked_wine_df <- wine_df[c("type", "quality")]
ranked_wine_df <- ranked_wine_df[c("type", "quality")]
ranked_wine_df <- ranked_wine_df[order(ranked_wine_df$quality),]
ranked_wine_df$ranked = rank(ranked_wine_df$quality)

n1 = nrow(ranked_wine_df[ranked_wine_df$type == "white",])
n2 = nrow(ranked_wine_df[ranked_wine_df$type == "red",])

R1 = sum(ranked_wine_df[ranked_wine_df$type == "white",]$ranked)
R2 = sum(ranked_wine_df[ranked_wine_df$type == "red",]$ranked)

U1 = (n1*n2) + (n1 * (n1 + 1) / 2) - R1
U2 = (n1*n2) + (n2 * (n2 + 1) / 2) - R2

#D - Calculate effect size

rbc <- abs(((2 * U1) / (n1 * n2)) - 1)





