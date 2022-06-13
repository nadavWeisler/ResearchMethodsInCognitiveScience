source("ex2_utils.R")
install.packages("dyplr")
library("dplyr")
install.packages("ggplot2")
library(ggplot2)
install.packages("effectsize")
library(effectsize)
install.packages("tidyr")
library(tidyr)

#Question 1
pokedex_df = read.csv("pokedex.csv")

#t test
df_t = filter(.data=pokedex_df, is_legendary == 0)
df_t = na.omit(select(.data=df_t, hp))

N = length(df_t$hp)
hp_mean = mean(df_t$hp)
hp_sd = sd(df_t$hp)

ggplot(df_t, aes(x = hp)) +
  geom_histogram(position="identity", alpha=0.5, bins = 20)

t_test_result = t.test(x=df_t$hp, mu=100, alternative="less")

effect = abs(mean(t_data$A) - mean(t_data$B))/cov(t_data$A, t_data$B)
effect

#One way Anova
df_anova = na.omit(select(.data = pokedex_df, attack, type))
df_anova$type = as.factor(df_anova$type)

ggplot(df_anova, aes(y = attack, x = type)) +
  geom_violin(fill = "lightgreen", draw_quantiles = 0.5)

one_way_anova_result = aov(formula = attack ~ type,
                           data=df_anova)
summary(one_way_anova_result)

eta_squared(one_way_anova_result)

#Two way Anova
df_anova_2 = filter(.data=pokedex_df, (generation > 0) & (generation < 4))
df_anova_2 = na.omit(select(.data = df_anova_2, speed, is_legendary,
                            generation, pokedex_number))
df_anova_2$is_legendary = as.factor(df_anova_2$is_legendary)
df_anova_2$generation = as.factor(df_anova_2$generation)

speed_sd = sd(df_anova_2$speed)
speed_mean = mean(df_anova_2$speed)

ggplot(df_anova_2, aes(x = generation, y = speed, fill = is_legendary)) +
  geom_boxplot(draw_quantiles = 0.5) + 
  labs(title = "Speed by generation and legendary",
       y = "Speed",
       x = "Generation",
       fill = "Legendary") + theme(plot.title=element_text(hjust=0.5))

two_way_anova_result = aov_ez(data = df_anova_2,
                      id = "pokedex_number",
                      between = list("generation", "is_legendary"),
                      dv = "speed", 
                      anova_table = list(es="pes"))

eta_squared(two_way_anova_result, partial = FALSE)

#Question 2
question_2_df = generate_data()
all_ssw = c()
one_way = aov_ez(data = question_2_df, 
                 id = "id",
                 dv = "y", 
                 between = list("x1"))
all_ssw = c(all_ssw, get_ssw(one_way))
two_way = aov_ez(data = question_2_df, 
            id = "id",
            dv = "y",
            between = list("x1", "x2"))
all_ssw = c(all_ssw, get_ssw(two_way))
three_way = aov_ez(data = question_2_df, 
            id = "id", 
            dv = "y", 
            between = list("x1", "x2", "x3"))
all_ssw = c(all_ssw, get_ssw(three_way))
four_way = aov_ez(data = question_2_df, 
            id = "id", 
            dv = "y", 
            between = list("x1", "x2", "x3", "x4"))
all_ssw = c(all_ssw, get_ssw(four_way))
five_way = aov_ez(data = question_2_df, 
            id = "id", 
            dv = "y",
            between = list("x1", "x2", "x3", "x4", "x5"))
all_ssw = c(all_ssw, get_ssw(five_way))
six_way = aov_ez(data = question_2_df,
            id = "id", 
            dv = "y",
            between = list("x1", "x2", "x3", "x4", "x5", "x6"))
all_ssw = c(all_ssw, get_ssw(six_way))

plot(x = c(1, 2, 3, 4, 5, 6), 
     y = all_ssw, 
     xlab = "Independant variables count",
     ylab = "SSW", 
     main = "SSW vs Independant variables count", type = "o", col = "red")

#Question 3
#A
t_data = read.csv("t_data.csv")
#1
rotems_t_1 = t.test(x = t_data$A, y = t_data$B, paired = FALSE)
effect = abs(mean(t_data$A) - mean(t_data$B)) / sd(t_data$A)

#2
rotems_t_2 = t.test(x = t_data$A, y = t_data$B, paired = TRUE)
effect = abs(mean(t_data$A) - mean(t_data$B)) / sd(t_data$A)
effect

#B
#1
anova_data = read.csv("anova_data.csv")

anova_data_roy = as.data.frame(pivot_longer(anova_data, cols=c("A", "B", "C"),
                                   names_to="col", values_to="value"))
anova_data_roy&col = as.factor(anova_data_roy$col) 

roy_anova = aov_ez(data = anova_data_roy, 
                                id = "X",
                                dv = "value", 
                                between = list("col"))
summary(roy_anova)
eta_squared(roy_anova, partial = FALSE)

#2
anova_data_kobi = as.data.frame(pivot_longer(anova_data, cols=c("A", "B", "C"),
                                          names_to="col", values_to="value"))

kobi_anova = aov_ez(data = anova_data_kobi, 
                    id = "X",
                    dv = "value", 
                    within = list("col"))
summary(kobi_anova)
eta_squared(kobi_anova, partial = FALSE)

