install.packages("dyplr")
library("dplyr")
library("ggplot2")
install.packages("ltm")
library(ltm)

# שחזור 

# טעינת נתונים 
replica_df <- read.csv("experiment8.csv")

# עיבוד מקדים: סינון נבדקים בעלי אחוז דיוק נמוך מ90% במטלת פריצה לתודעה
replica_df %>%   
  filter(ACC > 0.9)


# שחזור מבחן הקורלציה
cor.test(replica_df$MeanHSP,
         replica_df$RT,
         method = "pearson"
)

# יצירת גרף
p1 <- ggplot(replica_df, aes(y = MeanHSP, x = RT)) +
  ylab("HSP") + 
  xlab("NVPS")+ 
  geom_point() + 
  ggtitle("NVPS vs HSP")
p1 + geom_smooth(method=lm, formula = y ~ poly(x, 1))

# יצירת עמודת ממוצע
replica_df$mean <- rowMeans(replica_df[,c('Q1', 'Q2', 'Q3', 'Q4', 'Q5', 'Q6')],
                            na.rm=TRUE)

# שחזור המבחן מעמודת הממוצע שיצרתי בעצמי 
cor.test(replica_df$mean,
         replica_df$RT,
         method = "pearson"
         )

# יצירת גרף

p1 <- ggplot(replica_df, aes(y = mean, x = RT)) +
  ylab("HSP") + 
  xlab("NVPS")+ 
  geom_point() + 
  ggtitle("NVPS vs HSP")
p1 + geom_smooth(method=lm, formula = y ~ poly(x, 1))


# יצירת רגרסיה מרובה לחזוי פריצה לתודעה מהתשובות השונות לשאלון
reg <- lm(replica_df$RT ~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6, replica_df)
summary(reg)

# יצירת גרפים בין השאלות השונות לפריצה לתודעה

p_q1 <- ggplot(replica_df, aes(y = Q1, x = RT)) +
  ylab("Q1") + 
  xlab("NVPS")+ 
  geom_point() + 
  ggtitle("NVPS vs Q1")
p_q1 + geom_smooth(method=lm, formula = y ~ poly(x, 1))

p_q2 <- ggplot(replica_df, aes(y = Q2, x = RT)) +
  ylab("Q2") + 
  xlab("NVPS")+ 
  geom_point() + 
  ggtitle("NVPS vs Q2")
p_q2 + geom_smooth(method=lm, formula = y ~ poly(x, 1))

p_q3 <- ggplot(replica_df, aes(y = Q3, x = RT)) +
  ylab("Q3") + 
  xlab("NVPS")+ 
  geom_point() + 
  ggtitle("NVPS vs Q3")
p_q3 + geom_smooth(method=lm, formula = y ~ poly(x, 1))


p_q4 <- ggplot(replica_df, aes(y = Q4, x = RT)) +
  ylab("Q4") + 
  xlab("NVPS")+ 
  geom_point() + 
  ggtitle("NVPS vs Q4")
p_q4 + geom_smooth(method=lm, formula = y ~ poly(x, 1))

p_q5 <- ggplot(replica_df, aes(y = Q5, x = RT)) +
  ylab("Q5") + 
  xlab("NVPS")+ 
  geom_point() + 
  ggtitle("NVPS vs Q5")
p_q5 + geom_smooth(method=lm, formula = y ~ poly(x, 1))


p_q6 <- ggplot(replica_df, aes(y = Q6, x = RT)) +
  ylab("Q6") + 
  xlab("NVPS")+ 
  geom_point() + 
  ggtitle("NVPS vs Q6")
p_q6 + geom_smooth(method=lm, formula = y ~ poly(x, 1))

# חישוב האלפא של קרונבך על השאלון

only_hps_questions <- replica_df[c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6")]
cronbach.alpha(only_hps_questions)



