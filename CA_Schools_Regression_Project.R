library(tidyverse)
library(dplyr)
library(knitr)
library(ggplot2)
library(tidyr)
library(car)
library(MASS)
library(glmnet)
library(caret)
library(remotes)
remotes::install_github("pbreheny/hdrm")
library(hdrm)
library(nlme)
library(cluster)
library(factoextra)
library(knitr)


df <- read.csv("CASchools.csv")


### Part 1. 
summary <- df %>%
  group_by(grades) %>%
  summarise(N = n(),
            mean_math = mean(math, na.rm=TRUE),
            sd_math = sd(math, na.rm=TRUE),
            mean_read = mean(read, na.rm=TRUE),
            sd_read = sd(read, na.rm=TRUE)
            )

math_grade <- t.test(math ~ grades, data = df)
math_grade_pvalue <-  math_grade$p.value

read_grade <- t.test(read ~ grades, data = df)
read_grade_pvalue <-  read_grade$p.value


summary_table <- cbind.data.frame(Type = c("N", "math", "read"), "KK-06"= c(summary$N[1], paste0(round(summary$mean_math[1],2), "\u00b1", round(summary$sd_math[1],2)), paste0(round(summary$mean_read[1],2), "\u00b1", round(summary$sd_read[1],2))),"KK-08"= c(summary$N[2], paste0(round(summary$mean_math[2],2), "\u00b1", round(summary$sd_math[2],2)), paste0(round(summary$mean_read[2],2), "\u00b1", round(summary$sd_read[2],2))), pvalue=c(NA, round(math_grade_pvalue,4), round(read_grade_pvalue,4)))

kable(summary_table)



### Part 2.


data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
 return(data_sum)
}

df2 <- df


math <- data_summary(df2, varname=c("math"), 
                    groupnames=c("grades"))


read <- data_summary(df2, varname=c("read"), 
                    groupnames=c("grades"))


df3 <- rbind.data.frame(math, read)
df3$score = (c(rep("math",2),rep("read",2)))
df3$score = as.factor(df3$score)

ggplot(df3, aes(x=score, y=mean, fill=grades)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                 position=position_dodge(.9)) 


### Part 3.

df$ave = (df$math +df$read)/2
fit <- lm(ave~grades + students + teachers + calworks + lunch + computer + expenditure + income + english,
          data = df)
summary(fit)

vif(fit)


cor(df[ , c("students", "teachers", "calworks",  "lunch",  "computer",  "expenditure", "income", "english")])


### Part 4. 

df$stratio =  df$students / df$teachers
df$grades = as.factor(df$grades)
grades_pvalue <- summary(lm(ave ~ grades, data = df))$coefficients[2,4]
stratio_pvalue <- summary(lm(ave ~ stratio, data = df))$coefficients[2,4]
calworks_pvalue <- summary(lm(ave ~ calworks, data = df))$coefficients[2,4]
lunch_pvalue <- summary(lm(ave ~ lunch, data = df))$coefficients[2,4]
computer_pvalue <- summary(lm(ave ~ computer, data = df))$coefficients[2,4]
expenditure_pvalue <- summary(lm(ave ~ expenditure, data = df))$coefficients[2,4]
income_pvalue <- summary(lm(ave ~ income, data = df))$coefficients[2,4]
english_pvalue <- summary(lm(ave ~ english, data = df))$coefficients[2,4]

pvalues =  c(grades_pvalue,stratio_pvalue,calworks_pvalue,lunch_pvalue,computer_pvalue,expenditure_pvalue,income_pvalue,english_pvalue)

res <- cbind.data.frame(Variable = c("grades","stratio", "calworks", "lunch", "computer", "expenditure", "income", "english"),
                        p.value = pvalues, p.adjust = p.adjust(pvalues, method = "BH")) 

res


### Part 5.

df %>%
  ggplot(aes(x=lunch,y=ave)) +
  geom_point(alpha=0.5) +
  labs(x= "Lunch", y="Average Score")+
  geom_smooth(method=lm)+
  geom_text(data = filter(df, ave>700),aes(label=school))


### Part 6.

fit <- lm(ave ~ grades+stratio+calworks+lunch+computer+expenditure+income+english, data = df)
summary <- summary(fit)
summary

summary$coefficients[order(summary$coefficients[,4]),]


### Part 7.

plot(fit,which = 2)
shapiro.test(fit[['residuals']])

cooksD <- cooks.distance(fit)
n <- nrow(df)
plot(cooksD, main = "Cooks Distance for Influential Obs")
abline(h = 4/n, lty = 2, col = "blue")


influential_obs <- as.numeric(names(cooksD)[(cooksD > (4/n))])
influential_obs

length(influential_obs)


### Part 8.
require(MASS)
backward_fit <- stepAIC(fit, direction = "backward",trace = FALSE, k = log(n))
backward_fit$coefficients


#Predicted Average Score for Woodside Elementary
predict(backward_fit, df[df$school == "Woodside Elementary",c("grades", "lunch", "income", "english")])



### Part 9.

require(glmnet) 
df_new <- df %>% dplyr::select("ave","grades", "stratio", "calworks", "lunch", "computer", "expenditure", "income", "english")
df_new$grades <- as.numeric(dplyr::recode(df_new$grades, "KK-06" = "0", "KK-08" = "1"))

df_std <- as.data.frame(scale(df_new))
x = df_std[,-1]
y = df_std[,1]
model <- glmnet(x,y, alpha = 1, standardize = FALSE)
coef(model, s=max(model$lambda))


### Part 10.
plot(model, "lambda", label = TRUE)

### Part 11.

require(caret)

model <- train(
  x,
  y,
  data = df_std,
  method = 'glmnet',
  standardize = FALSE,
  trControl = trainControl(method = "LOOCV"),
  tuneGrid = expand.grid(alpha = 1,  lambda = seq(0,50,1))
)
model

plot(model$results$RMSE, model$results$lambda, col= ifelse(model$results$lambda == 0, "red", "black"), type = "p", xlab = "RMSE", ylab = "Lambda")


### Part 12.

coef(model$finalModel, 0)


require(hdrm)
bootstrap <- boot.glmnet(as.matrix(x), y, B = 1000, seed = 123, bar = FALSE)
bootstrap[4,]

var(x$lunch)

### Part 13.
fit_lme <- lme(ave ~ grades+stratio+calworks+lunch+computer+expenditure+income+english, random=~1|county, data = df)
anova1 <- anova(fit_lme)

anova1[5,]


anova1[8,]

anova1[9,]


### Part 14.

df_std2 <- cbind.data.frame(school = df$school, county = df$county,  df_std)

schools = df_std2[df_std2$county == "Los Angeles","school"]
df_std3 <- df_std2[df_std2$county == "Los Angeles", c("grades","stratio", "calworks", "lunch", "computer", "expenditure", "income", "english")]
rownames(df_std3) = schools

clusters <- hclust(dist(df_std3), method = "ward.D2")
plot(clusters)


### Part 15.


# computing gap statistic
set.seed(123)
gap_stat <- clusGap(df_std3, FUN = kmeans, nstart = 25,
                    K.max = 5, B = 1000)

#Result
print(gap_stat, method = "firstmax")


fviz_gap_stat(gap_stat)

# Average scores (original score, unstandardized version):

kmeans <-kmeans(df_std3, 5)
kmeans$centers

