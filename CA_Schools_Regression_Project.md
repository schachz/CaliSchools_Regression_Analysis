CA_Schools_RegressionAnalysis
================
By Zach S.

``` r
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
library(rmarkdown)
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
```

## Data description.

The data used here are from all 420 K-6 and K-8 districts in California
with data available for 1998 and 1999. Test scores are on the Stanford 9
standardized test administered to 5th grade students. School
characteristics (averaged across the district) include enrollment,
number of teachers (measured as “full-time equivalents”, number of
computers per classroom, and expenditures per student. Demographic
variables for the students are averaged across the district. The
demographic variables include the percentage of students in the public
assistance program CalWorks (formerly AFDC), the percentage of students
that qualify for a reduced price lunch, and the percentage of students
that are English learners (that is, students for whom English is a
second language).

Reference: Stock, J. H. and Watson, M. W. (2007). *Introduction to
Econometrics*, 2nd ed. Boston: Addison Wesley.

#### Dataset

Data file is available at
<https://github.com/schachz/CA_Schools_RegressionAnalysis/blob/main/CASchools.csv>

#### Dataset characteristics:

- district: character. District code.
- school: character. School name.
- county: factor indicating county.
- grades: factor indicating grade span of district.
- students: Total enrollment.
- teachers: Number of teachers.
- calworks: Percent qualifying for CalWorks (income assistance).
- lunch: Percent qualifying for reduced-price lunch.
- computer: Number of computers.
- expenditure: Expenditure per student.
- income: District average income (in USD 1,000).
- english: Percent of English learners.
- read: Average reading score.
- math: Average math score.

## Analysis

``` r
df <- read.csv("CASchools.csv")
```

### Part 1.

I will prepare a score table by grades (KK-06/KK-08) using R and the
knitr package’s kable function. Additionally, I will conduct appropriate
statistical tests to obtain p-values for each variable (except schools
N) when comparing the KK-06 group and the KK-08 group.

``` r
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
```

| Type | KK-06        | KK-08        | pvalue |
|:-----|:-------------|:-------------|-------:|
| N    | 61           | 359          |     NA |
| math | 661.08±19.38 | 652.03±18.35 | 0.0011 |
| read | 662.08±20.51 | 653.76±19.82 | 0.0043 |

### Part 2.

I will create a barplot to visualize the mean math score and mean read
score for grades KK-06 and KK-08. I will use different colors to
distinguish between the two grades and include standard deviation bars
on the plot.

``` r
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
```

![part2](https://github.com/schachz/CaliSchools_Regression_Analysis/assets/100746204/9bc9d020-c79d-4cc4-aebe-7561693b63e8)



### Part 3.

Next, I will calculate a new score variable by averaging math and read
scores (ave = (math + read) / 2). Then, I will fit a multivariable
linear regression model with this average score as the outcome variable
and use grades, students, teachers, calworks, lunch, computer,
expenditure, income, and english as predictors. During this process, I
will check for collinearity issues and identify highly correlated
predictors.

``` r
df$ave = (df$math +df$read)/2
fit <- lm(ave~grades + students + teachers + calworks + lunch + computer + expenditure + income + english,
          data = df)
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = ave ~ grades + students + teachers + calworks + 
    ##     lunch + computer + expenditure + income + english, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -31.4018  -5.0840  -0.0135   4.9089  27.9984 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.592e+02  4.064e+00 162.192  < 2e-16 ***
    ## gradesKK-08 -3.644e+00  1.207e+00  -3.020  0.00269 ** 
    ## students    -6.753e-04  1.457e-03  -0.463  0.64336    
    ## teachers     3.947e-03  3.210e-02   0.123  0.90220    
    ## calworks    -1.011e-01  5.744e-02  -1.759  0.07924 .  
    ## lunch       -3.560e-01  3.622e-02  -9.829  < 2e-16 ***
    ## computer     4.136e-03  2.759e-03   1.499  0.13466    
    ## expenditure  1.787e-03  7.339e-04   2.435  0.01532 *  
    ## income       6.124e-01  8.878e-02   6.898 2.01e-11 ***
    ## english     -2.231e-01  3.482e-02  -6.407 4.07e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.337 on 410 degrees of freedom
    ## Multiple R-squared:  0.8126, Adjusted R-squared:  0.8085 
    ## F-statistic: 197.6 on 9 and 410 DF,  p-value: < 2.2e-16

We can use the variance inflation factor (VIF) to check for
multicollinearity. A VIF value greater than 10 indicates potentially
correlation between a given predictor variable and other predictor
variables in the model.

``` r
vif(fit)
```

    ##      grades    students    teachers    calworks       lunch    computer 
    ##    1.092117  196.031793  219.309242    2.609757    5.818843    8.941366 
    ## expenditure      income     english 
    ##    1.304955    2.480921    2.444111

We see that the VIF for both students and teachers are greater than 10,
which could be an issue.

``` r
cor(df[ , c("students", "teachers", "calworks",  "lunch",  "computer",  "expenditure", "income", "english")])
```

    ##                students    teachers    calworks       lunch    computer
    ## students     1.00000000  0.99711606  0.09016139  0.12923399  0.92888209
    ## teachers     0.99711606  1.00000000  0.09264528  0.12429563  0.93724231
    ## calworks     0.09016139  0.09264528  1.00000000  0.73942180  0.05916015
    ## lunch        0.12923399  0.12429563  0.73942180  1.00000000  0.06138610
    ## computer     0.92888209  0.93724231  0.05916015  0.06138610  1.00000000
    ## expenditure -0.11228455 -0.09519483  0.06788857 -0.06103871 -0.07131050
    ## income       0.02839221  0.04300655 -0.51265102 -0.68443962  0.09434278
    ## english      0.35487921  0.35142111  0.31957593  0.65306072  0.29133881
    ##             expenditure      income     english
    ## students    -0.11228455  0.02839221  0.35487921
    ## teachers    -0.09519483  0.04300655  0.35142111
    ## calworks     0.06788857 -0.51265102  0.31957593
    ## lunch       -0.06103871 -0.68443962  0.65306072
    ## computer    -0.07131050  0.09434278  0.29133881
    ## expenditure  1.00000000  0.31448448 -0.07139604
    ## income       0.31448448  1.00000000 -0.30741949
    ## english     -0.07139604 -0.30741949  1.00000000

From the correlation matrix we can see that number of students is
strongly correlated with number of teachers as well as the number of
computers. Additionally, the number of teachers is strongly correlated
with the number of students and number of computers. This explains why
students and teachers have high VIF values.

### Part 4.

I will create a new variable representing student-teacher ratio (stratio
= students/teachers). Afterward, I will perform univariate linear
regression models for each predictor mentioned prior, using average
score as the dependent variable. Once I obtain p-values, I will control
for multiple testing using the Benjamini-Hochberg correction and
identify predictors with a false discovery rate below 5%.

``` r
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
```

    ##      Variable       p.value      p.adjust
    ## 1      grades  9.499417e-04  1.085648e-03
    ## 2     stratio  2.783308e-06  4.453293e-06
    ## 3    calworks  3.068954e-47  6.137908e-47
    ## 4       lunch 1.187593e-129 9.500747e-129
    ## 5    computer  1.313795e-01  1.313795e-01
    ## 6 expenditure  7.989251e-05  1.065233e-04
    ## 7      income  2.751906e-66  1.100762e-65
    ## 8     english  1.356648e-50  3.617729e-50

The above variables all have a false discovery rate of less than 5%.

### Part 5.

To visualize the relationship between the most significant continuous
predictor, which is lunch (based on p-value) and the average score, I
will plot a scatter plot. Additionally, I will fit a straight line to
show the trend of the scatter plot. I will also label the school names
of the schools with an average score greater than 700 in the figure.

``` r
df %>%
  ggplot(aes(x=lunch,y=ave)) +
  geom_point(alpha=0.5) +
  labs(x= "Lunch", y="Average Score")+
  geom_smooth(method=lm)+
  geom_text(data = filter(df, ave>700),aes(label=school))
```
![part5](https://github.com/schachz/CaliSchools_Regression_Analysis/assets/100746204/c8ca9b93-faff-42b0-8da9-ccff714f507c)

### Part 6.

For this step, I will fit a multivariable linear regression model, using
the outcome variable (i.e., average score) in part 4 as the dependent
variable and all 8 predictors in part 4 as the independent variables. I
will then print the coefficient table of the full model. I will identify
the three predictors (excluding the intercept) with the smallest
p-values.

``` r
fit <- lm(ave ~ grades+stratio+calworks+lunch+computer+expenditure+income+english, data = df)
summary <- summary(fit)
summary
```

    ## 
    ## Call:
    ## lm(formula = ave ~ grades + stratio + calworks + lunch + computer + 
    ##     expenditure + income + english, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -30.6078  -5.0155  -0.1285   4.9300  28.1041 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.665e+02  9.131e+00  72.994  < 2e-16 ***
    ## gradesKK-08 -3.457e+00  1.200e+00  -2.881  0.00418 ** 
    ## stratio     -2.865e-01  2.856e-01  -1.003  0.31650    
    ## calworks    -1.002e-01  5.734e-02  -1.747  0.08132 .  
    ## lunch       -3.570e-01  3.625e-02  -9.849  < 2e-16 ***
    ## computer     4.639e-04  1.028e-03   0.451  0.65209    
    ## expenditure  1.423e-03  8.847e-04   1.608  0.10862    
    ## income       6.159e-01  8.823e-02   6.980 1.19e-11 ***
    ## english     -2.283e-01  3.466e-02  -6.585 1.39e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.346 on 411 degrees of freedom
    ## Multiple R-squared:  0.8118, Adjusted R-squared:  0.8081 
    ## F-statistic: 221.6 on 8 and 411 DF,  p-value: < 2.2e-16

``` r
summary$coefficients[order(summary$coefficients[,4]),]
```

    ##                  Estimate   Std. Error    t value      Pr(>|t|)
    ## (Intercept) 666.492483417 9.1307582988 72.9942094 2.051424e-237
    ## lunch        -0.356994118 0.0362482278 -9.8485951  1.097422e-20
    ## income        0.615890204 0.0882311200  6.9804192  1.187276e-11
    ## english      -0.228273714 0.0346640436 -6.5853169  1.389785e-10
    ## gradesKK-08  -3.457294028 1.2001456132 -2.8807288  4.175307e-03
    ## calworks     -0.100194893 0.0573410186 -1.7473511  8.132331e-02
    ## expenditure   0.001422591 0.0008847419  1.6079165  1.086214e-01
    ## stratio      -0.286454472 0.2856257468 -1.0029014  3.164985e-01
    ## computer      0.000463912 0.0010281963  0.4511901  6.520905e-01

Lunch, income and english are the three predictors with the smallest
p-values.

### Part 7.

I will plot the QQ plot of model residuals for the model in part 6.
During this process, I will formally test if the Gaussian assumption for
the residuals is violated. Additionally, I will use the criteria of 4/n
as the Cook’s D to identify any influential points.

``` r
plot(fit,which = 2)
```
![part7](https://github.com/schachz/CaliSchools_Regression_Analysis/assets/100746204/a24d1953-2e01-417c-b368-ba07bc871e2a)

``` r
shapiro.test(fit[['residuals']])
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  fit[["residuals"]]
    ## W = 0.99063, p-value = 0.00904

Since our p-value of 0.00904 is less than .05, the Gaussian assumption
of the residuals is violated.

``` r
cooksD <- cooks.distance(fit)
n <- nrow(df)
plot(cooksD, main = "Cooks Distance for Influential Obs")
abline(h = 4/n, lty = 2, col = "blue")
```

![part7b](https://github.com/schachz/CaliSchools_Regression_Analysis/assets/100746204/413e1f7a-630e-4e0c-aa06-f3eaea811442)

``` r
influential_obs <- as.numeric(names(cooksD)[(cooksD > (4/n))])
influential_obs
```

    ##  [1]   3   6  10  75  80 135 163 171 180 262 273 297 324 335 367 405 416 417 419

``` r
length(influential_obs)
```

    ## [1] 19

Using 4/n as the Cook’s Distance criteria, we can identify 19 points as
influential points.

### Part 8.

Now, I will perform backward model selection by BIC, based on the full
model in part 6. I will then print the coefficient table of the final
model (selected model). Finally, I will calculate the predicted value
(i.e., predicted average score) for Woodside Elementary using the final
model.

``` r
require(MASS)
backward_fit <- stepAIC(fit, direction = "backward",trace = FALSE, k = log(n))
backward_fit$coefficients
```

    ## (Intercept) gradesKK-08       lunch      income     english 
    ## 667.0822044  -3.8028807  -0.3784749   0.7027336  -0.2231567

``` r
#Predicted Average Score for Woodside Elementary
predict(backward_fit, df[df$school == "Woodside Elementary",c("grades", "lunch", "income", "english")])
```

    ##     409 
    ## 687.275

### Part 9.

I will perform lasso regression using the same standardized dataset from
part 6. Before proceeding with lasso regression, I will standardize each
variable (both the outcome variable and predictors) to have a mean of 0
and a standard deviation of 1. For binary variables, I will transform
them to 0 or 1 and then standardize. The pre-specified λ in this
Lagrange form will be 50 (i.e. λ=50). I will identify the variables with
non-zero coefficients with λ=50.

``` r
require(glmnet) 
df_new <- df %>% dplyr::select("ave","grades", "stratio", "calworks", "lunch", "computer", "expenditure", "income", "english")
df_new$grades <- as.numeric(dplyr::recode(df_new$grades, "KK-06" = "0", "KK-08" = "1"))

df_std <- as.data.frame(scale(df_new))
x = df_std[,-1]
y = df_std[,1]
model <- glmnet(x,y, alpha = 1, standardize = FALSE)
coef(model, s=max(model$lambda))
```

    ## 9 x 1 sparse Matrix of class "dgCMatrix"
    ##                        s1
    ## (Intercept) -9.601694e-16
    ## grades       .           
    ## stratio      .           
    ## calworks     .           
    ## lunch        .           
    ## computer     .           
    ## expenditure  .           
    ## income       .           
    ## english      .

None have a lambda of 50.

### Part 10.

Using the standardized dataset from part 9, I will visualize the lasso
regression solution path. I will pre-specify the range of λ to be
(0,10,20,…,500). The x-axis will represent different values for λ, while
the y-axis will represent the coefficient values. I will use different
colors to indicate different predictors and include a legend for
color-predictor mapping.

``` r
plot(model, "lambda", label = TRUE)
```

![part10](https://github.com/schachz/CaliSchools_Regression_Analysis/assets/100746204/8aad2fca-ecb5-4e70-90e3-0d2f30746344)

### Part 11.

I will perform leave-one-out cross-validation using the standardized
dataset from part 9 to select the best tuning parameter λ using all
samples. The range of λ will be (0,1,2,…,50). To make the decision, I
will use mean squared error (MSE) as the criterion. I will visualize the
curve of MSE versus λ and mark the λ with the minimum MSE using a
different color on the curve. The optimum λ will be the one with the
minimum MSE. Under this λ, I will determine the number of predictors
with non-zero coefficients in a model with all subjects.

``` r
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
```

    ## glmnet 
    ## 
    ## 420 samples
    ##   8 predictor
    ## 
    ## No pre-processing
    ## Resampling: Leave-One-Out Cross-Validation 
    ## Summary of sample sizes: 419, 419, 419, 419, 419, 419, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   lambda  RMSE       Rsquared   MAE      
    ##    0      0.4429462  0.8033316  0.3415565
    ##    1      1.0011926  1.0000000  0.8087844
    ##    2      1.0011926  1.0000000  0.8087844
    ##    3      1.0011926  1.0000000  0.8087844
    ##    4      1.0011926  1.0000000  0.8087844
    ##    5      1.0011926  1.0000000  0.8087844
    ##    6      1.0011926  1.0000000  0.8087844
    ##    7      1.0011926  1.0000000  0.8087844
    ##    8      1.0011926  1.0000000  0.8087844
    ##    9      1.0011926  1.0000000  0.8087844
    ##   10      1.0011926  1.0000000  0.8087844
    ##   11      1.0011926  1.0000000  0.8087844
    ##   12      1.0011926  1.0000000  0.8087844
    ##   13      1.0011926  1.0000000  0.8087844
    ##   14      1.0011926  1.0000000  0.8087844
    ##   15      1.0011926  1.0000000  0.8087844
    ##   16      1.0011926  1.0000000  0.8087844
    ##   17      1.0011926  1.0000000  0.8087844
    ##   18      1.0011926  1.0000000  0.8087844
    ##   19      1.0011926  1.0000000  0.8087844
    ##   20      1.0011926  1.0000000  0.8087844
    ##   21      1.0011926  1.0000000  0.8087844
    ##   22      1.0011926  1.0000000  0.8087844
    ##   23      1.0011926  1.0000000  0.8087844
    ##   24      1.0011926  1.0000000  0.8087844
    ##   25      1.0011926  1.0000000  0.8087844
    ##   26      1.0011926  1.0000000  0.8087844
    ##   27      1.0011926  1.0000000  0.8087844
    ##   28      1.0011926  1.0000000  0.8087844
    ##   29      1.0011926  1.0000000  0.8087844
    ##   30      1.0011926  1.0000000  0.8087844
    ##   31      1.0011926  1.0000000  0.8087844
    ##   32      1.0011926  1.0000000  0.8087844
    ##   33      1.0011926  1.0000000  0.8087844
    ##   34      1.0011926  1.0000000  0.8087844
    ##   35      1.0011926  1.0000000  0.8087844
    ##   36      1.0011926  1.0000000  0.8087844
    ##   37      1.0011926  1.0000000  0.8087844
    ##   38      1.0011926  1.0000000  0.8087844
    ##   39      1.0011926  1.0000000  0.8087844
    ##   40      1.0011926  1.0000000  0.8087844
    ##   41      1.0011926  1.0000000  0.8087844
    ##   42      1.0011926  1.0000000  0.8087844
    ##   43      1.0011926  1.0000000  0.8087844
    ##   44      1.0011926  1.0000000  0.8087844
    ##   45      1.0011926  1.0000000  0.8087844
    ##   46      1.0011926  1.0000000  0.8087844
    ##   47      1.0011926  1.0000000  0.8087844
    ##   48      1.0011926  1.0000000  0.8087844
    ##   49      1.0011926  1.0000000  0.8087844
    ##   50      1.0011926  1.0000000  0.8087844
    ## 
    ## Tuning parameter 'alpha' was held constant at a value of 1
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final values used for the model were alpha = 1 and lambda = 0.

``` r
plot(model$results$RMSE, model$results$lambda, col= ifelse(model$results$lambda == 0, "red", "black"), type = "p", xlab = "RMSE", ylab = "Lambda")
```

![part11](https://github.com/schachz/CaliSchools_Regression_Analysis/assets/100746204/b9f0665b-be2b-4096-8d71-6f61b6f06cac)

The optimal lambda is 0. With a lambda of 0 all predictors have non-zero
coefficients.

### Part 12.

With the lambda set to 0, I will calculate the bootstrapping variance
and the 95% confidence interval for the lunch variable. I will use at
least B=1,000 bootstrapping resampling.

``` r
coef(model$finalModel, 0)
```

    ## 9 x 1 sparse Matrix of class "dgCMatrix"
    ##                        s1
    ## (Intercept) -8.406288e-16
    ## grades      -6.160937e-02
    ## stratio     -2.629420e-02
    ## calworks    -5.567783e-02
    ## lunch       -5.126883e-01
    ## computer     6.300457e-03
    ## expenditure  4.604418e-02
    ## income       2.336125e-01
    ## english     -2.141875e-01

``` r
require(hdrm)
bootstrap <- boot.glmnet(as.matrix(x), y, B = 1000, seed = 123, bar = FALSE)
bootstrap[4,]
```

    ##            Lower     Upper
    ## lunch -0.6303382 -0.399799

``` r
var(x$lunch)
```

    ## [1] 1

### Part 13.

In this step, I will fit a linear mixed-effects model to incorporate the
correlation structure among schools within the same county. I will use
the outcome variable and the 8 predictors from part 6. I will then
obtain the p-values for the three most significant predictors identified
in part 6 using this new model. Finally, I will compare these p-values
with the ones from the regular multivariable linear regression model in
part 6 to determine which model yields more significant p-values for
these three predictors.

``` r
fit_lme <- lme(ave ~ grades+stratio+calworks+lunch+computer+expenditure+income+english, random=~1|county, data = df)
anova1 <- anova(fit_lme)

anova1[5,]
```

    ##       numDF denDF  F-value p-value
    ## lunch     1   367 621.6877  <.0001

``` r
anova1[8,]
```

    ##        numDF denDF  F-value p-value
    ## income     1   367 39.59829  <.0001

``` r
anova1[9,]
```

    ##         numDF denDF  F-value p-value
    ## english     1   367 35.22867  <.0001

For the lunch variable, the linear mixed effects model yielded a more
significant p value. However for income and english, the multivariable
linear regression model yielded more significant p-values.

### Part 14.

To examine the similarities (clusters) among schools in the Los Angeles
county, I will create a subset data containing only the 27 schools in
that county. I will include only the 8 predictors specified in part 4. I
will then perform hierarchical clustering based on this subset data
using the ward.D2 method and I will plot the hierarchical tree
structure.

``` r
df_std2 <- cbind.data.frame(school = df$school, county = df$county,  df_std)

schools = df_std2[df_std2$county == "Los Angeles","school"]
df_std3 <- df_std2[df_std2$county == "Los Angeles", c("grades","stratio", "calworks", "lunch", "computer", "expenditure", "income", "english")]
rownames(df_std3) = schools

clusters <- hclust(dist(df_std3), method = "ward.D2")
plot(clusters)
```



![part14](https://github.com/schachz/CaliSchools_Regression_Analysis/assets/100746204/801658f7-f16a-49b7-bd6f-c64a5498f023)

### Part 15.

I will use the gap statistics method with at least B=1,000 Monte Carlo
(“bootstrap”) samples to determine the best number of clusters K. The
candidate range of K will be 1-5. I will estimate the number of clusters
and calculate the mean average score (original score, unstandardized
version) for each cluster.

``` r
# computing gap statistic
set.seed(123)
gap_stat <- clusGap(df_std3, FUN = kmeans, nstart = 25,
                    K.max = 5, B = 1000)

#Result
print(gap_stat, method = "firstmax")
```

    ## Clustering Gap statistic ["clusGap"] from call:
    ## clusGap(x = df_std3, FUNcluster = kmeans, K.max = 5, B = 1000, nstart = 25)
    ## B=1000 simulated reference sets, k = 1..5; spaceH0="scaledPCA"
    ##  --> Number of clusters (method 'firstmax'): 5
    ##          logW   E.logW       gap     SE.sim
    ## [1,] 3.015212 3.142537 0.1273246 0.04625423
    ## [2,] 2.743448 2.888598 0.1451503 0.04018135
    ## [3,] 2.587569 2.758566 0.1709972 0.04076946
    ## [4,] 2.405837 2.646253 0.2404160 0.04238612
    ## [5,] 2.254979 2.544870 0.2898909 0.04343489

``` r
fviz_gap_stat(gap_stat)
```
![part15](https://github.com/schachz/CA_Schools_RegressionAnalysis/assets/100746204/722f30f6-3d02-4925-a54d-801cca4760c5)

![](CA_Schools_Regression_Project_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
# Average scores (original score, unstandardized version):

kmeans <-kmeans(df_std3, 5)
kmeans$centers
```

    ##       grades   stratio   calworks       lunch    computer expenditure
    ## 1  0.4117182 0.6534849 -0.6408968 -0.81677575 -0.09036846  -0.3298772
    ## 2 -2.4230628 0.7099506 -0.7750252 -0.79850924  0.78537102  -0.8359961
    ## 3  0.4117182 1.1392490  0.5524188  0.07261621  2.00211644  -0.3821516
    ## 4  0.4117182 0.9302638  1.4270811  1.54107863  1.19865662  -0.4474920
    ## 5  0.4117182 0.9691776  0.5382902  0.95442536  0.04286176  -0.3063445
    ##        income     english
    ## 1  0.48048903 -0.54448188
    ## 2  0.84325453 -0.35952186
    ## 3 -0.09514126  0.04920336
    ## 4 -0.76679173  2.31263481
    ## 5 -0.38060204  0.63984722

The optimal number of clusters is 5.
