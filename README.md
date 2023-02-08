# ECON390
Duke University/Spring 2023/Econ 390:Tracking warzone economic activity in Ukraine/Group 6
############## 1. Packages ##############
library(haven)
library(tidyverse)
library(glmnet)
library(plm)
library(pls)
library(modelr)

############## 2. Working directory ##############

setwd("C:/Users/Буба/OneDrive/Рабочий стол/kse навчання/war/assignment 1")
getwd()

############## 3. Creating Data Objects  ############## 


#######
data_month <- read.csv("data_month_NL.csv")

# Create new date variable for plotting
# data_month$month <- paste('0', data_month$month, sep="")
data_month$Date <- paste(data_month$year,data_month$month,"01", sep="-")
data_month$Date <- as.Date(data_month$Date, format = "%Y-%m-%d")

# if we want a 2022 only dataset
data_month<-filter(data_month, name %in% c("Vinnytska", "Dnipropetrovska","Kirovohradska","Poltavska","Cherkaska"))

#######
regGDP <- read_dta("Region by year GDP panel.dta")
regGDP[regGDP$year == 2021, 3] <- NA 

hist(regGDP$GDP)

data_tidy_YR_test <- data_month |> group_by(year, reg, name) |> 
  mutate(NLI = nl_sum_4/(area_sq_km - 0.141*nl_nodata_4),
         NLI2 = nl_mean_4/nl_std_4)
data_tidy_YR_test <- filter(data_tidy_YR_test, year >= 2012 )

# arrange order of columns for easier viewing
data_tidy_YR_test <- data_tidy_YR_test |> 
  select(c(year, month, reg, name, area_sq_km, NLI, NLI2, tw_count), everything())


data_tidy_YR_test <- data_tidy_YR_test |> 
  mutate(across(c(NLI:tw_n_settlements), ~log(.x), .names = '{.col}'))

data_tidy_YR_test[data_tidy_YR_test == -Inf] <- 0

# Estimation Portion 2012-2020 (2021 we have monthly levels)
data_tidy_YR <- filter(data_tidy_YR_test, year < 2021) |>
  group_by(year, reg, name) |>
  summarise(across(c(NLI:tw_n_settlements), ~mean(.x, na.rm=TRUE), .names = '{.col}')) |>
  mutate(across(c(NLI:tw_n_settlements), ~round(.x, digits=3), .names = '{.col}'))

# Add yearly levels of GDP
data_tidy_YR <- left_join(data_tidy_YR, regGDP, by=c("year", "reg"))

data_tidy_YR$GDP <- log(data_tidy_YR$GDP)
colnames(data_tidy_YR)[25] <- "lng"


############### 4. Charting ###############
# NL monthly data



# NL yearly data
# Coding question: 
# How do plot the evolution over time of a variable across all region?
# Check use of facet_wrap
ggplot(filter(data_tidy_YR, year <=2021), aes(x=year, y=nl_mean_4))+
  geom_smooth(method = "lm") + facet_wrap(~name, scales = 'free_y') + 
  ylab("Average Luminosity")

# Coding question:
# How do we produce a series of graphs over time?
# How do we add simply OLS line to check the relationship between NL and GDP?
# Check us of geom_smooth(method="lm"). What other methods are available? How about Loess
# Scatter plots lights vs. GDP
ggplot(filter(data_tidy_YR, year <=2020), aes(x=log(nl_mean_4), y=lng))+
  geom_point() + geom_smooth(method="lm")+
  facet_wrap(~year, scales = 'free')

ggplot(filter(data_tidy_YR, year <=2020), aes(x=log(nl_mean_4), y=lng))+
  geom_point() + geom_smooth(method="lm")+ 
  geom_text(aes(label = name, color=name), check_overlap = TRUE)+
  theme(legend.position="none")


library(plm)
library(AER)
library(stargazer)


data_month1 <- read.csv("data_month_NL.csv")

# Create new date variable for plotting
# data_month$month <- paste('0', data_month$month, sep="")
data_month1$Date <- paste(data_month1$year,data_month1$month,"01", sep="-")
data_month1$Date <- as.Date(data_month1$Date, format = "%Y-%m-%d")
regGDP <- read_dta("Region by year GDP panel.dta")
regGDP[regGDP$year == 2021, 3] <- NA 

data_tidy_YR_test1 <- data_month1 |> group_by(year, reg, name) |> 
  mutate(NLI = nl_sum_4/(area_sq_km - 0.141*nl_nodata_4),
         NLI2 = nl_mean_4/nl_std_4)
data_tidy_YR_test1 <- filter(data_tidy_YR_test1, year >= 2012 )

# arrange order of columns for easier viewing
data_tidy_YR_test1 <- data_tidy_YR_test1 |> 
  select(c(year, month, reg, name, area_sq_km, NLI, NLI2, tw_count), everything())


data_tidy_YR_test1 <- data_tidy_YR_test1 |> 
  mutate(across(c(NLI:tw_n_settlements), ~log(.x), .names = '{.col}'))

data_tidy_YR_test1[data_tidy_YR_test1 == -Inf] <- 0

# Estimation Portion 2012-2020 (2021 we have monthly levels)
data_tidy_YR1 <- filter(data_tidy_YR_test1, year < 2021) |>
  group_by(year, reg, name) |>
  summarise(across(c(NLI:tw_n_settlements), ~mean(.x, na.rm=TRUE), .names = '{.col}')) |>
  mutate(across(c(NLI:tw_n_settlements), ~round(.x, digits=3), .names = '{.col}'))

# Add yearly levels of GDP
data_tidy_YR1 <- left_join(data_tidy_YR1, regGDP, by=c("year", "reg"))

panel_data <- filter(data_tidy_YR1, (name %in% c("Cherkaska", "Kirovohradska", "Dnipropetrovska", "Poltavska", "Vinnytska")))

panel_data$GDP <- log(panel_data$GDP)
colnames(panel_data)[25] <- "lng"

### Time and Unit FE
NL_tefe_mod <- plm(lng ~ NLI,
                   data = panel_data,
                   index = c("name", "year"),
                   model = "within",
                   effect = "twoways")
summary(NL_tefe_mod)
coeftest(NL_tefe_mod, vcov = vcovHC, type = "HC1")

# Time and Unit FE == OLS with dummies for Units and Years
NL_TU_lm_mod <- lm(lng ~ NLI + name+as.factor(year)-1 , data = panel_data)
summary(NL_TU_lm_mod)



#########################################
#########################################
library(leaps)
data_trends <- readRDS("data_NL_GT.rds")
data_trends<-filter(data_trends, name %in% c("Vinnytska","Dnipropetrovska","Kirovohradska","Poltavska","Cherkaska"))

# Add yearly levels of GDP
data_trends <- left_join(data_trends, regGDP, by=c("year", "reg"))
data_trends$GDP <- log(data_trends$GDP)
colnames(data_trends)[12] <- "lng"

regfit.full <- regsubsets(lng ~ Labor_index + Holiday_index + Mercedes_index + Emigration_index+ Washing_machine_index+Unemployment_benefits_index +MBA_degree_index ,data_trends)
summary(regfit.full)

reg.summary <- summary(regfit.full)
which.max(reg.summary$adjr2)
which.min(reg.summary$bic)
which.min(reg.summary$cp)
### Forward and Backward Stepwise Selection

###
regfit.fwd <- regsubsets(lng ~ Labor_index + Holiday_index + Mercedes_index + Emigration_index+ Washing_machine_index+Unemployment_benefits_index +MBA_degree_index, data = data_trends,
                         method = "forward")
regfit.fwd.summary<-summary(regfit.fwd)
which.max(regfit.fwd.summary$adjr2)
which.min(regfit.fwd.summary$bic)
which.min(regfit.fwd.summary$cp)

regfit.bwd <- regsubsets(lng ~ Labor_index + Holiday_index + Mercedes_index + Emigration_index+ Washing_machine_index+Unemployment_benefits_index +MBA_degree_index, data = data_trends,
                         method = "backward")
regfit.bwd.summary<-summary(regfit.bwd)
which.max(regfit.bwd.summary$adjr2)
which.min(regfit.bwd$bic)
which.min(regfit.bwd.summary$cp)
###
coef(regfit.full, 6)
coef(regfit.full, 5)
coef(regfit.fwd, 6)
coef(regfit.fwd, 5)
coef(regfit.bwd, 6)


### Choosing Among Models Using the Validation-Set Approach and Cross-Validation

###
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(data_trends),
                replace = TRUE)
test <- (!train)
###
regfit.best <- regsubsets(lng ~ Labor_index + Holiday_index + Mercedes_index + Emigration_index+ Washing_machine_index+Unemployment_benefits_index +MBA_degree_index,
                          data = data_trends[train, ], nvmax=7)
###
test.mat <- model.matrix(lng ~ Labor_index + Holiday_index + Mercedes_index + Emigration_index+ Washing_machine_index+Unemployment_benefits_index +MBA_degree_index, data = data_trends[test, ])
###
val.errors <- rep(NA, 7)
for (i in 1:7) {
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((data_trends$lng[test] - pred)^2)
}
###
val.errors
which.min(val.errors)
coef(regfit.best, 2)
###
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}
###
###

coef(regfit.best, 2)
###
k <- 5
n <- nrow(data_trends)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 7,
                    dimnames = list(NULL, paste(1:7)))
###
for (j in 1:k) {
  best.fit <- regsubsets(lng ~ Labor_index + Holiday_index + Mercedes_index + Emigration_index+ Washing_machine_index+Unemployment_benefits_index +MBA_degree_index,
                         data = data_trends[folds != j, ],
                         nvmax = 7)
  for (i in 1:7) {
    pred <- predict(best.fit, data_trends[folds == j, ], id = i)
    cv.errors[j, i] <-
      mean((data_trends$lng[folds == j] - pred)^2)
  }
}
###
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")
###
reg.best <- regsubsets(lng ~ Labor_index + Holiday_index + Mercedes_index + Emigration_index+ Washing_machine_index+Unemployment_benefits_index +MBA_degree_index, data = data_trends,
                       nvmax = 7)
coef(reg.best, 6)

## Ridge Regression and the Lasso

###
x <- model.matrix(lng ~ Labor_index + Holiday_index + Mercedes_index + Emigration_index+ Washing_machine_index+Unemployment_benefits_index +MBA_degree_index, data_trends)[, -1]
y <- data_trends$lng
### Ridge Regression

###
library(glmnet)
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
###
dim(coef(ridge.mod))
###
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))
###
ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))
###
predict(ridge.mod, s = 50, type = "coefficients")[1:7, ]
###
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]
###
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0,
                    lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])
mean((ridge.pred - y.test)^2)
###
mean((mean(y[train]) - y.test)^2)
###
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test, ])
mean((ridge.pred - y.test)^2)
###
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ],
                      exact = T, x = x[train, ], y = y[train])
mean((ridge.pred - y.test)^2)
lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = T, type = "coefficients",
        x = x[train, ], y = y[train])[1:7, ]
###
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
###
ridge.pred <- predict(ridge.mod, s = bestlam,
                      newx = x[test, ])
mean((ridge.pred - y.test)^2)
###
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:7, ]

### The Lasso

###
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
                    lambda = grid)
plot(lasso.mod)
###
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam,
                      newx = x[test, ])
mean((lasso.pred - y.test)^2)
###
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients",
                      s = bestlam)[1:8, ]
lasso.coef
lasso.coef[lasso.coef != 0]
