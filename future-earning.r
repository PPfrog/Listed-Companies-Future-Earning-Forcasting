#########################
## Earning Forecasting ##
#########################


# Data Preparation and packages loading
library(readr)
library(corrplot)
library(lmSubsets)
library(leaps)
library(broom)
library(ggplot2)

setwd("~/OneDrive - CUHK-Shenzhen/FIN 3380 Group Project 3")
df_total <- read_csv("Groupassign3.csv")
df_total$sale_at <- df_total$sale / df_total$at
df_total <- na.omit(df_total)
str(df_total)

# Split the training set and test set
set.seed(1)
train_index <- (df_total$fyear <= 2015)
ignore_var <- c("gvkey", "datadate", "permno", 
                "ib_p1", "at_p1", "actual_p1", 
                "consensus_p1", "ibesshrout_p1", "fyear")
df_train <- df_total[train_index, ]
df_test <- df_total[!train_index, ]
df_train <- df_train[, !(colnames(df_train) %in% ignore_var)]
df_test <- df_test[, !(colnames(df_test) %in% ignore_var)]



# df_train <- df_train[, (colnames(df_train) %in% keep_var)]
# df_test <- df_test[, (colnames(df_test) %in% keep_var)]

# Inspect the data frame
str(df_train); par(mfrow = c(1,1))
correlations <- cor(df_train, method="pearson")
corrplot(correlations, number.cex = .9, method = "circle", type = "full", tl.cex=0.8,tl.col = "black")

# Original liear model with all vars to forecast earning
form_earning <- as.formula("earn_p1 ~ earn + Research + SpecialItems + REC_AT + CHE_AT + sale_at")
lm_origin_earning <- lm(form_earning, data = df_train)
summary(lm_origin_earning)
# 
# Variable selection
lm_reduced_earning <- summary(regsubsets(earn_p1~.-AFE_p1-AFE, data = df_train, nvmax = 27))
write_csv(data.frame(lm_reduced_earning$outmat), "bestsubset_earning.csv")

# Drow the plot of the performance vs. # of vars
nvar_max <- length(lm_reduced_earning$adjr2)
par(mfrow=c(2,2), mai=c(0.8,0.8,0.4,0.4))
plot(x = 1:nvar_max, lm_reduced_earning$adjr2, xlab="Number of Variables", ylab = "Adjusted R^2",type = "b")
plot(x = 1:nvar_max, lm_reduced_earning$cp, xlab="Number of Variables", ylab = "Cp",type = "b")
plot(x = 1:nvar_max, lm_reduced_earning$bic, xlab="Number of Variables", ylab = "BIC",type = "b")
plot(x = 1:nvar_max, lm_reduced_earning$rss, xlab="Number of Variables", ylab = "RSS",type = "b")

# The final model selected by bestsubset
lm_reduced_earning <- lmSelect(earn_p1~.-AFE_p1-AFE, data= df_train, nbest = 1, penalty = "BIC")
lm_reduced_earning <- refit(lm_reduced_earning)
summary(lm_reduced_earning)
coef_reduced_earning <- tidy(summary(lm_reduced_earning))
write_csv(coef_reduced_earning, "coef_reduced_earning.csv")

# The final model selected by us
lm_selected_earning <- lm_origin_earning
coef_selected_earning <- tidy(summary(lm_selected_earning))
write_csv(coef_selected_earning, "coef_selected_earning.csv")

# Model evaluation by test set
# df_test <- df_test[-782, ] # this is an outlier
lm_selected_earning_pred <- as.numeric(predict(lm_selected_earning, newdata = df_test))
rsquare <- function(true, predicted) {
  sse <- sum((predicted - true)^2)
  sst <- sum((true - mean(true))^2)
  rsq <- 1 - sse / sst
  if (rsq < 0) rsq <- 0
  return (rsq)
}
earning_pred_rsq <- rsquare(df_test$earn_p1, lm_selected_earning_pred)
analyst_pred_rsq <- 1-sum(df_test$AFE_p1^2)/sum(df_test$earn_p1^2)
earning_pred_rsq
analyst_pred_rsq

earning_pred_mse <- (lm_selected_earning_pred - df_test$earn_p1)^2
analyst_pred_mse <- df_test$AFE_p1^2
mean(earning_pred_mse)
mean(analyst_pred_mse)

earning_pred_abs <- abs(lm_selected_earning_pred - df_test$earn_p1)
analyst_pred_abs <- abs(df_test$AFE_p1)
mean(earning_pred_abs)
mean(analyst_pred_abs)

error_total <- data.frame(
  model_pred_mse = earning_pred_mse,
  analyst_pred_mse = analyst_pred_mse,
  model_pred_abs = earning_pred_abs,
  analyst_pred_abs = analyst_pred_abs
)

error_mean<- data.frame(
  pred_rsqr_analyst = analyst_pred_rsq,
  pred_rsqr_model = earning_pred_rsq,
  ABFE_analyst = mean(analyst_pred_abs),
  ABFE_model = mean(earning_pred_abs),
  MSFE_analyst = mean(analyst_pred_mse),
  MSFE_model = mean(earning_pred_mse)
)

write_csv(error_total, "error_total.csv")
write_csv(error_mean, "error_mean.csv")

#########################
## AFE Forecasting ##
#########################

# Original liear model with all vars to forecast earning
form_afe <- as.formula("AFE_p1 ~ AFE + Research + SpecialItems + REC_AT + CHE_AT + sale_at")
lm_origin_afe <- lm(form_afe, data = df_train)
summary(lm_origin_afe)

# Variable selection
lm_reduced_afe <- summary(regsubsets(AFE_p1~.-earn_p1-earn, data = df_train, nvmax = 27))
lm_reduced_afe$outmat
write_csv(data.frame(lm_reduced_afe$outmat), "bestsubset_afe.csv")

# Drow the plot of the performance vs. # of vars
nvar_max <- length(lm_reduced_afe$adjr2)
par(mfrow=c(2,2), mai=c(0.8,0.8,0.4,0.4))
plot(x = 1:nvar_max, lm_reduced_afe$adjr2, xlab="Number of Variables", ylab = "Adjusted R^2",type = "b")
plot(x = 1:nvar_max, lm_reduced_afe$cp, xlab="Number of Variables", ylab = "Cp",type = "b")
plot(x = 1:nvar_max, lm_reduced_afe$bic, xlab="Number of Variables", ylab = "BIC",type = "b")
plot(x = 1:nvar_max, lm_reduced_afe$rss, xlab="Number of Variables", ylab = "RSS",type = "b")

# The final model selected by bestsubset
lm_reduced_afe <- lmSelect(AFE_p1~.-earn_p1-earn, data = df_train, nbest = 1, penalty = "BIC")
lm_reduced_afe <- refit(lm_reduced_afe)
summary(lm_reduced_afe)
coef_reduced_afe <- tidy(summary(lm_reduced_afe))
write_csv(coef_reduced_afe, "coef_reduced_afe.csv")

# The final model selected by us
lm_selected_afe <- lm_origin_afe
coef_selected_afe <- tidy(summary(lm_selected_afe))
write_csv(coef_selected_afe, "coef_selected_afe.csv")

# prediction visualization
qplot(lm_selected_earning_pred, df_test$earn_p1, shape = I(1)) + 
  geom_abline(slope = 1, intercept = 0, linetype = 2)+
  theme_bw(base_size = 12) +
  labs(title="Earning Prediction in Year 2017", 
       subtitle="With the extreme value",
       x="Predicted Earning", y="Actual Earning")

qplot(lm_selected_earning_pred[-782], df_test$earn_p1[-782], shape = I(1)) + 
  geom_abline(slope = 1, intercept = 0, linetype = 2)+
  theme_bw(base_size = 12) +
  labs(title="Earning Prediction in Year 2017", 
       subtitle="Without the extreme value",
       x="Predicted Earning", y="Actual Earning")


# table
keep <- c("earn","AFE","Research","SpecialItems","REC_AT","CHE_AT","sale_at")
df_selected = df_total[, keep]
stats_table <- matrix(ncol = 6,nrow = 0)
for (var in df_selected){
  stats_table <- rbind(stats_table, summary(var))
}
rownames(stats_table) <- keep
