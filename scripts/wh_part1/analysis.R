library(dplyr)
library(lmtest)
library(car)
library(glmnet)
library(Metrics)
library(MASS)
library(visreg)
library(segmented)
library(patchwork)
library(gridExtra)
library(mgcv)
library(plotly)
library(tidyverse)

# Load data
Master <- read.csv("C:/Users/khoul/OneDrive/Documents/WorldHappiness/master.0.CSV")

#### I. H1 TESTING : correlation between socioeco factors and WH ####
#### Corr ####
correlation <- cor(Master[c("Happiness.score", "GDP.pc", "Social.support", "Healthy.life.expectancy", 
                            "Freedom", "Perceptions.of.corruption", "Generosity")], 
                   use = "complete.obs")
correlation

heatmap(correlation, 
        symm = TRUE,  # symmetric
        main = "Correlation of H.Score and Socioeco var",
        xlab = "Variables", 
        ylab = "Variables",
        col = colorRampPalette(c("pink", "white", "purple"))(100),
        margins = c(13, 13),  # margins for labels
        cex.main = 1.5,  # main title
        cex.axis = 0.8,  # axis labels
        cex.lab = 0.8)  # axis titles
        

# Plotly heat
heatmaply <- plot_ly(z = correlation, 
                     type = "heatmap",
                     colorscale = "YlGnBu",  # Change the color scale here
                     reversescale = TRUE,
                     x = colnames(correlation), 
                     y = rownames(correlation)) %>%
  layout(title = "Correlation of H.Score and Socioeco var",
         xaxis = list(title = "Variables"),
         yaxis = list(title = "Variables"))

heatmaply

#### Reg (OLS) ####
Master <- na.omit(Master)
model <- lm(Happiness.score ~ GDP.pc + Social.support +
              Healthy.life.expectancy + Freedom + Perceptions.of.corruption +
              Generosity, data = Master)
summary(model)

# Reg model overall seems significant, gotta check for some bias doe

#### Hetero ####
bptest(model) # result proves heteroskedasticity in the model
plot(model, which = 1)

# Attempting to address heteroskedasticity, WLS model instead of OLS
residuals_squared <- residuals(model)^2

wls_model <- lm(Happiness.score ~ GDP.pc + Social.support +
                  Healthy.life.expectancy + Freedom + Perceptions.of.corruption +
                  Generosity,
                weights = 1/residuals_squared, data = Master)

# if you encounter an error when running WLSmodel for the first time
# run the original model again and go back to wls

summary(wls_model) # model significantly improved
bptest(wls_model) # WLS model handles hetero so well it becomes negligible, we now have homo

#### Normality of Resid and multiconllin ####

shapiro.test(resid(model)) #Shapiro says residuals are not 100% normally distributed
hist(resid(model), main = "Histogram of Residuals", xlab = "Residuals")

# sometimes it comes from multicollin, further diagnosis
vif_values <- vif(model)
print(vif_values)

# Check for multicollin
if (any(vif_values > 10)) {
  cat("Multicollinearity detected.\n")
} else {
  cat("No multicollinearity detected.\n")
}

# turns out there is no multicollin
# the following steps of this section, were originally attempted
# to deal with multicollin, but since there isn't any, 
# their remaining use is to build and evaluate our regression model

set.seed(123)  # For reproducibility
n <- nrow(Master)
train_indices <- sample(1:n, 0.8 * n)  # 80% for training, 20% for testing
train_data <- Master[train_indices, ]
test_data <- Master[-train_indices, ]

# Prepare predictor matrix and response vector for training data
X_train <- as.matrix(train_data[, c("GDP.pc", "Social.support", "Healthy.life.expectancy", "Freedom", "Perceptions.of.corruption", "Generosity")])
y_train <- train_data$Happiness.score

# Fit ridge regression model on training data
ridge_model <- glmnet(X_train, y_train, alpha = 0, lambda = 1)
summary(ridge_model)

# Perform cross-validation to tune lambda
cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0)
plot(cv_ridge)

# Prepare predictor matrix for testing data
X_test <- as.matrix(test_data[, c("GDP.pc", "Social.support", "Healthy.life.expectancy", "Freedom", "Perceptions.of.corruption", "Generosity")])

# Predict on testing set
y_pred <- predict(ridge_model, newx = X_test)

# Calculate predictions on the testing set
y_actual <- test_data$Happiness.score  # Actual values from the testing set

# Calculate Root Mean Squared Error (RMSE)
rmse <- rmse(y_actual, y_pred)
print(paste("RMSE:", rmse))

# Calculate R-squared (coefficient of determination)
rsquared <- 1 - sum((y_actual - y_pred)^2) / sum((y_actual - mean(y_actual))^2)
print(paste("R-squared:", rsquared))

# Obtain coefficients of the ridge model
coef(ridge_model)

#####




#### II. H2 TESTING: does marginal utility decrease at some point ####

#### Partial Regression plots ####
par(mfrow = c(3, 2))
visreg(model, "GDP.pc", scale = "response", main = "Partial Regression Plot: GDP per Capita")
visreg(model, "Social.support", scale = "response", main = "Partial Regression Plot: Social Support")
visreg(model, "Healthy.life.expectancy", scale = "response", main = "Partial Regression Plot: Healthy Life Expectancy")
visreg(model, "Freedom", scale = "response", main = "Partial Regression Plot: Freedom")
visreg(model, "Perceptions.of.corruption", scale = "response", main = "Partial Regression Plot: Perceptions of Corruption")
visreg(model, "Generosity", scale = "response", main = "Partial Regression Plot: Generosity")
par(mfrow = c(1, 1)) # so when running other graphs they get displayed properly
# All relationships are almost linear (linear enough)


#### Segmentation ####
# GDP pc
seg_model_gdp <- segmented(lm(Happiness.score ~ GDP.pc, data = Master), seg.Z = ~ GDP.pc)
summary(seg_model_gdp) #NA on U1 means the break point is not significant

# Social support 
seg_model_ss <- segmented(lm(Happiness.score ~ Social.support, data = Master),
                           seg.Z = ~ Social.support)
summary(seg_model_ss) #NA on U1 means the break point is not significant

# Healthy Life Expectancy
seg_model_hle <- segmented(lm(Happiness.score ~ Healthy.life.expectancy, data = Master),
                           seg.Z = ~ Healthy.life.expectancy)
summary(seg_model_hle) #NA on U1 means the break point is not significant

# Freedom of speech
seg_model_f <- segmented(lm(Happiness.score ~ Freedom, data = Master),
                           seg.Z = ~ Freedom)
summary(seg_model_f) #NA on U1 means the break point is not significant

# Perception of corruption
seg_model_poc <- segmented(lm(Happiness.score ~ Perceptions.of.corruption, data = Master),
                           seg.Z = ~ Perceptions.of.corruption)
summary(seg_model_poc) #NA on U1 means the break point is not significant

# Generosity
seg_model_g <- segmented(lm(Happiness.score ~ Generosity, data = Master),
                           seg.Z = ~ Generosity)
summary(seg_model_g) #NA on U1 means the break point is not significant

# Visualization of all segmentations

gdp_plot <- Master %>%
  ggplot(aes(x = GDP.pc, y = Happiness.score)) +
  geom_point() +
  labs(x = "GDP per Capita", y = "Happiness Score", title = "Segmented Regression for GDP per Capita") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  geom_smooth(data = Master, aes(x = GDP.pc), method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  theme_minimal()

ss_plot <- Master %>%
  ggplot(aes(x = Social.support, y = Happiness.score)) +
  geom_point() +
  labs(x = "Social Support", y = "Happiness Score", title = "Segmented Regression for Social Support") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  geom_smooth(data = Master, aes(x = Social.support), method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  theme_minimal()

hle_plot <- Master %>%
  ggplot(aes(x = Healthy.life.expectancy, y = Happiness.score)) +
  geom_point() +
  labs(x = "Healthy Life Expectancy", y = "Happiness Score", title = "Segmented Regression for Healthy Life Expectancy") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  geom_smooth(data = Master, aes(x = Healthy.life.expectancy), method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  theme_minimal()

f_plot <- Master %>%
  ggplot(aes(x = Freedom, y = Happiness.score)) +
  geom_point() +
  labs(x = "Freedom", y = "Happiness Score", title = "Segmented Regression for Freedom of Speech") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  geom_smooth(data = Master, aes(x = Freedom), method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  theme_minimal()

poc_plot <- Master %>%
  ggplot(aes(x = Perceptions.of.corruption, y = Happiness.score)) +
  geom_point() +
  labs(x = "Perceptions of Corruption", y = "Happiness Score", title = "Segmented Regression for Perception of Corruption") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  geom_smooth(data = Master, aes(x = Perceptions.of.corruption), method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  theme_minimal()

g_plot <- Master %>%
  ggplot(aes(x = Generosity, y = Happiness.score)) +
  geom_point() +
  labs(x = "Generosity", y = "Happiness Score", title = "Segmented Regression for Generosity") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  geom_smooth(data = Master, aes(x = Generosity), method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  theme_minimal()

# all
plots <- list(gdp_plot, ss_plot, hle_plot, f_plot, poc_plot, g_plot)
grid.arrange(grobs = plots, ncol = 2)

# investigate further
# arcsine transformation attempted but wasn't any helpful 

#### non linear regression ####
# General Additive Model
GAM <- gam(Happiness.score ~ s(GDP.pc) + s(Social.support) + s(Healthy.life.expectancy) + s(Freedom) + s(Perceptions.of.corruption) + s(Generosity), data = Master)

# Summary of the GAM model with original variables
summary(GAM) # our model is still linear showing no limit to contribution and no possible turn in relationships
par(mfrow = c(3, 2))
plot(GAM)
par(mfrow = c(1, 1))

#### End ####
