# Correlation and regression in R

library(openintro)
library(dplyr)
library(ggplot2)

options(scipen = 999)
## Loading data

data("ncbirths")

## Scatterplot of weight vs. weeks

str(ncbirths)

ggplot(ncbirths, aes(weeks, weight)) + geom_point()

## Boxplot of weight vs. weeks

ggplot(ncbirths, aes(x = cut(weeks, breaks = 5), weight)) + 
  geom_boxplot()

## Mammals scatterplot

data("mammals")

str(mammals)

ggplot(mammals, aes(x = BodyWt, y = BrainWt)) + geom_point()

## Baseball player scatterplot

data("mlbBat10")

str(mlbBat10)

ggplot(mlbBat10, aes(x = OBP, y = SLG)) + geom_point()

## Body dimensions scatterplot

data("bdims")

str(bdims)

ggplot(bdims, aes(hgt, wgt, col = factor(sex))) + geom_point()

## Smoking scatterplot

data("smoking")

str(smoking)

ggplot(smoking, aes(age, amtWeekdays)) + geom_point()

## Scatterplot with coord_trans()

ggplot(mammals, aes(x = BodyWt, y = BrainWt)) + geom_point() +
  coord_trans(x = "log10", y = "log10")

ggplot(mammals, aes(x = BodyWt, y = BrainWt)) + geom_point() +
  scale_x_log10() + scale_y_log10()

## Filter for AB greater than or equal to 200

ab_gt_200 <- mlbBat10 %>%
  filter(AB >= 200)

## Scatterplot of SLG vs. OBP

ggplot(ab_gt_200, aes(x = OBP, y = SLG)) +
  geom_point()

## Identify the outlying player
ab_gt_200 %>%
  filter(OBP < 0.2)

## Compute correlation

ncbirths %>%
  summarize(N = n(), r = cor(weight, mage))

## Compute correlation for all non-missing pairs - not considering NAs

ncbirths %>%
  summarize(N = n(), r = cor(weight, mage, use = "pairwise.complete.obs"))

## Run this and look at the plot

ggplot(data = mlbBat10, aes(x = OBP, y = SLG)) +
  geom_point()

## Correlation for all baseball players

mlbBat10 %>%
  summarize(N = n(), r = cor(OBP, SLG))

## Run this and look at the plot

mlbBat10 %>% 
  filter(AB > 200) %>%
  ggplot(aes(x = OBP, y = SLG)) + 
  geom_point()

## Correlation for all players with at least 200 ABs

mlbBat10 %>%
  filter(AB >= 200) %>%
  summarize(N = n(), r = cor(OBP, SLG))

## Run this and look at the plot

ggplot(data = bdims, aes(x = hgt, y = wgt, color = factor(sex))) +
  geom_point() 

## Correlation of body dimensions

bdims %>%
  group_by(sex) %>%
  summarize(N = n(), r = cor(wgt, hgt))

## Correlation among mammals, with and without log

mammals %>%
  summarize(N = n(), r = cor(BodyWt, BrainWt), 
            r_log = cor(log(BodyWt), log(BrainWt)))

## Scatterplot with regression line

ggplot(bdims, aes(hgt, wgt)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)
<<<<<<< HEAD
## bdims_summary

bdims_summary<- bdims %>%
  summarize(N = n(), r = cor(hgt, wgt), mean_hgt = mean(hgt),
            sd_hgt = sd(hgt), mean_wgt = mean(wgt), sd_wgt = sd(wgt))

bdims_summary

## Add slope and intercept

bdims_summary %>%
  mutate(slope = r*(sd_hgt/sd_wgt),
         intercept = mean_wgt - slope * mean_hgt)

## Linear model for weight as a function of height

lm(wgt~hgt, data = bdims)

## Linear model for SLG as a function of OBP

lm(SLG~OBP, data = mlbBat10)

## Log-linear model for body weight as a function of brain weight
data("mammals")

lm(log(BodyWt)~log(BrainWt), data = mammals)

## Show the coefficients

mod <- lm(wgt ~ hgt, data = bdims)

coef(mod)

## Show the full output

summary(mod)

## Mean of weights equal to mean of fitted values?

mean(bdims$wgt) == mean(fitted.values(mod))

## Mean of the residuals

mean(residuals(mod))

## Load broom

library(broom)

## Create bdims_tidy

bdims_tidy<- augment(mod)

## Glimpse the resulting data frame

glimpse(bdims_tidy)

## Print ben

ben<- data.frame(wgt = 74.8, hgt = 182.8)

## Predict the weight of ben

predict(mod, ben)

## Add the line to the scatterplot


coefs<- data.frame(intercept = -105.011254, hgt = 1.017617)


data("bdims")

ggplot(data = bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_abline(data = coefs, 
              aes(intercept = intercept, slope = hgt),  
              color = "dodgerblue")

## View summary of model

summary(mod)

## Compute the mean of the residuals

mean(residuals(mod))

## Compute RMSE

sqrt(sum(residuals(mod)^2)/df.residual(mod))

## Compute R-squared

bdims_tidy %>%
  summarize(var_y = var(wgt), var_e = var(.resid)) %>%
  mutate(R_squared = 1 - (var_e/var_y))

## Create nontrivial_players

nontrivial_players<- mlbBat10 %>%
  filter(AB >= 10 & OBP < 0.5)

## Fit model to new data

mod_cleaner<- lm(SLG~OBP, data = nontrivial_players)

## View model summary

summary(mod_cleaner)

## Visualize new model

ggplot(nontrivial_players, aes(OBP, SLG)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)

=======
>>>>>>> master
