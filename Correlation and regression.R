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
