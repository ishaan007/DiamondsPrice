---
title: "Diamonds and Price Prediction"
author: "Rushil Agarwal & Ishaan Arora"
date: "Monday, April 02, 2015"
output: ioslides_presentation
runtime: shiny
---

## All about it

This R Markdown presentation is made interactive using Shiny. The viewers of the presentation can change the assumptions underlying what's presented and see the results immediately. 

To learn more, see [Interactive Documents](http://rmarkdown.rstudio.com/authoring_shiny.html).

## Scatterplot review

```{r load data, echo=FALSE}
library(ggplot2)
library(GGally)
library(scales)
library(memisc)
library(gridExtra)
library(grid)
library(RColorBrewer)
library('bitops')
library('RCurl')
data(diamonds)
```

```{r Scatterplot review, echo=FALSE, warning=FALSE}
qplot(data = diamonds, x = carat, y = price,
      xlim = c(0, quantile(diamonds$carat, 0.99)),
      ylim = c(0, quantile(diamonds$price, 0.99)))+
  geom_point(fill = I('#F79420'), color = I('black'), shape=21)
```

## Linear Model
```{r Linear model, echo=FALSE, warning=FALSE}
ggplot(data = diamonds, aes (x = carat, y = price))+
  geom_point(color = '#F79420', alpha = 1/4)+
  stat_smooth(method = 'lm') + 
  scale_x_continuous(lim = c(0, quantile(diamonds$carat, 0.99))) +
  scale_y_continuous(lim = c(0, quantile(diamonds$price, 0.99)))
```

## sample 10,000 diamonds from the data set
```{r sample 10000, echo=FALSE, warning=FALSE}
set.seed(20022012)
diamond_samp <- diamonds[sample(1:length(diamonds$price), 10000), ]
ggpairs(diamond_samp, params = c(shape = I('.'), outlier.shape = I('.')))
```

## Connecting Demand and Price Distributions
```{r Connecting demand and distro}
plot1 <- qplot(data = diamonds, x = price, binwidth = 100, fill = I('#099DD9')) +
  ggtitle('Price')

plot2 <- qplot(data = diamonds, x = price, binwidth = 10.01, fill = I('#F79420')) +
  ggtitle('Price(log10)') +
  scale_x_log10()

grid.arrange(plot1, plot2, ncol=2)
```

## Scatterplot transformation
```{r Scatterplot transformation }
qplot(carat, price, data = diamonds) +
  scale_y_continuous(trans = log10_trans())+
  ggtitle('Price (log10) by carat')
```

## Further modification to scatterplot

```{r cuberoot transformation}
cuberoot_trans = function() trans_new('cuberoot', transform = function(x) x^(1/3),
                                      inverse = function(x) x^3)
```
```{r Use cuberoot_trans}
ggplot(aes(carat, price), data = diamonds) + 
  geom_point() + 
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat')
```


## Overplotting adjustment
```{r Overplotting Revisited}
ggplot(aes(carat, price), data = diamonds) + 
  geom_point() + 
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat')
```

## Price vc Carat & Clarity
```{r Price vs. Carat and Clarity}
ggplot(aes(x = carat, y = price, colour = clarity), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
    guide = guide_legend(title = 'Clarity', reverse = T,
    override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
    breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
    breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Clarity')
```

## Price Vs Carat and Cut
```{r Price vs. Carat and Cut}
ggplot(aes(x = carat, y = price, color = cut), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Cut', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Cut')
```

## Price vs Carat and color
```{r Price vs. Carat and Color}
ggplot(aes(x = carat, y = price, color = color), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Color', reverse = FALSE,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Color')
```

## Building the Linear Model
```{r Building the Linear Model}
m1 <- lm(I(log(price)) ~ I(carat^(1/3)), data = diamonds)
m2 <- update(m1, ~ . + carat)
m3 <- update(m2, ~ . + cut)
m4 <- update(m3, ~ . + color)
m5 <- update(m4, ~ . + clarity)
mtable(m1, m2, m3, m4, m5)
```


