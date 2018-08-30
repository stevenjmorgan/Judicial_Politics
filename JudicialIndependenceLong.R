# This code cleans 

rm(list=ls())

setwd("C:/Users/sum410/Dropbox/PSU2018-2019/Fall2018/Judicial/Memo1")

library(ggplot2)
library(stargazer)

# Read in data (R format)
load('LJI-estimates-20140422.RData')

# Create dataframe to store calculations
lji <- as.data.frame(matrix(nrow = 4, ncol = 4))
colnames(lji) <- c('Year', 'Mean', 'Median', 'Range')
lji$Year<- c(1980, 1990, 2000, 2010)

# Write function to calculate range (range() returns min. and max. value)
range.calc <- function(x) return(diff(range(x))) 

# Iterate through rows of dataframe and calculate descriptives subset by year
for (i in 1:nrow(lji)) {
  lji$Mean[i] <- mean(x$X.LJI.[which(x$X.year. == lji$Year[i])])
  lji$Median[i] <- median(x$X.LJI.[which(x$X.year. == lji$Year[i])])
  lji$Range[i] <- range.calc(x$X.LJI.[which(x$X.year. == lji$Year[i])])
}

lji$Year <- as.character(lji$Year)
stargazer(lji, summary = FALSE, type = 'text', rownames = FALSE) #latex


# Subset by US, Argentina, Germany, and Bolivia
states <- c('United States of America', 'Argentina', 'Germany', 'Bolivia')
#west <- x[which(x$X.country. %in% states),]

plot(x$X.year.[which(x$X.country. == states[1])], 
     x$X.LJI.[which(x$X.country. == states[1])])

plot(x$X.year.[which(x$X.country. == states[2])], 
     x$X.LJI.[which(x$X.country. == states[2])])

plot(x$X.year.[which(x$X.country. == states[3])], 
     x$X.LJI.[which(x$X.country. == states[3])])

plot(x$X.year.[which(x$X.country. == states[4])], 
     x$X.LJI.[which(x$X.country. == states[4])])

ggplot() + 
  geom_line(data = x[which(x$X.country. == states[1]),], aes(x = X.year., y = X.LJI., color = states[1])) +
  geom_line(data = x[which(x$X.country. == states[2]),], aes(x =  X.year., y = X.LJI., color = states[2])) +
  geom_line(data = x[which(x$X.country. == states[3]),], aes(x =  X.year., y = X.LJI., color = states[3])) +
  geom_line(data = x[which(x$X.country. == states[4]),], aes(x =  X.year., y = X.LJI., color = states[4])) +
  xlab('') +
  ylab('Judicial Independence Score') +
  scale_color_manual(values = c('United States of America' = '#009E73', 'Argentina' = '#0072B2',
                     'Germany' = '#E69F00', 'Bolivia' = '#D55E00')) +
  labs(color = '')
  
ggplot() + 
  geom_point(data = x[which(x$X.country. == states[1]),], aes(x = X.year., y = X.LJI., color = states[1])) +
  geom_point(data = x[which(x$X.country. == states[2]),], aes(x =  X.year., y = X.LJI., color = states[2])) +
  geom_point(data = x[which(x$X.country. == states[3]),], aes(x =  X.year., y = X.LJI., color = states[3])) +
  geom_point(data = x[which(x$X.country. == states[4]),], aes(x =  X.year., y = X.LJI., color = states[4])) +
  xlab('') +
  ylab('Judicial Independence Score') +
  scale_color_manual(values = c('United States of America' = '#009E73', 'Argentina' = '#0072B2',
                                'Germany' = '#E69F00', 'Bolivia' = '#D55E00')) +
  labs(color = '')

