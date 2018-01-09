library(tidyverse)
library(ggplot2); theme_set(theme_bw())
library(readr)

cdat <- read_csv("../data/Carbohydrate_diet.csv")
## print(cdat, n=8)

gg0 <- (ggplot(cdat, 
               aes(weight, carbohydrate))
        + geom_point()
)

print(gg0)
print(gg0 + geom_smooth(method="loess")
          + geom_smooth(method="lm", col="red"))

cdat <- mutate(cdat, f_age=cut_number(age,3))

gg1 <- (ggplot(cdat, aes(weight, carbohydrate, colour=f_age))
    + geom_point(aes(shape=f_age), size=4)        
)

print(gg1)
