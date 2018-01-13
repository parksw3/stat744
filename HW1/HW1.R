library(tidyverse)
library(ggplot2); theme_set(theme_bw())
library(lme4)
library(tsiR)
library(geosphere)
library(emdbook)

## http://datadryad.org/resource/doi:10.5061/dryad.r4q34

df <- read.csv("../data/measlesUKUS.csv")

transdf <- df %>% mutate(cases=ifelse(is.na(cases), 0, cases)) %>%
    rename(births=rec, time=decimalYear)

sumdf <- transdf %>%
    group_by(loc, country) %>%
    summarize(total=sum(cases))

syncdf <- transdf %>%
    group_by(loc, country, lon, lat) %>%
    do(normalized={
            r <- residuals(loess(log(cases+1)~time, data=.))
            (r-mean(r))/sd(r)
        }
    ) %>%
    left_join(sumdf)

city <- levels(syncdf$loc)
reslist <- vector('list', length(city)*(length(city)-1)/2)
for(i in 1:length(city)) {
    for(j in i:length(city)) {
        df1 <- syncdf %>% filter(loc==city[i])
        df2 <- syncdf %>% filter(loc==city[j])
        if ((df1$country==df2$country) && (i != j)) {
            reslist[[(i-1)*length(city)+j]] <- data.frame(
                country=df1$country,
                city1=city[i],
                city2=city[j],
                dist=distm(c(df1$lon, df1$lat), c(df2$lon, df2$lat), fun=distGeo),
                total=df1$total+df2$total,
                synchrony=sum(unlist(df1$normalized)*unlist(df2$normalized)),
                ccf=cor(unlist(df1$normalized), unlist(df2$normalized))
            )
            
        }
    }
}

synchrony <- reslist %>% 
    bind_rows

gg_synchrony <- ggplot(synchrony, aes(dist/1000, synchrony, col=country)) +
    geom_point(aes(shape=country), alpha=0.5) +
    geom_smooth(method="loess", se=FALSE, lwd=1.2) +
    scale_y_continuous("Synchrony measure") + 
    scale_x_log10("Distance between cities (km)") +
    scale_shape_manual(values=c(1, 2)) +
    theme(
        strip.background = element_blank(),
        legend.position=c(0.1, 0.85)
    )

ggsave("HW1_fig1.pdf", gg_synchrony, width=6, height=4)







