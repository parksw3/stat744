library(tidyverse)
library(ggplot2); theme_set(theme_bw(base_size = 12,
                                     base_family = "Times"))
library(geosphere)
library(ggmap)
library(gridExtra)

if (.Platform$OS.type=="windows") {
    windowsFonts(Times=windowsFont("Times"))
} 

save <- FALSE

scale_colour_discrete <- function(...,palette="Dark2") scale_colour_brewer(...,palette=palette)
scale_fill_discrete <- function(...,palette="Dark2") scale_fill_brewer(...,palette=palette)

## http://datadryad.org/resource/doi:10.5061/dryad.r4q34
df <- read.csv("../data/measlesUKUS.csv")

transdf <- df %>% mutate(cases=ifelse(is.na(cases), 0, cases)) %>%
    rename(births=rec, time=decimalYear) %>%
    filter(loc != "READING.US")

sumdf <- transdf %>%
    group_by(loc, country, lon, lat) %>%
    summarize(total=sum(cases))

syncdf <- transdf %>%
    group_by(loc, country, lon, lat) %>%
    do(normalized={
            r <- residuals(loess(log(cases+1)~time, data=.))
            (r-mean(r))/sd(r)
        }
    ) %>%
    left_join(sumdf)

city <- unique(syncdf$loc)
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

if (save) ggsave("HW1_fig1.pdf", gg_synchrony, width=6, height=4)

uk <- map_data(map = "world", region = "UK") %>%
    filter(!(subregion %in% c("Northern Ireland", "Scotland")), lat < 55.3)

london_synchrony <- synchrony %>% 
    filter(city1=="LONDON" | city2=="LONDON") %>%
    select(-total, -country) %>%
    group_by(city1, city2) %>%
    mutate(
        loc=if(city1=="LONDON") {city2} else {city1}
    ) %>%
    bind_rows(data.frame(
        loc="LONDON",
        synchrony=sum((unlist(filter(syncdf, loc=="LONDON")$normalized))^2)
    )) %>%
    left_join(sumdf, by="loc")

london_minmax <- london_synchrony %>%
    group_by %>%
    filter(loc != "LONDON") %>%
    summarize(
        min=.[which.min(ccf),][["loc"]],
        max=.[which.max(ccf),][["loc"]]
    ) %>%
    unlist

london_line <- london_synchrony %>%
    filter(loc %in% london_minmax) %>%
    rename(target.lon=lon, target.lat=lat) %>%
    mutate(lon=-0.105, lat=51.517)

ggplot(london_synchrony, aes(lon, lat)) +
    geom_polygon(data = uk, aes(x=long, y = lat, group = group), alpha=0.2) + 
    geom_segment(data=london_line, aes(xend=target.lon, yend=target.lat, col=synchrony), 
                 alpha=0.4, lwd=1.5, lty=2) +
    geom_point(aes(size=total, col=synchrony), alpha=0.9) +
    geom_text(data=london_line, aes(x=target.lon, y=target.lat, label=loc), vjust=2.8) +
    geom_text(x=-0.105, y=51.517, label="LONDON", vjust=3.7) +
    scale_size_continuous(range=c(3,20), guide=FALSE) +
    scale_color_gradient(low="#56B4E9", high="#D55E00") +
    theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.9, 0.8)
    )

ggplot(filter(transdf, loc %in% c(london_minmax)), aes(time, cases)) +
    geom_line(data=filter(transdf, loc=="LONDON") %>% select(-loc), lty=2) +
    geom_line(aes(col=loc)) +
    scale_y_log10() +
    facet_grid(loc~., scale="free")








