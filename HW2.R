library(tidyverse)
library(readr)
library(ggplot2); theme_set(theme_bw(base_size = 12,
                                     base_family = "Times"))
library(directlabels)
library(gridExtra)

if (.Platform$OS.type=="windows") {
    windowsFonts(Times=windowsFont("Times"))
} 

scale_colour_discrete <- function(...,palette="Dark2") scale_colour_brewer(...,palette=palette)
scale_fill_discrete <- function(...,palette="Dark2") scale_fill_brewer(...,palette=palette)

save <- FALSE

data <- readr::read_csv("https://bbolker.github.io/stat744/data/vaccine_data_online.csv")

data2 <- data %>%
    filter(disease != "Mumps") %>%
    mutate(vaccine=ifelse(is.na(as.logical(vaccine)), TRUE, FALSE)) %>%
    group_by(disease) %>%
    mutate(cv=factor(cumsum(vaccine)))

data3 <- data2 %>%
    group_by(disease, cv) %>%
    summarize(mc=mean(cases)) 

g1 <- ggplot(data3, aes(as.numeric(cv), mc, group=disease, col=disease)) +
    geom_point() +
    geom_line(aes(lty=disease)) +
    geom_dl(aes(label=disease), method = list(dl.trans(x = x - .3), "first.bumpup")) +
    scale_x_continuous(breaks=c(1, 2, 3), labels=c("Prevaccination", "First introduction", "Second introduction"), expand=c(0,0)) +
    scale_y_log10(position = "right") +
    annotate(geom = "text", x = 3, y = 5e5,
             label = "  Average cases\n  per year",
             hjust = 0) +
    theme(
        legend.position = "none",
        axis.title=element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(1,4.5,1,5.5), "lines")
    )

gt1 <- ggplotGrob(g1)
gt1$layout$clip[gt1$layout$name == "panel"] <- "off"

if (save) ggsave("HW2_fig1.pdf", gt1, width=8, height=6)

data_time <- data2 %>% 
    filter(vaccine != FALSE) %>%
    mutate(cv=as.factor(as.numeric(as.character(cv))-1)) %>%
    bind_rows(data2)

g2 <- ggplot(data_time, aes(year, cases)) +
    geom_line(aes(lty=cv)) +
    geom_point(data=data2 %>% filter(vaccine != FALSE)) +
    geom_dl(data=data2 %>% filter(vaccine != FALSE), aes(label=year), method=list(dl.trans(x= x + .3, y = y - .4), "first.bumpup")) +
    scale_y_log10(expand=c(0.1, 0.1)) +
    scale_linetype_manual(values=c(2, 1, 1)) +
    facet_grid(disease~., scale="free_y") +
    theme(
        legend.position = "none",
        strip.background = element_blank(),
        panel.spacing = unit(0, units="cm")
    )

if (save) ggsave("HW2_fig2.pdf", g2, width=6, height=8)


