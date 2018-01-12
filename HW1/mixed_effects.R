library(tidyverse)
library(lme4)
library(tsiR)

## http://datadryad.org/resource/doi:10.5061/dryad.r4q34

df <- read.csv("../data/measlesUKUS.csv")

cdf <- df %>% mutate(cases=ifelse(is.na(cases), 0, cases)) %>%
    group_by(loc, country) %>%
    mutate(ccase=cumsum(cases), crec=cumsum(rec))

rho_model <- cdf %>%
    group_by(loc, country) %>%
    do(data.frame(
        loc=.$loc,
        country=.$country,
        year=.$year,
        decimalYear=.$decimalYear,
        X=.$crec,
        Yhat=predict(loess(ccase~crec, data=., 
                           se=T, family='gaussian', degree=1, model=T))
    ))

rho <- rho_model %>%
    mutate(rho=derivative(X, Yhat))

tdf <- merge(cdf, rho) %>%
    mutate(true.cases=cases/rho,
           Z=residual.births(rho, Yhat, ccase)) %>%
    group_by(country, loc) %>%
    do(data.frame(
        biweek=head(.$biweek,-1),
        Inew=log(tail(.$true.cases,-1)+1),
        Iminus=log(head(.$true.cases,-1)+1),
        Sminus=log(head(0.035*.$pop+.$Z,-1))
    ))

fit <- lmer(Inew~ -1 + as.factor(biweek) + Iminus + offset(Sminus)
            + country + (as.factor(biweek)|loc), 
            data=tdf)

