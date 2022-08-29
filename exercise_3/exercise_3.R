# Run block by block

#------------
library(tidyverse)

data("nhanes", package = "mice")
head(nhanes)%>%knitr::kable()

#------------
bar_missing <- function(x){
    library(dplyr)
    library(reshape2)
    library(ggplot2)
    x %>%
        is.na %>%
        melt %>%
        ggplot(data = .,
               aes(x = Var2)) +
        geom_bar(aes(y=(..count..),fill=value),alpha=0.7,color="black")+scale_fill_manual(values=c("gold","red3"),name = "",
                                                                                          labels = c("Available","Missing"))+
        theme_minimal()+
        theme(axis.text.x = element_text(angle=45, vjust=0.5)) +
        labs(x = "Variables in Dataset",
             y = "Observations")+coord_flip()
}

nhanes%>%bar_missing()

#------------
nhanes$bmi=as.numeric(nhanes$bmi)

ggplot(nhanes,aes(x=bmi))+
    geom_density(fill="red",alpha=0.5)+
    theme_bw()

#------------
ggplot(nhanes,aes(x=chl))+
    geom_density(fill="blue",alpha=0.5)+
    theme_bw()

#------------
library(brms)

bform <- bf(bmi | mi() ~ age * mi(chl),family="skew_normal") +
    bf(chl | mi() ~ age) + set_rescor(FALSE)

mod<- brm(bform,data = nhanes,
          refresh = 0,
          cores = 4,
          iter = 2500, 
          warmup = 500, 
          chains = 1)

#------------
summary(mod)

#------------
theme_set(theme_bw())

plot(mod)

#------------
marginal_effects(mod, "age:chl")