## Slopegraphs
## Source: http://motioninsocial.com/tufte/#slopegraph
library(ggplot2)
library(ggthemes)
library(devtools)
library(RCurl)
library(plyr)
source_url('https://raw.githubusercontent.com/jkeirstead/r-slopegraph/master/slopegraph.r')
d <- read.csv(text = getURL('https://raw.githubusercontent.com/jkeirstead/r-slopegraph/master/cancer_survival_rates.csv'))
df <- build_slopegraph(d, x='year', y='value', group='group', method='tufte', min.space=0.05)
df <- transform(df, x=factor(x, levels=c(5,10,15,20),
                             labels=c("5 years", "10 years", "15 years", "20 years")), y=round(y))
plot_slopegraph(df) + labs(title="Estimates of % survival rates") +
  theme_tufte(base_size=16, ticks=F) + theme(axis.title=element_blank())