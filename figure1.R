library(tidyr)
library(ggplot2)

rm(list=ls()) 
trend = read.csv("wwf_env.csv", header = T)
colnames(trend)[1:3] = c("week","ENV","WWF")
trend[,1] = as.Date(trend[,1])
trend = gather(trend, key="variable", value = value, -week)
colnames(trend)[2]="NGO"

ggplot(data=trend, aes(x=week, y=value)) + 
    geom_line(aes(linetype=NGO)) + labs(x = "Year", y = "Weekly Google Trend", fill = "NGO") + theme_bw()
