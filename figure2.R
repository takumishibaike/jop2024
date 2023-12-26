library(plyr)
library(ggplot2)
library(weights)

rm(list=ls()) 

d = read.csv("clean_data.csv", header = T) # all

### Subgroup

# Resource Users (earlier version of the paper used the term "appropriators")
appropriators = rbind(d[which(d$Q34=="A few times a week"),], d[which(d$Q34=="Everyday"),])

# Preservationists
preservationists = d[which(d$Q28=="Very important"&d$Q30=="Very important"&d$Q34!="A few times a week"&d$Q34!="Everyday"),] 

# Mass Public
x=c(appropriators$id,preservationists$id)
x=(unique(x))
mass = d
for (i in 1:length(x)) {
    mass = mass[-which(mass$id==x[i]),]
}


##### Average responses

### Figure 2 (a): average responses based on information sources

sum_d = data.frame()

d = preservationists
for (i in c(1,2)) {
    x = d[which(d$organization_var==i),]
    sum_d = rbind(sum_d,data.frame("mean"=wtd.mean(x$Q17, x$weight),"sd"=sqrt(wtd.var(x$Q17, x$weight)),"se"=sqrt(wtd.var(x$Q17, x$weight))/sqrt(length(x$Q17)),"org"=i,"type"="pres"))
}

d = appropriators
for (i in c(1,2)) {
    x = d[which(d$organization_var==i),]
    sum_d = rbind(sum_d,data.frame("mean"=wtd.mean(x$Q17, x$weight),"sd"=sqrt(wtd.var(x$Q17, x$weight)),"se"=sqrt(wtd.var(x$Q17, x$weight))/sqrt(length(x$Q17)),"org"=i,"type"="appr"))
}

d = mass
for (i in c(1,2)) {
    x = d[which(d$organization_var==i),]
    sum_d = rbind(sum_d,data.frame("mean"=wtd.mean(x$Q17, x$weight),"sd"=sqrt(wtd.var(x$Q17, x$weight)),"se"=sqrt(wtd.var(x$Q17, x$weight))/sqrt(length(x$Q17)),"org"=i,"type"="mass"))
}


p= ggplot(sum_d, aes(x=type, y=mean, fill=factor(sum_d$org))) + 
    geom_bar(stat="identity", color="black", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), width=.2, position=position_dodge(.9)) +  labs(x = "Respondent Groups") + scale_x_discrete(labels = c("Preservationists","Resource Users","Mass Public"))+ coord_cartesian(ylim=c(3,7)) + scale_fill_grey(start = 0.4, end = .9, name = "NGO", labels = c("WWF", "ENV"))+  theme_bw()
p

### Figure 2 (b): average responses based on frames

sum_d = data.frame()

d = preservationists
for (i in c(1,2,3)) {
    x = d[which(d$frame_var==i),]
    sum_d = rbind(sum_d,data.frame("mean"=wtd.mean(x$Q17, x$weight),"sd"=sqrt(wtd.var(x$Q17, x$weight)),"se"=sqrt(wtd.var(x$Q17, x$weight))/sqrt(length(x$Q17)),"frame"=i,"type"="pres"))
}

d = appropriators
for (i in c(1,2,3)) {
    x = d[which(d$frame_var==i),]
    sum_d = rbind(sum_d,data.frame("mean"=wtd.mean(x$Q17, x$weight),"sd"=sqrt(wtd.var(x$Q17, x$weight)),"se"=sqrt(wtd.var(x$Q17, x$weight))/sqrt(length(x$Q17)),"frame"=i,"type"="appr"))
}

d = mass
for (i in c(1,2,3)) {
    x = d[which(d$frame_var==i),]
    sum_d = rbind(sum_d,data.frame("mean"=wtd.mean(x$Q17, x$weight),"sd"=sqrt(wtd.var(x$Q17, x$weight)),"se"=sqrt(wtd.var(x$Q17, x$weight))/sqrt(length(x$Q17)),"frame"=i,"type"="mass"))
}

p= ggplot(sum_d, aes(x=type, y=mean, fill= as.factor(frame) )) + 
    geom_bar(stat="identity", color="black", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), width=.2, position=position_dodge(.9)) +  labs(x = "Respondent Groups") + scale_x_discrete(labels = c("Preservationists","Resource Users","Mass Public")) + coord_cartesian(ylim=c(3,7)) + scale_fill_grey(start = 0.4, end = .9, name = "Frame", labels = c("Harm-and-threats", "Sustainable use","Pandemic"))+  theme_bw()
p

