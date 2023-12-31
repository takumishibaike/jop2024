library(ggplot2)
library(plyr)
library(reshape)
library(weights)

rm(list=ls()) 

d = read.csv("clean_data.csv", header = T)
d$frame_var = as.factor(d$frame_var)# 1=harm-and-threats, 2=sustainability, 3=pandemic frame
d$organization_var = as.factor(d$organization_var)# 1=WWF, 2=ENV (Vietnam)

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

### Regression models

m1_1 = lm(Q17~organization_var+frame_var+age+Q2+ relevel(Q6, ref="Less than high school degree") + relevel(Q8, ref="Not proud at all") + relevel(Q10, ref="Urban")+relevel(Q12, ref="Urban") + relevel(Q14, ref="Neither"),weights = weight, mass)
m1_2 = lm(Q17~frame_var+organization_var, weights = weight,mass)


m2_1 = lm(Q17~organization_var+frame_var+age+Q2+ relevel(Q6, ref="Less than high school degree") + relevel(Q8, ref="Not proud at all") + relevel(Q10, ref="Urban")+relevel(Q12, ref="Urban") + relevel(Q14, ref="Neither"), weights = weight, preservationists)
m2_2 = lm(Q17~frame_var+organization_var, weights = weight, preservationists)


m3_1 = lm(Q17~organization_var+frame_var+age+Q2+ relevel(Q6, ref="Less than high school degree") + relevel(Q8, ref="Not proud at all") + relevel(Q10, ref="Urban")+relevel(Q12, ref="Urban") + relevel(Q14, ref="Neither"), weights = weight, appropriators)
m3_2 = lm(Q17~frame_var+organization_var, weights = weight, appropriators)

### Visualization

m2_1Frame = data.frame(Variable = rownames(summary(m2_1)$coef)[2:4],
                       Coefficient = summary(m2_1)$coef[2:4, 1],
                       SE = summary(m2_1)$coef[2:4, 2],
                       modelName = "Preservationists")
m3_1Frame = data.frame(Variable = rownames(summary(m3_1)$coef)[2:4],
                       Coefficient = summary(m3_1)$coef[2:4, 1],
                       SE = summary(m3_1)$coef[2:4, 2],
                       modelName = "Resource Users")
m1_1Frame = data.frame(Variable = rownames(summary(m1_1)$coef)[2:4],
                       Coefficient = summary(m1_1)$coef[2:4, 1],
                       SE = summary(m1_1)$coef[2:4, 2],
                       modelName = "Mass Public")

allModelFrame = data.frame(rbind(m2_1Frame,m3_1Frame,m1_1Frame)) 

allModelFrame$Variable =  revalue(allModelFrame$Variable, c("organization_var2"="ENV","frame_var2"="Sustainable use frame", "frame_var3"="Pandemic frame"))

# Specify the width of your confidence intervals
interval1 = -qnorm((1-0.9)/2)  # 90% multiplier
interval2 = -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
allModelFrame$Variable = factor(allModelFrame$Variable, levels = c("ENV","Sustainable use frame","Pandemic frame"), order =TRUE)
zp1 = ggplot(allModelFrame, aes(colour = modelName))
zp1 = zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 = zp1 + geom_linerange(aes(x = Variable, 
                               ymin = Coefficient - SE*interval1,
                               ymax = Coefficient + SE*interval1),
                           lwd = 1,
                           position = position_dodge(width = 1/2))
zp1 = zp1 + geom_pointrange(aes(x = Variable, y = Coefficient, 
                                ymin = Coefficient - SE*interval2,
                                ymax = Coefficient + SE*interval2),
                            lwd = 1/2,
                            position = position_dodge(width = 1/2),
                            shape = 16)
zp1 = zp1 + theme_bw() + scale_colour_grey(start = .1, end = .6, name = "Group", labels = c("Preservationists","Resource Users","Mass Public")) + theme(axis.text.x = element_text(angle = 45,  vjust = 1, hjust=1))

print(zp1) 


