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

# Table 3 is based on this LaTex output
stargazer::stargazer(m1_1,m1_2, m2_1, m2_2, m3_1, m3_2, star.cutoffs = c(.05, .01, .001))
