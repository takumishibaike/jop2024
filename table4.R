library(weights)

rm(list=ls()) 

d = read.csv("clean_data.csv", header = T)
d$frame_var = as.factor(d$frame_var)# 1=harm-and-threats, 2=sustainability, 3=pandemic frame
d$organization_var = as.factor(d$organization_var)# 1=WWF, 2=ENV (Vietnam)

### Subgroup

# Preservationists (when overlapping resource users are not removed)
preservationists = d[which(d$Q28=="Very important"&d$Q30=="Very important"),] 

### Regression models

m2_1 = lm(Q17~organization_var+frame_var+age+Q2+ relevel(Q6, ref="Less than high school degree") + relevel(Q8, ref="Not proud at all") + relevel(Q10, ref="Urban")+relevel(Q12, ref="Urban") + relevel(Q14, ref="Neither"), weights = weight, preservationists)
m2_2 = lm(Q17~frame_var+organization_var, weights = weight, preservationists)


# Table 4 is based on this LaTex output
stargazer::stargazer(m2_1, m2_2, star.cutoffs = c(.05, .01, .001))
