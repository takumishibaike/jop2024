library(plyr)

rm(list=ls()) 

d = read.csv("clean_data.csv", header = T) # All

### Subgroup

# Resource User (earlier version of the paper used the term "appropriators")
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


##### The values found in Table 1 by each column (All, Preservationists, Resource Users, Mass Public)

### All

d$Q2 =  as.numeric(as.character(revalue(d$Q2, c("Female"=1, "Male"=0  ,"Other"=0))))

d$Q6 =  as.numeric(as.character(revalue(d$Q6, c("University degree"=3, "Post-graduate degree"=4,"High school degree"=2, "Less than high school degree"=1))))

d$Q8 =  as.numeric(as.character(revalue(d$Q8, c("Very proud"=1, "Somewhat proud"=1,"Not very proud"=0, "Not proud at all"=0,"I am not Vietnamese"=0))))

d$Q10 =  as.numeric(as.character(revalue(d$Q10, c("Urban"=1, "Rural"=0))))

d$Q12 =  as.numeric(as.character(revalue(d$Q12, c("Urban"=1, "Rural"=0,"Both"=0))))

d$Q14 =  as.numeric(as.character(revalue(d$Q14, c("Northern Vietnam"=1, "Southern Vietnam"=0,"Both equally"=0, "Neither"=0))))

# N
length(d$id)
# % Female
round(mean(d$Q2, na.rm = T)*100,1)
# Average age
round(mean(d$age, na.rm = T),1) 
# % University degree
round(sum(d$Q6 %in% c(3, 4))*100 / length(d$Q6),1)
# % Very proud to be Vietnamese
round(mean(d$Q8, na.rm = T)*100,1) 
# % Urban (current)
round(mean(d$Q10, na.rm = T)*100,1)
# % Urban (past)
round(mean(d$Q12, na.rm = T)*100,1)
# % Attachment to Northern Vietnam
round(mean(d$Q14, na.rm = T)*100,1)

### Preservationists
d = preservationists

d$Q2 =  as.numeric(as.character(revalue(d$Q2, c("Female"=1, "Male"=0  ,"Other"=0))))

d$Q6 =  as.numeric(as.character(revalue(d$Q6, c("University degree"=3, "Post-graduate degree"=4,"High school degree"=2, "Less than high school degree"=1))))

d$Q8 =  as.numeric(as.character(revalue(d$Q8, c("Very proud"=1, "Somewhat proud"=1,"Not very proud"=0, "Not proud at all"=0,"I am not Vietnamese"=0))))

d$Q10 =  as.numeric(as.character(revalue(d$Q10, c("Urban"=1, "Rural"=0))))

d$Q12 =  as.numeric(as.character(revalue(d$Q12, c("Urban"=1, "Rural"=0,"Both"=0))))

d$Q14 =  as.numeric(as.character(revalue(d$Q14, c("Northern Vietnam"=1, "Southern Vietnam"=0,"Both equally"=0, "Neither"=0))))

# N
length(d$id)
# % Female
round(mean(d$Q2, na.rm = T)*100,1)
# Average age
round(mean(d$age, na.rm = T),1) 
# % University degree
round(sum(d$Q6 %in% c(3, 4))*100 / length(d$Q6),1)
# % Very proud to be Vietnamese
round(mean(d$Q8, na.rm = T)*100,1) 
# % Urban (current)
round(mean(d$Q10, na.rm = T)*100,1)
# % Urban (past)
round(mean(d$Q12, na.rm = T)*100,1)
# % Attachment to Northern Vietnam
round(mean(d$Q14, na.rm = T)*100,1)

### Resource Users
d = appropriators

d$Q2 =  as.numeric(as.character(revalue(d$Q2, c("Female"=1, "Male"=0  ,"Other"=0))))

d$Q6 =  as.numeric(as.character(revalue(d$Q6, c("University degree"=3, "Post-graduate degree"=4,"High school degree"=2, "Less than high school degree"=1))))

d$Q8 =  as.numeric(as.character(revalue(d$Q8, c("Very proud"=1, "Somewhat proud"=1,"Not very proud"=0, "Not proud at all"=0,"I am not Vietnamese"=0))))

d$Q10 =  as.numeric(as.character(revalue(d$Q10, c("Urban"=1, "Rural"=0))))

d$Q12 =  as.numeric(as.character(revalue(d$Q12, c("Urban"=1, "Rural"=0,"Both"=0))))

d$Q14 =  as.numeric(as.character(revalue(d$Q14, c("Northern Vietnam"=1, "Southern Vietnam"=0,"Both equally"=0, "Neither"=0))))

# N
length(d$id)
# % Female
round(mean(d$Q2, na.rm = T)*100,1)
# Average age
round(mean(d$age, na.rm = T),1) 
# % University degree
round(sum(d$Q6 %in% c(3, 4))*100 / length(d$Q6),1)
# % Very proud to be Vietnamese
round(mean(d$Q8, na.rm = T)*100,1) 
# % Urban (current)
round(mean(d$Q10, na.rm = T)*100,1)
# % Urban (past)
round(mean(d$Q12, na.rm = T)*100,1)
# % Attachment to Northern Vietnam
round(mean(d$Q14, na.rm = T)*100,1)

### Mass Public
d = mass

d$Q2 =  as.numeric(as.character(revalue(d$Q2, c("Female"=1, "Male"=0  ,"Other"=0))))

d$Q6 =  as.numeric(as.character(revalue(d$Q6, c("University degree"=3, "Post-graduate degree"=4,"High school degree"=2, "Less than high school degree"=1))))

d$Q8 =  as.numeric(as.character(revalue(d$Q8, c("Very proud"=1, "Somewhat proud"=1,"Not very proud"=0, "Not proud at all"=0,"I am not Vietnamese"=0))))

d$Q10 =  as.numeric(as.character(revalue(d$Q10, c("Urban"=1, "Rural"=0))))

d$Q12 =  as.numeric(as.character(revalue(d$Q12, c("Urban"=1, "Rural"=0,"Both"=0))))

d$Q14 =  as.numeric(as.character(revalue(d$Q14, c("Northern Vietnam"=1, "Southern Vietnam"=0,"Both equally"=0, "Neither"=0))))

# N
length(d$id)
# % Female
round(mean(d$Q2, na.rm = T)*100,1)
# Average age
round(mean(d$age, na.rm = T),1) 
# % University degree
round(sum(d$Q6 %in% c(3, 4))*100 / length(d$Q6),1)
# % Very proud to be Vietnamese
round(mean(d$Q8, na.rm = T)*100,1) 
# % Urban (current)
round(mean(d$Q10, na.rm = T)*100,1)
# % Urban (past)
round(mean(d$Q12, na.rm = T)*100,1)
# % Attachment to Northern Vietnam
round(mean(d$Q14, na.rm = T)*100,1)
