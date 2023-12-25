library(anesrake)

rm(list=ls()) 

##### Preparation #####

d = read.csv("clean_data.csv", header = T)

# variables must be 1 to n
d$Q2 =  as.factor(as.character(revalue(d$Q2, c( "Male"=1, "Female"=2 ,"Other"=3))))
levels(d$Q2) = c("Male","Female", "Other")

d$Q6 =  as.factor(as.character(revalue(d$Q6, c("University degree"=2, "Post-graduate degree"=2,"High school degree"=1, "Less than high school degree"=1))))
levels(d$Q6) = c("Non-university","University")

d$Q10 =  as.factor(as.character(revalue(d$Q10, c("Urban"=1, "Rural"=2))))
levels(d$Q10) = c("Urban","Rural")

d$age_binary = findInterval(d$age,65)
d$age_binary = as.factor(d$age_binary+1)
levels(d$age_binary) = c("Young","Old")

# Find sample distributions

wpct(d$Q2) # Gender (male, female, other)
wpct(d$Q6) # Education (non-university vs university)
wpct(d$Q10) # Current place in rural vs urban
wpct(d$age_binary) # below 65 vs 65 or above

# Survey column

round(wpct(d$Q2)*100,1) # % Female (male, female, other)
round(wpct(d$age_binary)*100,1) # % Population above age 65
round(wpct(d$Q6)*100,1) # % University degree
round(wpct(d$Q10)*100,1) # % Urban (current)

# World Bank data (column)

gender = c(.49927,.50073,.01) #2020
names(gender) = c("Male","Female", "Other")
#2020 prediction based on the data from 1979, 89, 09
#https://data.worldbank.org/indicator/SE.SEC.CUAT.PO.ZS?locations=VN
educational_attainment = data.frame( rate = c(.01202,.02633,.12078), year = c(1979,1989,2009))
mod = lm(rate~year,educational_attainment)
newd = data.frame(year = c(2020))
predict(mod, newdata = newd)
education = c(1-predict(mod, newdata = newd),predict(mod, newdata = newd))
names(education) = c("Non-university","University")
area = c(.3734,.6266)#2020
names(area) = c("Urban","Rural")
age = c(.6894,.07866) #2020
names(age) = c("Young","Old")

# Weight calculation

targets = list(gender, education, area, age)
names(targets) = c("Q2", "Q6", "Q10", "age_binary")

d$caseid = 1:length(d$Q2)

set.seed(1)
anesrakefinder(targets, d, choosemethod = "total")

outsave = anesrake(targets, d, caseid = d$caseid,
                   verbose= FALSE, cap = 10,
                   choosemethod = "total",
                   type = "pctlim", pctlim = .05 , nlim = 4,
                   iterate = TRUE , force1 = TRUE)

# Post-adjustment column (see "Wtd %")
summary(outsave)

d$weightvec = unlist(outsave[1]) # Same as "weight" in clean_data
