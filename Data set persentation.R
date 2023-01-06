library(readxl)
FERTIL2 <- read_excel("FERTIL2.xls")
cl <- c("mnthborn",  "yearborn", " age",       "electric",  "radio",     "tv"  ,      "bicycle"  , "educ"  ,   
        "ceb"  ,     "agefbrth",  "children",  "knowmeth",  "usemeth"  , "monthfm" ,  "yearfm" ,   "agefm" ,   
        "idlnchld",  "heduc",     "agesq",     "urban",     "urbeduc",  "spirit",    "protest" ,  
        "catholic", "frsthalf",  "educ0",     "evermarr" )
my_data <- setNames(FERTIL2,cl)
str(my_data)
View(my_data)


#(i)
max(my_data$children)
min(my_data$children)
mean(my_data$children)



#(ii)
a=nrow(my_data)
b=sum(my_data$electric==1)
c=(b/a)*100
c

#(iii)

library(dplyr)
elect_child=my_data %>% select(children,electric) %>% filter(electric==1)
avg_chil_with_elec=mean(elect_child$children) 
avg_chil_with_elec #Average Children in Electricy households


non_elect_child=my_data %>% select(children,electric) %>% filter(electric==0)
avg_chil_with_no_elec=mean(non_elect_child$children) 
avg_chil_with_no_elec #Average Children in Non Electricy households

##Clearly the electricity households have fewer children



#(iv)Not directly but may be influency of electricity devices like TV and radio are more likely to promote fewer children and more liberal thoughts.




#(v)

my_data$electric <- as.factor(my_data$electric)
my_data$urban <- as.factor(my_data$urban)
str(my_data)



library(car)
#one way anova


#H0:mu(urban education)=mu(non urban education)
#Ha: at least one mean is different


m1 <- aov(educ~urban,my_data)
 summary(m1)



#two way anova
m2 <- aov(educ~electric+urban+electric:urban,my_data)
summary(m2)

'''
Interpret the results
From the ANOVA results, we can conclude the following, based on the p-values and a significance level of 0.05:

(i)the p-value of electric is <2e-16 (significant), which indicates that the levels of electric are associated with significant different in education.

(ii)the p-value of urban is < 2e-16 (significant), which indicates that the levels of urban are associated with significant different in education.

(iii)the p-value for the interaction between electric*urban is 7.06e-10(significant), which indicates that the relationships between urban and education depends on the electricity.
'''
