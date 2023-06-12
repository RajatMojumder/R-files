library("readr")
school_data_1 <- read_csv("https://raw.githubusercontent.com/hhsievertsen/applied_econ_with_r/main/data/school_data_1.csv")
head(school_data_1)

library(haven)
school_data_2 <- read_dta("C:/Users/rajat/Downloads/school_data_2.dta")
View(school_data_2)
tail(school_data_2,n=8)

library(readxl)
library("dplyr")
school_data_3 <- read_excel("C:/Users/rajat/Downloads/school_data_3.xlsx")
View(school_data_3)
glimpse(school_data_3)

school_data_merged <- merge(school_data_1,school_data_2,by="person_id")
dim(school_data_merged)

school_data_merged <- merge(school_data_merged,school_data_3,by=c("person_id","school_id"))

summary(school_data_merged)

#Alternatively we can use ncol() functions to find the number of columns
ncol(school_data_merged)

#load tidyr packages
library("tidyr")
#make data tidy (make long)
school_data_tidy <- school_data_merged%>%
                  pivot_longer(cols=starts_with("test_year"),
                 names_to="year",names_prefix="test_year_", names_transform=list(year=as.integer),
                values_to="test_score",
)

#ncol to get the number of columns of new dataset
ncol(school_data_tidy)

# load skimr
#install.packages("skimr")
library("skimr")
#Use skim() to skim the data

skim(school_data_tidy)

# Select only rows with no missing values
school_data_selected <- filter(school_data_tidy,!is.na(parental_schooling),!is.na(test_score))

# Use skim() to skim the data again
skim(school_data_selected)


#rename summercamp to summerschool
analysisdata <- rename(school_data_selected,summerschool=summercamp)

# use head to view the first 10 observations
head(analysisdata, n=10)

# Standarize test score
# Group analysisdata by year
analysisdata <- group_by(analysisdata,year)

# Create a new variable with mutate
analysisdata<-mutate(analysisdata, test_score=(test_score-mean(test_score))/sd(test_score))

# show mean of test_score
print(paste("Mean of test score",mean(analysisdata$test_score)))

# show sd of test_score
print(paste("SD of test score",sd(analysisdata$test_score)))

# load summary
#install.packages("modelsummary")
library("modelsummary")

# create a summary stat table
datasummary((`Female`=female)+
              (`Parental schooling (years)`=parental_schooling)+
              (`Parental income (log)`=parental_lincome)+
              (`Received reminder letter`=letter)+
              (`Test Score`=test_score)~
              (`Attended summer school`=Factor(summerschool))*
              (Mean+SD+P25+P50+P75),
            sparse_header = FALSE,
            data=filter(analysisdata,year==2),
            output='tab_descriptive_statistics.docx')

# load ggplot2
library("ggplot2")
# create a scatter plot with a fitted line
ggplot(analysisdata%>%filter(year==5),  
       aes(x=parental_lincome,y=test_score))+
  geom_smooth(color="#145c21") +
  geom_point(alpha=0.1,size=1,color="#63a668")+
  theme(panel.background = element_rect(fill="#ededed",color="#ededed"),
        plot.background = element_rect(fill="#ededed",color="#ededed"),
        panel.grid.major = element_line(colour="#a3a3a3",size=0.1))+
  labs(x="Log(Parental Income)",y="Test Score (Mean=0,SD=1)", title="Test scores & Parental income")
  

# load patchwork
#install.packages("patchwork")
library("patchwork")
# Create raw chart element
rawchart <- ggplot(analysisdata%>%filter(year==5),x=as.factor(fill))+
     theme_classic()
# Create bar chart of pre summer school test and summer school
p1 <- rawchart+
        geom_smooth(aes(x=parental_schooling,y=test_score))+
        geom_point(aes(x=parental_schooling,y=test_score),alpha=0.1)+
        labs(x="Parental schooling",y="Test Score Year 5")
p1
# Create bar chart of pre summer school test score and summer school
p2 <- rawchart+
         geom_bar(aes(x=as.factor(summerschool),y=test_score),
                  stat="summary",fun="mean")+
         labs(y="Test Score Year 5", x="Attended Summer School")
p2
# Create bar chart of parental schooling and summer school attendance
p3 <- rawchart+
              geom_boxplot(aes(x=as.factor(summerschool),y=parental_lincome))+
        labs(y="Parental Income (log)",x="Attended Summer School")
p3

# Combine charts
p1/(p2+p3)
# Export chart
ggsave("fig1.png")


# create a histogram and density chart
ggplot(filter(analysisdata,year==6),
       aes(x=test_score,fill=as.factor(summerschool)))+
       geom_histogram(aes(y=..density..),bins=50,alpha=0.5,
                      position="identity",color="white")+
       geom_density(alpha=0.0,size=1,show.legend=FALSE)+
       theme_minimal()+
       labs(y="Density",x="Test score",fill=" ")+
       scale_fill_brewer(palette="Set2",labels=c("No summer scchool","Summer school"))+
       theme(legend.position ="top")