install.packages("psych")
library(psych)
library(ggplot2)
library(reshape2)
library(ggthemes)

insurance <- read.csv("/Users/mahmoudalminawi/Downloads/insurance.csv")
summary(insurance)
describe(insurance)
str(insurance)

###########
CHARGES =
  ggplot(insurance, aes(x=charges)) + 
  geom_histogram(fill = "aquamarine3")+
  ggtitle("Distrbution of charges") + xlab("charges") + ylab("count") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

CHARGES +
  geom_vline(aes(xintercept=mean(charges)),
                     color="blue", linetype="dashed", size=1)

###########
smokersChargers <- subset(insurance, insurance$smoker == "yes")
  ggplot(smokersChargers, aes(x=charges)) + 
  geom_histogram(fill = "aquamarine3")+
  ggtitle("Distrbution of charges for smokers")  + 
  xlab("charges") + ylab("count")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(aes(xintercept=mean(charges)),
             color="blue", linetype="dashed", size=1)
###########3
  NonSmokersChargers <- subset(insurance, insurance$smoker == "no")
  ggplot(NonSmokersChargers, aes(x=charges)) + 
    geom_histogram(fill = "aquamarine3")+
    ggtitle("Distrbution of charges for non smokers") + xlab("charges ") + ylab("count") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(aes(xintercept=mean(charges)),
             color="blue", linetype="dashed", size=1)
  ##########
  maleChargers <- subset(insurance, insurance$sex == "male")
  ggplot(maleChargers, aes(x=charges)) + 
    geom_histogram(fill = "aquamarine3")+
    ggtitle("Distrbution of charges for males") + xlab("charges") + ylab("count") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(aes(xintercept=mean(charges)),
               color="blue", linetype="dashed", size=1)
  ##########
  femaleChargers <- subset(insurance, insurance$sex == "female")
  ggplot(femaleChargers, aes(x=charges)) + 
    geom_histogram(fill = "aquamarine3")+
    ggtitle("Distrbution of charges for fermales") + xlab("charges") + ylab("count") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(aes(xintercept=mean(charges)),
               color="blue", linetype="dashed", size=1)
  
  ########
  
  BMI =
    ggplot(insurance, aes(x=bmi)) + 
    geom_histogram(fill = "aquamarine3")+
    ggtitle("Distrbution of bmi") + xlab("bmi") + ylab("count") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  BMI +
    geom_vline(aes(xintercept=mean(bmi)),
               color="blue", linetype="dashed", size=1)
  
  ###########
  smokersBmi <- subset(insurance, insurance$smoker == "yes")
  ggplot(smokersBmi, aes(x=bmi)) + 
    geom_histogram(fill = "aquamarine3")+
    ggtitle("Distrbution of bmi for smokers") + xlab("bmi") + ylab("count") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(aes(xintercept=mean(bmi)),
               color="blue", linetype="dashed", size=1)
  ###########3
  NonSmokersBmi <- subset(insurance, insurance$smoker == "no")
  ggplot(NonSmokersBmi, aes(x=bmi)) + 
    geom_histogram(fill = "aquamarine3")+
    ggtitle("Distrbution of bmi for non smokers") + xlab("bmi") + ylab("count") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(aes(xintercept=mean(bmi)),
               color="blue", linetype="dashed", size=1)
  ##########
  maleBmi <- subset(insurance, insurance$sex == "male")
  ggplot(maleBmi, aes(x=bmi)) + 
    geom_histogram(fill = "aquamarine3")+
    ggtitle("Distrbution of bmi for males") + xlab("bmi") + ylab("count") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(aes(xintercept=mean(bmi)),
               color="blue", linetype="dashed", size=1)
  ##########
  femaleBmi <- subset(insurance, insurance$sex == "female")
  ggplot(femaleBmi, aes(x=bmi)) + 
    geom_histogram(fill = "aquamarine3")+
    ggtitle("Distrbution of bmi for females") + xlab("bmi") + ylab("count") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(aes(xintercept=mean(bmi)),
               color="blue", linetype="dashed", size=1)
  
  
#########################
  AGE =
    ggplot(insurance, aes(x=age)) + 
    geom_histogram(fill = "aquamarine3")+
    ggtitle("Distrbution of age") + xlab("age") + ylab("count") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  AGE +
    geom_vline(aes(xintercept=min(age)),
               color="blue", linetype="dashed", size=1)
  
  ###########
  smokersAge <- subset(insurance, insurance$smoker == "yes")
  ggplot(smokersAge, aes(x=age)) + 
    geom_histogram(fill = "aquamarine3")+
    ggtitle("Distrbution of age for smokers") + xlab("age") + ylab("count") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(aes(xintercept=min(age)),
               color="blue", linetype="dashed", size=1)
  ###########3
  NonSmokersAge <- subset(insurance, insurance$smoker == "no")
  ggplot(NonSmokersAge, aes(x=age)) + 
    geom_histogram(fill = "aquamarine3")+
    ggtitle("Distrbution of age for non smokers") + xlab("age") + ylab("count") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(aes(xintercept=min(age)),
               color="blue", linetype="dashed", size=1)
  ##########
  maleAge <- subset(insurance, insurance$sex == "male")
  ggplot(maleAge, aes(x=age)) + 
    geom_histogram(fill = "aquamarine3")+
    ggtitle("Distrbution of age for males") + xlab("age") + ylab("count") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(aes(xintercept=min(age)),
               color="blue", linetype="dashed", size=1)
  ##########
  femaleAge <- subset(insurance, insurance$sex == "female")
  ggplot(femaleAge, aes(x=age)) + 
    geom_histogram(fill = "aquamarine3")+
    ggtitle("Distrbution of age for females") + xlab("age") + ylab("count") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(aes(xintercept=min(age)),
               color="blue", linetype="dashed", size=1)
###############
  library(dplyr)
  
  BmiGroups <- insurance %>% mutate(bmigroups = case_when(bmi < 18.5   ~ 'Under Weight',
                                               bmi >= 18.5  & bmi <= 24.9 ~ 'Normal Weight',
                                               bmi >= 25  & bmi <= 29.9 ~ 'Overweight',
                                               bmi > 30  ~ 'obsese'))
BmiGroups <- BmiGroups[,c(3,8)]
piePlotData <- BmiGroups %>% group_by(bmigroups) %>% summarise(bmi = n())
piePlotData <- piePlotData[c(1:4),]

  ggplot(piePlotData, aes(x="", y=bmi, fill=bmigroups)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void()  +
    scale_fill_brewer(palette="Set1")+
    ggtitle("BMI and weight category count")  + ylab("age-category")
  
  ########################
  
  AgeGroups <- insurance %>% mutate(agegroup = case_when(age < 18   ~ 'Teenager',
                                                         age >= 18  & age <= 35 ~ 'Youngs',
                                                         age >= 35  & age <= 55 ~ 'Adults',
                                                         age > 55  ~ 'Elders'))
  AgeGroups <- AgeGroups[,c(1,8)]
  piePlotDataAge <- AgeGroups %>% group_by(agegroup) %>% summarise(age = n())
  ggplot(piePlotDataAge, aes(x="", y=age, fill=agegroup)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void()  +
    scale_fill_brewer(palette="Set1")+
    ggtitle("age and Age category")  + ylab("age category")
  
  ###################
  bmiGroupsForBoxPlot <- insurance %>% mutate(bmigroups = case_when(bmi < 18.5   ~ 'Under Weight',
                                                                    bmi >= 18.5  & bmi <= 24.9 ~ 'Normal Weight',
                                                                    bmi >= 25  & bmi <= 29.9 ~ 'Overweight',
                                                                    bmi > 30  ~ 'obsese',
                                                                    bmi = NA ~ 'obsese'))

  
  ggplot(bmiGroupsForBoxPlot, aes(bmigroups, charges,fill = smoker))+geom_boxplot()+ 
    ggtitle("BMI and charges among the smookers") + xlab("BMI group") + ylab("charges") + 
    theme_fivethirtyeight() + theme(
      plot.title = element_text(color="black", size=14, face="bold.italic"),
      axis.title.x = element_text(color="black", size=14, face="bold"),
      axis.title.y = element_text(color="black", size=14, face="bold")) 
  
  ggplot(bmiGroupsForBoxPlot, aes(bmigroups, charges,fill = sex))+geom_boxplot()+ 
    ggtitle("BMI and charges among the sex") + xlab("BMI group") + ylab("charges") + 
    theme_fivethirtyeight() + theme(
      plot.title = element_text(color="black", size=14, face="bold.italic"),
      axis.title.x = element_text(color="black", size=14, face="bold"),
      axis.title.y = element_text(color="black", size=14, face="bold")) 
  
  ggplot(bmiGroupsForBoxPlot, aes(bmigroups, bmi,fill = sex))+geom_boxplot()+ 
    ggtitle("BMI groups and Bmi among the sex") + xlab("BMI group") + ylab("BMI") + 
    theme_fivethirtyeight() + theme(
      plot.title = element_text(color="black", size=14, face="bold.italic"),
      axis.title.x = element_text(color="black", size=14, face="bold"),
      axis.title.y = element_text(color="black", size=14, face="bold")) 
  
  ggplot(bmiGroupsForBoxPlot, aes(bmigroups, bmi,fill = smoker))+geom_boxplot()+ 
    ggtitle("BMI and BMI groups among the sex") + xlab("BMI group") + ylab("BMI") + 
    theme_fivethirtyeight() + theme(
      plot.title = element_text(color="black", size=14, face="bold.italic"),
      axis.title.x = element_text(color="black", size=14, face="bold"),
      axis.title.y = element_text(color="black", size=14, face="bold")) 
  ###########
  
  AgeGroupsForBoxPlot <- insurance %>% mutate(agegroup = case_when(age < 18   ~ 'Teenager',
                                                         age >= 18  & age <= 35 ~ 'Youngs',
                                                         age >= 35  & age <= 55 ~ 'Adults',
                                                         age > 55  ~ 'Elders'))
  
  
  ggplot(AgeGroupsForBoxPlot, aes(agegroup, charges,fill = sex))+geom_boxplot()+ 
    ggtitle("Age and charges among the sex") + xlab("Age group") + ylab("charges") + 
    theme_fivethirtyeight() + theme(
      plot.title = element_text(color="black", size=14, face="bold.italic"),
      axis.title.x = element_text(color="black", size=14, face="bold"),
      axis.title.y = element_text(color="black", size=14, face="bold")) 
  
  ggplot(AgeGroupsForBoxPlot, aes(agegroup, charges,fill = smoker))+geom_boxplot()+ 
    ggtitle("Age and charges among the smookers") + xlab("Age group") + ylab("charges") + 
    theme_fivethirtyeight() + theme(
      plot.title = element_text(color="black", size=14, face="bold.italic"),
      axis.title.x = element_text(color="black", size=14, face="bold"),
      axis.title.y = element_text(color="black", size=14, face="bold")) 
  
  
  ############
  ggplot(AgeGroupsForBoxPlot, aes(AgeGroupsForBoxPlot$region, charges,fill = smoker))+geom_boxplot()+ 
    ggtitle("region and charges among the smokers or not") + xlab(" region") + ylab("charges") + 
    theme_fivethirtyeight() + theme(
      plot.title = element_text(color="black", size=14, face="bold.italic"),
      axis.title.x = element_text(color="black", size=14, face="bold"),
      axis.title.y = element_text(color="black", size=14, face="bold")) 
  #####################
install.packages("plotly")
  library(plotly)
  
  library(hrbrthemes)
  library(ggplotly)
  library(viridis)
 ppp <- ggplot(bmiGroupsForBoxPlot, aes(x=age, y=charges, size = children, color=smoker)) +
    geom_point(alpha=0.5)+ scale_size( name="no of children") + 
    scale_fill_viridis(discrete=TRUE, option="A") + theme_ipsum()+
    theme(legend.position="bottom") +
    ylab("charges") +
    xlab("age")+ ggtitle("Age, charges and the no of children") 
    

#############
 
 library(ggplot2)
 ggplot(bmiGroupsForBoxPlot, aes(fill=bmigroups, x=region)) + 
   geom_bar(position="stack", stat="count") +
   scale_fill_viridis(discrete = T) +
   ggtitle("Studying 4 regions...") +
   theme_ipsum() +
   xlab("Region")
 
 ggplot(bmiGroupsForBoxPlot, aes(fill=region, x=sex)) + 
   geom_bar(position="stack", stat="count") +
   scale_fill_viridis(discrete = T) +
   ggtitle("Studying 4 regions and sex...") +
   theme_ipsum() +
   xlab("sex")
 
 
 p <- ggplot(bmiGroupsForBoxPlot, aes(fill=smoker,x=children)) + 
   geom_bar( stat="count") +
   scale_fill_viridis(discrete = T) +
   ggtitle("Studying children...") +
   theme_ipsum() +
   xlab("No. of Children") 

 ###########
 htmlwidgets::saveWidget(as_widget(p), "index.html")
 
