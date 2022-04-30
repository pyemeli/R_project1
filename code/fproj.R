


#```{r warning=FALSE, message=FALSE}
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(agricolae))
suppressPackageStartupMessages(read_csv('BRFSS2015.csv'))



#```{r warning=FALSE, message=FALSE}
f_proj <- read_csv('BRFSS2015.csv')
f_proj <- f_proj %>%
  clean_names()




#head(f_proj)

#-------------Q1
#Q1: How many people have any kind of health care coverage?
# nbr people with any kind health care
Q1 <- as.data.frame(f_proj%>%
                      select(hlthpln1)%>%
                      filter(hlthpln1==1))
Q1 <- group_by(Q1)
Q1 <- count(Q1)
Q1

#---------------Q2
#Q2: What is the average "Number of Days Mental Health Not Good" for those in
#Pennsylvania who have numeric data? 
#Make sure to change the response corresponding to none to 0

Q2 <- as.data.frame(f_proj%>%
                      filter(menthlth <= 30 & menthlth >= 1 & state== 42 )%>%
                      select(menthlth, state))
Q2 <- as.data.frame(Q2%>%
  summary(avrg_MENTHLTH=average(menthlth),n=n(), na.rm=TRUE))

Q2 <- Q2$Freq
Q2 <- Q2[4]
Q2
#88 IS nONE
#77 IS DONT KKMOW/ NT SURE
#99 IS REFUSED
#1-30 IS MENTHLTH
# 42 IS Pennsylvania

#-------------Q3
#Q3: Compare only those who have and have not arthritis, rheumatoid arthritis, gout, etc. 
#For those groupings, convert reported weight in kilograms to pounds.
#Then, compute the mean and standard deviation of the newly created weight in pounds
#variable. Use the conversion 1KG = 2.20462 LBS. Make sure the units are in pounds,
#not two decimals implied. The names of the variables should be mean_weight and
#sd_weight. mean_weight should equal 183.04.


Q3 <- as.data.frame(f_proj%>%
                    filter(havarth3==1 , havarth3==2 |
                             wtkg3!=99999 , wtkg3!=7777 |
                             !is.na(havarth3) ,!is.na(havarth3) |
                             !is.na( wtkg3) , !is.na( wtkg3))%>%
                      select(havarth3, wtkg3))
Q3 <- na.omit(Q3)
Q33 <- mutate(Q3, wt_LBS_3=(wtkg3*2.20432)/(10^(2)))
Q333 <- select(Q33, wt_LBS_3)

sd_weight <- sd(unlist(Q333))
mean_weight <- mean(unlist(Q333))
Q3<- mutate(Q3, sd_weight, mean_weight)
Q3 <- select(Q3,sd_weight,mean_weight)
Q3


#--------Q4
# For the next questions you'll be exploring the relationship between marital status and minutes of
#total physical activity per week. Ignore those who refused to answer, weren't asked, or had
#missing data. You'll need to convert the marital status variable to a factor. Then:

#build data for question 4,
Data_Q4 <- as.data.frame(f_proj%>%
                           filter(marital!=9 ,
                             !is.na(marital) ,!is.na(marital) ,
                             !is.na(pa1min) , !is.na(pa1min), na.rm=TRUE))
Data_Q4$marital <- as.factor(Data_Q4[ , 'marital'])

head(Data_Q4)
s <- select(Data_Q4, marital,pa1min)

#----------Q4
# Q4: Remove outliers from minutes of total physical activity per week using 0.997 and 0.003 as criteria.
# What percentage of observations remain?  Assign that value to Q4


Data_Q4_upper <- quantile(Data_Q4$pa1min,0.997,na.rm = TRUE)
Data_Q4_lower <- quantile(Data_Q4$pa1min,0.003,na.rm = TRUE)
Data_Q4_out <- which(Data_Q4$pa1min > Data_Q4_upper | Data_Q4$pa1min < Data_Q4_lower)
Q4 <- round((nrow(Data_Q4)-length(Data_Q4_out))/nrow(Data_Q4)*100,2)
Q4
Data_Q4_use <- Data_Q4[-Data_Q4_out,] # this is data set without outliner


#-------------Q5
#Answer the following questions using the dataset without outliers.
#● Q5:  Group by marital status and calculate the mean, standard deviation, minimum, and
#maximum of total exercise, to two decimals.


#Data_Q4_use <- as_tibble(Data_Q4_use)
Q5 <-as.data.frame( Data_Q4_use%>%
#  filter(marital!=9)%>%
  group_by(marital)%>%
  summarize( mean_pa1min = mean(pa1min),sd_pa1min = 
               sd(pa1min), min_pa1min = 
               min(pa1min),max_pa1min = 
               max(pa1min),na.rm=TRUE, n=n()
             ))


#Q5<- as.data.frame(Q5%>%
#                      round(mean_pa1min ,2)%>%
#                      round(sd_pa1min ,2)%>%
#                      round(min_pa1min ,2)%>%
#                      round(max_pa1min ,2))
Q5

#-----------Q6
#Answer the following questions using the dataset without outliers.
#Q6: Create a boxplot for total exercise by marital status.


Q6 <- ggplot(Data_Q4_use, aes(x=pa1min, y=marital,color = marital))+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
             outlier.size=2, notch=FALSE, fill = 'yellow')+
  labs(title = 'total exercise by marital status')+
  stat_summary(fun = mean,geom = "point",size = 2,color = "blue") +
    theme_classic()
Q6

#-----------Q7

#Answer the following questions using the dataset without outliers.
#Q7: Run a regression predicting exercise by marital status.  Assign the model sumto Q7.



model1_sumto <-lm(pa1min ~ marital, data=Data_Q4_use)
model1_sumto <- summary(model1_sumto)
Q7 <- model1_sumto
Q7


#---------Q8
#Answer the following questions using the dataset without outliers.
#Q8: Run an ANOVA comparing exercise across marital status, and assign the
#TukeyHSD post-hoc test to Q8.


model1_sumto <-lm(pa1min ~ marital, data=Data_Q4_use)
model2_sumto <- aov(model1_sumto)
#model1_sumto <- summary(model2_sumto)
tukey.test <- TukeyHSD(model2_sumto)

Q8 <- tukey.test
Q8
#Q88 <- HSD.test(model2_sumto, trt = 'group')
#Q88

#--------Q9
#Answer the following questions using the dataset without outliers.
#Q9: Run a regression as in Q7, but add total fruits consumed per day.  Based on thR-squared and AIC, what is the better model?  Assign the better AIC value to Q9.



Q9 <- Data_Q4_use%>%
  filter(pa1min | frutsum, na.rm=TRUE)
model3_sumto <-lm(pa1min ~ marital + frutsum, data=Q9)
model4_sumto <- summary(model3_sumto)
#model4_sumto
Q44 <-round(AIC(model3_sumto, k= 2),2)
Q9<- Q44
Q9

#-------Q!0
#For the final section, you will choose four variables to explore we previously have not.
#Complete the following;
#● Q10: Remove any outliers.  Briefly explain why you chose the method you used.  Make
#sure to comment it out.

#--
#build data for question 10,here we choose diagnosed with heart attack(cvdinfr4), by sex and by physical activity(PACAT1)
Data_Q10 <- as.data.frame(f_proj%>%
#                            filter(htm4)%>%
                            filter()%>%
                            filter(maxvo2!=99900)%>%
                            filter(!is.na(htin4))%>%
                            filter(!is.na(bmi5cat))%>%
                            filter(!is.na(sex))%>%
                            filter(!is.na(maxvo2)))
#Data_Q10$pacat1 <- as.factor(Data_Q10[ , 'pacat1'])
Data_Q10$sex <- as.factor(Data_Q10[ , "sex"])

head(Data_Q10)
Data_Q11 <- select(Data_Q10, htin4,maxvo2,bmi5cat, sex)

Data_Q11



#-------Q10
Data_Q10_upper <- quantile(Data_Q10$maxvo2,0.997,na.rm = TRUE)
Data_Q10_lower <- quantile(Data_Q10$maxvo2,0.003,na.rm = TRUE)
Data_Q10_out <- which(Data_Q10$maxvo2 > Data_Q10_upper | Data_Q10$maxvo2< Data_Q10_lower)
Q10 <- round((nrow(Data_Q10)-length(Data_Q10_out))/nrow(Data_Q10)*100,2)
Data_Q10_use <- Data_Q10[-Data_Q10_out,] # this is data set without outliner
t <- select(Data_Q10_use, htin4,maxvo2,bmi5cat, sex)
Q10

#--------
#For the final section, you will choose four variables to explore we previously have not.
#Q11: Address the values of any variables.  For instance, is “none” equal to a value other
#than 0? Are there extra decimals implied?

#-----------Q11
#htin4 =Reported_height_in_inches
#36 - 95 Height in inches Notes:  value multiplied by
#      BLANK 
      
#maxvo2=Estimated_Maximum_Oxygen_Consumption
#0 - 501 Estimated Maximum Oxygen Consumption  (two implied decimal places)
#99900 Don’t know/Not Sure/Refused/Missing 

#bmi5cat=Comp_body_mass_index_categorie
#1 Underweight Notes: _BMI5 < 1850 (_BMI5 has 2 implied decimal places) 
#2 Normal Weight Notes: 1850 <= _BMI5 < 2500 
#3 Overweight Notes: 2500 <= _BMI5 < 3000 
#4 Obese Notes: 30000 <= _BMI5 < 9999 
#BLANK Don’t know/Refused/Missing Notes: _BMI5 = 9999 

#sex
#   1 Male
#  2 Female 



#For the final section, you will choose four variables to explore we previously have not.
#Q12: Complete exploratory analyses doing appropriate visualizations with ggplot2.

t1 <- t%>%
  mutate( sex= recode(sex, "1" = "Male", "2" ="Female"))%>%
  mutate( bmi5cat= recode(bmi5cat, "1" ="Underweight",
                      "2" ="Normal_Weight",
                      "3" ="Overweightt",
                      "4" ="Obese"))%>%
  rename(Comp_body_mass_index_categories=bmi5cat )%>%
  rename( Estimated_Maximum_Oxygen_Consumption =maxvo2)%>%
  rename( Reported_height_in_inches = htin4 )
Q12 <- ggplot(data = t1,mapping=aes(x=Estimated_Maximum_Oxygen_Consumption,y=Reported_height_in_inches,color=sex))+geom_line()+facet_wrap(vars(Comp_body_mass_index_categories))+theme_bw()
Q12



#For the final section, you will choose four variables to explore we previously have not.
#Q13: Run basic descriptive statistics
Q13 <- summary(t1)
Q13

#For the final section, you will choose four variables to explore we previously have not.
#Q14: Finally, run an appropriate regression predicting one of those variables.  Identify
#the best model.


#predic the (Estimated_Maximum_Oxygen_Consumption =maxvo2) giving the information on Comp_body_mass_index_categories=bmi5cat) , (Reported_height_in_inches = htin4) and sex.

Q14 <- lm (formula=maxvo2~bmi5cat+htin4+sex, data=t)
Q14 <- summary(Q14)
Q14




