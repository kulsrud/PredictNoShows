## ----setup, include=FALSE----------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----load, echo=FALSE, include = FALSE---------------------------------

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org") #For general data cleaning
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org") #For creating test and train set
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org") #For displaying graphs side by side
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org") #For manipulating date variables
if(!require(woeBinning)) install.packages("WoeBinning", repos = "http://cran.us.r-project.org") #For binning variables automatically

#ML Packages
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org") #For running some caret functions
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org") #For producing ROC curves
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org") #For visualizing rpart plots
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org") #For rpart functions
if(!require(DMwR)) install.packages("DMwR", repos = "http://cran.us.r-project.org") #For running algorithms in caret

library("tidyverse")
library("caret")
library("gridExtra")
library("lubridate")
library("e1071")
library("pROC")
library("rpart.plot")
library("DMwR")
library("rpart")

url<- "https://github.com/kulsrud/PredictNoShows/raw/master/" 
file<- "KaggleV2-May-2016.csv"
df<- read.csv(paste(url, file, sep = ""))


## ----description, echo=FALSE-------------------------------------------

#Correct typos in variable names
names(df)[names(df)=="Hipertension"]<- "Hypertension"
names(df)[names(df)=="Handcap"]<- "Handicap"
names(df)[names(df)=="No.show"]<- "No_show"

#To show top 4 rows of sample data
knitr::kable(head(df, 4), "markdown") 



## ----cleaning, echo = FALSE, include = FALSE---------------------------

head(df) #The dataset is tidy, with each row representing one appointment slot 
colSums((is.na(df))) #There are no missing values in the dataset

#Transformed date variables to date format, turn No-show into boolean variable, and estimate difference between time of booking and appointment
df<- df %>% mutate(ScheduledDay = as_datetime(str_remove(str_replace(ScheduledDay, "T", " "), "Z")),
                      AppointmentDay = as_datetime(str_remove(str_replace(AppointmentDay, "T", " "), "Z")),
                      AppointmentWeekDay = weekdays(AppointmentDay),
                      ScheduledWeekDay = weekdays(ScheduledDay),
                      ScheduledHour = hour(ScheduledDay),
                      No_show = ifelse(No_show=="No", FALSE, TRUE),
                      BookingDifference = as.numeric(AppointmentDay - ScheduledDay))




## ----base, echo = FALSE------------------------------------------------

#Estimate overall No-show proportion
knitr::kable(prop.table(table(df$No_show)), col.names = c("No-show", "Proportion"), format = "markdown", digits = 3)

#Highlight total duration of dataset
min_date<- min(df$AppointmentDay)
max_date<- max(df$AppointmentDay)
myDates <-seq(from = min_date, to = max_date, by = "days")
duration<- length(which(wday(myDates) %in% c(2:6)))


## ----weekday, echo = FALSE, fig.align='center', fig.height=4-----------

#Look at No-shows by scheduled weekday
scheduled<- df %>%
     group_by(ScheduledWeekDay) %>% #To see individual movies by name
     summarize(No_show = mean(No_show)) %>% 
     ggplot(aes(factor(ScheduledWeekDay, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), ordered = TRUE), No_show))+
     geom_bar(stat = "identity")+
     coord_flip()+
    scale_y_continuous(limits = c(0, 0.25))+
     labs(title = "No-show proportion, by weekday scheduled",
       x = NULL, 
       y = NULL)

#Look at No-shows by appointment weekday
appointment<- df %>%
    group_by(AppointmentWeekDay) %>% #To see individual movies by name
     summarize(No_show = mean(No_show)) %>% 
      ggplot(aes(factor(AppointmentWeekDay, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), ordered = TRUE), No_show))+
      geom_bar(stat = "identity")+
      coord_flip()+
      scale_y_continuous(limits = c(0, 0.25))+
      labs(title = "No-show proportion, by appointment weekday",
       x = NULL, 
       y = NULL)

#Visualize results
grid.arrange(scheduled, appointment)


## ----saturdays, echo = FALSE, fig.align='center'-----------------------

#Scheduled on Saturdays vs. No-show
df %>% 
  group_by(factor(ScheduledWeekDay, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>% 
  summarize(n = n(), No_show = mean(No_show)) %>%
  knitr::kable(digits = 2, col.names = c("Scheduled Weekday", "Appointments", "Percent No-show"))


## ----saturdays_3, echo = FALSE, fig.align='center'---------------------

#Appointment on Saturdays vs. Walk-ins
df %>% 
  group_by(factor(AppointmentWeekDay, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>% 
  summarize(n = n(), No_show = mean(No_show)) %>%
  knitr::kable(digits = 2, col.names = c("Appointment Weekday", "Appointments", "Percent No-show"))



## ----hours, echo = FALSE, fig.align='center', fig.height=4, fig.width=6----

#Look at No-shows by hour of the day
df %>%
     group_by(ScheduledHour) %>% #To see individual movies by name
     summarize(No_show = mean(No_show)) %>% 
     ggplot(aes(ScheduledHour, No_show))+
        geom_bar(stat = "identity")+
  labs(title = "Appointment no-show proportion",
       x = "Hour of day scheduled", 
       y = "No-show proportion")



## ----wait, echo = FALSE, fig.align='center'----------------------------

#Bin difference between scheduled time and appointment time
df %>%
  group_by(BookDiffBin = ntile(BookingDifference, 12)) %>%
  summarize(No_show = mean(No_show), BookDiff = mean(BookingDifference)) %>%
  ggplot(aes(BookDiffBin, No_show))+
  geom_bar(stat = "identity", size = 1)+
  #geom_step(aes(ntile(BookDiffBin, 3)))
  geom_label(aes(x = BookDiffBin, y = No_show + 0.05, label = round(BookDiff, 0)))+
  scale_x_continuous(breaks = c(seq.int(1, 12, 1)))+
  labs(title = "Avg difference scheduling and appointment",
       x = "Bin no.", 
       y = "No-show proportion")


## ----bins, echo = FALSE, fig.align='center', fig.width=5, fig.height=3.5----


#Bin based on Intuition about walk-ins
df %>%
  mutate(AppointmentType = ifelse(BookingDifference > 1, ifelse(BookingDifference > 168, "Long-term", "Within one week"), "Walk-in")) %>%
  group_by(AppointmentType) %>%
  summarize(No_show = mean(No_show), BookDiff = mean(BookingDifference)) %>%
  ggplot(aes(reorder(AppointmentType,No_show), No_show))+
  geom_bar(stat = "identity", size = 1)+
  geom_label(aes(x = AppointmentType, y = No_show + 0.05, label = round(BookDiff, 0)))+
  #scale_x_discrete(breaks = NULL)+
  scale_y_continuous(limits = c(0, 0.4))+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "No-show likelihood, Binned by Appointment Type",
       x = "Appointment Type", 
       y = "Likelihood of No-show")


## ----demographics, echo = FALSE, fig.align='center', fig.height=3, fig.width=5----

#Looking at patient effects
df %>%
  group_by(PatientId) %>%
  summarize(No_show = mean(No_show), 
            n = n(), 
            AppointmentsPerDay = n / duration) %>% #Assuming that multiple appointments per day means longer appointments (i. e. units scheduled back to back)
  ggplot(aes(AppointmentsPerDay, No_show))+
  geom_point()+
  labs(title = "No-show proportion by patient ID",
       x = "Average number of appointments per day", 
       y = "Average proportion No-shows")
  


## ----age, echo = FALSE, warning=FALSE, fig.align='center', message=FALSE, fig.width=5, fig.height=4.5----

#Looking at age effects
df %>% 
  filter(Age < 100) %>% 
  group_by(Age) %>%
  summarize(No_show = mean(No_show), n = n()) %>% 
  ggplot(aes(Age, No_show))+
  geom_point()+
  geom_smooth(method = "auto")+
  labs(title = "No-show proportion by age",
       x = "Age", 
       y = "Average proportion No-shows")


## ----gender, echo = FALSE, fig.align='center', fig.width=5, fig.height=3----

#Looking at Gender effects
df %>%
  group_by(Gender) %>%
  summarize(No_show = mean(No_show)) %>%
  ggplot(aes(Gender, No_show))+
  geom_bar(stat = "identity")+
  labs(title = "No-show proportion by gender",
       x = "Gender", 
       y = "Proportion No-shows")
#No evidence of gender effects


## ----neighbourhoods, echo = FALSE, fig.align='center', fig.width=5-----

#Top 5 no-show areas - low n
df %>% 
  group_by(Neighbourhood) %>%
  summarize(`No_show (High)` = mean(No_show), n = n()) %>%
  arrange(-`No_show (High)`) %>%
  top_n(5, `No_show (High)`) %>%
  knitr::kable(format = "markdown", digits = 3)



## ----nhood_2, echo = FALSE, fig.align='center', fig.width=5------------

#Bottom 5 no-show areas - also low n
df %>% 
  group_by(Neighbourhood) %>%
  summarize(`No_show (Low)` = mean(No_show), n = n()) %>%
  arrange(`No_show (Low)`) %>%
  top_n(5, -`No_show (Low)`)%>%
  knitr::kable(format = "markdown", digits = 3)




## ----neighbourhood_2, echo = FALSE, message=FALSE, fig.align='center', fig.width=5, fig.height=4----

#Visualize neighbourhood population vs. likelihood of no-show
df %>% 
  group_by(Neighbourhood) %>%
  summarize(No_show = mean(No_show), n = n()) %>%
  filter(n>50) %>%
    ggplot(aes(n, No_show))+
    geom_point()+
    geom_smooth(method = "auto", se = FALSE)+
    labs(title = "No-show proportion by neighbourhood population",
         subtitle = "Filtered for more than 50 appointments",
       x = "Neighbourhood population", 
       y = "Proportion No-shows")



## ----neighbourhood_3, echo = FALSE, fig.align='center', fig.width=9, fig.height=4.5----

#Group by neighbourhood group, define variable
Nhood_quartiles<- df %>% 
  group_by(Neighbourhood) %>%
  summarize(No_show = mean(No_show), n = n()) %>%
  select(No_show) %>%
  summary() %>%
  as.data.frame() %>%
  slice(c(2, 3, 5)) %>%
  mutate(quartiles = str_sub(Freq, start = -8)) %>% 
  .$quartiles %>%
  as.numeric(digits = 4)

#Define quartile names
names(Nhood_quartiles)<- c("Below median", "Median", "Above median")

#Visualize neighbourhood no-show likelihood by quartile
df %>% 
  group_by(Neighbourhood) %>%
  summarize(No_show = mean(No_show), n = n()) %>%
  mutate(Nhood_bin = if_else(No_show > median(Nhood_quartiles), 
                             if_else(No_show > max(Nhood_quartiles), "Fourth quartile", "Third quartile"), 
                             if_else(No_show > min(Nhood_quartiles), "Second quartile", "First quartile")),
         Nhood_bin = factor(Nhood_bin, levels = c("First quartile", "Second quartile", "Third quartile", "Fourth quartile"), ordered = TRUE)) %>%
    ggplot(aes(reorder(Neighbourhood, No_show), No_show, fill = Nhood_bin))+
    geom_bar(stat = "identity")+
    scale_fill_manual("Neighbourhood Quartile", values = c("steelblue", "lightblue", "grey50", "grey10"))+
    theme(axis.text.y = element_blank())+
    coord_flip()+
    labs(title = "No-show proportion by neighbourhood",
       x = "Neighbourhood", 
       y = "Proportion No-shows")
  



## ----neighbourhood_4, echo = FALSE, include=FALSE----------------------

#Code to store neighbourhood bins in new variable for feature engineering later. 
Nhood_bin<- df %>% 
  group_by(Neighbourhood) %>%
  summarize(No_show = mean(No_show), n = n()) %>%
  mutate(Nhood_bin = if_else(No_show > median(Nhood_quartiles), 
                             if_else(No_show > max(Nhood_quartiles), "Fourth_quartile", "Third_quartile"), 
                             if_else(No_show > min(Nhood_quartiles), "Second_quartile", "First_quartile")),
         Nhood_bin = factor(Nhood_bin, levels = c("First_quartile", "Second_quartile", "Third_quartile", "Fourth_quartile"), ordered = TRUE)) %>%
  select(Neighbourhood, Nhood_bin)
 
  
## ----pastnoshows, echo = FALSE, fig.align='center', message=FALSE------

#Show mean likelihood of missing appointment if missed past appointents
df %>%
  arrange(AppointmentDay) %>%
  group_by(PatientId) %>%
  mutate(PastNo_show = ifelse(cumsum(No_show)-1 <0, 0, cumsum(No_show)-1)) %>%
  ungroup() %>%
  group_by(PastNo_show) %>%
  summarize(n = n(), No_show = mean(No_show)) %>%
  ggplot(aes(PastNo_show, No_show))+
  geom_point()+
  geom_smooth(method = "auto", se = FALSE)+
  scale_y_continuous(limits = c(0,1.05))+
  labs(title = "No-show proportion by past missed appointments",
     x = "Number of missed appointments", 
     y = "Proportion No-shows")



## ----past_no_show_validate, evaluate = FALSE, echo = FALSE, include = FALSE----

#Code to validate that no-shows were estimated correctly. 

#Select highest no-shows to validate number of no-shows
TopNo_shows<- df %>%
  arrange(AppointmentDay) %>%
  group_by(PatientId) %>%
  mutate(PastNo_show = ifelse(cumsum(No_show)-1 <0, 0, cumsum(No_show)-1)) %>%
  ungroup() %>%
  filter(PastNo_show %in% 14:17) %>% 
  select(PatientId) %>% 
  mutate(PatientId = as.character(PatientId)) %>%
  distinct() %>%
  .$PatientId

#Validating highest number of cumulative No-shows
df %>%
  arrange(AppointmentDay) %>%
  group_by(PatientId) %>%
  mutate(PastNo_show = ifelse(cumsum(No_show)-1 <0, 0, cumsum(No_show)-1)) %>%
  ungroup() %>%
  mutate(PatientId = as.character(PatientId)) %>%
  filter(PatientId %in% c(TopNo_shows)) %>%
  ggplot(aes(AppointmentDay, PastNo_show, color = PatientId))+
  scale_color_manual("Top-3 No-show Patient IDs", values = c("steelblue", "grey50", "grey10"))+
  geom_line(size = 1)+
   labs(title = "Cumulative No-shows by Appointment Date",
     x = "Appointment Date", 
     y = "Cumulative No-shows")

#appears as though No-shows cumulate correctly based on the input data. 

## ----medical, echo = FALSE, fig.align='center'-------------------------

#Make a subset of data with preexisting conditions tidy as one variable. Observe each as an average against dependent variable, as well as the sum. Start with grouping by dependent variable and then taking mean of each and total sum. 
df %>% 
  group_by(No_show) %>%
  summarize(Hypertension = mean(Hypertension),
            Diabetes = mean(Diabetes), 
            Alcoholism = mean(Alcoholism),
            Handicap = mean(Handicap),
            PreExisting = mean(c(Hypertension, Diabetes, Alcoholism, Handicap))) %>%
  pivot_longer(c(Hypertension, Diabetes, Alcoholism, Handicap, PreExisting), names_to = "Condition", values_to = "Proportion", names_repair = "minimal") %>%
    ggplot(aes(reorder(Condition, Proportion), Proportion, fill = No_show))+
    geom_bar(stat = "identity", position = "dodge")+
    scale_fill_manual("No-show", values = c("steelblue", "grey50"))+
    coord_flip()+    
    labs(title = "No-show proportion by Medical Condition",
       x = "Condition", 
       y = "Proportion No-shows")



## ----other, echo = FALSE, fig.align='center', fig.height=3.5, fig.width=4----

#Similar to above, estimating no-show likelihood by other binary variables
df %>% 
  group_by(No_show) %>%
  summarize(Scholarship = mean(Scholarship), 
            SMS_received = mean(SMS_received)) %>%
  pivot_longer(c(Scholarship, SMS_received), names_to = "Protective", values_to = "Proportion", names_repair = "minimal") %>%
    ggplot(aes(reorder(Protective, Proportion), Proportion, fill = No_show))+
    geom_bar(stat = "identity", position = "dodge")+
    scale_fill_manual("No-show", values = c("steelblue", "grey50"))+
    labs(title = "No-show proportion by Other Outcomes",
       x = "Outcome", 
       y = "Proportion No-shows")
  #coord_flip()


## ----sms_1, evaluate = FALSE, echo=FALSE, include = FALSE, fig.align='center'----

#Evaluate sms received over time
df %>% 
  group_by (AppointmentDay) %>%
  summarize(SMS_received = mean(SMS_received), 
            BookingDifference = mean(BookingDifference), 
            No_show = mean(No_show)) %>%
  ggplot(aes(AppointmentDay, SMS_received))+
  geom_point(stat = "identity")+
  geom_smooth(aes(x = AppointmentDay, y = No_show), method = "auto", span = 0.4)+
  geom_line(aes(x = AppointmentDay, y = No_show), color = "darkred")+
  scale_color_manual("No-show")

#Check which days there were no SMS's sent out
SMS_down<- df %>% 
  group_by (AppointmentDay) %>%
  summarize(SMS_received = mean(SMS_received), 
            BookingDifference = mean(BookingDifference), 
            No_show = mean(No_show)) %>%
  filter(SMS_received == 0) %>%
  select(AppointmentDay) %>%
  .$AppointmentDay
  
#Check whether downtime in SMSes explains variation (it doesn't)
df %>% 
  filter(!AppointmentDay %in% SMS_down) %>%
  group_by(No_show) %>%
  summarize(Scholarship = mean(Scholarship), 
            SMS_received = mean(SMS_received)) %>%
  pivot_longer(c(Scholarship, SMS_received), names_to = "Protective", values_to = "Proportion", names_repair = "minimal") %>%
    ggplot(aes(reorder(Protective, Proportion), Proportion, fill = No_show))+
    geom_bar(stat = "identity", position = "dodge")+
    scale_fill_manual("No-show", values = c("steelblue", "grey50"))+
    labs(title = "No-show proportion by Other Outcomes", 
         subtitle = "Filtered out non-SMS days",
       x = "Outcome", 
       y = "Proportion No-shows")



## ----sms_2, echo = FALSE-----------------------------------------------

#Look as SMS reminders for each appointment type - They aren't sent for walk-ins, which already have much lower no-show rates. For apointments within 1 week there is no difference, and for long-term appointments they make patients about 6% more likely to show up to their appointments. 
df %>%
  mutate(SMS_received = ifelse(SMS_received == 0, FALSE, TRUE),
         AppointmentType = ifelse(BookingDifference > 1, ifelse(BookingDifference > 168, "Long-term", "Within one week"), "Walk-in"))%>%
  group_by(AppointmentType, SMS_received) %>%
  summarize(No_show = mean(No_show)) %>%
    ggplot(aes(reorder(AppointmentType, No_show), No_show, fill = SMS_received))+
    geom_bar(stat = "identity", position = "dodge")+
    scale_fill_manual("SMS Received", values = c("steelblue", "grey50"))+
    labs(title = "No-show by appointment type and SMS",
       x = "Appointment Type", 
       y = "Proportion No-shows")




## ----define_df, echo = FALSE-------------------------------------------

#Clean up dataset based on insights
ml_df <- df %>%
  filter (Age < 100) %>%
  left_join(Nhood_bin, by = "Neighbourhood") %>%
  arrange(AppointmentDay) %>%
  group_by(PatientId) %>%
  mutate(PastNo_show = ifelse(cumsum(No_show)-1 <0, 0, cumsum(No_show)-1)) %>%
  ungroup() %>%
  group_by(PatientId) %>%
  mutate(AppointmentsPerDay = n() / duration) %>%
  ungroup() %>%
  select(-c(PatientId, AppointmentID, ScheduledDay, AppointmentDay, ScheduledWeekDay, Neighbourhood, Alcoholism, Handicap, Gender)) %>%
  mutate(No_show = str_replace(No_show, "TRUE", "No_show"),
         No_show = str_replace(No_show, "TRUE", "Present"))





## ----preprocessing, echo = FALSE, include = FALSE----------------------

##Preprocessing

#Categorized variables
cat_list <- c("AppointmentWeekDay", "Nhood_bin", "No_show")
ml_df[cat_list] <- lapply(ml_df[cat_list], factor)

#Center and scale numeric columns
#Standardized Variables
stand_list <- c("Age", "ScheduledHour", "PastNo_show", "AppointmentsPerDay", "BookingDifference") 

stand<- preProcess(ml_df[stand_list], method = c("center", "scale"))
print(stand)
summary(ml_df[stand_list])
ml_df[stand_list]<- predict(stand, ml_df[stand_list])


#Variables with no feature engineering
noeng_list <- c("Scholarship", "Hypertension", "Diabetes", "SMS_received")

#confirm that all variables are accounted for. 
length(colnames(ml_df)) == length(c(stand_list, cat_list, noeng_list))

#Confirm that all numeric variables are standardized
lapply(ml_df[stand_list], (hist))

#Confirm that variable names are acceptable
make.names(c(lapply(ml_df[cat_list], levels)), unique = TRUE)




## ----partition, echo = FALSE-------------------------------------------

set.seed(1)
#Ceate index to split in test and training sets
test_index <- createDataPartition(y = ml_df$No_show, times = 1, 
                                   p = 0.2, list = FALSE)
train_df <- ml_df[-test_index,] #defining train set
test_df <- ml_df[test_index,] #defining test set



## ----train_controls, echo = FALSE--------------------------------------

#Set training control method
ctrl<- trainControl(method = "cv", #Set method to 10-fold cv
                    classProbs = TRUE, #Estimate class probabilities when training
                    summaryFunction = twoClassSummary, #Assign type of summary (allows for producing ROC curves)
                    savePredictions = "final") #Allows for using probability threshold tuning

#load("ml-2020-06-05.Rdata") #Load past results to make RMD knitting go faster 



## ----model, echo = FALSE, evaluate = FALSE-----------------------------

set.seed(1) #Set seed to enable results replication

model<- "rpart" #Set model as rpart
ctrl$sampling<- NULL #Don't use sampling strategies yet

#Train model
base_fits <- lapply(model, function(model){
	print(model)
	train(No_show ~ .,
              data = train_df,
              method = model,
	            metric = "ROC",
	            trControl = ctrl)
})


## ----ROC_1, echo = FALSE, results="hide"-------------------------------

#Record results from training model
names(base_fits)<- "rpart - no sampling"

ROC <- sapply(base_fits, function(model){ 
	print(model)
	ROC<- mean(model$results$ROC)
	matrix(ROC)

})



## ----results_1, echo = FALSE-------------------------------------------

#Organize and visualize results from initial model
ROC_df<- data.frame(ROC)
ROC_df$model<- names(ROC)
rownames(ROC)<- NULL

ROC_best_df<- ROC_df %>% 
  arrange(-ROC) %>%
  top_n(5, ROC) #Visualize top 5 models

ROC_best_df %>% knitr::kable(format = "markdown", digits = 3)



## ----sampling, echo = FALSE, evaluate = FALSE--------------------------

set.seed(1)

#Test different sampling strategies
model<- c("rpart")

sampling_method<- c("down", "up", "rose", "smote")
ctrl$sampling<- sampling_method

sampling_fits <- lapply(sampling_method, function(sampling_method){ 	print(sampling_method)
	train(No_show ~ .,
              data = train_df,
              method = model,
	            metric = "ROC",
	            trControl = ctrl)
})



## ----ROC_2, echo = FALSE, results="hide"-------------------------------

#Create values for table
model<- c("rpart")
sampling_method<- c("down", "up", "rose", "smote")
names(sampling_fits) <- paste(model,"-", sampling_method)

#Create matrix of ROC
sampling_ROC <- sapply(sampling_fits, function(model){ 
	print(model)
	ROC<- mean(model$results$ROC)
	matrix(ROC)

})



## ----results_2, echo = FALSE-------------------------------------------

#Add results to running scorecard
ROC<- append(ROC, sampling_ROC)

ROC_df<- data.frame(ROC)
ROC_df$model<- names(ROC)
rownames(ROC)<- NULL

ROC_best_df<- ROC_df %>% 
  arrange(-ROC) %>%
  top_n(5, ROC)

ROC_best_df %>% knitr::kable(format = "markdown", digits = 3)


## ----tree, echo = FALSE, fig.width=9, fig.height=6---------------------

# Visualize the decision tree with rpart.plot
rpart.plot(sampling_fits$`rpart - smote`$finalModel, box.palette="RdBu", shadow.col="gray", nn=TRUE)



## ----importance, echo = FALSE------------------------------------------


#Calculate and visualize variable importance
imp <- varImp(sampling_fits$`rpart - smote`)

important<- imp$importance %>%
  mutate(Feature = rownames(imp$importance)) %>%
  arrange(-Overall) 

important %>%
  select(Feature, Overall) %>%
  rename(Importance = Overall) %>%
  knitr::kable(format = "markdown", digits = 2)



## ----important_fit, echo = FALSE, evaluate = FALSE---------------------

#Take out unimportant features to filter ml dataset, clean up feature names
important_vars<- important %>%  
  filter(Overall > 2) %>%
  select(Feature) %>%
  .$Feature

important_vars <- str_remove(string = important_vars, pattern = "\\.[:alpha:]{1}") #Clean up feature names

important_vars <- append(important_vars, "No_show") #Add dependent variable

#Test rpart results as baseline
set.seed(1)

model<- "rpart"
ctrl$sampling<- NULL
train_df_imp<- train_df[,important_vars]

imp_fits <- lapply(model, function(model){
	print(model)
	train(No_show ~ .,
              data = train_df_imp,
              method = model,
	            metric = "ROC",
	            trControl = ctrl)
})



## ----ROC_3, echo=FALSE, results="hide"---------------------------------

#Document ml results
names(imp_fits)<- "rpart - no sampling - important vars"

imp_ROC <- sapply(imp_fits, function(model){ 
	print(model)
	ROC<- mean(model$results$ROC)
	matrix(ROC)

})



## ----results_3, echo = FALSE-------------------------------------------

#Add results to running scorecard
ROC<- append(ROC, imp_ROC)

ROC_df<- data.frame(ROC)
ROC_df$model<- names(ROC)
rownames(ROC)<- NULL

ROC_best_df<- ROC_df %>%
  arrange(-ROC) %>%
  top_n(6, ROC)

ROC_best_df %>% knitr::kable(format = "markdown", digits = 3)


## ----models, echo = FALSE, evaluate = FALSE----------------------------

set.seed(1)

#Test different models using first 10,000 rows and important features
model<- c("naive_bayes","knn", "kknn",
          "mlp", "monmlp","gbm", "multinom",
          "avNNet", "glm", "C5.0")

sampling_method<- NULL
ctrl$sampling<- sampling_method

model_fits <- lapply(model, function(model){
	print(model)
	train(No_show ~ .,
              data = train_df_imp[1:10000,],
              method = model,
	            metric = "ROC",
	            trControl = ctrl)
})


## ----ROC_4, echo = FALSE, results="hide"-------------------------------

#Document results from ml
model<- c("naive_bayes","knn", "kknn",
          "mlp", "monmlp","gbm", "multinom", 
          "avNNet", "glm", "C5.0")

names(model_fits) <- paste(model, "- no sampling - important vars")

#Create matrix of accuracy
model_ROC <- sapply(model_fits, function(model){ 
	print(model)
	ROC<- mean(model$results$ROC)
	matrix(ROC)

})




## ----results_4, echo = FALSE-------------------------------------------

#Add results to running scorecard
ROC<- append(ROC, model_ROC)

ROC_df<- data.frame(ROC)
ROC_df$model<- names(ROC)
rownames(ROC)<- NULL

ROC_df %>% 
  arrange(-ROC) %>%
  select(model, ROC) %>%
  top_n(8, ROC) %>% 
  knitr::kable(format = "markdown", digits = 3)


## ----tune, echo = FALSE, evaluate = FALSE------------------------------

#Run best performing algoithms on full dataset
set.seed(1)

model<- c("gbm", "mlp", "C5.0")
model

sampling_method<- "smote"
ctrl$sampling<- sampling_method

tune_fits <- lapply(model, function(model){
	print(model)
	train(No_show ~ .,
              data = train_df,
              method = model,
	            metric = "ROC",
	            trControl = ctrl)
})



## ----ROC_5, echo = FALSE, results="hide"-------------------------------

#Record results from ml
model<- c("gbm", "mlp", "C5.0")
names(tune_fits) <- paste(model, "- smote - full dataset")

#Create matrix of accuracy
tune_ROC <- sapply(tune_fits, function(model){ 
	print(model)
	ROC<- mean(model$results$ROC)
	matrix(ROC)

})



## ----results_5, echo = FALSE-------------------------------------------

#Add results to running scorecard
ROC<- append(ROC, tune_ROC)

ROC_df<- data.frame(ROC)
ROC_df$model<- names(ROC)
rownames(ROC)<- NULL

ROC_df %>% 
  arrange(-ROC) %>%
  select(model, ROC) %>%
  top_n(5, ROC) %>% 
  knitr::kable(format = "markdown", digits = 3)
	


## ----best_fits_results, echo = FALSE-----------------------------------

#Compare tuning parameters and potential for additional tuning
limits<- c(0.72, 0.78) #Set limits to enable consistent comparisons

ggplot(tune_fits$`C5.0 - smote - full dataset`)+
  scale_y_continuous(limits = limits)+ 
  ggtitle("C5.0, Smote, Full Dataset")

ggplot(tune_fits$`mlp - smote - full dataset`)+
  scale_y_continuous(limits = limits)+
  ggtitle("Mlp, Smote, Full Dataset")

ggplot(tune_fits$`gbm - smote - full dataset`)+
  scale_y_continuous(limits = limits)+
  ggtitle("Gbm, Smote, Full Dataset")



## ----C50_tune, echo = FALSE, evaluate = FALSE--------------------------

set.seed(1)

#Run final tuned C5.0 model
getModelInfo("C5.0")
tune_fits$`C5.0 - smote - full dataset`$bestTune

#model
model<- "C5.0"
model

#sampling
sampling_method<- "smote"
ctrl$sampling<- sampling_method

#tuning
grid <- expand.grid(.winnow = c(FALSE), .trials=c(20, 25, 30), .model=c("rules"))

C50_tune <- lapply(model, function(model){
	print(model)
	train(No_show ~ .,
              data = train_df,
              method = model,
	            metric = "ROC",
	            trControl = ctrl,
	            tuneGrid = grid)
})



## ----ROC_6, echo = FALSE, results="hide"-------------------------------

#Record results from final model
model<- "C5.0"
names(C50_tune) <- paste(model, "- smote - full dataset - tuning")

ggplot(C50_tune$`C5.0 - smote - full dataset - tuning`)+
  ggtitle("C5.0, Smote, Full Dataset, Tuned")


#Create matrix of accuracy
tune_C50 <- sapply(C50_tune, function(model){ 
	print(model)
	ROC<- mean(model$results$ROC)
	matrix(ROC)

})



## ----results_6, echo = FALSE-------------------------------------------

#Add final results to running scorecard
ROC<- append(ROC, tune_C50)

ROC_df<- data.frame(ROC)
ROC_df$model<- names(ROC)
rownames(ROC)<- NULL

ROC_df %>% 
  arrange(-ROC) %>%
  select(model, ROC) %>%
  top_n(5, ROC) %>% 
  knitr::kable(format = "markdown", digits = 3)


## ----best, echo = FALSE------------------------------------------------

#Store best models in object for optimizing probability threshold
best_models<- list(C50_tune$`C5.0 - smote - full dataset - tuning`,
            tune_fits$`gbm - smote - full dataset`,
            tune_fits$`mlp - smote - full dataset`)

names(best_models)<- c("C5.0", "gbm", "mlp")



## ----threshold, echo = FALSE, results="hide", warning=FALSE------------

# Calculate the optimum threshold value and set prediction thresholds
precision_threshold<- .8 #This is set to indicate lowest acceptable precision value

#Function to estimate optimal probability thresholds
prob_thresholds <- sapply(best_models, function(model){ 
print(model)
	#calculate resampling stats
resample_stats <- thresholder(model,threshold = seq(.01, .99, by = 0.04), final = TRUE)
  #Optimize probability threshold
prob_threshold <- resample_stats %>%
  filter(Precision>=precision_threshold) %>%
  filter(Sensitivity==max(Sensitivity)) %>%  
  .$prob_threshold
matrix(prob_threshold)
})


## ----optimize_prob, echo = FALSE---------------------------------------

#Pick out precision and sensitivity from resampled training data
model_stats<- sapply(best_models, function(model){ 
  stats <- thresholder(model,threshold = prob_thresholds[model$method], final = TRUE) %>% 
  select(prob_threshold, Sensitivity, Precision) 
matrix(stats)
})

#Turn resampled precision and sensitivity results into data frame
rownames(model_stats)<- c("prob_threshold", "Sensitivity", "Precision")
model_stats<- data.frame(t(model_stats)) %>%
  mutate(model = colnames(model_stats)) %>%
  mutate(Precision = as.numeric(Precision),
         Sensitivity = as.numeric(Sensitivity)) %>%
  rename(Model = model)

#Present data frame in nice table
model_stats %>% 
  select(-prob_threshold) %>%
  select(Model, Precision, Sensitivity) %>%
  knitr::kable(format = "markdown", digits = 3)


## ----metrics, echo = FALSE, warning=FALSE------------------------------

#Store best model in object to facilitate final code
best_model<- best_models$C5.0

#Predict values on testing partition based on optimal probability threshold
PRbest <- predict(best_model, test_df, type = "prob")
pred_best <- ifelse(PRbest$No_show >= prob_thresholds["C5.0"], "No_show", "Present") %>% as.factor()

## ----roc, echo = FALSE, message = FALSE--------------------------------

#Plot ROC curve 
plot.roc(test_df$No_show, PRbest$No_show, print.auc = TRUE, print.thres = "best", main = "C5.0, tuned", legacy.axes = TRUE)


## ----confmat_1, echo = FALSE-------------------------------------------

#Call confusion matrix
xtab<- confusionMatrix(pred_best, test_df$`No_show`, mode = "prec_recall")
print(xtab)


## ----confmat_2, echo = FALSE-------------------------------------------

#Create data frame to visualize confusion matrix
pr_metric_df<- data.frame(obs = test_df$No_show, prob = PRbest$No_show, pred = pred_best)

#Visualize confusion matrix
pr_metric_df %>% 
  mutate(obs=factor(obs, levels=c("No_show","Present"),ordered=TRUE)) %>%
   ggplot(aes(x=obs,y=prob,color=pred))+
   geom_jitter(width = 0.3, height = 0.00, alpha = 0.5)+
     scale_y_continuous(breaks = c(seq.int(0.0, 1, 0.2)))+
     geom_vline(aes(xintercept = 1.5), linetype = "dashed")+
     geom_hline(aes(yintercept = prob_thresholds["C5.0"]), linetype = "dashed")+
     scale_color_manual(values = c("steelblue", "grey45"), name = "Prediction", labels = c("No-Show", "Present"))+
     geom_label(data = data.frame(c(0.65, 0.9)), aes(x = 0.65, y = 0.9), label = str_wrap("True Positives", width = 8), color = "black")+
     geom_label(data = data.frame(c(0.65, 0.1)), aes(x = 0.65, y = 0.1, label = str_wrap("False Negatives", width = 8)), color = "black")+
     geom_label(data = data.frame(c(2.35, 0.1)), aes(x = 2.35, y = 0.1, label = str_wrap("True Negatives", width = 8)), color = "black")+
     geom_label(data = data.frame(c(2.35, 0.9)), aes(x = 2.35, y = 0.9, label = str_wrap("False Positives", width = 8)), color = "black")+
     labs(title = "Confusion Matrix Visual Summary",
          subtitle = "No-show predictions vs. actuals",
          y = "Predicted Probability of No-show",
          x = "Observed Values")


