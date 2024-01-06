# Load necessary libraries
library(tidyverse)

# (1) Intro: Load in the SPY stock return dataset
spy_data <- read.csv("spy.csv") %>% 
  as_tibble() %>%
  arrange(date) %>%
  mutate(id = row_number())  # Create a new column "id"

#Let's start by plotting a density plot of the percentage returns:
spy_data%>%
  ggplot(aes(x=per_chg_from_day_prior))+
  geom_density(alpha=.4,fill="blue")+
  coord_cartesian(xlim=c(-.05,.05))
  
#Get a statistical summary:
summary(spy_data$per_chg_from_day_prior)

####################################################################
# (2) Finding the "Cut-Off" Points that Define the "Event Periods"


#We will first define a "threshold" to use to determine if a stock return
#is "abnormal" or "typical":
cut_points <- c(-.02, .02)

#Now we will create a new column called "event_type" that will be used
#to indicate if the current observation experienced a percentage return
#that was greater than (or less than) the threshhold
# Create "event_type" column
spy_data <- spy_data %>%
  mutate(
    event_type = ifelse(per_chg_from_day_prior < cut_points[1], -1,
                        ifelse(per_chg_from_day_prior > cut_points[2], 1, 0))
  )

table(spy_data$event_type)

####################################################################
# (3) Now compute the "days since", which is the number of trading days
#since the last time the stock had a "negative" or "positive" event,
#where "event" is defined as the closing price surpassing the threshold 
#defined earlier in step (2):

spy_data <- spy_data %>%
  arrange(id) %>%
  mutate(
    last_neg_id = ifelse(event_type == -1, id, NA),
    last_pos_id = ifelse(event_type == 1, id, NA)
  ) %>%
  fill(last_neg_id, last_pos_id, .direction = "down")

spy_data <- spy_data %>%
  mutate(
    num_obs_since_last_neg = row_number() - last_neg_id,
    num_obs_since_last_pos = ifelse(is.na(last_pos_id), NA, row_number() - last_pos_id)
  ) %>%
  select(-last_neg_id, -last_pos_id)

#Plot the number of days since last event (either negative or positive):
spy_data %>%
  gather(num_obs_since_last_neg:num_obs_since_last_pos,key="type",value="num_since")%>%
  ggplot(aes(x = num_since)) +
  geom_density(bins = 100,alpha = 0.7) +
  labs(title = "Distribution of Time Differences for Paired Abnormal Event",
       x = "Days Since Last Abnormal Event")+
  coord_cartesian(xlim=c(0,200))

#Now we will plot the density (i.e. the "distribution") of the number of days
#since the last negative/positive event (respectively in two different plots):
spy_data %>%
  gather(num_obs_since_last_neg:num_obs_since_last_pos,key="type",value="num_since")%>%
  ggplot(aes(x = num_since,fill=type)) +
  geom_density(bins = 100,alpha = 0.7) +
  labs(title = "Distribution of Time Differences for Negative Events",
       x = "Days Since Last Negative Event")+
  coord_cartesian(xlim=c(0,200))

#Plot the relationship between "number of days since" and the change in the stock price.
spy_data%>%
  gather(num_obs_since_last_neg:num_obs_since_last_pos,key="type",value="num_since")%>%
  ggplot(aes(x=num_since,y=per_chg_from_day_prior,color=type))+
  geom_smooth()+
  coord_cartesian(xlim=c(0,30))

spy_data%>%
  gather(num_obs_since_last_neg:num_obs_since_last_pos,key="type",value="num_since")%>%
  filter(num_since<30)%>%
  ggplot(aes(x=as.factor(num_since),y=per_chg_from_day_prior,color=type))+
  geom_boxplot()

# (4) Distribution of Time Differences (+/-) Between Events
#Now instead of looking at the "days since last positive/negative" events,
#let's study the time BETWEEN positive/negative events.

#To do this, we will first separate the positive from the negative events:

# (a) Filter out observations where "event_type" is equal to -1
time_diff_neg_data<-
    spy_data%>%
      filter(event_type==-1)%>%
      mutate(time_diff = id - lag(id))%>%
      mutate(type="Negative")

# (b) Filter out observations where "event_type" is equal to 1
time_diff_pos_data<-
  spy_data%>%
  filter(event_type==1)%>%
  mutate(time_diff = id - lag(id))%>%
  mutate(type="Positive")

#And now we put them back together:
# (c) Now combine them together into the same dataframe:
time_diff_data<-
  time_diff_neg_data%>%
  union_all(time_diff_pos_data)


#Now we can plot the differences BETWEEN the times:
time_diff_data %>%
  ggplot(aes(x = time_diff,fill=type)) +
  geom_density(bins = 200,alpha = 0.7) +
  labs(title = "Distribution of Time Differences for Positive and Negative Events",
       x = "Days Between Positive and Negative Events")+
  coord_cartesian(xlim=c(0,50))

#Get a statistical summary:
summary((time_diff_data%>%filter(event_type==-1))$time_diff)
summary((time_diff_data%>%filter(event_type==1))$time_diff)

# (5) Relationship Between Previous # of Days (+/-) and Current # of Days
time_diff_data%>%
  ggplot(aes(x=as.Date(date),y=time_diff,color=type))+
  geom_line()

#Compute the lags
tdn<-time_diff_data%>%
  filter(event_type == -1)%>%
  mutate(time_diff_before = lag(time_diff))

tdp<-time_diff_data%>%
  filter(event_type == 1)%>%
  mutate(time_diff_before = lag(time_diff))

time_diff_data<-
  tdn%>%
  union_all(tdp)

#First, try a scatter plot
time_diff_data%>%
  filter(time_diff_before<150)%>%
  ggplot(aes(x=time_diff_before,y=time_diff,color=type))+
  geom_point()+
  geom_smooth()

#Now try the boxplots
time_diff_data%>%
  ggplot(aes(x=cut(log(time_diff_before),breaks=10),y=log(time_diff),color=type))+
  geom_boxplot()


#First, try a scatter plot
time_diff_data%>%
  ggplot(aes(x=log(time_diff_before),y=log(time_diff),color=type))+
  geom_point()+
  geom_smooth()

#Now try the boxplots
time_diff_data%>%
  ggplot(aes(x=cut(log(time_diff_before),breaks=10),y=log(time_diff),color=type))+
  geom_boxplot()

