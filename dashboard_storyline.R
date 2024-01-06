# Load necessary libraries
library(tidyverse)
library(plotly)

# Intro: Load in the SPY stock return dataset
spy_data <- read.csv("spy.csv") %>% 
  as_tibble() %>%
  arrange(date) %>%
  mutate(id = row_number())  # Create a new column "id"

################################################################
              #Step (0) Some Data Wrangling
################################################################
# Default lower and upper bounds
lower_bound <- -0.01
upper_bound <- 0.01

#We will first define a "threshold" to use to determine if a stock return
#is "abnormal" or "typical":
cut_points <- c(lower_bound, upper_bound)

#Now we will create a new column called "event_type" that will be used
#to indicate if the current observation experienced a percentage return
#that was greater than (or less than) the threshhold
# Create "event_type" column
spy_data <- spy_data %>%
  mutate(
    event_type = ifelse(per_chg_from_day_prior < cut_points[1], -1,
                        ifelse(per_chg_from_day_prior > cut_points[2], 1, 0))
  )


#Now compute the "days since", which is the number of trading days
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


#Distribution of Time Differences (+/-) Between Events
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

rm(time_diff_neg_data)
rm(time_diff_pos_data)

#Keep originals
spy_data_org<-spy_data
time_diff_data_org<-time_diff_data

#Filter the data by date.
start_date <- as.Date("2000-01-10")
end_date <- as.Date("2000-01-20")

spy_data <- spy_data_org %>%
  filter(between(as.Date(date), start_date, end_date))

time_diff_data <- time_diff_data_org %>%
  filter(between(as.Date(date), start_date, end_date))

#Reset dataset:  #RUN THESE TO RESET THE FILTER
#spy_data<-spy_data_org
#time_diff_data<-time_diff_data_org

################################################################
#Step (1) Visual Cutoff Analysis
################################################################

#Density plot of returns with cutoff
density_data <- density(spy_data$per_chg_from_day_prior)

p1<-ggplot(spy_data, aes(x = per_chg_from_day_prior)) +
  geom_density(fill = "lightgrey", color = "black") +
  geom_vline(xintercept = c(lower_bound, upper_bound), linetype = "dashed", color = "red") +
  geom_ribbon(data = data.frame(x = density_data$x, y = density_data$y),
              aes(x = x, ymax = ifelse(x < lower_bound | x > upper_bound, y, 0), ymin = 0),
              fill = "red", alpha = 0.5) +
  geom_ribbon(data = data.frame(x = density_data$x, y = density_data$y),
              aes(x = x, ymax = ifelse(x < lower_bound | x > upper_bound, 0, y), ymin = 0),
              fill = "lightgrey", alpha = 0.5) +
  labs(title = "Density Plot of Percentage Returns",
       x = "Percentage Returns",
       y = "Density") +
  theme_minimal()

ggplotly(p1)

#Time series plot
p2<-ggplot(spy_data, aes(x = as.Date(date), y = per_chg_from_day_prior)) +
  geom_rect(aes(xmin = as.Date(min(date)), xmax = as.Date(max(date)),
                ymin = -.1, ymax = lower_bound),
            fill = "red", alpha = 0.1) +
  geom_rect(aes(xmin = as.Date(min(date)), xmax = as.Date(max(date)),
                ymin = upper_bound, ymax = .1),
            fill = "red", alpha = 0.1) +
  geom_rect(aes(xmin = as.Date(min(date)), xmax = as.Date(max(date)),
                ymin = lower_bound, ymax = upper_bound),
            fill = "gray", alpha = 0.1) +
  geom_line(color = "blue") +
  geom_hline(yintercept = c(lower_bound, upper_bound), linetype = "dashed", color = "red") +
  labs(title = "Percentage Return Over Time",
       x = "Date",
       y = "Percentage Return") +
  theme_minimal()
ggplotly(p2)

#table of positive and negative events:
pos_neg_table<-
  spy_data%>%
    filter(per_chg_from_day_prior<=lower_bound | 
             per_chg_from_day_prior>=upper_bound)
  

################################################################
#Step (2) Time Since Last Abnormal Event
################################################################
day_max<-30
split<-FALSE

#Plot the number of days since last event (either negative or positive):
p2_1<-spy_data %>%
  gather(num_obs_since_last_neg:num_obs_since_last_pos,key="type",value="num_since")%>%
  ggplot(aes(x = num_since)) +
  geom_density(bins = 100,alpha = 0.7) +
  labs(title = "Distribution of Time Differences for Paired Abnormal Event",
       x = "Days Since Last Abnormal Event")+
  coord_cartesian(xlim=c(0,200))

if(split){
p2_1<-spy_data %>%
  gather(num_obs_since_last_neg:num_obs_since_last_pos,key="type",value="num_since")%>%
  ggplot(aes(x = num_since,fill=type)) +
  geom_density(bins = 100,alpha = 0.7) +
  labs(title = "Distribution of Time Differences for Negative Events",
       x = "Days Since Last Negative Event")+
  coord_cartesian(xlim=c(0,200))
}

# Plot 2_2: Scatter plot with smoothed line
p2_2 <- spy_data %>%
  gather(num_obs_since_last_neg:num_obs_since_last_pos, key = "type", value = "num_since") %>%
  ggplot(aes(x = num_since, y = per_chg_from_day_prior, color = type)) +
  geom_smooth() +
  coord_cartesian(xlim=c(1,day_max))
  labs(title = "Relationship between Number of Days Since and Stock Price Change",
       x = "Number of Days Since",
       y = "Percentage Change from Previous Day",
       color = "Event Type")+
  scale_color_manual(values = c("num_obs_since_last_neg" = "red", "num_obs_since_last_pos" = "green"),
                     labels = c("num_obs_since_last_neg" = "Negative Abnormal Event",
                                "num_obs_since_last_pos" = "Positive Abnormal Event"))


# Plot 2_3: Boxplot
p2_3 <- spy_data %>%
  gather(num_obs_since_last_neg:num_obs_since_last_pos, key = "type", value = "num_since") %>%
  filter(num_since < day_max) %>%
  filter(num_since > 0)%>%
  ggplot(aes(x = as.factor(num_since), y = per_chg_from_day_prior, color = type)) +
  geom_boxplot() +
  labs(title = "Boxplot of Stock Price Change Over Time",
       x = "Number of Days Since",
       y = "Percentage Change from Previous Day",
       color = "Event Type")+
  scale_color_manual(values = c("num_obs_since_last_neg" = "red", "num_obs_since_last_pos" = "green"),
                     labels = c("num_obs_since_last_neg" = "Negative Abnormal Event",
                                "num_obs_since_last_pos" = "Positive Abnormal Event"))

################################################################
#Step (3) Time Differences Between Negative and Positive Abnormal Events, Repsectively
################################################################

#Now we can plot the differences BETWEEN the times:
p3_1<-time_diff_data %>%
  filter(time_diff<day_max)%>%
  ggplot(aes(x = time_diff,fill=type)) +
  geom_density(bins = 200,alpha = 0.7) +
  labs(title = "Distribution of Time Differences for Positive and Negative Events",
       x = "Days Between Positive and Negative Events")


p3_2 <- time_diff_data %>%
  filter(time_diff > 0)%>%
  ggplot(aes(x = time_diff, y = per_chg_from_day_prior, color = type)) +
  geom_smooth() +
  labs(title = "Relationship between Abnormal Event Day Difference and Stock Price Change",
       x = "Number Of Days Between Abnormal Events",
       y = "Percentage Change from Previous Day",
       color = "Event Type")+
  scale_color_manual(values = c("Negative" = "red", "Positive" = "green"),
                     labels = c("Negative" = "Negative Abnormal Event",
                                "Positive" = "Positive Abnormal Event"))+
  coord_cartesian(xlim=c(0,day_max))


p3_3 <- time_diff_data %>%
  filter(time_diff < day_max) %>%
  filter(time_diff > 0)%>%
  ggplot(aes(x = as.factor(time_diff), y = per_chg_from_day_prior, color = type)) +
  geom_boxplot() +
  labs(title = "Boxplot of Daily Stock Price Change For Each Day Difference",
       x = "Number of Days Between Abnormal Event",
       y = "Percentage Change from Previous Day",
       color = "Event Type")+
  scale_color_manual(values = c("Negative" = "red", "Positive" = "green"),
                     labels = c("Negative" = "Negative Abnormal Event",
                                "Positive" = "Positive Abnormal Event"))

################################################################
#Step (4) Predicting Time Differences (Can past time differences impact future?)
################################################################
lag_n<-1
log_scale<-TRUE

#Compute the lags
tdn<-time_diff_data%>%
  filter(event_type == -1)%>%
  mutate(time_diff_before = lag(time_diff,lag_n))%>%
  drop_na(time_diff_before)

tdp<-time_diff_data%>%
  filter(event_type == 1)%>%
  mutate(time_diff_before = lag(time_diff,lag_n))%>%
  drop_na(time_diff_before)

time_diff_lag<-
  tdn%>%
  union_all(tdp)

rm(tdn)
rm(tdp)


#First, try a scatter plot
p4_1<-time_diff_lag %>%
  ggplot(aes(x = time_diff_before, y = time_diff, color = type)) +
  geom_point() +
  geom_smooth() +
  coord_cartesian(xlim = c(1, day_max)) +
  labs(
    title = paste0("Relationship between Time Difference (Lag t-", lag_n, ") and Time Difference"),
    x = paste0("Time Difference Before (Lag t-",lag_n,")"),
    y = "Time Difference Current (t)",
    color = "Event Type"
  )

if(log_scale){
  p4_1<-time_diff_lag %>%
    mutate(time_diff_before=log(time_diff_before),
           time_diff = log(time_diff))%>%
    ggplot(aes(x = time_diff_before, y = time_diff, color = type)) +
    geom_point() +
    geom_smooth() +
    labs(
      title = paste0("Relationship between Log of Time Difference (Lag t-", lag_n, ") and Log of Time Difference"),
      x = paste0("Log of Time Difference Before (Lag t-",lag_n,")"),
      y = "Log of Time Difference Current (t)",
      color = "Event Type"
    )
  
}

p4_2<-time_diff_lag %>%
  filter(time_diff_before < day_max)%>%
  ggplot(aes(x = as.factor(time_diff_before), y = time_diff, color = type)) +
  geom_boxplot() +
  labs(
    title = paste0("Relationship between Time Difference (Lag t-", lag_n, ") and Time Difference"),
    x = paste0("Time Difference Before (Lag t-",lag_n,")"),
    y = "Time Difference Current (t)",
    color = "Event Type"
  )

if(log_scale){
  p4_2<-time_diff_lag %>%
    mutate(time_diff_before=log(time_diff_before),
           time_diff = log(time_diff))%>%
    ggplot(aes(x = as.factor(round(time_diff_before,1)), y = time_diff, color = type)) +
    geom_boxplot() +
    labs(
      title = paste0("Relationship between Log of Time Difference (Lag t-", lag_n, ") and Log of Time Difference"),
      x = paste0("Log of Time Difference Before (Lag t-",lag_n,")"),
      y = "Log of Time Difference Current (t)",
      color = "Event Type"
    )
}




ggplotly(p4_1)
ggplotly(p4_2)

################################################################
#Step (5) Do Time Differences Happen Differently Throughout Time? 
################################################################

#COMPLETED:
#Fix split option on Tab 3


#OUTSTANDING:

#Next Tasks:

#Block 1
#Break apart the date column into year, quarter, month, week, day of week
#Determine which plots to design based on time.  Which columns will be in filter, plot, or both?
#Generate code to do generate the plots and generalize the inputs
#Attach the plots to a 5th tab in the dashboard

#Block 2:
#Cleanup dashboard, add descriptions, change styling to make it more professional
#Find how to publish RShiny Dashboard
#Connect it to mylesgarvey.com website
#Write ADHD Article using screenshots from the tool, linking to the github and the dashboard itself








