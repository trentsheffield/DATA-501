# This project is for DATA 511 - Big Data Strategy for Samford University.
# We have been tasked to collect data on a topic we are curious about, compile
# data, and produce visualization to find trends in the data.
# I am looking into email response times from my company, Jacksonville State.

#Importing packages
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
install.packages("reshape")
library(reshape)


#Import data
email_response <- read_csv("hw5_data.csv")

#Calculating the correlation coefficient between my response time and the response
#time
email_response$mytime_num <- as.numeric(email_response$my_response_time)
email_response$responsetime_num <- 
  as.numeric(email_response$return_response_time)
response_corr <- cor(email_response$mytime, email_response$responsetime_num)
response_corr

#Scatter plot for data showing differences in Microsoft Outlook and Microsoft 
#Teams response times
email_viz <- ggplot(data=email_response, aes(x=my_response_time, 
                                             y=return_response_time, 
                                             color=service)) +
  geom_point() +
  geom_smooth(method=lm,
              se=FALSE) +
  xlab('My Response Time') + 
  ylab('Return Response Time') +
  scale_colour_manual(values=c("#0072C6", "#7B83EB"),
                        name="Communication Platform",
                        breaks=c("MO", "MT"),
                        labels=c("Microsoft Outlook", "Microsoft Teams")) +
  theme(legend.position = "top")
  
email_viz

#Visualization of times emails were received
received_times <- read_csv("email_times.csv")
m <- mean(received_times$times)
std <- sqrt(var(received_times$times))
resp_viz <- ggplot(data = received_times, aes(x=times)) +
  geom_histogram() +
  xlab("Time of the Day") + 
  ylab("Number of Emails") +
  ggtitle("Time of Day Emails Were Received") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))
resp_viz
