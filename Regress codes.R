nudata <- read.csv("Desktop/PhD/Spring 2022/DSBA 6211 Adv Biz Analytics/Group Project/listings 2.csv")
str(nudata)
summary(nudata)

#keeping relevant columns using the column index number
df <- nudata[c(15, 28, 33, 34, 40, 47, 48, 56, 57, 61:67)]
str(df)
summary(df)

#convert price variable which is character to numeric
df$price <- as.numeric(gsub('[$,]', '', df$price))


#convert character variables to categorical
df$host_response_time <- factor(df$host_response_time)
df$room_type <- factor(df$room_type)

str(df)

#impute missing variables with the mean
install.packages("Hmisc")
library(Hmisc)

df$host_response_time[df$host_response_time=="N/A"] <- 'within a few hours'

df$minimum_nights_avg_ntm <- with(df, impute(minimum_nights_avg_ntm, mean))
df$maximum_nights_avg_ntm <- with(df, impute(maximum_nights_avg_ntm, mean))
df$review_scores_rating <- with(df, impute(review_scores_rating, mean))
df$review_scores_accuracy <- with(df, impute(review_scores_accuracy, mean))
df$review_scores_cleanliness <- with(df, impute(review_scores_cleanliness, mean))
df$review_scores_checkin <- with(df, impute(review_scores_checkin, mean))
df$review_scores_communication <- with(df, impute(review_scores_communication, mean))
df$review_scores_location <- with(df, impute(review_scores_location, mean))
df$review_scores_value <- with(df, impute(review_scores_value, mean))


install.packages("caret")
install.packages("e1071")
install.packages("car")
install.packages("pROC")
install.packages("dplyr")

library(caret)
library(e1071)
library(car)
library(pROC)
library(dplyr)


# Using VIF to test multicollinearity
vif(lm(formula=price ~ . , family = binomial(link='logit'),data = df))

#linear regression
mod <- lm(price ~ host_response_time+room_type+accommodates+minimum_nights_avg_ntm+
            maximum_nights_avg_ntm+number_of_reviews+number_of_reviews_ltm+
            review_scores_rating+review_scores_accuracy+review_scores_cleanliness+
            review_scores_checkin+review_scores_communication+review_scores_location+
            review_scores_value, data = df)
summary(mod)

#type of listing in each neighborhood
list_neigh <-  df %>% 
  group_by(neighbourhood_cleansed, room_type) %>% 
  summarize(Freq = n())

listing <-  df %>% 
  filter(room_type %in% c("Private room","Entire home/apt","Hotel room", "Shared room")) %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarize(sum = n())

listing_ratio <- merge (list_neigh, listing, by="neighbourhood_cleansed")

listing_ratio <- property_ratio %>% 
  mutate(ratio = Freq/sum)

plot <- ggplot(listing_ratio, aes(x=neighbourhood_cleansed, y = ratio, fill = room_type)) +
  geom_bar(position = "dodge", stat="identity") + 
  xlab("Neighborhood") + ylab ("Count") +
  scale_fill_discrete(name = "Listing Type") + 
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Which types of Listings are there in Denver?",
          subtitle = "Map showing Count of Listing Type by Neighborhood ") +
  theme(plot.title = element_text(face = "bold", size = 14) ) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35", hjust = 0.5)) +
  theme(plot.caption = element_text(color = "grey68"))+scale_color_gradient(low="#d3cbcb", high="#852eaa")+
  xlab("Neighborhood") + ylab("Percentage")

plot+coord_flip()

#Data partition with the Caret package
# Set a random seed
set.seed(101)
trainIndex <- createDataPartition(df$price,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)
# Create Training Data
df.train <- df[trainIndex,]
# Create Validation Data
df.valid <-df[-trainIndex,]


model1 <- train(price~host_response_time+room_type+accommodates+minimum_nights_avg_ntm+
                  maximum_nights_avg_ntm+number_of_reviews+number_of_reviews_ltm+
                  review_scores_rating+review_scores_accuracy+review_scores_cleanliness+
                  review_scores_checkin+review_scores_communication+review_scores_location+
                  review_scores_value,
                data=df.train,
                na.action=na.pass)

summary(model1)


CorrReg2 <- lm(lab_seats_corr ~ lab_vote_corr, data = gb_labour1)
mod <- lm(price ~ number_of_reviews, data = df)

plot(price ~ number_of_reviews)
abline(CorrReg2)










