library(readxl)
NYC_2019 <- read_excel("D:/SJSU/Data Mining for BA/Project/AB_NYC_2019.xlsx")
abnb = NYC_2019
dim(abnb)
str(abnb)
table(is.na(abnb))

#Exploratory data Analysis
###1
library(dplyr)
library(ggplot2)
property_df <-  abnb %>% 
  group_by(neighbourhood_group, room_type) %>% 
  summarize(Freq = n())

total_property <-  abnb %>% 
  filter(room_type %in% c("Private room","Entire home","Entire home")) %>% 
  group_by(neighbourhood_group) %>% 
  summarize(sum = n())

property_ratio <- merge (property_df, total_property, by="neighbourhood_group")

property_ratio <- property_ratio %>% 
  mutate(ratio = Freq/sum)

ggplot(property_ratio, aes(x=neighbourhood_group, y = ratio, fill = room_type)) +
  geom_bar(position = "dodge", stat="identity") + 
  xlab("Borough") + ylab ("Count") +
  scale_fill_discrete(name = "Property Type") + 
  scale_y_continuous(labels = scales::percent) +
  ggtitle("",
          subtitle = "Count of Listing Type by Borough ") +
  theme(plot.title = element_text(face = "bold", size = 14) ) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35", hjust = 0.5)) +
  theme(plot.caption = element_text(color = "grey68"))+scale_color_gradient(low="#d3cbcb", high="#852eaa")+
  scale_fill_manual("Property Type", values=c("#e06f69","#357b8a", "#7db5b8", "#59c6f3", "#f6c458")) +
  xlab("Neighborhoods") + ylab("Percentage")

###2
abnb %>% 
  filter(!(is.na(neighbourhood_group))) %>% 
  filter(!(neighbourhood_group == "Unknown")) %>% 
  group_by(neighbourhood_group) %>% 
  summarise(mean_price = mean(price, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(neighbourhood_group, mean_price), y = mean_price, fill = neighbourhood_group)) +
  geom_col(stat ="identity", color = "black", fill="#357b8a") +
  coord_flip() +
  theme_gray() +
  labs(x = "Neighbourhood Group", y = "Price") +
  geom_text(aes(label = round(mean_price,digit = 2)), hjust = 2.0, color = "white", size = 3.5) +
  ggtitle("", subtitle = "") + 
  xlab("Neighbourhood Group") + 
  ylab("Mean Price") +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "darkblue", hjust = 0.5),
        axis.title.y = element_text(),
        axis.title.x = element_text(),
        axis.ticks = element_blank())

###3
abnb %>% 
  filter(!(is.na(room_type))) %>% 
  filter(!(room_type == "Unknown")) %>% 
  group_by(room_type) %>% 
  summarise(mean_price = mean(price, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(room_type, mean_price), y = mean_price, fill = room_type)) +
  geom_col(stat ="identity", color = "black", fill="#357b8a") +
  coord_flip() +
  theme_gray() +
  labs(x = "Room Type", y = "Price") +
  geom_text(aes(label = round(mean_price,digit = 2)), hjust = 2.0, color = "white", size = 3.5) +
  ggtitle("", subtitle = "") + 
  xlab("Type") + 
  ylab("Average Price") +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "darkblue", hjust = 0.5),
        axis.title.y = element_text(),
        axis.title.x = element_text(),
        axis.ticks = element_blank())


#Data Cleaning
##Identifying null value variables
apply(abnb,2,function(x) sum(is.na(x)))

##Filling Null Values
abnb$reviews_per_month = ifelse(is.na(abnb$reviews_per_month),ave(abnb$reviews_per_month, FUN = function(x) mean(x, na.rm = TRUE)),abnb$reviews_per_month)
table(is.na(abnb))

##Deleting unnecessary variables
abnb = abnb[,-c(1,2,3,4,6,13)]
str(abnb)
apply(abnb,2,function(x) sum(is.na(x)))

##Changing data type 
abnb$neighbourhood_group<- as.factor(abnb$neighbourhood_group)
abnb$room_type<- as.factor(abnb$room_type)

##Splitting Train and Test
subset = sample(nrow(abnb), nrow(abnb)*0.7)
abnb_train = abnb[subset, ]
abnb_test = abnb[-subset, ]

#Model 1
abnb_model_1 = lm (price ~ neighbourhood_group + latitude + longitude + room_type + minimum_nights  + number_of_reviews + reviews_per_month + calculated_host_listings_count +availability_365, data = abnb_train)
summary_model_1 = summary(abnb_model_1)
summary_model_1
#MSE
summary_model_1$sigma^2
#MAE
mean(abs(summary_model_1$sigma))
summary_model_1$r.squared
summary_model_1$adj.r.squared
AIC(abnb_model_1)
BIC(abnb_model_1)

#Starting points for Backward elimination
nullmodel = lm(price ~ 1, data = abnb_train)
fullmodel = lm(price ~ ., data = abnb_train)

##backward elimination
model.step <- step(fullmodel, direction = "backward")

#Model_2
abnb_model_2 = lm (price ~ neighbourhood_group + latitude + longitude + room_type + number_of_reviews + reviews_per_month + calculated_host_listings_count +availability_365, data = abnb_train)
summary_model_2 = summary(abnb_model_2)
summary_model_2$sigma^2 #MSE
mean(abs(summary_model_2$sigma)) #MAE
summary_model_2$r.squared
summary_model_2$adj.r.squared
AIC(abnb_model_2)
BIC(abnb_model_2)

#Backward Elimination Summary
library(olsrr)
BWDfit.aic = ols_step_backward_p(abnb_model_1)
BWDfit.aic = ols_step_backward_aic(abnb_model_1)
BWDfit.aic

#Prediction
#In sample
pi = predict(abnb_model_2)
#MSE
mean((pi - abnb_train$price)^2)
#MAE
mean(abs(pi - abnb_train$price))

#Out-of-sample
pi = predict(abnb_model_2, newdata = abnb_test)
#MSE
mean((pi - abnb_test$price)^2)
#MAE
mean(abs(pi - abnb_test$price))

##Cross validation
library(boot)
#MSE
abnb_model3 = glm(price ~ neighbourhood_group + latitude + longitude + room_type + number_of_reviews + reviews_per_month + calculated_host_listings_count +availability_365, data = abnb)
cv.glm(data = abnb, glmfit = abnb_model3, K = 10)$delta[2]
#MAE
abnb_model3 = glm(price~neighbourhood_group + latitude + longitude + room_type + number_of_reviews + reviews_per_month + calculated_host_listings_count +availability_365, data = abnb)
MAE_cost = function(pi, r) {
  return(mean(abs(pi - r)))
}
cv.glm(data = abnb, glmfit = abnb_model3, cost = MAE_cost, K = 10)$delta[2]


##Regression Tree
library(rpart)
library(rpart.plot)

##Model Fitting
abnb_rpart <- rpart(formula = price ~ ., data = abnb_train)

#printing and plotting
abnb_rpart
prp(abnb_rpart,digits =4, extra =1)

#Better to avoid tree pruning in this case sometimes
#pruning
abnb_largetree = rpart(formula = price~., data = abnb_train, cp = 0.001)

#printing and plotting
prp(abnb_largetree)
plotcp(abnb_largetree)
printcp(abnb_largetree)

treepruned = prune(abnb_largetree, cp = 0.062)
prp(treepruned)

#Optimal tree model
abnb_final = rpart(formula = price~., data = abnb_train, cp = 0.062)

#In-Sample Prediction
abnb_train_pred_tree = predict(abnb_rpart)
resid = abnb_train_pred_tree - abnb_train$price #finding residuals
mean(resid^2) #MSE
mean(abs(resid)) #MAE

#Out-of-Sample prediction
abnb_test_pred_tree = predict(abnb_rpart, abnb_test)
resid = abnb_test_pred_tree - abnb_test$price #finding residuals
mean(resid^2) #MSE
mean(abs(resid)) #MAE
head(mean(abnb_test$price))


##Gradient Boosting 
#Fitting
library(gbm)
abnb.gbm <- gbm(price~., data = abnb_train, distribution = 'gaussian', n.trees = 5000, interaction.depth = 4)
summary(abnb.gbm)
abnb.pred<- predict(abnb.gbm, abnb_train)
#MSE
mean((abnb_train$price-abnb.pred)^2)
#MAE
mean(abs(abnb.pred - abnb_train$price))

#Out-of-Sample-Prediction
abnb.pred2<- predict(abnb.gbm, abnb_test)
#MSE
mean((abnb_test$price-abnb.pred2)^2)
#MAE
mean(abs(abnb.pred2 - abnb_test$price))

#Improvising the model
abnb.gbm.final <- gbm(price~., data = abnb_train, distribution = "gaussian", n.trees=5000, interaction.depth=4, shrinkage = 0.1, verbose=F)
summary(abnb.gbm.final)

#In-Sample-Prediction
abnb.pred1<- predict(abnb.gbm.final, abnb_train)
#MSE
mean((abnb_train$price-abnb.pred1)^2)
#MAE
mean(abs(abnb.pred1 - abnb_train$price))

#Out-of-Sample-Prediction
abnb.pred2<- predict(abnb.gbm.final, abnb_test)
#MSE
mean((abnb_test$price-abnb.pred2)^2)
#MAE
mean(abs(abnb.pred2 - abnb_test$price))
