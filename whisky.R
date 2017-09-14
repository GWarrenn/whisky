library("ggplot2")
library("directlabels")
library("plyr")
library("ggthemes")

##########################################

## Regression on Whisky Age and Bottle Price

whisky_data_w_years <- read.csv("P:\\whisky.csv")

y <- whisky_data_w_years$price
x <- whisky_data_w_years$year

initial_model <- lm(y ~ x)

summary(initial_model)$r.squared

predict(initial_model,data.frame(x=15),interval="confidence")

ggplot(whisky_data_w_years, aes(x=whisky_data_w_years$year, y=whisky_data_w_years$price),) + 
  geom_smooth(method='glm',formula=y~x) + geom_point(aes(colour=whisky_data_w_years$rating),size=4) +
  labs(caption = "Source: Whiskyton API https://github.com/cuducos/whiskyton", 
         x = "Whisky Age (Years)", y = "Bottle Price",colour="Rating") + 
  ggtitle("Whisky Age & Price") + theme_fivethirtyeight() + 
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text()) + ylab('Bottle Price')

y <- whisky_data_w_years$price
x <- whisky_data_w_years$year
r <- whisky_data_w_years$region

model_w_region <- lm(y ~ x + r)

summary(model_w_region)$r.squared

predict(model_w_region,data.frame(x=15,r="Speyside"),interval="confidence")
predict(model_w_region,data.frame(x=15,r="Japan"),interval="confidence")
predict(model_w_region,data.frame(x=15,r="Highland"),interval="confidence")

##########################################

whisky_data <- read.csv("P:\\whisky.csv")

whisky_data$mean_rating <- mean(whisky_data$rating)
whisky_data$mean_price <- mean(whisky_data$price)
whisky_data$value[whisky_data$rating>=whisky_data$mean_rating & whisky_data$price>=whisky_data$mean_price] <- "Above average rating & price" 
whisky_data$value[whisky_data$rating<whisky_data$mean_rating & whisky_data$price>=whisky_data$mean_price] <- "Below average rating & Above average price" 
whisky_data$value[whisky_data$rating>=whisky_data$mean_rating & whisky_data$price<whisky_data$mean_price] <- "Above average rating & Below average price" 
whisky_data$value[whisky_data$rating<whisky_data$mean_rating & whisky_data$price<whisky_data$mean_price] <- "Below average rating & price" 

ggplot(whisky_data, aes(x=whisky_data$rating, y=whisky_data$price)) + 
  geom_point(aes(colour=whisky_data$value),size=4) + geom_vline(xintercept = whisky_data$mean_rating) + 
  geom_hline(yintercept = whisky_data$mean_price) +
  labs(caption = "Source: Whiskyton API https://github.com/cuducos/whiskyton", 
       x = "Rating", y = "Bottle Price",colour="Rating") + 
  ggtitle("Whisky Rating & Price") + theme_fivethirtyeight() + 
  theme(plot.title = element_text(hjust = 0.5),axis.title = element_text()) + ylab('Bottle Price')

## Best bang for your buck (for the classy cheapskates)

whisky_data <- read.csv("P:\\whisky_all.csv")

whisky_data$best_value <- whisky_data$rating/whisky_data$price

top_10 <- arrange(whisky_data,desc(best_value))

top_10 = top_10[1:10,]

ggplot(top_10, aes(x = reorder(top_10$name, top_10$best_value), y = top_10$best_value,fill=top_10$best_value)) + 
  geom_bar(stat = "identity",show.legend=FALSE) + coord_flip() + 
  labs(caption = "Source: Whiskyton API https://github.com/cuducos/whiskyton", 
       y = "Best Value: Rating/Price", x = "") +
  geom_text(aes(label=paste("price = $",top_10$price,sep="")), vjust=-1,hjust=-0.1, position=position_dodge(.5), size=5,) +
  geom_text(aes(label=paste("rating =",top_10$rating)), vjust=1,hjust=-0.1, position=position_dodge(.5), size=5) +  ylim(c(0,8))
