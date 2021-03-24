################################################################################
#Air France Internet Marketing Case Study
#Team 13 - Yasmeen Khaled, Anna Mourao, Brian Kurniawan, Doston Yunusmatov
#Hult International Business School
#Data Science: R - MsBA1
################################################################################

#Calling the Air France Excel File
library(readxl)
airfrance <- read_excel("C:/Users/dosto/OneDrive/Desktop/R Team Assignment/Air France Case Spreadsheet Supplement.xls",
                        sheet = "DoubleClick")
View(original_airfrance)

#Analyzing and understanding the data within Air France
summary(original_airfrance)
subset(original_airfrance)

#Creating a new data frame composed of only the variables we deem important
#Looking at the frequency table for each variable

new_airfrance <- original_airfrance[, c("Publisher Name", "Keyword", "Match Type", "Campaign", "Keyword Group",
                               "Status", "Search Engine Bid", "Clicks", "Avg. Cost per Click",
                               "Impressions", "Engine Click Thru %", "Avg. Pos.", "Trans. Conv. %",
                               "Total Cost/ Trans.", "Amount", "Total Cost", "Total Volume of Bookings")]

#Checking if the new_airfrance data set has any NA values
sum(is.na(new_airfrance))

#Changing variable types to numeric
new_airfrance$`Publisher Name` <-  as.numeric(as.factor(new_airfrance$`Publisher Name`))
#Google-Global = 1, Google-US = 2, MSN-Global = 3, MSN-US = 4, Overture-Global = 5, Overture-US = 6, Yahoo-US = 7
table(new_airfrance$`Publisher Name`)

new_airfrance$`Match Type` <- as.numeric(as.factor(new_airfrance$`Match Type`))
#Advance = 1, Broad = 2, Exact = 3, N/A = 4, Standard = 5
table(new_airfrance$`Match Type`)

new_airfrance$Campaign <- as.numeric(as.factor(new_airfrance$Campaign))
#Air France Branded = 1, Air France Branded & French Destinations = 2, Air France Global Campaign = 3
#Business Class = 4, French Destinations = 5, General Terms = 6, Geo Targeted Atlanta = 7, Geo Targeted Boston = 8
#Geo Targeted Chicago = 9, Geo Targeted Cincinnati = 10, Geo Targeted DC = 11, Geo Targeted Detroit = 12
#Geo Targeted Houston = 13, Geo Targeted Los Angeles = 14, Geo Targeted Miami = 15, Geo Targeted New York = 16
#Geo Targeted Philadelphia = 17, Geo Targeted San Francisco = 18, Geo Targeted Seattle = 19, Google_Yearlong 2006 = 20
#Outside Western Europe = 21, Paris & France Terms = 22, Unassigned = 23, Western Europe Destinations = 24
table(new_airfrance$Campaign)

new_airfrance$`Keyword Group` <- as.numeric(as.factor(new_airfrance$`Keyword Group`))
table(new_airfrance$`Keyword Group`)

new_airfrance$Status <- as.numeric(as.factor(new_airfrance$Status))
#Deactivated = 1, Live = 2, Paused = 3, Sent = 4, Unavailable = 5
table(new_airfrance$Status)

#Adding a new variable to the data set - PoA (Probability of Action)
#PoA = Engine Click Thru % * Trans. Conv. %
#How many people perform an action before getting to the website * How many people perform an action after getting to the website
#PoA is the probability of an action being performed on the website based on visits before and after getting to it
new_airfrance$PoA <- new_airfrance$`Engine Click Thru %` * new_airfrance$`Trans. Conv. %`
table(new_airfrance$PoA)

#Adding a new variable to the data set - RoA (Return on Advertising Expenditure)
#RoA = Amount / Total Cost
#Marketing metric that measures the amount of revenue a business earns for each dollar it spends on advertising
new_airfrance$RoA <- new_airfrance$Amount / new_airfrance$`Total Cost`
table(new_airfrance$RoA)

#The frequency tables showed a vast number of observations with 0 values in RoA and PoA 
#We've decided to divided the datasets to get a deeper look into the observations


################################################################################
#CREATING SEPARATE DATAFRAMES TO ANALYZE DATA BETTER
################################################################################


#Creating new dataframe in which there is no 0 value for RoA
new_airfrance_subset <- subset(new_airfrance, RoA!=0)

#Dissecting the dataframe based on publisher to analyze specified observations
google_airfrance <- subset(new_airfrance_subset, `Publisher Name`== 1:2)
msn_airfrance <- subset(new_airfrance_subset, `Publisher Name`== 3:4)
overture_airfrance <- subset(new_airfrance_subset, `Publisher Name`== 5:6)
yahoo_airfrance <- subset(new_airfrance_subset, `Publisher Name`== 7)


################################################################################
#CREATING SCATTER PLOTS
################################################################################


#Calling ggplot function
library(ggplot2)
#Calling the plotly function for interactive scatter plots
library(plotly)

#Analyzing Publisher Name impacts on RoA
publisher_roa_scatter <- ggplot()+
  labs(title = "Publisher Name Impact", x="Publisher Name", y="RoA")+
  geom_point(data = google_airfrance, aes(x=`Publisher Name`, y=RoA), color="red")+
  geom_point(data = msn_airfrance, aes(x=`Publisher Name`, y=RoA), color="blue")+
  geom_point(data = yahoo_airfrance, aes(x=`Publisher Name`, y=RoA), color="purple")+
  geom_point(data = overture_airfrance, aes(x=`Publisher Name`, y=RoA), color="green")
ggplotly(publisher_roa_scatter)

#Analyzing Publisher Name impacts on PoA
publisher_poa_scatter <- ggplot()+
  labs(title = "Publisher Name Impact", x="Publisher Name", y="PoA")+
  geom_point(data = google_airfrance, aes(x=`Publisher Name`, y=PoA), color="red")+
  geom_point(data = msn_airfrance, aes(x=`Publisher Name`, y=PoA), color="blue")+
  geom_point(data = yahoo_airfrance, aes(x=`Publisher Name`, y=PoA), color="purple")+
  geom_point(data = overture_airfrance, aes(x=`Publisher Name`, y=PoA), color="green")
ggplotly(publisher_poa_scatter)


#Analyzing Publisher Name impacts on Total Volume of Bookings
tvb_scatter <- ggplot(data = new_airfrance_subset)+
  labs(title = "Publisher Name Impact", x="Publisher Name", y="Total Volume of Bookings")+
  geom_point(data = google_airfrance, aes(x=`Publisher Name`, y=`Total Volume of Bookings`), color="red")+
  geom_point(data = msn_airfrance, aes(x=`Publisher Name`, y=`Total Volume of Bookings`), color="blue")+
  geom_point(data = yahoo_airfrance, aes(x=`Publisher Name`, y=`Total Volume of Bookings`), color="purple")+
  geom_point(data = overture_airfrance, aes(x=`Publisher Name`, y=`Total Volume of Bookings`), color="green")

ggplotly(tvb_scatter)

#Analyzing Keyword Group impacts on RoA and PoA
keyword_group_scatter <- ggplot(data = new_airfrance_subset)+
  labs(title = "Keyword Group Impact", x="Keyword Group", y="")+
  geom_point(aes(x=`Keyword Group`, y=PoA), color="orange")+
  geom_point(aes(x=`Keyword Group`, y=RoA), color="purple")

ggplotly(keyword_group_scatter)

#Analyzing Match Type impacts on RoA and PoA
match_type_scatter <- ggplot(data = new_airfrance_subset)+
  labs(title = "Match Type Impact", x="Match Type", y="")+
  geom_point(aes(x=`Match Type`, y=RoA), color="red")+
  geom_point(aes(x=`Match Type`, y=PoA), color="blue")

ggplotly(match_type_scatter)


################################################################################
#REGRESSION ANALYSIS
################################################################################

#Creating training and testing dataset
new_airfrance_random <- function(df, n){
  
  size <- nrow(df)
  training_index <- sample(1:size, n*size)
  
  training_data <- df[training_index, ]
  testing_data <- df[-training_index, ]
  return(list(training_data, testing_data))
}#Closing the random function

#Creating new dataframe to remove the Inf observation in the RoA variable
new_airfrance2 <- subset(new_airfrance_subset, RoA!="Inf")
new_airfrance_random_data <- new_airfrance_random(df = new_airfrance2, n=0.8)



#Pulling the training and testing dataset

my_training <- new_airfrance_random_data[[1]]
my_testing <- new_airfrance_random_data[[2]]

#############################################
#install.packages("corrplot")
library(corrplot)
#install.packages("ggcorrplot")
library(ggcorrplot)
library(ggplot2)

my_training_cor <- cor(my_training[, 7:19])
ggcorrplot(my_training_cor, method = "circle")

#Creating a model for logit regression

my_linear <- lm(`Engine Click Thru %` ~ `Avg. Pos.`, data = my_training)
summary(my_linear)  

### The result is that every unit increase in average position od the ads will decrease Engine Click Thru by 4.24 


### Creating a multiple regression model

my_multiple <- glm(my_training$Amount ~ my_training$`Avg. Cost per Click`+my_training$`Engine Click Thru %`
                   +my_training$`Avg. Pos.`+my_training$`Trans. Conv. %`+my_training$`Total Volume of Bookings`
                   +my_training$Clicks)

summary(my_multiple)

my_multiple2 <- glm(my_training$RoA ~ my_training$`Avg. Cost per Click`+my_training$`Engine Click Thru %`
                    +my_training$`Avg. Pos.`+my_training$`Trans. Conv. %`+my_training$`Total Volume of Bookings`
                    +my_training$Clicks)

summary(my_multiple2)

my_multiple3 <- glm(my_training$`Total Volume of Bookings` ~ my_training$`Avg. Cost per Click`+my_training$`Engine Click Thru %`
                    +my_training$`Avg. Pos.`+my_training$`Trans. Conv. %`
                    +my_training$Clicks)

summary(my_multiple3)

my_final_linear <- lm(my_training$RoA ~ my_training$PoA)
summary(my_final_linear)








  
  