#Installing the packages
install.packages("dummies")
install.packages("caret")
install.packages("e1071")
install.packages("fpp2")
install.packages("lattice")
install.packages("gains")
install.packages("pROC")
install.packages("MASS")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("corrplot")
install.packages("arules")
install.packages("recommenderlab")
install.packages("rpart")
install.packages("rpart.plot")

#importing the packages
library(dummies)
library(fpp2)
library(lattice)
library(gains)
library(e1071)
library(pROC)
library(MASS)
library(caret)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(corrplot)
library(arules)
library(recommenderlab)
library(rpart)
library(rpart.plot)

#Reading the data
newyork.df<-read.csv('R_Project_NYC.csv')
head(newyork.df)
str(newyork.df)
summary(newyork.df)

#To check the number of missing values
sapply(newyork.df, function(x) sum(is.na(x)))
##square_feet variable has 43768 records having NA out of 44317. This shows that the data was not collected/recorded properly for this variable
##Thus, we need to eliminate the square_feet variable.

#Eliminating the square_feet variable
names(newyork.df)
newyork.df<-data.frame(newyork.df[,-18])

#To check the number of missing values
sapply(newyork.df, function(x) sum(is.na(x)))
#Removing the NA values
newyork.df<-na.omit(newyork.df)
sapply(newyork.df, function(x) sum(is.na(x)))
View(newyork.df)
#Even after removing the NA values the data still contains "N/A" values 
##which needs to replaced with NA 
newyork.df[newyork.df=="N/A"] <- NA
sapply(newyork.df, function(x) sum(is.na(x)))

#Imputing the NA values
newyork.df$host_response_time<-as.character(newyork.df$host_response_time)
newyork.df$host_response_time[is.na(newyork.df$host_response_time)]<- "Not specified"
newyork.df$host_response_rate<-as.character(newyork.df$host_response_rate)
newyork.df$host_response_rate[is.na(newyork.df$host_response_rate)]<-"0%"
sapply(newyork.df, function(x) sum(is.na(x)))

##Removing the blank values
newyork.df <- newyork.df[-which(newyork.df$host_response_time == ""), ]
View(newyork.df)

newyork.df$host_response_time<-as.factor(newyork.df$host_response_time)
newyork.df$host_response_rate<-gsub("%", "", newyork.df$host_response_rate)
newyork.df$host_response_rate<-as.numeric(newyork.df$host_response_rate)

#Splitting the amenities column into readable information
Amen<-as.character(newyork.df$amenities)
Amen<-gsub("[][!#$%()*.:;\"<=>@^_`|~.{}]", "", Amen)
library(stringr)
StrA<-(str_split_fixed(Amen, ",", 87))
Id<-as.data.frame(newyork.df[,1])
Final<-cbind(Id,StrA)
library(reshape2)
colnames(Final)[1] <- "id"
Amenities<-recast(Final, id ~ value, id.var = "id", length)


#Combining the Amenities required for further analysis
newyork.df$'24_hour_checkin'<-as.factor(Amenities$`24-hour check-in`)
newyork.df$'Air_Conditioning'<-as.factor(Amenities$`Air conditioning`)
newyork.df$'Heating'<-as.factor(Amenities$Heating)
newyork.df$'Television'<-as.factor(as.numeric(Amenities$TV|Amenities$`Cable TV`))
newyork.df$'Baby_Friendly'<-as.factor(as.numeric(Amenities$`Baby bath`|Amenities$`Baby monitor`|Amenities$`Babysitter recommendations`))
newyork.df$'Family/Kid_Friendly'<-as.factor(Amenities$`Family/kid friendly`)
newyork.df$'Coffee_Maker'<-as.factor(Amenities$`Coffee maker`)
newyork.df$'Fire_Extinguisher'<-as.factor(Amenities$`Fire extinguisher`)
newyork.df$'First_Aid_Kit'<-as.factor(Amenities$`First aid kit`)
newyork.df$'Free_Parking'<-as.factor(as.numeric(Amenities$`Free parking on premises`|Amenities$`Free parking on street`))
newyork.df$'Hot_water'<-as.factor(as.numeric(Amenities$`Hot water`|Amenities$`Hot water kettle`))
newyork.df$'Hair_dryer'<-as.factor(Amenities$`Hair dryer`)
newyork.df$'Indoor_Fireplace'<-as.factor(Amenities$`Indoor fireplace`)
newyork.df$'Patio/balcony'<-as.factor(Amenities$`Patio or balcony`)
newyork.df$'private_bathroom'<-as.factor(Amenities$`Private bathroom`)
newyork.df$'Self_checkin'<-as.factor(Amenities$`Self Check-In`)
newyork.df$'Suitable_for_events'<-as.factor(Amenities$`Suitable for events`)
newyork.df$'Breakfast'<-as.factor(Amenities$Breakfast)
newyork.df$'Elevator'<-as.factor(Amenities$Elevator)
newyork.df$'Kitchen'<-as.factor(as.numeric(Amenities$Kitchen|Amenities$Microwave|Amenities$Oven|Amenities$Stove))
newyork.df$'Pillow&Mattress'<-as.factor(as.numeric(Amenities$`Extra pillows and blankets`|Amenities$`Firm mattress`|Amenities$`Bed linens`))
newyork.df$'Laptop_Friendly_Workspace'<-as.factor(as.numeric(Amenities$`Laptop friendly workspace`|Amenities$`Wireless Internet`|Amenities$`Ethernet connection`|Amenities$Internet|Amenities$`Pocket wifi`))
newyork.df$'Pets_Allowed'<-as.factor(as.numeric(Amenities$`Pets allowed`|Amenities$`Pets live on this property`|Amenities$Cats|Amenities$Dogs|Amenities$`Other pets`))
newyork.df$'Washer/Dryer'<-as.factor(as.numeric(Amenities$Washer|Amenities$`Washer / Dryer`|as.numeric(!Amenities$Dryer)))
newyork.df$'Wheelchair_Accessible'<-as.factor(as.numeric(Amenities$`Accessible-height bed`|Amenities$`Accessible-height toilet`|Amenities$`Wheelchair accessible`|Amenities$`Step-free access`))
newyork.df$'Essentials'<-as.factor(as.numeric(Amenities$Essentials|Amenities$Hangers|Amenities$Iron))


#removing the amenities column
names(newyork.df)
newyork.df<-data.frame(newyork.df[,-c(17)])

#The Variable neighbourhood_cleansed has 217 different levels
##Mapping neighbourhoods to 8 different locations
str(newyork.df$neighbourhood_cleansed)
newyork.df$Location[newyork.df$neighbourhood_cleansed == 'Marble Hill'| newyork.df$neighbourhood_cleansed =='Allerton' 
                    | newyork.df$neighbourhood_cleansed == 'Baychester'| newyork.df$neighbourhood_cleansed == 'Belmont'| newyork.df$neighbourhood_cleansed =='City Island'
                    | newyork.df$neighbourhood_cleansed =='Claremont Village'| newyork.df$neighbourhood_cleansed == 'Clason Point'
                    |newyork.df$neighbourhood_cleansed =='Co-op City'| newyork.df$neighbourhood_cleansed == 'East Morrisania'| newyork.df$neighbourhood_cleansed == 'Eastchester'|newyork.df$neighbourhood_cleansed == 'Edenwald'
                    | newyork.df$neighbourhood_cleansed =='Fieldston'| newyork.df$neighbourhood_cleansed == 'Fordham'| newyork.df$neighbourhood_cleansed == 'Highbridge'| newyork.df$neighbourhood_cleansed == 'Hunts Point'
                    | newyork.df$neighbourhood_cleansed =='Inwood'| newyork.df$neighbourhood_cleansed == 'Kingsbridge' 
                    | newyork.df$neighbourhood_cleansed =='Morris Heights'| newyork.df$neighbourhood_cleansed == 'Morris Park'| newyork.df$neighbourhood_cleansed == 'Morrisania'| newyork.df$neighbourhood_cleansed == 'Mott Haven'| newyork.df$neighbourhood_cleansed == 'Mount Eden' 
                    | newyork.df$neighbourhood_cleansed =='Mount Hope'| newyork.df$neighbourhood_cleansed == 'North Riverdale'| newyork.df$neighbourhood_cleansed == 'Norwood, Parkchester'| newyork.df$neighbourhood_cleansed == 'Pelham Bay' 
                    | newyork.df$neighbourhood_cleansed =='Pelham Gardens'| newyork.df$neighbourhood_cleansed == 'Port Morris'| newyork.df$neighbourhood_cleansed == 'Riverdale'| newyork.df$neighbourhood_cleansed == 'Schuylerville'| newyork.df$neighbourhood_cleansed == 'Soundview'
                    | newyork.df$neighbourhood_cleansed =='Throgs Neck'| newyork.df$neighbourhood_cleansed == 'Tremont'| newyork.df$neighbourhood_cleansed == 'Unionport'| newyork.df$neighbourhood_cleansed == 'University Heights'| newyork.df$neighbourhood_cleansed == 'Van Nest' 
                    | newyork.df$neighbourhood_cleansed =='Wakefield'| newyork.df$neighbourhood_cleansed == 'West Farms'| newyork.df$neighbourhood_cleansed == 'Westchester Square'| newyork.df$neighbourhood_cleansed == 'Williamsbridge'| newyork.df$neighbourhood_cleansed =='Woodlawn' 
                    | newyork.df$neighbourhood_cleansed =='Bronxdale'| newyork.df$neighbourhood_cleansed =='Longwood'|newyork.df$neighbourhood_cleansed =='Norwood'|newyork.df$neighbourhood_cleansed == 'Parkchester'] <- 'Bronx, NY'
newyork.df$Location[newyork.df$neighbourhood_cleansed =='Bath Beach'| newyork.df$neighbourhood_cleansed == 'Bay Ridge'
                    | newyork.df$neighbourhood_cleansed =='Bedford-Stuyvesant'| newyork.df$neighbourhood_cleansed =='Bensonhurst'| newyork.df$neighbourhood_cleansed == 'Bergen Beach'
                    | newyork.df$neighbourhood_cleansed =='Boerum Hill'| newyork.df$neighbourhood_cleansed =='Borough Park'| newyork.df$neighbourhood_cleansed =='Brooklyn Heights'| newyork.df$neighbourhood_cleansed =='Brownsville'
                    | newyork.df$neighbourhood_cleansed =='Bushwick'| newyork.df$neighbourhood_cleansed =='Canarsie'| newyork.df$neighbourhood_cleansed == 'Carroll Gardens'
                    | newyork.df$neighbourhood_cleansed =='Clinton Hill'| newyork.df$neighbourhood_cleansed == 'Cobble Hill'| newyork.df$neighbourhood_cleansed =='Columbia St'| newyork.df$neighbourhood_cleansed =='Concord'
                    | newyork.df$neighbourhood_cleansed =='Coney Island'| newyork.df$neighbourhood_cleansed == 'Crown Heights'| newyork.df$neighbourhood_cleansed =='Cypress Hills'
                    | newyork.df$neighbourhood_cleansed =='Downtown Brooklyn'| newyork.df$neighbourhood_cleansed =='Dyker Heights'| newyork.df$neighbourhood_cleansed =='East Flatbush'
                    | newyork.df$neighbourhood_cleansed =='East New York'| newyork.df$neighbourhood_cleansed == 'Flatbush'| newyork.df$neighbourhood_cleansed == 'Flatlands'
                    | newyork.df$neighbourhood_cleansed =='Fort Greene'| newyork.df$neighbourhood_cleansed =='Fort Hamilton'| newyork.df$neighbourhood_cleansed =='Gowanus'| newyork.df$neighbourhood_cleansed =='Gravesend'
                    | newyork.df$neighbourhood_cleansed =='Greenpoint'
                    | newyork.df$neighbourhood_cleansed =='Kensington'| newyork.df$neighbourhood_cleansed =='Manhattan Beach'| newyork.df$neighbourhood_cleansed =='Mill Basin'
                    | newyork.df$neighbourhood_cleansed =='Navy Yard'| newyork.df$neighbourhood_cleansed =='Park Slope'| newyork.df$neighbourhood_cleansed =='Prospect Heights'
                    | newyork.df$neighbourhood_cleansed =='Prospect-Lefferts Gardens'| newyork.df$neighbourhood_cleansed =='Red Hook'| newyork.df$neighbourhood_cleansed =='Sea Gate'
                    | newyork.df$neighbourhood_cleansed =='Sheepshead Bay'| newyork.df$neighbourhood_cleansed =='South Slope'| newyork.df$neighbourhood_cleansed == 'Spuyten Duyvil'
                    | newyork.df$neighbourhood_cleansed =='Sunset Park'| newyork.df$neighbourhood_cleansed == 'Vinegar Hill'| newyork.df$neighbourhood_cleansed == 'Williamsburg'| newyork.df$neighbourhood_cleansed == 'Windsor Terrace'
                    | newyork.df$neighbourhood_cleansed == 'DUMBO'|newyork.df$neighbourhood_cleansed=='Midwood'] <- 'Brooklyn, NY'
newyork.df$Location[newyork.df$neighbourhood_cleansed =='Civic Center'| newyork.df$neighbourhood_cleansed =='NoHo'| newyork.df$neighbourhood_cleansed =='Nolita'
                    | newyork.df$neighbourhood_cleansed =='Lower East Side'| newyork.df$neighbourhood_cleansed =='Tribeca'| newyork.df$neighbourhood_cleansed =='Two Bridges'| newyork.df$neighbourhood_cleansed =='Chinatown'
                    | newyork.df$neighbourhood_cleansed =='East Village'| newyork.df$neighbourhood_cleansed =='Financial District'| newyork.df$neighbourhood_cleansed =='Greenwich Village'
                    | newyork.df$neighbourhood_cleansed =='Little Italy'| newyork.df$neighbourhood_cleansed == 'SoHo'| newyork.df$neighbourhood_cleansed =='Stuyvesant Town'| newyork.df$neighbourhood_cleansed == 'West Village'
                    | newyork.df$neighbourhood_cleansed =='Battery Park City'] <- 'Downtown, Manhattan, NY'
newyork.df$Location[newyork.df$neighbourhood_cleansed =='East Harlem'| newyork.df$neighbourhood_cleansed =='Harlem'| newyork.df$neighbourhood_cleansed =='Washington Heights'] <- 'Uptown, Manhattan, NY'
newyork.df$Location[newyork.df$neighbourhood_cleansed =="Upper East Side"| newyork.df$neighbourhood_cleansed == "Midtown"
                    | newyork.df$neighbourhood_cleansed == "Oakwood"| newyork.df$neighbourhood_cleansed == "Chelsea"
                    | newyork.df$neighbourhood_cleansed == "Concourse"| newyork.df$neighbourhood_cleansed == "Concourse Village"
                    | newyork.df$neighbourhood_cleansed == "Flatiron District"| newyork.df$neighbourhood_cleansed == "Hell's Kitchen"
                    | newyork.df$neighbourhood_cleansed == "Kips Bay"| newyork.df$neighbourhood_cleansed == "Morningside Heights"
                    | newyork.df$neighbourhood_cleansed == "Murray Hill"| newyork.df$neighbourhood_cleansed == "Theater District"
                    | newyork.df$neighbourhood_cleansed == "Gramercy"|newyork.df$neighbourhood_cleansed=='Melrose'|newyork.df$neighbourhood_cleansed=='Upper West Side'] <- 'Midtown, Manhattan, NY'
newyork.df$Location[newyork.df$neighbourhood_cleansed =='Arverne'| newyork.df$neighbourhood_cleansed == 'Astoria'| newyork.df$neighbourhood_cleansed =='Bay Terrace'
                    | newyork.df$neighbourhood_cleansed =='Bayside'| newyork.df$neighbourhood_cleansed =='Bayswater'| newyork.df$neighbourhood_cleansed =='Briarwood'
                    | newyork.df$neighbourhood_cleansed =='Brighton Beach'| newyork.df$neighbourhood_cleansed == 'Cambria Heights'| newyork.df$neighbourhood_cleansed =='College Point'
                    | newyork.df$neighbourhood_cleansed =='Corona'| newyork.df$neighbourhood_cleansed == 'Ditmars Steinway'| newyork.df$neighbourhood_cleansed =='Douglaston'
                    | newyork.df$neighbourhood_cleansed =='East Elmhurst'| newyork.df$neighbourhood_cleansed =='Edgemere'| newyork.df$neighbourhood_cleansed == "Elmhurst"
                    | newyork.df$neighbourhood_cleansed =='Far Rockaway'| newyork.df$neighbourhood_cleansed == 'Flushing'| newyork.df$neighbourhood_cleansed == 'Forest Hills'
                    | newyork.df$neighbourhood_cleansed =='Fresh Meadows'| newyork.df$neighbourhood_cleansed == 'Glen Oaks'| newyork.df$neighbourhood_cleansed =='Glendale'
                    | newyork.df$neighbourhood_cleansed =='Hollis'| newyork.df$neighbourhood_cleansed == 'Hollis Hills'| newyork.df$neighbourhood_cleansed =='Holliswood'
                    | newyork.df$neighbourhood_cleansed =='Howard Beach'| newyork.df$neighbourhood_cleansed == 'Jackson Heights'| newyork.df$neighbourhood_cleansed == 'Jamaica'
                    | newyork.df$neighbourhood_cleansed =='Jamaica Estates'| newyork.df$neighbourhood_cleansed == 'Jamaica Hills'| newyork.df$neighbourhood_cleansed =='Kew Gardens'
                    | newyork.df$neighbourhood_cleansed =='Kew Gardens Hills'| newyork.df$neighbourhood_cleansed =='Laurelton'| newyork.df$neighbourhood_cleansed =='Little Neck'| newyork.df$neighbourhood_cleansed =='Long Island City'
                    | newyork.df$neighbourhood_cleansed =='Maspeth'| newyork.df$neighbourhood_cleansed == 'Middle Village'| newyork.df$neighbourhood_cleansed =='Neponsit'
                    | newyork.df$neighbourhood_cleansed =='Ozone Park'| newyork.df$neighbourhood_cleansed == 'Queens Village'| newyork.df$neighbourhood_cleansed == 'Rego Park'
                    | newyork.df$neighbourhood_cleansed =='Richmond Hill'
                    | newyork.df$neighbourhood_cleansed =='Ridgewood'| newyork.df$neighbourhood_cleansed == 'Rockaway Beach'| newyork.df$neighbourhood_cleansed =='Rosedale'
                    | newyork.df$neighbourhood_cleansed =='South Ozone Park'| newyork.df$neighbourhood_cleansed == 'Springfield Gardens'| newyork.df$neighbourhood_cleansed == 'St. Albans'
                    | newyork.df$neighbourhood_cleansed =='Sunnyside'| newyork.df$neighbourhood_cleansed =='Whitestone'| newyork.df$neighbourhood_cleansed =='Woodhaven'| newyork.df$neighbourhood_cleansed == 'Woodside'
                    | newyork.df$neighbourhood_cleansed =='Belle Harbor'| newyork.df$neighbourhood_cleansed == 'Bellerose'] <- 'Queens, NY'
newyork.df$Location[newyork.df$neighbourhood_cleansed =='Clifton'| newyork.df$neighbourhood_cleansed =='Dongan Hills'| newyork.df$neighbourhood_cleansed =='Lighthouse Hill'
                    | newyork.df$neighbourhood_cleansed =='Arden Heights'| newyork.df$neighbourhood_cleansed =='Arrochar'| newyork.df$neighbourhood_cleansed =='Castleton Corners'
                    | newyork.df$neighbourhood_cleansed =='Eltingville'| newyork.df$neighbourhood_cleansed =='Emerson Hill'| newyork.df$neighbourhood_cleansed =='Graniteville'| newyork.df$neighbourhood_cleansed =='Grant City'
                    | newyork.df$neighbourhood_cleansed =='Great Kills'| newyork.df$neighbourhood_cleansed =='Grymes Hill'| newyork.df$neighbourhood_cleansed =='Mariners Harbor'
                    | newyork.df$neighbourhood_cleansed =='Midland Beach'| newyork.df$neighbourhood_cleansed == 'New Brighton'| newyork.df$neighbourhood_cleansed == 'New Springville'
                    | newyork.df$neighbourhood_cleansed =='Port Richmond'| newyork.df$neighbourhood_cleansed == 'Randall Manor'| newyork.df$neighbourhood_cleansed == 'Richmondtown'
                    | newyork.df$neighbourhood_cleansed =='Shore Acres'| newyork.df$neighbourhood_cleansed =='Silver Lake'| newyork.df$neighbourhood_cleansed =='St. George'
                    | newyork.df$neighbourhood_cleansed =='Stapleton'| newyork.df$neighbourhood_cleansed == 'Todt Hill'| newyork.df$neighbourhood_cleansed =='Tompkinsville'
                    | newyork.df$neighbourhood_cleansed =='Tottenville'| newyork.df$neighbourhood_cleansed == 'West Brighton'| newyork.df$neighbourhood_cleansed == 'Castle Hill'| newyork.df$neighbourhood_cleansed == 'Howland Hook'
                    | newyork.df$neighbourhood_cleansed =='Huguenot'| newyork.df$neighbourhood_cleansed == 'Rosebank'| newyork.df$neighbourhood_cleansed == 'Woodrow'| newyork.df$neighbourhood_cleansed == 'South Beach'] <- 'Staten Island'
newyork.df$Location[newyork.df$neighbourhood_cleansed =='Roosevelt Island'] <- 'Roosevelt Island, NY'
newyork.df$Location <- as.factor(newyork.df$Location)
str(newyork.df)
#Now, the datset has one more column named "Location" with 8 levels
levels(newyork.df$Location)

#Converting the review_scores_rating on a scale of 1 to 5
newyork.df$rating[newyork.df$review_scores_rating >= 0 & newyork.df$review_scores_rating <= 10] <- '1'
newyork.df$rating[newyork.df$review_scores_rating > 10 & newyork.df$review_scores_rating <= 35] <- '2'
newyork.df$rating[newyork.df$review_scores_rating > 35 & newyork.df$review_scores_rating <= 60] <- '3'
newyork.df$rating[newyork.df$review_scores_rating > 60 & newyork.df$review_scores_rating <= 90] <- '4'
newyork.df$rating[newyork.df$review_scores_rating > 90 & newyork.df$review_scores_rating <= 100] <- '5'

newyork.df$rating <- as.factor(newyork.df$rating)
str(newyork.df)
#removing the review_scores_rating column
names(newyork.df)
newyork.df<-data.frame(newyork.df[,-c(6,24)])

#Not enough data to calculate a success variable based on rating
summary(newyork.df$number_of_reviews)
hist(newyork.df$number_of_reviews,breaks = c(0,20,50,100,150,200,300,500))


#Calculating the occupancy and the revenue generated
oc <- function(a){30-a}
newyork.df$occupancy <- oc(newyork.df$availability_30)
mul <- function(a,b){a*b}
newyork.df$revenue <- mul(newyork.df$price, newyork.df$occupancy)
success<-as.integer(ifelse(newyork.df$success_based_on_revenue=="0",0,1))

#Calculating the Success variable based on the revenue generated
agg<-data.frame(aggregate(newyork.df$revenue, by = list(newyork.df$Location), FUN = mean))

newyork.df$avg_revenue[newyork.df$Location == 'Bronx, NY'] <- agg[1,2]
newyork.df$avg_revenue[newyork.df$Location == 'Brooklyn, NY'] <- agg[2,2]
newyork.df$avg_revenue[newyork.df$Location == 'Downtown, Manhattan, NY'] <- agg[3,2]
newyork.df$avg_revenue[newyork.df$Location == 'Midtown, Manhattan, NY'] <- agg[4,2]
newyork.df$avg_revenue[newyork.df$Location == 'Queens, NY'] <- agg[5,2]
newyork.df$avg_revenue[newyork.df$Location == 'Roosevelt Island, NY'] <- agg[6,2]
newyork.df$avg_revenue[newyork.df$Location == 'Staten Island'] <- agg[7,2]
newyork.df$avg_revenue[newyork.df$Location == 'Uptown, Manhattan, NY'] <- agg[8,2]

newyork.df$success_based_on_revenue <- ifelse(newyork.df$revenue >= newyork.df$avg_revenue, 1,0)
table(newyork.df$success_based_on_revenue)
newyork.df$success_based_on_revenue <- as.factor(newyork.df$success_based_on_revenue)

str(newyork.df)





#Filtering the price variable
hist(newyork.df$price)
summary(newyork.df$price)
newyork.df<-subset(newyork.df,price>0)

#Filtering the bedrooms variable
hist(newyork.df$bedrooms)
table(newyork.df$bedrooms)
#There are very few entries more than 8 bedrooms. So, we would delete this as 
# if later any of this becomes a part of test data, we would get an error since it would never be trained.
# Removing entries having more than 8 bedrooms
newyork.df <- newyork.df[newyork.df$bedrooms <= 8, ]

##remove property type which have frequency less than 5 of the total records
PT <- as.data.frame(table(newyork.df$property_type))
names(PT) <- c("property_Type","Freq")
Freq <- PT$property_Type[PT$Freq <= 5]
Freq
newyork.df$property_type[newyork.df$property_type == "Boat"|newyork.df$property_type =="Cabin"|
                           newyork.df$property_type =="Castle"|newyork.df$property_type =="Cave"|
                           newyork.df$property_type =="Chalet"|newyork.df$property_type =="Earth House"|
                           newyork.df$property_type =="In-law"|newyork.df$property_type =="Serviced apartment"|
                           newyork.df$property_type =="Tent"|newyork.df$property_type =="Train"|newyork.df$property_type =="Treehouse"|
                           newyork.df$property_type =="Yurt"] <- NA
sapply(newyork.df, function(x) sum(is.na(x)))
newyork.df<-na.omit(newyork.df)
sapply(newyork.df, function(x) sum(is.na(x)))
newyork.df$property_type <- droplevels(newyork.df$property_type)
levels(newyork.df$property_type)



###EDA

# correlation between numeric variable
numeric.var <- sapply(newyork.df, is.numeric)
corr.matrix <- cor(newyork.df[,numeric.var])
corrplot(corr.matrix, main = '\n\n Correlation plot for Numeric Variables',
         method = "number")

#Boxplot>Bedrooms,log(Price)
ggplot(newyork.df, aes(x = factor(bedrooms) , y = log(price), fill = factor(bedrooms))) +
  geom_boxplot(alpha=0.5) +
  labs(x = "No. of Bedrooms", y = "price")+
  scale_fill_manual(values= rainbow(12))

#Barplot>Bedrooms,Success based on revenue
ggplot(newyork.df, aes(x=bedrooms,fill=success_based_on_revenue)) + 
  geom_bar(position = "dodge") + scale_x_continuous(breaks=0:14) +
  geom_text(aes(label=..count..),stat="count",position=position_dodge(0.9),vjust=-0.2)+ 
  theme(text = element_text(size=10))

#Boxplot>location,log(Price)
ggplot(newyork.df, aes(x = factor(Location) , y = log(price), fill = factor(Location))) +
  geom_boxplot(alpha=0.5) +
  labs(x = "Locations", y = "Price")+
  scale_fill_manual(values= rainbow(9))+
  theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1))

#Boxplot>Room type,log(Price)
ggplot(newyork.df, aes(x = factor(room_type) , y = log(price), fill = factor(room_type))) +
  geom_boxplot(alpha=0.5) +
  labs(x = "Type of Room", y = "Price")+
  scale_fill_manual(values= rainbow(9))+
  theme(text = element_text(size=10))

#Boxplot>Business Travel Ready,log(Price)
ggplot(newyork.df, aes(x = factor(is_business_travel_ready) , y = log(price), fill = factor(is_business_travel_ready))) +
  geom_boxplot(alpha=0.5) +
  labs(x = "is_business_travel_ready", y = "Price")+
  scale_fill_manual(values= rainbow(9))+
  theme(text = element_text(size=10))

#Barplot>Business travel ready,Success based on revenue
ggplot(newyork.df, aes(x=Family.Kid_Friendly,fill=success_based_on_revenue)) + 
  geom_bar(position = "dodge") +
  geom_text(aes(label=..count..),stat="count",position=position_dodge(0.9),vjust=-0.2)+ 
  theme(text = element_text(size=10))

#Barplot>Business travel ready,Success based on revenue
ggplot(newyork.df, aes(x=Family.Kid_Friendly,fill=success_based_on_revenue)) + 
  geom_bar(position = "dodge") +
  geom_text(aes(label=..count..),stat="count",position=position_dodge(0.9),vjust=-0.2)+ 
  theme(text = element_text(size=10))

#Types of bed
ggplot(newyork.df, aes(x=property_type)) + 
     geom_bar(position = "dodge") +
     geom_text(aes(label=..count..),stat="count",position=position_dodge(0.9),vjust=-0.2)+ 
  theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1))

# Exploratory analysis for all the amenities
p1 <- ggplot(newyork.df, aes(x=X24_hour_checkin)) + ggtitle("X24_hour_checkin") + xlab("X24_hour_checkin") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(newyork.df, aes(x=Air_Conditioning)) + ggtitle("Air_Conditioning") + xlab("Air_Conditioning") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(newyork.df, aes(x=Heating)) + ggtitle("Heating") + xlab("Heating") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(newyork.df, aes(x=Television)) + ggtitle("Television") + xlab("Television") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p5 <- ggplot(newyork.df, aes(x=Baby_Friendly)) + ggtitle("Baby_Friendly") + xlab("Baby_Friendly") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p6 <- ggplot(newyork.df, aes(x=Family.Kid_Friendly)) + ggtitle("Family.Kid_Friendly") + xlab("Family.Kid_Friendly") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p7 <- ggplot(newyork.df, aes(x=Coffee_Maker)) + ggtitle("Coffee_Maker") + xlab("Coffee_Maker") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p8 <- ggplot(newyork.df, aes(x=Fire_Extinguisher)) + ggtitle("Fire_Extinguisher") + xlab("Fire_Extinguisher") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8,ncol=2)


p9 <- ggplot(newyork.df, aes(x=First_Aid_Kit)) + ggtitle("First_Aid_Kit") + xlab("First_Aid_Kit") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p10 <- ggplot(newyork.df, aes(x=Free_Parking)) + ggtitle("Free_Parking") + xlab("Free_Parking") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p11 <- ggplot(newyork.df, aes(x=Hot_water)) + ggtitle("Hot_water") + xlab("Hot_water") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p12 <- ggplot(newyork.df, aes(x=Suitable_for_events)) + ggtitle("Suitable_for_events") + xlab("Suitable_for_events") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p13 <- ggplot(newyork.df, aes(x=Breakfast)) + ggtitle("Breakfast") + xlab("Breakfast") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p14 <- ggplot(newyork.df, aes(x=Wheelchair_Accessible)) + ggtitle("Wheelchair_Accessible") + xlab("Wheelchair_Accessible") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p15 <- ggplot(newyork.df, aes(x=Laptop_Friendly_Workspace)) + ggtitle("Laptop_Friendly_Workspace") + xlab("Laptop_Friendly_Workspace") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p16 <- ggplot(newyork.df, aes(x=Kitchen)) + ggtitle("Kitchen") + xlab("Kitchen") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p9, p10, p11, p12, p13, p14, p15, p16, ncol=2)


p17 <- ggplot(newyork.df, aes(x=Hair_dryer)) + ggtitle("Hair_dryer") + xlab("Hair_dryer") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p18 <- ggplot(newyork.df, aes(x=Indoor_Fireplace)) + ggtitle("Indoor_Fireplace") + xlab("Indoor_Fireplace") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p19 <- ggplot(newyork.df, aes(x=Patio.balcony)) + ggtitle("Patio.balcony") + xlab("Patio.balcony") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p20 <- ggplot(newyork.df, aes(x=Essentials)) + ggtitle("Essentials") + xlab("Essentials") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p21 <- ggplot(newyork.df, aes(x=Self_checkin)) + ggtitle("Self_checkin") + xlab("Self_checkin") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p22 <- ggplot(newyork.df, aes(x=Elevator)) + ggtitle("Elevator") + xlab("Elevator") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p23 <- ggplot(newyork.df, aes(x=Pillow.Mattress)) + ggtitle("Pillow.Mattress") + xlab("Pillow.Mattress") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p24 <- ggplot(newyork.df, aes(x=Pets_Allowed)) + ggtitle("Pets_Allowed") + xlab("Pets_Allowed") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p17, p18, p19, p20, p21, p22, p23, p24, ncol=2)



#Plots for all the amenities taking into consideration the success of a airbnb rental based on its revenue

ny <- c(28:53)
for (val in ny) {
  #Function for calculating the average success rate and the average monthly revenue
  Amenity<-function(val){
    success<-as.integer(newyork.df$success_based_on_revenue)
    success<-as.integer(ifelse(newyork.df$success_based_on_revenue=="0",0,1))
    names1 <- c("yes/No","SuccessRate")
    names2 <- c("Yes/No","Avg.Revenue")
    #Average success rate 
    agg3<-data.frame(setNames(aggregate(success, by = list(newyork.df[[val]]), FUN = mean),names1))
    
    #Average revenue
    agg4<-data.frame(setNames(aggregate(newyork.df$revenue, by = list(newyork.df[[val]]), FUN = mean),names2))
    agg5<-cbind(agg3,agg4)
    agg5<-agg5[,-3]
    print(agg5)
  }
  
  print(names(newyork.df[val]))
  Amenity(val)
  
 
  
  plot <- ggplot(newyork.df, aes(x=newyork.df[[val]],fill=success_based_on_revenue)) +
    
    geom_bar(position = "dodge") +
    
    geom_text(aes(label=..count..),stat="count",position=position_dodge(0.9),vjust=-0.2)+
    
    xlab(names(newyork.df[val]))
  
  theme(text = element_text(size=10))
  
  print(plot)
  
}

##Associative rules [AMENITIES]
### Create a Binary Incidence Matrix
amenities.df <- newyork.df[, c(24,29,31,32,33,45,47,49,52)]
amenities.df$is_business_travel_ready <- ifelse(amenities.df$is_business_travel_ready == "t", 1, 0)
amenities.df <- ifelse(amenities.df == 1, 1, 0)
amenities.mat <- as.matrix(amenities.df)


###  Convert Binary Incidence Matrix -> Transactions Database
amenities.trans <- as(amenities.mat, "transactions")
inspect(head(amenities.trans, 10))

### Amenity-Frequency Plot
itemFrequencyPlot(amenities.trans, horiz = TRUE, col = "blue", border = NA,xlab = "Amenity Frequency Plot")


### Apriori function
rules <- apriori(amenities.trans, 
                 parameter = list(supp= 200/4000, conf = 0.5, target = "rules"))

### Inspecting top 30 rules
inspect(head(sort(rules, by = "lift"),30))



##MODELS


#Splitting the dataset into training and validation data[Train=70%  Validation=30%]
set.seed(96)
training.index <- createDataPartition(newyork.df$price, p = 0.7, list = FALSE)
final.train <- newyork.df[training.index, ]
final.valid <- newyork.df[-training.index, ]

###MODEL 1------
#Linear Regression with all variables
LM<-lm(price ~ ., data= final.train)
options(scipen=999)
summary(LM)

#Histogram of price variable
hist(newyork.df$price)

#Histogram of log(price)
hist(log(newyork.df$price))


#Linear Regression with log(price) as the dependent variable
LM1<-lm(log(price) ~ ., data= final.train)
options(scipen=999)
summary(LM1)

##Selecting the significant predictors

names(final.train)
Sign_Pred<-final.train[,c(4,6,7,9,10,11,12,18,19,21,22,25,27,30,33,44,53,54,55,57,59)]
vif_names<-names(Sign_Pred)
vif_names
def_form <- as.formula(paste("log(price) ~" ,paste(vif_names, collapse = " +"),sep = ""))
Final_LM <- lm(def_form, data = final.train)
summary(Final_LM)



linear.reg.pred <- predict(Final_LM, final.valid)
predictnew<-data.frame(cbind(Actuals=log(final.valid$price),Pred=linear.reg.pred))
CorAcc<-cor(predictnew)
CorAcc

RMSE(linear.reg.pred,log(final.valid$price))


#Plotting the actual and predicted
ggplot(predictnew,aes(x=Actuals,y=Pred))+geom_point()+geom_abline(color="blue")

#MODEL2------

#Logistic regression with all variables
Sign_Pred1<-final.train[,c(2:15,17:19,21:55)]
vif_names1<-names(Sign_Pred1)
def_form1 <- as.formula(paste("success_based_on_revenue ~" ,paste(vif_names1, collapse = " +"),sep = ""))
Lreg<-glm(def_form1, data= final.train, family= "binomial")
options(scipen=999)
summary(Lreg)

#Logistic regression with significant variables
Sign_Pred2<-final.train[,c(4,6,7,9:13,17,18,21,22,24,26,27,33,54)]
vif_names2<-names(Sign_Pred2)
def_form2 <- as.formula(paste("success_based_on_revenue ~" ,paste(vif_names2, collapse = " +"),sep = ""))
LogModel<-glm(def_form2, data= final.train, family= "binomial")
options(scipen=999)
summary(LogModel)
LogModel.pred <- predict(LogModel, final.valid[,-59],type = "response")
predict<-ifelse(LogModel.pred > 0.5,1,0)
confusionMatrix(table(Predicted = predict, Actual = final.valid$success_based_on_revenue),positive = '1')

success_num <- ifelse(final.valid$success_based_on_revenue == '0', 0,1)

gain <- gains(success_num, LogModel.pred, groups = 10)

### Plot Lift Chart
plot(c(0,gain$cume.pct.of.total*sum(success_num))~c(0,gain$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", main = "Lift Chart", type = "l")
lines(c(0,sum(success_num))~c(0, dim(final.valid)[1]), lty = 5)

### Plot decile-wise chart
heights <- gain$mean.resp/mean(success_num)
midpoints <- barplot(heights, names.arg = gain$depth,  ylim = c(0,9), col = "blue",  
                     xlab = "Percentile", ylab = "Mean Response", 
                     main = "Decile-wise lift chart")
### add labels to columns
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)

#MODEL3---------
##LDA
#90% of the price are lower than 250, so get rid of the outliers
newyork.df1 <- subset(newyork.df,newyork.df$price <= 250)  

#Creating a Price Categorical variable
newyork.df1$priceCat<- NA
#separate the price into 3 price categories by 33%, 66%, 99% of the dataset
newyork.df1$priceCat <- as.factor(ifelse(newyork.df1$price >= 1 & newyork.df1$price <= 75, 'Low',ifelse(newyork.df1$price > 127 & newyork.df1$price <= 250,'Medium','High')))

#Splitting the dataset into training and validation data[Train=70%  Validation=30%]
set.seed(96)
training.index1 <- createDataPartition(newyork.df1$price, p = 0.7, list = FALSE)
#select significant variables
final.train1 <- newyork.df1[training.index1, c(2,3,9,10,11,12,13,21,22,54,55,57,60)]
final.valid1 <- newyork.df1[-training.index1, c(2,3,9,10,11,12,13,21,22,54,55,57,60)]



#Normalizing the data
norm.values1  <- preProcess(final.train1, method = c("center", "scale"))
# Transform the data using the estimated parameters
train.norm1 <- predict(norm.values1, final.train1)
valid.norm1 <- predict(norm.values1, final.valid1)

LDA <-lda(priceCat ~ ., data= train.norm1)
LDA
summary(LDA) 
LDA.reg.pred <- predict(LDA, valid.norm1)
confusionMatrix(LDA.reg.pred$class,valid.norm1$priceCat)

LDA.plot <- cbind(train.norm1, predict(LDA)$x)
ggplot(LDA.plot, aes(LD1,LD2)) +
  geom_point(aes(color = priceCat))



#MODEL 4---------
##Decision Tree Algorithm

# Create a Decision tree model with default parameters
Tree <- rpart(success_based_on_revenue~ ., data = final.train,cp=0.006)
options(scipen = 999)
prp(Tree)
ly.rf.predicted2<-predict(Tree,final.valid)
df8 <- data.frame(test=final.valid$success_based_on_revenue,Pred=ly.rf.predicted2)
df8$v<-NA
df8$v<-ifelse(df8$Pred.1>0.5,1,0)
confusionMatrix(table(df8$test,df8$v),positive = "1")

tree_num <- ifelse(final.valid$success_based_on_revenue == '0', 0,1)
gain <- gains(tree_num, ly.rf.predicted2[,2], groups = 6)

### Plot Lift Chart
plot(c(0,gain$cume.pct.of.total*sum(tree_num))~c(0,gain$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", main = "Lift Chart", type = "l")
lines(c(0,sum(tree_num))~c(0, dim(final.valid)[1]), lty = 5)





