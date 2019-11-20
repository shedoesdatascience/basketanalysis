###############################################
#### Name: basketanalysis.R ####
#### Author: Sheenal Srivastava            ####
### Purpose: Insights and model data       ####
###############################################

options(warnings=-1)

#### 1. Import and load packages ####  
PKGs <- c("arules","arulesViz","tidyverse","knitr","DataExplorer", "ggplot2", "plyr", "dplyr","RColorBrewer","readr")
lapply(PKGs, library, character.only = TRUE ,quietly = T)



## read csv into R dataframe
retail <- read_delim("C:/Users/User/Downloads/TEST.csv", 
                     "|")

## remove rows with Nas or missing values
retail <- retail[complete.cases(retail), ]
retail <- na.omit(retail) 
plot_missing(retail) #no missing values

## Convert data types to make them meaningful for analysis
retail <- retail %>% 
  mutate(MerchCategoryName = as.factor(MerchCategoryName)) %>%
  mutate(CategoryName = as.factor(CategoryName)) %>% 
  mutate(SubCategoryName = as.factor(SubCategoryName)) %>%
  mutate(StoreState = as.factor(StoreState)) %>%
  mutate(OrderType = as.factor(OrderType)) %>% 
  mutate (BasketID = as.numeric(BasketID)) %>% 
  mutate(MerchCategoryCode = as.numeric(MerchCategoryCode)) %>%
  mutate(CategoryCode = as.numeric(CategoryCode)) %>% 
  mutate(SubCategoryCode = as.numeric(SubCategoryCode)) %>% 
  mutate(ProductName = as.factor(ProductName))

#remove NAs as there are some codes with "Freight" or have "C" in them
summary(retail)
retail <- na.omit(retail) 



#remove negative sales and negative quantity as these may be erroneous values
retail <- retail[retail$Qty > 0, ]
retail <- retail[retail$SalesGross > 0,]


#### Data exploration ####
ggplot(data = retail) +
  geom_bar(mapping = aes(x = StoreState))


retail %>% 
  dplyr::count(StoreState)

# Finding: Highest number of transactions from VIC, followed by QLD - shows sales distribution

#Only plot min to 3rd Quartile as Max is an outlier with 17,523 and hides distribution
#Furthermore, sales gross value of 0.013 does not make sense
#Finding: Most gross sales values around $0-$40 (median is $37.60). Distribution is skewed right. 
ggplot(retail) + 
  geom_histogram(mapping = aes(x = SalesGross), binwidth = 0.5) +
  coord_cartesian(xlim = c(0, 100))

#Gross sales distribution by state
ggplot(data = retail, mapping = aes(x = SalesGross, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = StoreState), binwidth = 0.1)+
  coord_cartesian(xlim=c(0,100))

#better to use boxplot for clearer distribution of Data
#Boxplots ordered by state to see trend
ggplot(data = retail) +
  geom_boxplot(mapping = aes(x = reorder(StoreState, SalesGross, FUN = median), y = SalesGross))
#Most data concentrated between the $0-40 range. Rest are outliers. Highest oulier for NT (max sales gross for data)


#Not worth exploring other categorical values such as Category code due to too many values - i.e. 99 merch category code

retail %>% 
  dplyr::count(StoreState, CategoryName) %>%  
  ggplot(mapping = aes(x = StoreState, y = CategoryName)) +
  geom_tile(mapping = aes(fill = n))

#Finding: Highest number of transactions in Vic related to 1) Camping, Apparel and & Gas/BBQ. 
#Finding: Minimum to no transactions for bikes, paint & panel and promotions
# Finding: Ski transactions not popular in most states - maybe stock less quantity or none at all?


#plotting takes too long - use summary stats instead
retail_grp_year <- group_by(retail, # data_frame object
                            CalendarYear) # column name to group by
tally(retail_grp_year)
#Finding: 2.5 times more transactions in 2016 than 2017. What went wrong? 

# what is the mean Gross Sales value per year?
summarize(retail_grp_year, 
          mean(SalesGross)   # calculate the annual mean of airt
) 

#Despite the higher number of transactions in 2016 over 2017, mean Gross Sales was higher in 2017 (86.0 over 69.0)

#Let's look at monthly sales

retail_grp_month <- group_by(retail, # data_frame object
                             CalendarMonth) # column name to group by
View(tally(retail_grp_month))
#In 2016, highest number of sales in January and March with steep declines in Sept to Nov and then an increase in Dec. 
#However, transactions continue to decline in 2017 with an increase in Dec (Xmas season)

#Deduction: As highest number of sales is for BBQ & Gas and Camping year, it makes sense that sales for these products is high only during holiday season
#Recommendation: May want to explore whether stores have sufficient stock for these products in Dec-Jan as they are the most popular



# what is the mean Gross Sales value per month?
View(summarize(retail_grp_month, 
               mean(SalesGross)   # calculate the annual mean of airt
) )

#Finding: Despite the steady decline in the number of transactions, mean gross sales continue to increase month on month with it being highest in Dec 2017
#Deductions: Fewer customers but purchases of products of greater value. 


#### 3. Basket Analysis to determine which products are customers likely to buy together in order to make recommendations for products

## Note: Outliers in Gross Sales have not been treated in data as not relevant for basket analysis
transactionData <- ddply(retail,c("BasketID","BasketDate"),
                         function(df1)paste(df1$ProductName,
                                            collapse = ","))

#set column BasketID and BasketDate of dataframe transactionData  to null as they will not be used in analysis
transactionData$BasketID <- NULL
#set column Date of dataframe transactionData
transactionData$BasketDate <- NULL
#Rename column to items
colnames(transactionData) <- c("items")
#Show Dataframe transactionData
View(transactionData)

#Write data to file
write.csv(transactionData,"C:/Users/User/Downloads/market_basket_transactions.csv", quote = FALSE, row.names = FALSE)


#load data in transaction form
tr <- read.transactions('C:/Users/User/Downloads/market_basket_transactions.csv', format = 'basket', sep=',')
#sep tell how items are separated. In this case you have separated using ','

summary(tr)
#There are 1019952 collections (baskets) of items and 21209 items. 

#Density tells the percentage of non-zero cells in a sparse matrix. 
#It is the total number of items that are purchased divided by a possible number of items in that matrix. 
#You can calculate how many items were purchased by using density:  1019952*21209*0.0000953 = 2061545

#Element (itemset/transaction) length distribution: This tells you you how many transactions are there for 1-itemset, 
#for 2-itemset and so on. 
#The first row is telling you a number of items and the second row is telling you the number of transactions

#Findings: Majority of baskets (87%) consist of between 1-3 items (546138+234643+109888)/1019952
#Min number of items in a basket = 1 and max = 46 (only one basket)
#Most popular items are gas bottle, gas bottle refill, gripwell, and peg tent

itemFrequencyPlot(tr,topN=10,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'), main="Relative Item Frequency Plot")
# If absolute it will plot numeric frequencies of each item independently. 
#If relative it will plot how many times these items have appeared as compared to others.

#highest sales for gas related stuff - stock them together in store or send customer EDM


#The apriori will take tr as the transaction object on which mining is to be applied. parameter will allow you to set min_sup and min_confidence. 
#The default values for parameter are minimum support of 0.1, 
#the minimum confidence of 0.8, maximum of 10 items (maxlen).


# Min Support as 0.001, confidence as 0.5 - This was done because output included 0 rules for where 80% or more (confidence)
# had a probability of being bought together. This is expected due to the sparsity in data for frequent items bought where majority of purchases relate to gas related products
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.5,maxlen=10))
#Support = proportion of transactions that contain the given item-set
#Support used to get frequent item-sets only
#Confidence = % of transactions in which item A is bought with item B in all transactions of A
#Lift gives the correlation between A and B in the rule A=>B. 
# Correlation shows how one item-set A effects the item-set B.
#If the rule had a lift of 1,then A and B are independent and no rule can be derived from them.
#If the lift is > 1, then A and B are dependent on each other, and the degree of which is given by lift value.
#If the lift is < 1, then presence of A will have negative effect on B.
summary(association.rules) #customer buys either 2 items or 7 items (rule length)
#Probability of customers buying items together with confidence ranges from 54% to 79%, where buying item A has a positive
# effect on buying item B (as lift values are all greater than 1)

#inspect rules
inspect(association.rules[1:7])
#interpretation of rules
# If a customer buys the 9kg gas bottle, there is a 79% chance that customer will also buy its refill




#remove rules that are subsets of larger rules
subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1) # get subset rules in vector
length(subset.rules)  #
subset.association.rules <- association.rules[-subset.rules] # remove subset rules
inspect(subset.association.rules)


#finding rules related to given items
#Tried different variations but no rules resulted as expected despite dropping confidence to 20%
giftcard.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.2),appearance = list(lhs="GIFTCARD",default="rhs"))#people who bought giftcard also bought

# Filter rules with confidence greater than 0.5 or 50%
subRules<-association.rules[quality(association.rules)$confidence>0.4]
#Plot SubRules
plot(subRules)
plot(subRules,method="two-key plot") #order is number of items in rule i.e. 2

plotly_arules(subRules)

plot(subRules, method = "graph",  engine = "htmlwidget")

plot(subRules, method="paracoord")

#Conclusion: The most popular/frequent items have confounded the analysis to some extent where it appears that we can 
# only make recommendations with respect to only seven association rules with confidence. This is due to uneven distribution of 
# of items by frequency in the basket. 

