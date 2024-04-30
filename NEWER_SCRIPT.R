#Loading of Libraries
#Call relevant libraries
library(readxl)
library(ggplot2)
library(skimr)
library(dplyr)
library(gridExtra)

rm(list=ls())
# Set working Directory
setwd("C:/Users/Admin/Documents/Seun Ore/New_folder1")

#Import the data from an excel file
REP_DATA <- read_excel("rep_13.xlsx")
EV_DATA <- read_excel("ev_13.xlsx")

#Inspect the data (observations and variables) for blanks,missing values and outliers
str(REP_DATA) 
str(EV_DATA)
skim(REP_DATA)
skim(EV_DATA)

#Data Cleaning and reprocessing

## Knowing that commission can not be negative, we are removing it as outlier
EV_DATA <- EV_DATA %>% 
  filter(commissions > 0)

### dropping rows with blanks (NA's) in the data majorly from commision, purchase and promotions
EV_DATA <- EV_DATA[complete.cases(EV_DATA), ]

# checking for cleansing 
skim(EV_DATA)
str(EV_DATA)

## droping variable training in rep_13 dataset has it contains alot of missing rows
REP_DATA <- REP_DATA[, -which(names(REP_DATA) == "training")]

###Merging clean 
# Joining REP_DATA and EV_DATA with Inner_join to merge only the rows with matching rep_IDs in both files
REP_EV_DATA <- EV_DATA %>% 
  inner_join(REP_DATA, by = "rep_id")

### Checking
dim(REP_EV_DATA)
skim(REP_EV_DATA)

REP_EV_DATA <- REP_EV_DATA[complete.cases(REP_EV_DATA), ]

### Transformation
#1. Mutating to change the spelling of close name observations in variable (product, jobtype, gender, and training) and 
#   Mutating to Round figures of all observations in variable (commissions, marketing, purchase, and period) to the nearest whole number
REP_EV_DATA2 <- REP_EV_DATA %>%
  mutate (product = recode(product, "sport" = "sports"),
          jobtype = recode(jobtype, "ft" = "full time"),
          gender = recode(gender, "m" = "male"),
          campaign = recode(campaign, "fbook" = "facebook"),
          commissions = round(commissions),
          marketing = round(marketing),
          purchase = round(purchase),
          period = round(period),
          period = as.factor(period),
         # period = recode(period, "0" = "1")
         )

#2. Filtering out unrelated observations from variables (product, promotions, buyer, and campaign)
REP_EV_DATA3 <- REP_EV_DATA2 %>%
  filter(!(product == "farm") &
           !(promotions == "tell") &
           !(buyer == "twos") &
           !(campaign == "still"))

#3. regrouping experience into 4 rating categories
REP_EV_DATA3 <- REP_EV_DATA3 %>%
  mutate(rating_experience = case_when(experience <= 10 ~ "0-10",
                                       experience <= 20 ~ "11-20",
                                       experience <= 30 ~ "21-30",
                                       experience <= 40 ~ "31- 40"))

skim(REP_EV_DATA3)
## Descriptive analysis for categorical data 
# Apply the table function to each character column and convert to percentage
des_rep_data <- lapply(REP_DATA[, sapply(REP_DATA, is.character)], table)
des_rep_data

des_rep_ev_data <- lapply(REP_EV_DATA3[, sapply(REP_EV_DATA3, is.character)], table)
des_rep_ev_data


des_c_percent_REP_EV_DATA3 <- lapply(REP_EV_DATA3[, sapply(REP_EV_DATA3, is.character)], function(x) prop.table(table(x)) * 100)
des_c_percent_REP_EV_DATA3

## product Analysis
cross_tab <- table(REP_EV_DATA3$product, REP_EV_DATA3$campaign)
cross_tab

cross_tab <- table(REP_EV_DATA3$product, REP_EV_DATA3$buyer)
cross_tab

cross_tab <- table(REP_EV_DATA3$product, REP_EV_DATA3$jobtype)
cross_tab

########## DATA ANALYSIS ###########

## How does marketing of the product influence the purchase earn by the retailer.
# this help to know if there is a relationship between marketing an dpurchase
Figure_1 <- ggplot(REP_EV_DATA3, aes(y = purchase, x = marketing)) + 
  geom_point()+
  labs(x = "Purchase in euros", y = "Marketing in euros", title = "Relationship of purchase and money spent on marketing") +
  geom_smooth(method = lm, se = FALSE) + 
  theme_classic()

print(Figure_1)
ggsave(filename = "graph/figure_1.jpg", device = "jpeg")

### How does promotion influences sales
Figure_2 <- ggplot(REP_EV_DATA3, aes(y = purchase, x = promotions)) + 
  geom_jitter(col = "tan4")+
  labs(y = "Purchase in euros", x = "Promotion", title = "Distribution of purchase based on promotion") +
  theme_minimal()

print(Figure_2)
ggsave(filename = "graph/Figure_2.jpg", device = "jpeg")

## How does the gender of sales rep influence purchase
Figure_3 <- ggplot(REP_EV_DATA3, aes(y = purchase, x = gender)) + 
  geom_jitter(col = "brown") + 
  labs(y = "Purchase in euros", x = "gender", title = "Distribution of purchase based on the gender of sales rep") +
  theme_minimal()
print(Figure_3)
ggsave(filename = "graph/Figure_3.jpg", device = "jpeg")

# Which of the product increase or decreases sales
Figure_4 <- ggplot(REP_EV_DATA3, aes(y = purchase, x = product)) + 
  geom_violin(fill = "yellow4")+
  labs(y = "Purchase in euros", x = "product", title = "Distribution of purchase based on product") +
  theme_minimal()

print(Figure_4)
ggsave(filename = "graph/figure_4.jpg", device = "jpeg")

## The relationship between purchase and product can be further observed theor campaigns and product
Figure_5 = REP_EV_DATA3 %>% 
  count(campaign, product) %>%
  mutate(PCT = prop.table(n)) %>%
  ggplot(aes(x = campaign, y = PCT, fill = product, label= scales::percent(round(PCT, 3)))) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(position = position_dodge(width = 0.8), hjust = -0.1, color = "black") +
  scale_fill_manual(labels=c("sedan", "sports", "suv" ),
                    values = c("yellow4", "greenyellow", "green4")) +
  labs(title = "Sales of product through campaigns",
       x = "campaign",
       y = "Percentage of the product bought", 
       fill = "Product") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip()

print(Figure_5)
ggsave(filename = "graph/Figure_5.jpg", device = "jpeg")

# Purchase from buyers based on products, identifying our target audiences 
Figure_6 = REP_EV_DATA3 %>% 
  count(buyer, product) %>%
  mutate(PCT = prop.table(n)) %>%
  ggplot(aes(x = buyer, y = PCT, fill = product, label= scales::percent(round(PCT, 3)))) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.5)) +
  geom_text(position = position_dodge(width = 0.5), hjust = -0.05, color = "black") +
  scale_fill_manual(labels=c("sedan", "sports", "suv" ),
                    values = c("yellow4", "greenyellow", "green4")) +
  labs(title = "Customers interest and products",
       x = "buyer",
       y = "Percentage of products ", 
       fill = "Product") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip()

print(Figure_6)
ggsave(filename = "graph/Figure_6.jpg", device = "jpeg")

# Purchase from buyers based on products, identifying our target audiences 
Figure_7 = REP_EV_DATA3 %>% 
  count(qualification, product) %>%
  mutate(PCT = prop.table(n)) %>%
  ggplot(aes(x = qualification, y = PCT, fill = product, label= scales::percent(round(PCT, 3)))) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.5)) +
  geom_text(position = position_dodge(width = 0.5), hjust = -0.15, color = "black") +
  scale_fill_manual(labels=c("sedan", "sports", "suv" ),
                    values = c("yellow4", "greenyellow", "green4")) +
  labs(title = "Customers interest and products",
       x = "Qualification",
       y = "Percentage of products ", 
       fill = "Product") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip()

print(Figure_7)
ggsave(filename = "graph/Figure_7.jpg", device = "jpeg")


# How does sale rep years of experience and qualification influences sales 
Figure_8 <- REP_EV_DATA3 %>%
  count(jobtype, product) %>%
  mutate(PCT = prop.table(n)) %>%
  ggplot(aes(x = jobtype, y = PCT, fill = product, label= scales::percent(round(PCT, 3)))) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(position = position_dodge(width = 0.3), hjust = -0.05, color = "black") +
  scale_fill_manual(labels=c("sedan", "sports", "suv" ),
                    values = c("yellow4", "greenyellow", "green4")) +
  labs(title = "Customers interest and products",
       x = "Jobtype",
       y = "Percentage of product", 
       fill = "product") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip()

print(Figure_8)
ggsave(filename = "graph/Figure_8.jpg", device = "jpeg")


# How does sale rep years of experience and qualification influences sales 
Figure_9 <- REP_EV_DATA3 %>%
  count(jobtype, qualification) %>%
  mutate(PCT = prop.table(n)) %>%
  ggplot(aes(x = jobtype, y = PCT, fill = qualification, label= scales::percent(round(PCT, 3)))) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(position = position_dodge(width = 0.4), hjust = -0.005, color = "black") +
  scale_fill_manual(labels=c("HND", "BSc", "MSc" ),
                    values = c("tan1", "greenyellow", "red4")) +
  labs(title = "Sale representative qualifications and job type",
       x = "Jobtype",
       y = "Percentage of qualification", 
       fill = "Qualification") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip()

print(Figure_9)
ggsave(filename = "graph/Figure_9.jpg", device = "jpeg")



# Performance dashboard
All_4 = grid.arrange(Figure_4,Figure_5,Figure_6,Figure_7)
print(All_4)
ggsave(filename = "graph/All_4.jpg", device = "jpeg")


### APPENDIX
## This can be in the APPENDIX as a plus to the descriptive table in the 
#descriptive section of the report
Variab <- c("product","promotions","buyer","campaign",  "jobtype" ,"qualification", "gender")
for (k in Variab){
  barplot(table(REP_EV_DATA3[[k]]), col = "green4") + title(main = paste0("Frequency distribution of ", k), xlab = paste0(k), ylab = "Frequency")
}