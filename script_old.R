#Loading of Libraries
library(tidyverse)
library(readxl)
library(stats)

# data importing, Cleaning and Preprocessing
rep_13 <- read_excel("C:/Users/Admin/Documents/Seun Ore/New_folder1/rep_13.xlsx")
ev_13 <- read_excel("C:/Users/Admin/Documents/Seun Ore/New_folder1/ev_13.xlsx")

summary(rep_13)
summary(ev_13)
names(rep_13)

### Cleaning data : consistency, removal of outlier done in excel
ev_13$Period <- floor(ev_13$period)# to ensure period takes value from 1 to 12 month aa
### dropping rows with NA's
ev_13 <- ev_13[complete.cases(ev_13), ]

## droping variable training in rep_13 dataset has it contains alot of missing rows
rep_13 <- rep_13[, -which(names(rep_13) == "training")]

###Merging clean 
marged_Data <- merge.data.frame(ev_13, rep_13, by = "rep_id")
dim(marged_Data)
summary(marged_Data)
names(marged_Data)

# standard deviation
# Calculate standard deviation for all numeric columns
sds <- apply(marged_Data[, sapply(marged_Data, is.numeric)], 2, sd)

# Print the results
print(sds)

## Descriptive analysis for categorical data 
des_c_R <- lapply(rep_13[, sapply(rep_13, is.character)], table)
des_c_R

## EV data
des_c_EV <- lapply(ev_13[, sapply(ev_13, is.character)], table)
des_c_EV

# Apply the table function to each character column and convert to percentage
des_c_percent_EV <- lapply(ev_13[, sapply(ev_13, is.character)], function(x) prop.table(table(x)) * 100)
des_c_percent_EV


# Visualisation
## creating variables to easy the for loop processs
Variab <- c("product","promotions","buyer","campaign",  "jobtype" ,"qualification", "gender")
variable_1 = c("commissions","marketing","purchase" )
variable_2 = c("product" , "campaign", "promotions","buyer")
variable_3 = c("jobtype","qualification", "gender","experience")

## descriptive analysis
u = ggplot(marged_Data, aes(Period)) +
  geom_bar(binwidth = 3)
ggsave(u, path = "C:/Users/Admin/Documents/Seun Ore/New_folder1/graph", device = "png")

for (k in Variab){
  barplot(table(marged_Data[[k]]), col = "green4") + title(main = paste0("Frequency distribution of ", k), xlab = paste0(k), ylab = "Frequency")
}



## product Analysis
cross_tab <- table(marged_Data$product, marged_Data$campaign)
cross_tab

cross_tab <- table(marged_Data$product, marged_Data$buyer)
cross_tab

cross_tab <- table(marged_Data$product, marged_Data$jobtype)
cross_tab

for (i in variable_1) {
  p <- ggplot(marged_Data, aes(x = Period, y = marged_Data[[i]])) +
    geom_col(col = "brown") +
    facet_wrap(~product, scales = "free_y") +
    labs(title = paste("Distribution of product verses", i, "over 12 months"),
         x = "Period", y = i) +
    theme_minimal()
  print(p)
  ggsave(p, path = "C:/Users/Admin/Documents/Seun Ore/New_folder1/graph", device = "png")
  
}

## Purchase analysis
## To study the relationship between the purchase and money spent on marketing
figure_4 <- ggplot(marged_Data, aes(x= marketing, y = purchase)) + 
  geom_point()+
  labs(y = "Purchase in euros", x = "marketing in euros", title = "Relationship of purchase and money spent on marketing") +
  geom_smooth(method = lm, se = FALSE) + 
  theme_classic()
ggsave(figure_4, path = "C:/Users/Admin/Documents/Seun Ore/New_folder1/graph", device = "png")


## Distribution of sales based on Sales representative bio information
for (x in variable_3) {
  SR_graph <- ggplot(marged_Data, aes(y = purchase, x = marged_Data[[x]])) +
    geom_jitter(col = "brown") +
    labs(title = paste("Sales verses", x),
         x = x, y = "Purchase") +
    theme_minimal()
  print(SR_graph)
  ggsave(SR_graph, path = "C:/Users/Admin/Documents/Seun Ore/New_folder1/graph", device = "png")
  
}

## Distribution of Purchase verses other idicator of EVs
for (x in variable_2) {
  G <- ggplot(marged_Data, aes(y = purchase, x = marged_Data[[x]])) +
    geom_violin(col = "green4") +
    labs(title = paste("Sales verses", x),
         x = x, y = "Purchase") +
    theme_minimal()
  print(G)
  ggsave(G, path = "C:/Users/Admin/Documents/Seun Ore/New_folder1/graph", device = "png")
  
}

