library(tidyverse)
library(ggplot2)
library(gridExtra)

data<-USArrests                           

# checking if the data is a frame or a tibble, and the data is a dataframe
is.data.frame(data)

# added a categorical variable as SafetyIndex. To calculate safety index I created a new column as CrimeRate
# This is just sum of all the columns for particular row, then based on this I filled in the safety index.
data$CrimeRate <- rowSums(data)
data$SafetyIndex <- c(0*50)

for (i in 1:50) {
  if(data[i,"CrimeRate"] < 200){
    data[i,6] <- "Safe"
  }else if(data[i,"CrimeRate"]>=200 && data[i,"CrimeRate"]<300){
    data[i,6] <- "Moderate"
  }else if(data[i,"CrimeRate"]>=300){
    data[i,6] <- "Poor" 
  }
}

#  converted row names to state column
data$State <- attributes(data)$row.names

# we will be converting the data frame to a tibble as its a neater format of the data frame
# and it is used with the tidyverse and ggplot2 packages

data_tib <- as_tibble(data)
is_tibble(data_tib)

# Performing checks to determine quality of the data (missing values, out liners, etc.)
sum(is.na(data_tib))

# There are no missing values in the data set.

# This is the description of the data
summary(data_tib)

nrow(data_tib)
ncol(data_tib)
# How big is it (number of observations, variables) : 50 observations (Rows), 7 variables (Columns)

is.numeric(data_tib$Murder)
is.numeric(data_tib$Assault)
is.numeric(data_tib$UrbanPop)
is.numeric(data_tib$Rape)
is.numeric(data_tib$CrimeRate)
is.numeric(data_tib$SafetyIndex)
is.numeric(data_tib$State)
# how many numeric variables : 5 variables are numeric
# how many categorical variables : 2 variable (State and SafetyIndex) is Categorical

# description of the variables : 
  # 1. Murder: Numeric 0-18
  # 2. Assault: Numeric 0-350
  # 3. UrbanPop: Numeric, urban population percentage 30-100
  # 4. Rape: Numeric 0-50
  # 5. State: String, Name of the state

# Are there any missing values? No
sum(is.na(data_tib))

# Any duplicate rows? There are no duplicate values
sum(duplicated(data_tib))

# Summary of the data
summary(data_tib)

# Computing murder rate as per the SafetyIndex
aggregate(data_tib$Murder, by = list(data_tib$SafetyIndex), FUN = summary)

# Relationship between variables: Here I am plotting the crime rate based on the urban population,
relationship_graph <- ggplot(data_tib, aes(x = UrbanPop, y = Assault, color = Murder, size = Rape)) +
  geom_point() + scale_color_continuous(name = "Murder", low = "blue", high = "red") + scale_size_continuous(name = "Rape")

relationship_graph

# Line plot for Murder vs. UrbanPop
murder_trend <- ggplot(data_tib, aes(x = UrbanPop, y = Murder)) +
  geom_line() +
  labs(title = "Trend in Murder Rate with Urban Population",
       x = "UrbanPop",
       y = "Murder Rate")

# Line plot for Assault vs. UrbanPop
assault_trend <- ggplot(data_tib, aes(x = UrbanPop, y = Assault)) +
  geom_line() +
  labs(title = "Trend in Assault Rate with Urban Population",
       x = "UrbanPop",
       y = "Assault Rate")

# Line plot for Rape vs. UrbanPop
rape_trend <- ggplot(data_tib, aes(x = UrbanPop, y = Rape)) +
  geom_line() + labs(title = "Trend in Rape Rate with Urban Population",
       x = "UrbanPop",
       y = "Rape Rate")

# here is the trend of all the variables against the urban population
grid.arrange(murder_trend, assault_trend, rape_trend, ncol = 1)

# Summary of the data: there are overall 15 safe, 16 moderate and 19 poor state in the country 
data_tib %>% group_by(SafetyIndex) %>% summarise(count = n()) %>% ggplot() + geom_bar(mapping = aes(x = SafetyIndex, y = count),  stat = "identity" )


# Finally I have been able to compute which states are safer to live in compared to others, as well as I am able to 
# analyze the crime rate compared to the urban population in the state.