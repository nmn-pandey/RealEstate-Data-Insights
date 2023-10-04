## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Add code here to load any required libraries with `library()`.  
# We suggest you use `install.package()` for any required packages externally to this document 
# since installation only need be done once.
#install.packages("ggplot2")
#install.packages("ggrepel")
#install.packages("tidyverse")
#install.packages("validate")
#install.packages('caret')

library(ggplot2) # for plotting
library(ggrepel) # for plot labels in pie chart
library(tidyverse) # for mutating data 
library(modeest) # for calculating mode
library(validate) # for validator object
library(car) # for QQ plot method and vif
library(moments) # for skewness test
library(caret) # for confusion matrix 


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Assign your student id into the variable SID, for example:
SID <- 2246041                  # This is an example, replace 2101234 with your actual ID
SIDoffset <- (SID %% 100) + 1    # Your SID mod 100 + 1

load("house-analysis.RDa")
# Now subset the housing data set
# Pick every 100th observation starting from your offset
# Put into your data frame named mydf (you can rename it) -> renamed to housedf
housedf <- house.analysis[seq(from=SIDoffset,to=nrow(house.analysis),by=100),]


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Custom function for plotting used towards data quality analysis

#Defining functions for plotting via ggplot

#Custom Pie chart using ggplot
custom_pie <- function(var,  title="", xlab, ylab){
  if(title==""){
    title=paste("Pie Chart for",xlab)
  }
  
  df <- as.data.frame(table(var))
  df2 <- df %>% 
    mutate(csum = rev(cumsum(rev(Freq))), 
           pos = Freq/2 + lead(csum, 1),
           pos = if_else(is.na(pos), Freq/2, pos))
  ggplot(df, aes(x = "" , y = Freq, fill = var)) + geom_col(width = 1, color=alpha("white",alpha = 0.3)) +
  coord_polar(theta = "y") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(round(100*Freq/sum(Freq),2), "%")),
                   size = 3, nudge_x = 1, show.legend = FALSE, color=alpha("#DDDDDD", alpha = 1)) +
  guides(fill = guide_legend(title = xlab)) + ggtitle(title) +
  theme_void()
}

#Custom Bar chart using ggplot
custom_bar <- function(var, title="", xlab, ylab){
  if(title==""){
    title=paste("Bar Graph for",xlab)
  }
  df <- as.data.frame(table(var))
  ggplot(df, aes(x=var, y=Freq, fill=var)) + 
    geom_bar(width = 1, stat = "identity", color=alpha("white",alpha = 0.3))  + 
    guides(fill=guide_legend(title=xlab)) + 
    ggtitle(title) + xlab(xlab) + ylab(ylab)  + 
    theme_minimal() + 
    geom_text(aes(x = var, y = Freq + max(Freq)/50, label = Freq), size=3)
}

#Custom Box Plot using ggplot
custom_box <- function(var, title="", fill="#bfe9ff", xlab, ylab){
  if(title==""){
    title=paste("Box Plot for",xlab)
  }
  ggplot(data = housedf, aes(x="",y=var)) + 
    geom_boxplot(fill=fill) + 
    ggtitle(title) + xlab(xlab) + ylab(ylab)  + 
    theme_minimal()  
}

#Custom Histogram using ggplot
custom_hist <- function(data, var, title="", fill="#ff6e7f", xlab, ylab, binwidth=1){
  if(title=="") title=paste("Histogram for",xlab,"vs",ylab)
  ggplot(data = data, aes(x=var)) + 
    geom_histogram(binwidth=binwidth, fill=fill,color=alpha("white",alpha = 0.3)) + 
    ggtitle(title) + xlab(xlab) + ylab(ylab)  + 
    theme_minimal() + 
    stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5, binwidth = binwidth, size= 3)
}


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Checking dimensions of the data
dim(housedf)

# Getting the variable names
names(housedf)

# Getting the structure and checking the data type
str(housedf)

# Displaying the numerical summary of the dataframe
summary(housedf)

# Eyeballing the data
#View(housedf)
head(housedf)

# Get no of Unique values in every column
# Loop through all columns in the dataset
for(col in names(housedf)) {
  # Get the unique values in the column
  unique_values <- unique(housedf[[col]])
  # Get no of unique values
  length_unique <- length(unique_values)
  
  # If no of unique values is less, e.g. 20 supposedly
  if(length_unique < 20) {
    # Print the length and value of unique values if unique values are limited
    print(paste(col, "has", length_unique,"unique values; Repeated values are ", toString(unique_values)))
    }
  
  else{
    # Print the length of unique values in the column if unique values are not limited
    print(paste(col, "has", length_unique,"unique values;"))
  }
}


# From metadata, uniqueness check above, and data summary, 
# we can deduce the following:
# id: unique, numeric and non-negative
# price: numeric and non-negative
# mq: numeric and non-negative
# floor: categorical, 1 to 7
# n_rooms: categorical, -1, 2, 3, 4, 5
# n_bathrooms: categorical, 1, 2, 3
# has_terrace: binary, either 0 or 1
# has_alarm: binary, either 0 or 1
# heating: categorical, (either "autonomous" or "other" from the metadata)
# has_air_conditioning: binary, either 0 or 1
# has_parking: binary, either 0 or 1
# is_furnished: binary, either 0 or 1 

# Data Quality Check using Validator 
# the rules are built based on outputs from summary() and View()
house.rules <- validator(uniqId = is_unique(id),
                         posPrice = price > 0,
                         posMq = mq > 0,
                         posFloor = floor > 0,
                         posRooms = n_rooms > 0,
                         posBath = n_bathrooms > 0,
                         okTerc = is.element(has_terrace, c(0,1)),
                         okAlrm = is.element(has_alarm, c(0,1)),
                         okHeat = is.element(heating, c("autonomous","other")),
                         okAC = is.element(has_air_conditioning, c(0,1)),
                         okPrk = is.element(has_parking, c(0,1)),
                         okFur = is.element(is_furnished, c(0,1))
                         )
housedf.qual.chk <- confront(housedf, house.rules)
summary(housedf.qual.chk)

# Counting NA values
colSums(is.na(housedf))

# Analysing id variable via numerical summary and checking if id is unique
summary(housedf$id)
nrow(housedf)==sum(housedf$id==unique(housedf$id))

# Numerical Summaries, Boxplot and Histogram for numerical variables
# Analysing price
summary(housedf$price)
custom_box(housedf$price/100000, xlab="price", ylab="in 100k") 
custom_hist(data=housedf, var=housedf$price/100000, xlab="price (in 100k)", ylab="Frequency")

# Analysing mq
summary(housedf$mq)
custom_box(housedf$mq, xlab="mq", ylab="") 
custom_hist(data = housedf, var = housedf$mq, xlab = "mq", ylab = "sq. meters", binwidth = 100)

# Frequency Distribution Table, Piechart and Barplot for Categorical Variables
# Analysing floor
table(housedf$floor)
custom_pie(var=housedf$floor, title = "Pie Chart for floor distribution", xlab= "floor", ylab ="count")
custom_bar(var=housedf$floor, xlab= "floor", ylab ="count")

# Analysing n_rooms
table(housedf$n_rooms)
custom_pie(var=housedf$n_rooms, title = "Pie Chart for n_rooms distribution", xlab= "n_rooms", ylab ="count")
custom_bar(var=housedf$n_rooms, xlab= "n_rooms", ylab ="count")

# Analysing n_bathrooms
table(housedf$n_bathrooms)
custom_pie(var=housedf$n_bathrooms, title = "Pie Chart for n_bathrooms distribution", xlab= "n_bathrooms", ylab ="count")
custom_bar(var=housedf$n_bathrooms, xlab= "n_bathrooms", ylab ="count")

# Analysing has_terrace
table(housedf$has_terrace)
custom_pie(var=housedf$has_terrace, title = "Pie Chart for has_terrace distribution", xlab= "has_terrace", ylab ="count")
custom_bar(var=housedf$has_terrace, xlab= "has_terrace", ylab ="count")

# Analysing has_alarm
table(housedf$has_alarm)
custom_pie(var=housedf$has_alarm, title = "Pie Chart for has_alarm distribution", xlab= "has_alarm", ylab ="count")
custom_bar(var=housedf$has_alarm, xlab= "has_alarm", ylab ="count")

# Analysing heating
table(housedf$heating)
custom_pie(var=housedf$heating, title = "Pie Chart for heating distribution", xlab= "heating", ylab ="count")
custom_bar(var=housedf$heating, xlab= "heating", ylab ="count")

# Analysing has_air_conditioning
table(housedf$has_air_conditioning)
custom_pie(var=housedf$has_air_conditioning, title = "Pie Chart for has_air_conditioning distribution", xlab= "has_air_conditioning", ylab ="count")
custom_bar(var=housedf$has_air_conditioning, xlab= "has_air_conditioning", ylab ="count")

# Analysing has_parking
table(housedf$has_parking)
custom_pie(var=housedf$has_parking, title = "Pie Chart for has_parking distribution", xlab= "has_parking", ylab ="count")
custom_bar(var=housedf$has_parking, xlab= "has_parking", ylab ="count")

# Analysing is_furnished
table(housedf$is_furnished)
custom_pie(var=housedf$is_furnished, title = "Pie Chart for is_furnished distribution", xlab= "is_furnished", ylab ="count")
custom_bar(var=housedf$is_furnished, xlab= "is_furnished", ylab ="count")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Data Cleaning

# Create new dataframe
housedf_clean <- housedf

# Viewing / correcting mq
housedf_clean[housedf_clean$mq==0,]
similar_set <- housedf[housedf$floor==1 & housedf$n_rooms==3 & housedf$n_bathrooms==2 & housedf$has_terrace==0 &
                         housedf$has_alarm==0 & housedf$heating=="autonomous" & housedf$has_air_conditioning==0 &
                         housedf$has_parking==0 & housedf$is_furnished==0,]
cor.test(similar_set$price, similar_set$mq)
# In case of high correlation, we impute by the mean of mq from similar_set
#housedf_clean$mq[housedf_clean$mq==0] <- round(mean(similar_set$mq))
# In case of low / no correlation, we remove the observation having mq as 0
housedf_clean <- housedf_clean[which(housedf_clean$mq != 0),]

# Correcting n_rooms
# replace n_rooms == -1 with mode of n_rooms
housedf_clean$n_rooms[which(housedf$n_rooms == -1)] <- mlv(housedf_clean$n_rooms, method = "mfv") 

# Correcting heating
housedf_clean$heating[housedf_clean$heating=='autonamous'] <- 'autonomous'

# Alternate corrections
# replace mq < 5, with mean of mq
#housedf_clean$mq[which(housedf_clean$mq <5)] <- mean(housedf_clean$mq)
# Replace n_rooms = -1 with positive value
#housedf_clean$n_rooms <- abs(housedf_clean$n_rooms)


# Converting the categorical columns into factors
categorical_vars <- c("floor", "n_rooms", "n_bathrooms", "has_terrace","has_alarm","heating","has_air_conditioning", "has_parking", "is_furnished")
for(var in categorical_vars){
  housedf_clean[,var] <- as.factor(housedf_clean[,var])
}

# Checking data types and summary for housedf_clean
summary(housedf_clean)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Defining a custom function mysummary() to show variable summary 
# and visualise the data in the form of histogram and bar plot
mysummary <- function(var, var_name){
  print(summary(var))
  # For numerical variables
  if(class(var) == "numeric" || class(var) == "integer"){
    # Histogram with density line
    print(ggplot(housedf_clean, aes(x = var)) + 
            geom_histogram(aes(y=..density..), color="#111111", fill="#555555") +
            geom_density(alpha=.2, fill="#00C0F0") +
            labs(title = paste("Distribution for", var_name), x = var_name, y = "count"))
    # Skewness
    print(skewness(var))
    # QQplot 
    qqPlot(var, distribution="norm", ylab = var_name, main = paste("Quantile Quantile Plot for",var_name), col.lines = "#006080")
  }  
  # For Categorical Variables
  else{
    freq.df <- as.data.frame(table(var))
    # Bar Plot
    print(ggplot(data = housedf_clean) + aes(x=var) + geom_bar(color="#111111", fill="#555555") + 
            geom_text(data = freq.df, aes(x = var, y = Freq + max(Freq)/50, label = Freq), size=3) +
            xlab(var_name) + ylab("Frequency") + ggtitle(paste("Distribution for", var_name)))
  }
}

# Analysing Individual Variable

# Analysing price
mysummary(housedf_clean$price, var_name = "price")

# Analysing mq
mysummary(housedf_clean$mq, var_name = "mq")

# Analysing floor
mysummary(housedf_clean$floor, var_name = "floor")

# Analysing n_rooms
mysummary(housedf_clean$n_rooms, var_name = "n_rooms")

# Analysing n_bathrooms
mysummary(housedf_clean$n_bathrooms, var_name = "n_bathrooms")

# Analysing has_terrace
mysummary(housedf_clean$has_terrace, var_name = "has_terrace")

# Analysing has_alarm
mysummary(housedf_clean$has_alarm, var_name = "has_alarm")

# Analysing heating
mysummary(housedf_clean$heating, var_name = "heating")

# Analysing has_air_conditioning
mysummary(housedf_clean$has_air_conditioning, var_name = "has_air_conditioning")

# Analysing has_parking
mysummary(housedf_clean$has_parking, var_name = "has_parking")

# Analysing is_furnished
mysummary(housedf_clean$is_furnished, var_name = "is_furnished")


# Checking correlation between numerical columns

# Checking correlation between the price and mq
cor.test(housedf_clean$price, housedf_clean$mq)

# Scatter plots for dependency between price and mq
ggplot(data = housedf_clean) + aes(x=price/100000, y=mq) + geom_point() + 
  labs(title = "Distribution of mq vs price", x="Price in 100k", y="Area in Meter Square") + 
  geom_smooth(method = "lm", color = "#006080")

# Separate floor from rest of categorical variables for using fisher.test 
# as the frequency of floor6, floor7 is 3 and 5
remaining.categorical <- categorical_vars[!categorical_vars == "floor"]

# Checking dependence among floor and remaining categorical variables using fisher.exact 
# Perform the fisher.exact test of independence for floor and other categorical columns
for (j in 1:length(remaining.categorical)) {
    fisher.result <- fisher.test(table(housedf_clean$floor, housedf_clean[,remaining.categorical[j]]), simulate.p.value = T)
    if(fisher.result$p.value<0.05){
      cat("\n")
      print(paste("Dependance between floor and", remaining.categorical[j], "is significant with a p-value of", round(fisher.result$p.value,4)))
    }
}


# Checking dependence among remaining categorical variables using chi-sq test
# Perform the chi-squared test of independence for each pair of columns
for (i in 1:(length(remaining.categorical)-1)) {
  for (j in (i+1):length(remaining.categorical)) {
    chisq.result <- chisq.test(housedf_clean[,remaining.categorical[i]], housedf_clean[,remaining.categorical[j]], simulate.p.value = T)
    #print(paste("Chi-Square Test between",columns[i], "and", columns[j], "gives p-value of", round(chisq.result$p.value,3)))
    if(chisq.result$p.value<0.05){
      cat("\n")
      print(paste("Dependance between",remaining.categorical[i], "and", remaining.categorical[j], "is significant with a p-value of", round(chisq.result$p.value,4)))
      }
  }
}

# Defining a custom function compare.category() to check the following 
# for all (numerical variable, categorical variables) pairs in the data:
# 1) Aggregate mean for numerical variable across different categories of the categorical variable
# 2) ANOVA testing for difference in average value of numerical variable across different categories
# 3) Box Plot indicating the spread of numerical variable across different categories

compare.category <- function(num_var, cat_var){

  # Creating custom formula for categorical variable ~ numerical variable 
  compare.formula <- as.formula(paste0(num_var," ~ ",cat_var))
  
  # Aggregate mean for numerical variable vs categorical variable
  print(paste("Aggregate Mean of",num_var,"between categories of",cat_var))
  print(aggregate(compare.formula,  data = housedf_clean, FUN="mean"))
  cat("\n\n")
  
  # ANOVA test to check if there's significant difference in average of the numerical variable of different categories
  print(paste("ANOVA Test for",num_var,"~",cat_var))
  print(summary.lm(aov(compare.formula, data=housedf_clean)))
  
  # Auxiliary code for aesthetics and scaling
  x_var <- housedf_clean[,cat_var]
  y_var <- housedf_clean[,num_var]
  box_col <- "#FF6080"
  y_label <- num_var
  if(num_var == "price"){
    y_var <- y_var / 100000
    y_label <- paste(num_var,"(in 100k)")
    box_col <- "#006080"
  }
  
  print(paste("Box Plot for",num_var,"vs",cat_var))
  # Box plot for price vs categorical 
  print(ggplot(data = housedf_clean) + aes(x = x_var, y = y_var) +
    geom_boxplot(notch = F, color = box_col) +
    labs(title = paste("Box Plot for",num_var,"distribution across",cat_var,"categories"), x = cat_var, y = y_label))
}

# Aggregate Mean, ANOVA Test, and Boxplot for price vs floor
compare.category(num_var = "price", cat_var = "floor")

# Aggregate Mean, ANOVA Test, and Boxplot for mq vs floor
compare.category(num_var = "mq", cat_var = "floor")

# Aggregate Mean, ANOVA Test, and Boxplot for price vs n_rooms
compare.category(num_var = "price", cat_var = "n_rooms")

# Aggregate Mean, ANOVA Test, and Boxplot for mq vs n_rooms
compare.category(num_var = "mq", cat_var = "n_rooms")

# Aggregate Mean, ANOVA Test, and Boxplot for price vs n_bathrooms
compare.category(num_var = "price", cat_var = "n_bathrooms")

# Aggregate Mean, ANOVA Test, and Boxplot for mq vs n_bathrooms
compare.category(num_var = "mq", cat_var = "n_bathrooms")

# Aggregate Mean, ANOVA Test, and Boxplot for price vs has_terrace
compare.category(num_var = "price", cat_var = "has_terrace")

# Aggregate Mean, ANOVA Test, and Boxplot for mq vs n_rooms
compare.category(num_var = "mq", cat_var = "has_terrace")

# Aggregate Mean, ANOVA Test, and Boxplot for price vs has_alarm
compare.category(num_var = "price", cat_var = "has_alarm")

# Aggregate Mean, ANOVA Test, and Boxplot for mq vs has_alarm
compare.category(num_var = "mq", cat_var = "has_alarm")

# Aggregate Mean, ANOVA Test, and Boxplot for price vs heating
compare.category(num_var = "price", cat_var = "heating")

# Aggregate Mean, ANOVA Test, and Boxplot for mq vs heating
compare.category(num_var = "mq", cat_var = "heating")

# Aggregate Mean, ANOVA Test, and Boxplot for price vs has_air_conditioning
compare.category(num_var = "price", cat_var = "has_air_conditioning")

# Aggregate Mean, ANOVA Test, and Boxplot for mq vs has_air_conditioning
compare.category(num_var = "mq", cat_var = "has_air_conditioning")

# Aggregate Mean, ANOVA Test, and Boxplot for price vs has_parking
compare.category(num_var = "price", cat_var = "has_parking")

# Aggregate Mean, ANOVA Test, and Boxplot for mq vs has_parking
compare.category(num_var = "mq", cat_var = "has_parking")

# Aggregate Mean, ANOVA Test, and Boxplot for price vs is_furnished
compare.category(num_var = "price", cat_var = "is_furnished")

# Aggregate Mean, ANOVA Test, and Boxplot for mq vs is_furnished
compare.category(num_var = "mq", cat_var = "is_furnished")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Building Maximal Model
price.max.model <- lm(formula = price ~ ., data = housedf_clean)

# Summary of the model
summary(price.max.model)

# Updating the model using transformation of mq variable
price.max.model <- update(price.max.model, . ~ . + log(mq))

# Summary of the updated model
summary(price.max.model)

# Updating the model using step() method
price.model <- step(price.max.model)

# Summary of the model
summary(price.model)

# Diagnostic plots for the final model
plot(price.model)

# Checking multi-collinearity in the model parameters
# No multi-collinearity as the value of VIF in the output is almost equal to 1
vif(price.model)



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Model building
furnished.max.model <- glm(formula = is_furnished ~ ., data = housedf_clean, family = "binomial")

# Summary of the model
summary(furnished.max.model)

# Updating model using step() to remove non-significant terms
furnished.model <- step(furnished.max.model)

# Summary of the updated model
summary(furnished.model)
#plot(furnished.model)

# Predict the probability of property being furnished
housedf_clean$pfurnished <- predict(furnished.model, type = "response")

# Analysing pfurnished values
summary(housedf_clean$pfurnished)

# Calculating Chi-Square statistic for furnished model
chisq.furnished <- furnished.model$null.deviance - furnished.model$deviance 
chisq.furnished

# Calculating predictor values degree of freedom
df.furnished <- furnished.model$df.null - furnished.model$df.residual
df.furnished

# Calculating p-value from chi-square statistics
pchisq(chisq.furnished, df = df.furnished)

# Transforming pfurnished to binary factors (1 if pfurnished > 0.5, 0 otherwise)
predictions_factor <- as.factor(ifelse(housedf_clean$pfurnished > 0.5, 1, 0))

# Call the confusionMatrix() function to generate the confusion matrix
confusion_matrix <- confusionMatrix(data = predictions_factor, reference = housedf_clean$is_furnished, positive = "1")

# Display the confusion matrix
confusion_matrix

# Odd ratios
exp(coef(furnished.model))


