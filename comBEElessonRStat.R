# author: Kadee Lawrence
# email: knlawrence@wisc.edu
# date: 2023_08_23
# comBEE: Intro to Rstats

#Install basic Rstats package and datasets to work with
#These are typically automatically installed, to check click 'packages' 
#If listed with a check in the box it is installed and in your library
install.packages("stats")
install.packages("datasets")
install.packages("graphics")
library(stats)
library(datasets)
library(graphics)

# other packages available online
# https://cran.r-project.org/web/packages/available_packages_by_name.html

#list data sets available to work with from 'datasets'
data()

#look at specific dataset (just type name as seen in list)
trees
#Can look at more details by opening 'packages' tab and clicking 'dataset' and 'trees'
#Will tell you whats in the data set along with run examples

#write trees into our environment
trees_data <- trees

# https://www.dataquest.io/blog/write-functions-in-r/
#Find basic stats of tree height in data set 'trees_data'
mean(trees_data$Height)
median(trees_data$Height)
range(trees_data$Height)
quantile(trees_data$Height)

#What if you want to look at specific quantile?
quantile(trees_data$Height, 0.25)

#Count the number of occurrences of each value in 'Height'
table(trees_data$Height, useNA = "always")

# https://www.educative.io/answers/what-are-comparison-operators-in-r
#Count how many trees are 74ft and 80ft tall
length(which(trees_data$Height == 74))
length(which(trees_data$Height == 80))
#Count how many trees are 74ft or 80ft tall in one script
length(which(trees_data$Height == 74 | trees_data$Height == 80))
#Count how many trees are 74ft or taller
length(which(trees_data$Height >= 74))
#Count of trees that are NOT 74ft
length(which(trees_data$Height != 74))

#think as working from in-out
#we are looking for tree height from 'trees_data'
#and we want to know 'which' are the asked 'length'

#Say we want this as frequency
#Find the amount of trees sampled
nrow(trees_data)

#Make function to find frequency of occurrence of height
Freq_Tree_Height <- function(x) {
  z <- length(which(trees_data$Height == (x)))
  y <- nrow(trees_data)
  
  output <- ((z) / (y)) 
  
  return(output)
}

#Use Function to find frequency of trees 74ft 
Freq_Tree_Height(74)

#double check the function by calculating it out
((length(which(trees_data$Height == 74)))/(nrow(trees_data)))
#know 31 observations and 74ft occurs 2 times
(2/31)

#make histogram of tree height
hist(trees_data$Height, col = "wheat", main = "Tree Height", xlab = "Height", ylab = "Occurrence", breaks = 15, xlim = c(60, 90))
#add abline (quantile 25%)
abline(v = quantile(trees_data$Height, 0.25), col = "red")

#Ways to check for normal distribution in trees_data$Height
ks.test(trees_data$Height, "pnorm")
# https://www.r-bloggers.com/2023/05/checking-normality-in-r/#:~:text=The%20Shapiro%2DWilk%20test%20is,that%20the%20distribution%20is%20normal.
shapiro.test(trees_data$Height)
