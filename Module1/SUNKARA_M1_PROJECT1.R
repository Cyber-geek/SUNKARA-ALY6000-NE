#1.Print your name at the top of the script
print("Jeevan Rishi Kumar Sunkara")

#2.Install the vcd package
install.packages("vcd")

#3.Import the vcd library
library(vcd)

#4.Plot a sales ~ temp scatter plot using the data below:
#Sales data: (7,11,15,20,19,11,18,10,6,22) 
#Temperature data: (69,81,77,84,80,97,87,70,65,90) 
Sales = c(7,11,15,20,19,11,18,10,6,22)
Temperature = c(69,81,77,84,80,97,87,70,65,90)
plot(Sales,Temperature)

#5.Find the mean temperature
mean(Temperature)

#6.Delete the 3rd element from the sales vector
Sales = Sales[-3]
Sales

#7.Insert 16 as the 3rd element into the sales vector
Sales[3]=16
Sales

#8.Create a vector <names> with elements Tom, Dick, Harry
Names=c("Tom","Dick","Harry")
Names

#9.Create a 5 row and 2 column matrix of 10 integers
Matrix1= matrix(1:10, nrow=5, ncol=2)
Matrix1

#10.Create a data frame <icSales> with sales and temp attributes
icSales = data.frame(Sales,Temperature)

#11.Display the data frame structure of icScales
str(icSales)

#12.Display a summary of the icScales data frame
summary(icSales)

#13.Import the dataset Student.csv
install.packages("data.table")
library(data.table)
students = fread("Student.csv")
students

#14.Display only the variable names of the Student.csv dataset 
variable.names(students)