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
Sales[-3]
Sales

#7.Insert 16 as the 3rd element into the sales vector
Sales[3]=16
Sales

#8.Create a vector <names> with elements Tom, Dick, Harry (pg 22)
Names=c("Tom","Dick","Harry")

#9.Create a 5 row and 2 column matrix of 10 integers (pg 23)