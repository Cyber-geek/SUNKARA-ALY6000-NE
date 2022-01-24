# Printing the top of the document
print("Plotting Basics: SUNKARA")

install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("moments")

# Loading libraries
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(moments)

# Load the dataset from FSAData library
df <- FSAdata::BullTroutRML2

# See the first and last 3 rows of the initial import
headtail(df, 3)

# Filtering the dataset, but keeping the same variable name
df <- df[df$lake == "Harrison",]

# First and last 5 rows, along with a basic structure and summary overview
headtail(df, 5)
str(df)
summary(df)

# Defining objects for xaxis and yaxis :
xaxis <- c(0,500)
yaxis <- c(0,15)

# Attaching the data frame will make the following plots much easier. Below, Plot 1.
attach(df)
plot(age ~ fl, main="Plot 1: Harrison Lake Trout", xlab="Fork Length (mm)", ylab="Age (yrs)",
     xlim=xaxis, ylim=yaxis, pch=20)

# Plot 2: Histogram. Note we had to use the ylim vector for xlim due to this being a histogram (examination of one variable, the “X” variable in this case) vs. a scatter plot (X and Y).

hist(df$age, main="Plot 2: Harrison Fish Age Distribution", xlab="Age (yrs)", ylab="Frequency",
     xlim=yaxis, col="cadetblue", col.main="cadetblue")

# Using R’s colorRampPalette to map a color to each era
cpf <- colorRampPalette(colors=c("lightgreen", "darkgreen"), space="Lab")
num_levels <- nlevels(df$era)
df_era_colors <- cpf(num_levels)

# Plot 3: Overdense Plot which uses aforementioned shades of green, legend added for clarity

plot(age ~ fl, main="Plot 3: Harrison Density Shaded by Era", xlab="Fork Length (mm)",
     ylab="Age (yrs)", xlim=xaxis, ylim=yaxis, pch=20, col=df_era_colors)
legend(x = "topleft", legend = paste(levels(df$era)), col = df_era_colors, pch = 20)

# Create “tmp” with the first and last 3 records of (filtered) dataframe.
tmp <- headtail(df,3)

# Display “era” values in “tmp”
print(tmp$era)

# Vectors for “pch” and “col”
pchs <- c("+","x")
cols <- c("red", "gray60")

# Ensure that tmp$era is defined as numeric
tmp$era <- as.numeric(tmp$era)

# Initialize colors with tmp$era
initialize(col, tmp$era)

# Plot 4
plot(age ~ fl, xlab="Fork Length (mm)", ylab="Age (yrs)", pch=pchs, col=cols, xlim=xaxis,
     ylim=yaxis, main="Plot 4: Symbol & Color by Era")

# Plot 5: Regression Overlay
plot(age ~ fl, xlab="Fork Length (mm)", ylab="Age (yrs)", pch=pchs, col=cols, xlim=xaxis,
     ylim=yaxis, main="Plot 5: Regression Overlay")
abline(lm(age ~ fl))

# Plot 6: Legend Overlay
plot(age ~ fl, xlab="Fork Length (mm)", ylab="Age (yrs)", pch=pchs, col=cols, xlim=xaxis,
     ylim=yaxis, main="Plot 6: Legend Overlay")
legend(x = "topleft", legend = paste(levels(df$era)), pch = pchs, col = cols)
abline(lm(age ~ fl))

# Summary Statistics for "Old" and "New" eras to provide some color
oldera <- df[df$era=='1977-80',]
newera <- df[df$era=='1997-01',]
summary(oldera)
summary(newera)

# Just for good measure, detaching the data frame
detach(df)