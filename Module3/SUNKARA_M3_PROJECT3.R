#Printing Name on Top
print("Jeevan Rishi Kumar Sunkara")

#Installation of Packages
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("plyr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("data.table")
install.packages("ggplot2")

#Loading required Packages
library("FSA")
library("FSAdata")
library("magrittr")
library("plyr")
library("dplyr")
library("tidyr")
library("tidyverse")
library("data.table")
library("ggplot2")

#Adding inchBio to bio Data Frame
bio = fread("inchBio.csv")

#3. Display the head, tail and structure of <bio>
headtail(bio)
str(bio)

#4. Create an object, <counts>, that counts and lists all the species records
counts <- plyr::count(bio$species)
counts

#5. Display just the 8 levels (names) of the species
levels(bio$species)

#6. Create a <tmp> object that displays the different species and the number of record of each species in the dataset.
tmp <-table(bio$species)
tmp

#7. Create a subset, <tmp2>, of just the species variable and display the first five records
tmp2 <- subset(bio, select = species)
head(tmp2, 5)

#8. Create a table, <w>, of the species variable. Display the class of w
w <-table(bio$species)
class(w)

#9. Convert <w> to a data frame named <t> and display the results
t <-as.data.frame(w)
t
class(t)

#10. Extract and display the frequency values from the <t> data frame
t$Freq

#11. Create a table named <cSpec> from the bio species attribute (variable) and confirm that you created a table which displays the number of species in the dataset <bio>
cSpec <-table(bio$species)
class(cSpec)
cSpec

#12. Create a table named <cSpecPct> that displays the species and percentage of records for each species. Confirm you created a table class.
cSpecPct <- prop.table(table(bio$species))*100
cSpecPct
class(cSpecPct)

#13. Convert the table, <cSpecPct>, to a data frame named <u> and confirm that <u> is a data frame
u <-as.data.frame(cSpecPct)
u
class(u)

#14 Plot 1 : Fish Count
par(mar = c(5, 5, 5, 5))
plot1 = barplot(cSpec,main ="Fish Count",horiz = "FALSE", las =1, ylab = "COUNTS",col = "lightgreen",las=2,cex.names = 0.5, cex.axis = 0.6, ylim = c(0,250))
#Adding Frequency to get accurate Fish Count
text(y=signif(t$Freq), plot1, signif(t$Freq),pos=3, cex=0.8)

#14.1 Plot 1.1 : Fish Count Horizontally
par(mar = c(5, 5, 5, 5))
plot1.1 = barplot(cSpec,main ="Fish Count",horiz = "TRUE", las =1, xlab = "COUNTS",col = "lightgreen",las=2,cex.names = 0.5, cex.axis = 0.6, xlim = c(0,250))
#Adding Frequency to get accurate Fish Count
text(x=signif(t$Freq), plot1.1, signif(t$Freq),pos=4, cex=0.5)

#15 Plot 2 : Fish Relative Frequency
par(mar = c(8, 5, 5, 5))
plot2 = barplot((cSpecPct/10),col="lightblue",main="Fish Relative Frequency",ylim = c(0,4),ylab = "Percentage %",las =2)

#16 Rearrange the <u> cSpec Pct data frame in descending order of relative frequency. Save the rearranged data frame as the object <d> 
d <- dplyr::arrange(u, desc(Freq))

#17 Rename the <d> columns Var 1 to Species, and Freq to RelFreq
names(d) <- c("Species", "RelFreq")
d

#18 Add new variables to <d> and call them cumfreq, counts, and cumcounts
d$cumfreq <- cumsum(d$RelFreq)
d$counts <- (d$RelFreq* nrow(bio))/100
d$cumcounts <- cumsum(d$counts)

#19 Create a parameter variable <def_par> to store parameter variables 
def_par <- par(mar = c(10, 6, 6, 6), cex=0.7, fg="grey62")

#20 Plot 3 : Species Pareto
pc <- barplot(d$counts,
              space = 0.15,
              width = 1,
              border = NA,
              axes = F,
              main="Species Pareto",
              ylab= "Cummulative Counts",
              names.arg = d$Species,
              ylim = c(0,3.05*max(d$counts, na.rm = TRUE)),
              cex.axis = 0.70,las=2,col=c("lightblue", "yellow", "lightgreen", "gold",  "orange", "red","aquamarine3", "grey"))

#21 Add a cumulative counts line to the <pc> plot with the following:
lines(d$cumcounts,pch=19,col="cyan4",type = 'b',cex= 0.70)

#22 Place a grey box around the pareto plot
box()

#23 Add a left side axis with the following specifications
axis(side=2,at = d$cumcounts,tick= TRUE,line= NA,col.ticks = "grey62",col.axis = "grey62",cex.axis = 0.8,las=2)

#24 Add axis details on right side of box with the specifications
axis(side=4,at= c(0, d$cumcounts),tick= TRUE,col= "cyan4",col.axis="cyan3",cex.axis= 0.80,labels=paste(round(c(0,d$cumfreq)),'%'),las=2)

#25 Display the finished Species Pareto Plot (without the star watermarks). Have your last name on the plot
text(1,650,"SUNKARA", cex=.75, col = "cyan4")

