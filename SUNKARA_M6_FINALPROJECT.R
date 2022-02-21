#Printing Name on Top
print("Jeevan Rishi Kumar Sunkara")

#Installing Packages
install.packages("readr")
install.packages("data.table")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggwordcloud")
install.packages("tidytext")
install.packages("ggmap")
install.packages("plotly")
install.packages("viridis")
install.packages("grid")
install.packages("gridExtra")
install.packages("wordcloud2")
install.packages("tm")
install.packages("shiny")


#Importing Packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggwordcloud)
library(stringr)
library(tidytext)
library(readr)
library(data.table)
library(tidyverse)
library(ggmap)
library(plotly)
library(viridis)
library(grid)
library(gridExtra)
library(wordcloud2)
library(tm)
library(shiny)


shows = fread("Netflix_Data.csv")
Netflix = shows
head(shows)


#Data Cleaning
# 1. Split columns listed_in, country, director, and cast into multiple columns by delimiter ",", 
# and unpivot to long table.

shows <- shows %>% 
  
# Split column "listed_in" by delimiter
mutate(listed_in = str_split(listed_in, ",")) %>%  
  
# Split column "country" by delimiter
mutate(country = str_split(country, ",")) %>% 
  
# Split column "director" by delimiter
mutate(director = strsplit(director, ",")) %>%
  
#Split column "cast" by delimiter
mutate(cast = strsplit(cast, ",")) %>%
  
# Trim all characters in the data frame to remove extra white spaces
mutate(across(where(is.character), str_trim))

# 2. Change data type of column "dated_added" to "Date".
shows$date_added <- as.Date(shows$date_added, "%B %d, %Y")

# 3. Remove duplicates.
shows = unique(shows)
shows

# 4. Revise rating mismatch
# Rating has 3 strange values, which look more like duration instead of rating.
unique(shows$rating)

# Filter down to these records and find that duration column is blank,
# which means those values should be duration instead of rating.
filter(shows, rating %in% c("74 min", "84 min", "66 min"))

# Revise the mismatch
shows["duration"][shows["rating"] == "74 min"] <- "74 min"
shows["duration"][shows["rating"] == "84 min"] <- "84 min"
shows["duration"][shows["rating"] == "66 min"] <- "66 min"
shows["rating"][shows["rating"] == "74 min"] <- ""
shows["rating"][shows["rating"] == "84 min"] <- ""
shows["rating"][shows["rating"] == "66 min"] <- ""

#5. Replacing NA with "UNKNOWN"
Netflix[is.na(Netflix)] <- "UNKNOWN"

#Descriptive Analysis
str(Netflix)
describe(Netflix)
dim(Netflix)
head(Netflix)

#Function to call Parameters for a Graph
fig <- function(width, height){
  options(repr.plot.width = width, repr.plot.height = height)
}

# Group by type
shows_type <- shows %>% 
  group_by(type) %>% 
  summarise(
    number_of_shows = length(unique(show_id)),
    .groups = "drop"
  ) 


#Doughnut chart calculations
shows_type <- mutate(
  shows_type,
  fraction = number_of_shows / sum(number_of_shows),
  ymax = cumsum(fraction),
  ymin = c(0, head(ymax, n=-1)),
  labelposition = (ymax + ymin)/2
)

# Plot 1: Doughnut Plot
doughnut <- 
  ggplot(shows_type,
         aes(
           ymax = ymax,
           ymin = ymin,
           xmax = 4, 
           xmin = 3,
           fill = type
         )) +
  geom_rect() +
  geom_label(x = 3.5, 
             aes(
               y = labelposition, 
               label = paste0(type,"\n",
                              round(fraction*100,1),"%")
             ),
             size = 4 ) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  coord_polar(theta = "y") +
  xlim(c(2,4)) +
  theme_void() +
  theme(legend.position = "none")

fig(5,5)
doughnut

# Group by type and year
type_year <- shows %>% 
  mutate(year_added = as.numeric(format(date_added, "%Y"))) %>% 
  group_by(type, year_added) %>% 
  summarise(
    number_of_shows = length(unique(show_id)),
    .groups = "drop"
  )

# Set fig size
fig(15,6)

#Plot 2: Bar chart for amount of content by Rating
ggplot(type_year,
       aes(
         x = YearAdded,
         y = Number of Shows,
         fill = type
       )) +
  geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) +
  geom_text(
    aes(label = number_of_shows), 
    position = position_dodge(width = 0.9), 
    vjust = -0.2, 
    size = 5,
    na.rm = TRUE) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))


#Plot 3: Pareto Chart
# Calculate number of shows for each country
by_country <- shows %>%
  group_by(country) %>%
  filter(!(is.na(country) | country == "")) %>%
  summarise(
    number_of_shows = length(unique(show_id)),
    .groups = "drop"
  )

# Recode country names
by_country$country <- recode(by_country$country, "United States" = "USA", "United Kingdom" = "UK")

# Performing the descending order
by_country <- by_country[order(-by_country$number_of_shows),] 

# Converting country variable to a factor
by_country$country <- factor(by_country$country, levels = by_country$country)

# Adding a cumulative sum of number_of_shows, and relative cumulative percentage
by_country$cumulative <- cumsum(by_country$number_of_shows)
by_country$cum_percent <- cumsum(by_country$number_of_shows)/sum(by_country$number_of_shows)

# Adding a label for a single point
by_country$cum_percent_label <- ifelse(by_country$country == "Egypt", paste(round(by_country$cum_percent*100,2), "%"), NA)

# Set fig size
fig(12,8)

# Plot Perato
ggplot(
  data = filter(by_country, cum_percent <= 0.8), 
  aes(x = country)) +
  
  geom_bar(
    aes(
      y = number_of_shows,
      fill = country
    ),
    stat = "identity"
  ) +
  scale_fill_viridis(discrete = TRUE, direction = -1) +
  
  geom_point(
    aes(y = cumulative),
    color = "red",
    pch = 16, 
    size = 3
  ) +
  
  geom_label(
    aes(
      y = cumulative, 
      label = cum_percent_label
    ), 
    vjust = 1.5, 
    hjust = 0.6, 
    na.rm = TRUE 
  ) +
  
  geom_path(
    aes(
      y = cumulative, 
      group = 1
    ),
    color = "grey50",
    size = 1
  ) +
  
  scale_y_continuous(
    sec.axis = sec_axis(~./sum(by_country$number_of_shows))
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.6),
    axis.text =element_text(size = 15), 
    axis.title = element_text(size = 20), 
    legend.position = "none")


#Plot4: List of Top 10 Content Distributed Countries
options(repr.plot.width = 14, repr.plot.height = 8)
ggplot(country_count,
       aes(reorder(country,count_country),
           count_country)) +
  geom_col(fill='brown2') +
  coord_flip() + 
  theme(plot.title = element_text(hjust=0.5,size=16),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title = element_text(size=16),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        plot.subtitle = element_text(hjust=0.5,size=14)) +
  labs(x= 'Country', 
       y = 'No of Distributions',
       title = 'List of Top 10 Content Distributed Countries') +
  geom_label(aes(label = count_country,y=count_country+150)) +
  facet_wrap(~type)


#Plot 5: Amount Of Content By Rating
netflix = Netflix
df_by_rating_only_full <- netflix %>% group_by(rating) %>% summarise(count = n())
fig5 <- plot_ly(df_by_rating_only_full, labels = ~rating, values = ~count, type = 'pie')
fig5 <- fig5 %>% layout(title = 'Amount Of Content By Rating',
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig5

#Plot 6: No of ratings given for Movies vs TV Shows
options(repr.plot.width = 14, repr.plot.height = 8)
ggplot(rating_number,
       aes(reorder_within(rating,number_of_rating,type),number_of_rating,fill=type)) + 
  scale_x_reordered() +
  geom_col() + 
  coord_flip() +
  facet_wrap(~type,scales='free_y') +
  theme(plot.title = element_text(hjust=0.5,size=16),
        axis.text.x = element_text(size=16,hjust=0.95,vjust=0.2),
        axis.text.y = element_text(size=16),
        axis.title = element_text(size=16),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16)) + 
  geom_label(aes(label = number_of_rating,
                 y=number_of_rating+100),
             show.legend = FALSE) +
  scale_fill_discrete(name='Type') +
  labs(x='Rating',
       y='Amount',
       title = 'Number of Each Rating Movies vs TV Shows')

#Plot 7: Top 5 Genres of Netflix
titles = Netflix
s3 <- strsplit(titles$listed_in, split = ", ")
titles_listed_in <- data.frame(type = rep(titles$type, sapply(s3, length)), listed_in = unlist(s3))
titles_listed_in$listed_in <- as.character(gsub(",","",titles_listed_in$listed_in))

df_by_listed_in_full <- titles_listed_in %>% group_by(listed_in) %>% summarise(count = n()) %>%
  arrange(desc(count)) %>% top_n(5)

fig7 <- plot_ly(df_by_listed_in_full, x = ~listed_in, y = ~count, type = 'bar', marker = list(color = '#EE3B3B'))
fig7 <- fig7 %>% layout(xaxis=list(categoryorder = "array", categoryarray = df_by_listed_in_full$listed_in, title="Genre"), yaxis = list(title = 'Count'), title="5 Top Genres On Netflix")

fig7
labs(x='Rating',
     y='Amount',
     title = 'No of Ratings given for Movies vs TV Shows')