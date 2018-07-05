library(shiny)
library(ggplot2)
library(dplyr)

shows <- read.csv("ready.csv", header=TRUE)

shows$TV.Rating <- gsub('Â', '', shows$TV.Rating)
shows$Series <- gsub(' $', '', shows$Series, perl=T)
shows$TV.Rating <- gsub("(^[[:space:]]*)|([[:space:]]*$)", "", shows$TV.Rating)




shows
write.csv(head(shows), file = "finalR1.csv")



shows[c("Genre.1", "Genre2","Genre3")]
unique(unlist(shows[c("Genre.1", "Genre2","Genre3")]))
print(max(shows$Votes))

c("All", sort(unique(shows$TV.Rating)))

paste(shows$Ep.name)

ggplot(shows %>% group_by(Series) %>% summarise(n=n()), aes(Series,n)) +
         geom_bar(stat="identity")

shows %>% group_by(Series) %>% summarise(n=n())