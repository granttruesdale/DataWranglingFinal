rm(list=ls())

# Data for Project

library(xml2)
page <- read_html('https://www.boxofficemojo.com/chart/ww_top_lifetime_gross/?ref_=bo_lnav_hm_shrt')
imdb <- read.csv("imdb_top_1000.csv")
ImdbTitles <- imdb$Series_Title
ImdbTitles

page <- read_html('https://www.boxofficemojo.com/chart/ww_top_lifetime_gross/')
page1 <- xml_text(xml_find_all(page, '//td[@class="a-text-left mojo-field-type-title"]/a[@class="a-link-normal"]'))
Titles <- character(0)
for(i in seq(from=200, to=800, by=200)){
  url <- paste('https://www.boxofficemojo.com/chart/ww_top_lifetime_gross/?offset=',i, sep="")
  page <- read_html(url)
  Titles2 <- xml_text(xml_find_all(page, '//td[@class="a-text-left mojo-field-type-title"]/a[@class="a-link-normal"]'))
  Titles <- c(Titles, Titles2)
}
FinalTitles <- c(page1, Titles)
SharedMovies <- intersect(FinalTitles, ImdbTitles)

variable <- seq(from = 1, to=600, by=3)

page <- read_html('https://www.boxofficemojo.com/chart/ww_top_lifetime_gross/')
page1 <- xml_text(xml_find_all(page, '//td[@class="a-text-right mojo-field-type-money"]'))
page1 <- page1[variable]
Earnings <- character(0)
for(j in seq(from=200, to=800, by=200)){
  url <- paste('https://www.boxofficemojo.com/chart/ww_top_lifetime_gross/?offset=',j, sep="")
  page <- read_html(url)
  Earnings2 <- xml_text(xml_find_all(page, '//td[@class="a-text-right mojo-field-type-money"]'))
  Earnings2 <- Earnings2[variable]
  Earnings <- c(Earnings, Earnings2)
}
GrossEarnings <- c(page1, Earnings)

test <- data.frame(FinalTitles, GrossEarnings)
test2 <- data.frame(SharedMovies)
test3 <- merge(test,test2,by.x = 'FinalTitles', by.y = 'SharedMovies', all.y = T)

finaldata <- merge(imdb, test3, by.x = 'Series_Title', by.y = 'FinalTitles', all.y = T)
finaldata <- distinct(finaldata, Series_Title, .keep_all = T)
finaldata$Released_Year <- as.numeric(finaldata$Released_Year)
finaldata$Released_Year[is.na(finaldata$Released_Year)] <- 1995
finaldata$Meta_score <- as.numeric(finaldata$Meta_score)
finaldata$Runtime <- as.numeric(substring(finaldata$Runtime,1,3))
finaldata$GrossEarnings <- substring(finaldata$GrossEarnings, 2,30)
finaldata$GrossEarnings <- gsub(',',"",finaldata$GrossEarnings)
finaldata$GrossEarnings <- as.numeric(finaldata$GrossEarnings)

#write.csv(finaldata, 'Movies.csv')

library(dplyr)

# Analysis 1
library(dplyr)
finaldata2 <- group_by(finaldata, Genre)
GenreRating <- summarise(finaldata2, Movies = n(), Min=min(IMDB_Rating),Average=mean(IMDB_Rating),Median=median(IMDB_Rating), Max=max(IMDB_Rating))

# Analysis 2
GenreEarnings <- summarise(finaldata2, Movies=n(),Min=min(GrossEarnings),Average=mean(GrossEarnings),Median=median(GrossEarnings), Max=max(GrossEarnings))

# Analysis 3
plot(finaldata$IMDB_Rating, finaldata$GrossEarnings, xlab = 'Rating', ylab = 'Earnings')
cor(finaldata$IMDB_Rating, finaldata$GrossEarnings)

# Analysis 4
## Analysis 3.3
top10_Directors<-sort(table(finaldata$Director), decreasing = TRUE)[1:10]
top10_Directors

finaldata3<-group_by(finaldata, GrossEarnings)
Directors_Earnings<-subset(finaldata3, Director == "Steven Spielberg" | Director == "Christopher Nolan" | Director == "David Fincher" |
                           Director == "Peter Jackson" | Director == "Robert Zemeckis", select = c(Series_Title, Director, GrossEarnings))
Directors_Earnings

rich<-tapply(Directors_Earnings$GrossEarnings, Directors_Earnings$Director, FUN = sum)
barplot(rich,
        col = "blue",
        main = "Gross Earnings by Director",
        xlab = "Director",
        ylab = "Earnings")

