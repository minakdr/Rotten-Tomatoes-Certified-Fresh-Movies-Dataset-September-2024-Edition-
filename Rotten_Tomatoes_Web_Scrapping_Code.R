library(httr)
library(rvest)
library(dplyr)

# list of URLs and corresponding genres , you can expend it to more genres if needed and add more pages 

urls <- list(
  action = 'https://www.rottentomatoes.com/browse/movies_at_home/critics:certified_fresh~genres:action?page=10',
  horror = 'https://www.rottentomatoes.com/browse/movies_at_home/critics:certified_fresh~genres:horror?page=10',
  comedy = 'https://www.rottentomatoes.com/browse/movies_at_home/critics:certified_fresh~genres:comedy?page=10',
  romance = 'https://www.rottentomatoes.com/browse/movies_at_home/critics:certified_fresh~genres:romance?page=10',
  documentary = 'https://www.rottentomatoes.com/browse/movies_at_home/critics:certified_fresh~genres:documentary?page=10',
  animation = 'https://www.rottentomatoes.com/browse/movies_at_home/critics:certified_fresh~genres:animation?page=10',
  crime = 'https://www.rottentomatoes.com/browse/movies_at_home/critics:certified_fresh~genres:crime?page=10',
  scifi = 'https://www.rottentomatoes.com/browse/movies_at_home/critics:certified_fresh~genres:sci_fi?page=2',
  fantasy = 'https://www.rottentomatoes.com/browse/movies_at_home/critics:certified_fresh~genres:fantasy?page=10'
)

titles_list <- list()
audience_scores_list <- list()
critics_scores_list <- list()
genres_list <- list()

# function to scrape data from a URL
scrape_data <- function(url, genre) {
  # Read the page
  page <- read_html(GET(url, timeout(60)))
  
  # extract data using the html tags found after inspecting the website 
  titles <- page %>%
    html_nodes('span.p--small') %>% 
    html_text(trim = TRUE)
  
  audience_scores <- page %>%
    html_nodes('rt-text[slot="audienceScore"]') %>%
    html_text(trim = TRUE)
  
  critics_scores <- page %>%
    html_nodes('rt-text[slot="criticsScore"]') %>%
    html_text(trim = TRUE)
  
  # ensure equal lengths by padding with NA
  max_length <- max(length(titles), length(audience_scores), length(critics_scores))
  
  titles <- c(titles, rep(NA, max_length - length(titles)))
  audience_scores <- c(audience_scores, rep(NA, max_length - length(audience_scores)))
  critics_scores <- c(critics_scores, rep(NA, max_length - length(critics_scores)))
  
  list(
    titles = titles,
    audience_scores = audience_scores,
    critics_scores = critics_scores,
    genre = rep(genre, max_length)  # Replicate genre for each title
  )
}

# a for loop for each URL to scrape the adequate data
for (genre in names(urls)) {
  data <- scrape_data(urls[[genre]], genre)
  
  #store data in lists
  titles_list[[genre]] <- data$titles
  audience_scores_list[[genre]] <- data$audience_scores
  critics_scores_list[[genre]] <- data$critics_scores
  genres_list[[genre]] <- data$genre
}

results <- data.frame(
  title = unlist(titles_list),
  audience_score = unlist(audience_scores_list),
  critic_score = unlist(critics_scores_list),
  genre = unlist(genres_list),
  stringsAsFactors = FALSE
)


#check the results , you may notice an unwanted column at the beginning without a title . it will disappear as you save it as a csv

head(results)
str(results)

# Save as a csv 

write.csv(results, "MoviesRatingRottenTomatoExtended.csv", row.names = FALSE)