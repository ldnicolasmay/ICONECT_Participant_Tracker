library(dplyr)

weeks_1 <- 1:26
weeks_2 <- 27:52
df <- tibble(
  step = vector(mode = "character", 
                length = length(c(weeks_1, weeks_2)) * 5),
  stage = vector(mode = "character", 
                 length = length(c(weeks_1, weeks_2)) * 5),
  rc_field = vector(mode = "character", 
                    length = length(c(weeks_1, weeks_2)) * 5))


for (i in weeks_1) {
  df[(i-1)*5+1, ] <- 
    list(paste0("Telephone Chat - Week ", i), 
         "Intervention Period 1", NA_character_)
  df[(i-1)*5+2, ] <- 
    list(paste0("Video Chat - Week ", i, " Day 1"), 
         "Intervention Period 1", NA_character_)
  df[(i-1)*5+3, ] <- 
    list(paste0("Video Chat - Week ", i, " Day 2"), 
         "Intervention Period 1", NA_character_)
  df[(i-1)*5+4, ] <- 
    list(paste0("Video Chat - Week ", i, " Day 3"), 
         "Intervention Period 1", NA_character_)
  df[(i-1)*5+5, ] <- 
    list(paste0("Video Chat - Week ", i, " Day 4"), 
         "Intervention Period 1", NA_character_)
}

for (i in weeks_2) {
  df[(i-1)*5+1, ] <- 
    list(paste0("Telephone Chat - Week ", i), 
         "Intervention Period 2", NA_character_)
  df[(i-1)*5+2, ] <- 
    list(paste0("Video Chat - Week ", i, " Day 1"), 
         "Intervention Period 2", NA_character_)
  df[(i-1)*5+3, ] <- 
    list(paste0("Video Chat - Week ", i, " Day 2"), 
         "Intervention Period 2", NA_character_)
  df[(i-1)*5+4, ] <- 
    list(paste0("Video Chat - Week ", i, " Day 3"), 
         "Intervention Period 2", NA_character_)
  df[(i-1)*5+5, ] <- 
    list(paste0("Video Chat - Week ", i, " Day 4"), 
         "Intervention Period 2", NA_character_)
}

readr::write_csv(df, "df.csv", na = "")
