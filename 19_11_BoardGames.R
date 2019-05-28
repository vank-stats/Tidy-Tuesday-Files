library(readr)
library(ggplot2)
library(gghighlight)

board_games <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")


# Graph looking at ratings for Dominion

ggplot(board_games) +
  geom_point(aes(x = users_rated, y = average_rating, color = mechanic), 
             alpha = .3) +
  gghighlight(grepl("Dominion", name), label_key = name,
              label_params = list(size = 3))


# Best game each year

best <- group_by(board_games, year_published) %>% 
  summarize(best = max(average_rating))
board_games <- left_join(board_games, best, by = "year_published")
board_games <- mutate(board_games, isbest = (average_rating/best) == 1)

ggplot(board_games) +
  geom_point(aes(x = year_published, y = average_rating), alpha = .3) +
  gghighlight(isbest, average_rating > 8,
              label_key = name, 
              label_params = list(size = 3))



# Practicing with highlighted regions

ggplot(board_games) +
  geom_ribbon(aes(x = year_published, ymin = 2, ymax = 3), 
              fill = 'lightgray') +
  geom_point(aes(x = year_published, y = average_rating), 
             color = 'red') +
  gghighlight(average_rating > 2, average_rating < 3)

  


# What makes for a good game?

# Scatterplots of quantitative variables
plot(select(board_games, average_rating, min_players, max_players, 
            playing_time, min_age, year_published))

# Clean up the data for things like 800 players and 60000 minute playtimes
games_clean <- filter(board_games, playing_time < 10000, max_players < 100)


# Set minimum players 0, playingtime 0, max players 0, min age 0 all to NA
games_clean <- mutate(games_clean, 
                      min_players = case_when(min_players == 0 ~ NA,
                                              min_players > 0 ~ min_players),
                      playing_time - case_when(playing_time == 0 ~ NA,
                                               playing_time > 0 ~ playing_time))

# Change max_players to levels?