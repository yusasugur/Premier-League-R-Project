library(dplyr)

stats <- read.csv(file = "england-premier-league-matches-2018-to-2019-stats.csv")
stats <-as_tibble(stats)


matches_selected <- stats %>% select(attendance,home_team_name,away_team_name,home_team_goal_count,odds_ft_home_team_win)

head(matches_selected)

matches_selected %>% arrange(desc(attendance))

matches_selected %>% mutate(homeWinChance=(10-odds_ft_home_team_win)*10) %>% top_n(10,homeWinChance)

matches_selected %>% group_by(home_team_name)  %>% summarise(total=mean(attendance))   %>% top_n(10,total) %>% arrange(desc(total))


library(ggplot2)
########
#This graph show during 2018/2019 season in EPL, which teams are scored most at their home.
bar<-matches_selected %>% group_by(home_team_name)   %>% summarise(total=mean(home_team_goal_count))  %>% top_n(10,total) %>% arrange(desc(total))

ggplot(bar,aes(x=home_team_name,y=total))+ 
  geom_bar(stat = "identity") +labs(title = "English Premier League 2018-2019 Season Teams with Highest Home Goal Average",
                                   x = "Team", y = "Average Goal per Game")

########
#This graph show during 2018/2019 season in EPL, which teams have highest win chance at their home.

bar2 <- matches_selected %>% mutate(homeWinChance=(10-odds_ft_home_team_win)*10) %>% group_by(home_team_name) %>% summarise(total=mean(homeWinChance)) %>% top_n(10,total)  %>% arrange(desc(total)) 

ggplot(bar2,aes(x=home_team_name,y=total))+ 
  geom_bar(stat = "identity")+ labs(title = "English Premier League 2018-2019 Season Teams with Most Home Win Chance Percantage",
                                    x = "Team Name", y = "Win Percantage")

########
#This graph show during 2018/2019 season in EPL, which referees give the most cards per game, includes both yellow and red cards.  

bar3 <- stats %>% select(referee)
totalCard <- stats%>% transmute(totalCard = home_team_yellow_cards + home_team_red_cards + away_team_yellow_cards + away_team_red_cards)
bar3 <- dplyr::bind_cols(bar3,totalCard)

bar3 <- bar3 %>% group_by(referee) %>% summarise(totalCards=mean(totalCard))%>% top_n(10,totalCards)  %>% arrange(desc(totalCards))

ggplot(bar3,aes(x=referee,y=totalCards))+ 
  geom_bar( stat="identity",width=.7)+ labs(title = "English Premier League 2018-2019 Season Most Card-Happy Referees",
                                     x = "Referee", y = "Average Card per Game")
