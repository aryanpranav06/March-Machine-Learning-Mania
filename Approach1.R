
library(data.table) 
library(dplyr) 
library(reshape)

TourneySeeds <- fread("TourneySeeds.csv")
SampleSubmission <- fread("SampleSubmission.csv")
Seasons <- fread("Seasons.csv")
Teams <- fread("Teams.csv")
TourneySlots <- fread("TourneySlots.csv")
TourneyDetailedResults <- fread("TourneyDetailedResults.csv")
TourneyCompactResults <- fread("TourneyCompactResults.csv")
RegularSeasonCompactResults <- read.csv("RegularSeasonCompactResults.csv")
RanksKenPom <- fread("RanksKenPom.csv")

TourneySeeds <- TourneySeeds %>% 
  mutate(SeedNum = gsub("[A-Z+a-z]", "", Seed)) %>% select(Season, Team, SeedNum)

RegularSeasonCompactResults$home <- RegularSeasonCompactResults$Wloc
levels(RegularSeasonCompactResults$home) <- list("-1"="A", "1"="H", "0"="N")
RegularSeasonCompactResults$home <- 
  as.numeric(as.character(RegularSeasonCompactResults$home))

library(lme4)

games.to.predict <- cbind(SampleSubmission$Id, colsplit(SampleSubmission$Id, split = "_", names = c('season', 'team1', 'team2')))   
colnames(games.to.predict)[1] <- "Id"
games.to.predict$home <- 0

season <- 2016

sub1 <- RegularSeasonCompactResults %>% 
  filter(Season==season) %>% 
  mutate(team1=as.factor(Wteam), team2=as.factor(Lteam), outcome=1, ptdiff=Wscore/Lscore, dw=.99^(132-Daynum)) %>% 
  select(team1, team2, home, outcome, ptdiff, dw)
sub2 <- RegularSeasonCompactResults %>% 
  filter(Season==season) %>% 
  mutate(team1=as.factor(Lteam), team2=as.factor(Wteam), home=-1*home, outcome=0, ptdiff=Lscore/Wscore, dw=.99^(132-Daynum)) %>% 
  select(team1, team2, home, outcome, ptdiff, dw)
reg.results <- rbind(sub1, sub2)

m.ptdiff <- lmer(ptdiff ~ home +  (1 | team1) + (1 | team2), data = reg.results, weight=dw) 
pred.pt.diffs <- predict(m.ptdiff, games.to.predict[games.to.predict$season==season,], 
                         type="response")
games.to.predict[games.to.predict$season==season,"point_diff_pred"]<- (pred.pt.diffs - 0.5)



sub1 <- RegularSeasonCompactResults %>% 
  filter(Season==season) %>% 
  mutate(team1=as.factor(Wteam), team2=as.factor(Lteam), outcome=1, ptdiff=Wscore/Lscore) %>% 
  select(team1, team2, home, outcome, ptdiff)
sub2 <- RegularSeasonCompactResults %>% 
  filter(Season==season) %>% 
  mutate(team1=as.factor(Lteam), team2=as.factor(Wteam), home=-1*home, outcome=0, ptdiff=Lscore/Wscore) %>% 
  select(team1, team2, home, outcome, ptdiff)
reg.results <- rbind(sub1, sub2)
m.bt <- glmer(outcome ~ home +  (1 | team1) + (1 | team2), data = reg.results, family=binomial()) 
games.to.predict[games.to.predict$season==season,"bt_pred"]<- predict(m.bt, games.to.predict[games.to.predict$season==season,], 
                                                                      type="response")

games.to.predict = games.to.predict %>% filter(season == 2016)

temp <- left_join(games.to.predict, RanksKenPom, by=c("season"="Year", "team1"="Team"))
games.to.predict <- left_join(temp, RanksKenPom, by=c("season"="Year", "team2"="Team"))


colnames(games.to.predict)[which(names(games.to.predict) %in% c("Pyth.x", "Pyth.y" ))] <- c("Team1Pyth", "Team2Pyth")
colnames(games.to.predict)[which(names(games.to.predict) %in% c("Rank.x", "Rank.y" ))] <- c("Team1Rank", "Team2Rank")

games.to.predict <- games.to.predict %>% mutate(Team1Rank = as.numeric(Team1Rank), Team2Rank = as.numeric(Team2Rank), Team1Pyth = as.numeric(Team1Pyth), Team2Pyth = as.numeric(Team2Pyth))

games.to.try = games.to.predict

games.to.try <- games.to.try %>% mutate(rank.pred = 0.5 + 0.014*(Team2Rank - Team1Rank))
games.to.try = games.to.try %>% na.omit()
games.to.try[games.to.try$rank.pred > 0.9, "rank.pred"] = 0.9
games.to.try[games.to.try$rank.pred < 0.1, "rank.pred"] = 0.1

games.to.try <- games.to.try %>% mutate(Pred = 0.6*rank.pred + 0.2*point_diff_pred + 0.2*bt_pred)
write.csv(games.to.try %>% select(Id, Pred), 'Sample_Submission.csv', row.names=FALSE)

