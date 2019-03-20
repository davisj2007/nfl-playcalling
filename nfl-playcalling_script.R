###load packages and get rid of scientific notation
options(scipen=999)
library(tidyverse)
library(nflscrapR)
library(scales)
library(ggjoy)
library(tidymodels)
library(ggbeeswarm)
library(summarytools)


####load data
#reg_pbp_2009 <- season_play_by_play(2009)
#reg_pbp_2010 <- season_play_by_play(2010)
#reg_pbp_2011 <- season_play_by_play(2011)
#reg_pbp_2012 <- season_play_by_play(2012)
#reg_pbp_2013 <- season_play_by_play(2013)
#reg_pbp_2014 <- season_play_by_play(2014)
#reg_pbp_2015 <- season_play_by_play(2015)
#reg_pbp_2016 <- season_play_by_play(2016)
#reg_pbp_2017 <- season_play_by_play(2017)
#reg_pbp_2018 <- season_play_by_play(2018)

#create master file
#data_complete <- bind_rows(reg_pbp_2009,reg_pbp_2010,reg_pbp_2011,reg_pbp_2012,reg_pbp_2013,reg_pbp_2014,
#reg_pbp_2015,reg_pbp_2016,reg_pbp_2017,reg_pbp_2018)

##so I don't have to wait on the API anymore
data_complete <- read_csv("C:/Users/JacobDavis/Desktop/R Projects/NFL/QB_WinProb_Def/pbp_data_20092018.csv")
 
#Clean data and make pre play win probability easier to handle
data_complete1 <- data_complete %>% 
  filter(!(is.na(PlayType) | 
           PlayType == "Two Minute Warning" |
           PlayType == "No Play" |
           PlayType == "End of Game" |
           PlayType == "Quarter End" |
           PlayType == "Half End" |
           PlayType == "Timeout")) %>%
   mutate(pre.prob = ifelse(posteam == HomeTeam, 
                            Home_WP_pre, 
                            Away_WP_pre),
          down = ifelse(is.na(down), -1, down),
          pre.prob = case_when(
            Onsidekick == 1 & SideofField == HomeTeam ~ Home_WP_pre,
            Onsidekick == 1 & SideofField == AwayTeam ~ Away_WP_pre,
            posteam == HomeTeam ~ Home_WP_pre,
            posteam == AwayTeam ~ Away_WP_pre),
          PlayType = as.factor(PlayType),
          posteam = replace(posteam,posteam == "JAX", "JAC"),
          posteam = replace(posteam,posteam == "STL", "LA"),
          posteam = replace(posteam,posteam == "LA", "LAR"),
          posteam = replace(posteam,posteam == "SD", "LAC"),
          DefensiveTeam = replace(DefensiveTeam,DefensiveTeam == "JAX", "JAC"),
          DefensiveTeam = replace(DefensiveTeam,DefensiveTeam == "STL", "LA"),
          DefensiveTeam = replace(DefensiveTeam,DefensiveTeam == "LA", "LAR"),
          DefensiveTeam = replace(DefensiveTeam,DefensiveTeam == "SD", "LAC"),
          HomeTeam = replace(HomeTeam,HomeTeam == "JAX", "JAC"),
          HomeTeam = replace(HomeTeam,HomeTeam == "STL", "LA"),
          HomeTeam = replace(HomeTeam,HomeTeam == "LA", "LAR"),
          HomeTeam = replace(HomeTeam,HomeTeam == "SD", "LAC"),
          AwayTeam = replace(AwayTeam,AwayTeam == "JAX", "JAC"),
          AwayTeam = replace(AwayTeam,AwayTeam == "STL", "LA"),
          AwayTeam = replace(AwayTeam,AwayTeam == "LA", "LAR"),
          AwayTeam = replace(AwayTeam,AwayTeam == "SD", "LAC"),
          SideofField = replace(SideofField,SideofField == "JAX", "JAC"),
          SideofField = replace(SideofField,SideofField == "STL", "LA"),
          SideofField = replace(SideofField,SideofField == "LA", "LAR"),
          SideofField = replace(SideofField,SideofField == "SD", "LAC"),
          PlayType2 = case_when(
            PlayType == "Punt"~ "Punt", 
            PlayType=="Spike"~ "Spike",
            PlayType=="Field Goal"~"Field Goal",
            !is.na(TwoPointConv) ~ "Two Point Conversion",
            PlayType=="Extra Point"~ "Extra Point",
            PlayType=="QB Kneel"~ "QB Kneel",
            down == 4 & PlayType == "Pass"~ "4d Pass",
            PlayType == "Pass"~ "Pass",
            down == 4 & PlayType == "Run"~ "4d Run",
            PlayType == "Run"~ "Run",
            PlayType == "Sack"~ "Pass",
            PlayType == "Kickoff" & Onsidekick == 1 ~ "Onside Kick",
            PlayType == "Kickoff" & Onsidekick == 0 ~ "Kickoff"),
          PlayType2 = as_factor(PlayType2),
          posteam = as_factor(posteam),
          DefensiveTeam = as_factor(DefensiveTeam),
          HomeTeam = as_factor(HomeTeam),
          AwayTeam = as.factor(AwayTeam)
            ) %>%
  filter(!(is.na(pre.prob)))

#summarize frequency of each play
sumdata <- freq(data_complete1$PlayType2, 
                order = "freq",
                style = "grid",  report.nas = FALSE)

#create a nice HTML summary table for printing
print(sumdata, method = "browser", file = "summary_table.html")



#Distribution of win probability
ggplot(data_complete1,aes(pre.prob)) +
  geom_histogram(color = "white", 
                 binwidth = .05) + 
  geom_vline(xintercept = .5, linetype = "dashed")+
  scale_x_continuous(labels = percent_format())+
  scale_y_continuous(labels = number_format(big.mark = ","))+
  labs(x = element_blank(),
       y = element_blank(),
       title = "The average play come with a 50% chance to win the game",
       subtitle = "Distribution of plays run from 2009-2018 by pre-play win probability\n")

ggsave("prob_histogram.jpeg", device = "jpeg")

#what plays do we call?
data_complete1 %>% 
  ggplot(aes(fct_reorder(PlayType2, pre.prob, .fun = median), pre.prob)) +
  geom_boxplot(aes(fill = PlayType2)) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  theme(legend.position = "none") +
  labs(x = element_blank(),
       y = "\nPre-play Win Probability for possesing team",
       title = "Teams are punting in the wrong situations",
       subtitle = "Distribution of pre-snap win probability by type of play run, all plays 2009-2018\n",
       caption = "SOURCE: nflscrapR")

ggsave("play_dist_overall.jpeg", device = "jpeg")

#let's jitter to show more data
data_complete1 %>% 
  ggplot(aes(fct_reorder(PlayType2, pre.prob, .fun = median), pre.prob)) +
  geom_quasirandom(method = "frowney", 
                   alpha = 1/5,
                   size = 1/20, 
                   aes(color = PlayType2)) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  theme(legend.position = "none") +
  labs(x = element_blank(),
       y = "\nPre-snap Win Probability for possesing team",
       title = "4th down passes are a desperate measure",
       subtitle = "Every play from 2009-2018 called by play type and win probability (n=398,465)\n",
       caption = "SOURCE: nflscrapR")

ggsave("play_dist_overall_jitter.jpeg", device = "jpeg")


#Finally, let's summarize with average and median calculations
data_complete1 %>% group_by(PlayType2) %>% 
  summarize(Median = median(pre.prob), 
            Average = mean(pre.prob)) %>%
  gather(key = key, value = value, Median, Average) %>%
  ggplot(aes(fct_reorder(PlayType2, value),value)) + 
  geom_col(aes(fill = key),
           position="dodge") + 
  scale_y_continuous(labels = percent_format())+
  labs(x = element_blank(),
       y = element_blank(),
       subtitle = "Average and median pre-snap win probability, by play type\n")+
  guides(fill = guide_legend(reverse = TRUE))+
  coord_flip() + 
  theme(legend.title = element_blank())
ggsave("summary_playtypes.jpeg", device ="jpeg")


#let's build a classification model to see how predictable play calling is based on situation---------------
#create our test and training datasets
set.seed(4595)
data_split <- rsample::initial_split(data_complete1, 
                                     strata = "PlayType2", p = 0.7)
data_train <- rsample::training(data_split)
data_test <- rsample::testing(data_split)

####let's use xgboost because it's fast and accurate
#first, let's set up the model
class.model <- boost_tree(mode = "classification", 
                          mtry = varying(), 
                          trees = varying(), 
                          min_n = varying(),
                          tree_depth = varying())


#create a random grid of hyperparameters for some light grid search
set.seed(1234)
class.grid <- grid_random(
  mtry %>% range_set(c(2, 50)),
  trees %>% range_set(c(2, 2000)),
  min_n %>% range_set(c(2,30)),
  tree_depth %>% range_set(c(2,15)),
  size = 5
)

#apply the random grid to our model
for(i in 1:nrow(class.grid)) {
  print(merge(class.model, class.grid[i, ]))
}

#tune models
class.model1 <- boost_tree(mode = "classification", 
                           mtry = 43, 
                           trees = 634, 
                           min_n = 25,
                           tree_depth = 8) %>%
  set_engine("xgboost") %>%
  fit(PlayType2 ~ pre.prob +
                 down +
                 qtr +
                 TimeSecs +
                 yrdline100 + 
                 ydstogo +
                 ScoreDiff +
                 Season +
                 No_Score_Prob +
                 Field_Goal_Prob +
                 Touchdown_Prob +
                 ExPoint_Prob + 
                 TwoPoint_Prob, data = data_train)

class.model2 <- boost_tree(mode = "classification", 
                           mtry = 16, 
                           trees = 607, 
                           min_n = 17,
                           tree_depth = 5) %>%
  set_engine("xgboost") %>%
  fit(PlayType2 ~ pre.prob +
        down +
        qtr +
        TimeSecs +
        yrdline100 + 
        ydstogo +
        ScoreDiff +
        Season +
        No_Score_Prob +
        Field_Goal_Prob +
        Touchdown_Prob +
        ExPoint_Prob + 
        TwoPoint_Prob, data = data_train)

class.model3 <- boost_tree(mode = "classification", 
                           mtry = 15, 
                           trees = 319, 
                           min_n = 28,
                           tree_depth = 6) %>%
  set_engine("xgboost") %>%
  fit(PlayType2 ~ pre.prob +
        down +
        qtr +
        TimeSecs +
        yrdline100 + 
        ydstogo +
        ScoreDiff +
        Season +
        No_Score_Prob +
        Field_Goal_Prob +
        Touchdown_Prob +
        ExPoint_Prob + 
        TwoPoint_Prob, data = data_train)

class.model4 <- boost_tree(mode = "classification", #this was the best model
                           mtry = 11, 
                           trees = 81, 
                           min_n = 26,
                           tree_depth = 9) %>%
  set_engine("xgboost") %>%
  fit(PlayType2 ~ pre.prob +
        down +
        qtr +
        TimeSecs +
        yrdline100 + 
        ydstogo +
        ScoreDiff +
        Season +
        No_Score_Prob +
        Field_Goal_Prob +
        Touchdown_Prob +
        ExPoint_Prob + 
        TwoPoint_Prob, data = data_train)

class.model5 <- boost_tree(mode = "classification", 
                           mtry = 13, 
                           trees = 439, 
                           min_n = 3,
                           tree_depth = 4) %>%
  set_engine("xgboost") %>%
  fit(PlayType2 ~ pre.prob +
        down +
        qtr +
        TimeSecs +
        yrdline100 + 
        ydstogo +
        ScoreDiff +
        Season +
        No_Score_Prob +
        Field_Goal_Prob +
        Touchdown_Prob +
        ExPoint_Prob + 
        TwoPoint_Prob, data = data_train)

#No parameters specified
class.model.naive <- boost_tree(mode = "classification") %>%
  set_engine("xgboost") %>%
  fit(PlayType2 ~ pre.prob +
        down +
        qtr +
        TimeSecs +
        yrdline100 + 
        ydstogo +
        ScoreDiff +
        Season +
        No_Score_Prob +
        Field_Goal_Prob +
        Touchdown_Prob +
        ExPoint_Prob + 
        TwoPoint_Prob, data = data_train)


#time to see what model performed the best.  
test_results <- data_test %>%
  bind_cols(
    predict(class.model1, new_data = data_test)) %>%
  rename(pred.1 = .pred_class) %>%
  bind_cols(
    predict(class.model2, new_data = data_test)) %>%
  rename(pred.2 = .pred_class) %>%
  bind_cols(
    predict(class.model3, new_data = data_test)) %>%
  rename(pred.3 = .pred_class) %>%
  bind_cols(
    predict(class.model4, new_data = data_test)) %>%
  rename(pred.4 = .pred_class) %>%
  bind_cols(
    predict(class.model5, new_data = data_test)) %>%
  rename(pred.5 = .pred_class) %>%
  bind_cols(
    predict(class.model.naive, new_data = data_test)) %>%
  rename(pred.naive = .pred_class)

#check the metrics  
metrics(test_results,PlayType2, pred.1)
metrics(test_results,PlayType2, pred.2)
metrics(test_results,PlayType2, pred.3)
metrics(test_results,PlayType2, pred.4)
metrics(test_results,PlayType2, pred.5)
metrics(test_results,PlayType2, pred.naive)

#okay, we decided on our model, now let's fit the entire dataset to it
data.complete.analysis <- data_complete1 %>%
  bind_cols(
    predict(class.model4, new_data = data_complete1)) %>%
  rename(predict.play = .pred_class) %>% 
  filter(!is.na(posteam)) %>%
  mutate(accurate = ifelse(predict.play == PlayType2, 1, 0))

#save the completed file for later, just in case
write.csv(data.complete.analysis, "predictedplays.csv")

#let's create a confusion table, check the summary statistics, and viz
confusion.table <- conf_mat(data.complete.analysis,PlayType2, predict.play)
summary(confusion.table)
autoplot(confusion.table, type = "heatmap") + 
  labs(x = "\nPredicted Play Call",
       y = "Actual Play Call\n",
       title = "The model correctly predicts ~77% of plays",
       subtitle = "Confusion matrix of play classification model\n")
ggsave("confusion_matrix.jpeg", device = "jpeg", width = 9)



#what teams were hardest to predict?
data.complete.analysis %>% 
  group_by(posteam) %>%
  summarize(accuracy = mean(accurate)- 0.763) %>%
  ggplot(aes(fct_reorder(posteam, accuracy), accuracy)) + 
  geom_col(aes(fill = accuracy)) + 
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-.04,.04))+
  scale_fill_gradient2(low = "#F4364C", mid = "#cec51b", high = "#228CA0")+
  labs(x = element_blank(),
       y = "\nRelative accuracy compared to overall model",
       title = "Does it surprise anyone that NE is relatively less predictable?") + 
  coord_flip() + theme(legend.position = "none")

ggsave("Average_predictability_byteam.jpeg", device = "jpeg")




####okay, let's see if deviating from "typical" play-calling is good or bad
#first, let's summarize all of the games
game.data <- data.complete.analysis %>%
  group_by(GameID, posteam, HomeTeam, AwayTeam) %>%
  summarize(play.calling = mean(accurate),
            total.plays = n())

#load the game result data 
game.outcomes <- bind_rows(season_games(2009),
                           season_games(2010),
                           season_games(2011),
                           season_games(2012),
                           season_games(2013),
                           season_games(2014),
                           season_games(2015),
                           season_games(2016),
                           season_games(2017),
                           season_games(2018))

#save the completed file for later, just in case
write.csv(game.outcomes, "game_results.csv")

#make GameID a number for joining
game.outcomes <- game.outcomes %>%
  mutate(GameID = as.numeric(GameID))

#Join our datasets                      
game.data <- left_join(game.data, game.outcomes, by = c("GameID")) 

#create the win indicator
game.model <- game.data %>%
  mutate(win = case_when(
    posteam == HomeTeam & homescore > awayscore ~ 1,
    posteam == HomeTeam & homescore < awayscore ~ 0,
    posteam == AwayTeam & awayscore > homescore ~ 1,
    posteam == AwayTeam & awayscore < homescore ~0
  )) %>% filter(!is.na(win))

# check to see if there is a specific pattern in play.calling distribution
ggplot(game.model, aes(play.calling)) + 
  geom_histogram(color = "white", aes(fill = win)) + 
  scale_x_continuous(labels = percent_format(accuracy = 1)) + 
  labs(x = "\nDistribution of per game accuracy of generalized play calling model",
       y = element_blank(),
       title = "Predictability of play calling alone isn't enough\nto separate winners and losers")+
  facet_wrap(~win) + 
  theme(legend.position = "none")

ggsave("histogram_predictability.jpeg", device = "jpeg")


#Alternative visualization
ggplot(game.model, aes(play.calling)) + 
  ggjoy::geom_joy(aes(y = as.factor(win))) + 
  scale_x_continuous(labels = percent_format(accuracy = 1)) + 
  scale_y_discrete(labels = c("Loss", "Win")) + 
  theme(legend.position = "none")

###there doesn't seem to be much of a pattern to determining a win versus a loss, but let's model it anyway
#create our test and training datasets
set.seed(1542)
game_split <- rsample::initial_split(game.model, p = 0.7)
game_train <- rsample::training(game_split)
game_test <- rsample::testing(game_split)

#fit logistic regression to see if "predictability" influences basic likelihood to win
win.model <- glm(win ~ play.calling +
                   posteam,
                 data = game_train, family = "binomial")

summary(win.model)
plot(win.model)

#predict our test data
game_model_results <- game_test
game_model_results$pred <- predict(win.model, newdata = game_model_results, type = "response")

#calculate brier score and broad accuracy
game_model_results <- game_model_results %>%
  mutate(expected = ifelse(pred > .5,1,0),
         accuracy = ifelse(win == expected, 1,0),
         brier = (pred - win)^2,
         pred.range = cut(pred, breaks = seq(from = 0, to = 1, by = .1)))

#check basic accuracy, calculate brier score, and brier skill score
mean(game_model_results$accuracy)
mean(game_model_results$brier)
1-(brier/.5)

#Create calibration set
win.confusion <- game_model_results %>%
  group_by(pred.range) %>%
  summarize(correct = mean(win),
            n = n())

#plot calubration and control for n
ggplot(win.confusion, aes(pred.range, correct))+
  geom_col(aes(fill = n))+
  geom_text(aes(label = percent(correct, accuracy = 1), hjust = -.1))+
  scale_y_continuous(breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1), 
                     labels = percent_format(accuracy = 1))+
  expand_limits(y = c(0,1))+
  labs(x = "Assigned probability band\n",
       y = "\nProportion called correctly",
       title = "Predictability + Team does an okay job of picking winners",
       subtitle = "Calibration accuracy by prediction bands") +
  coord_flip()

ggsave("logit_confusion_bar.jpeg", device = "jpeg")



#simulate a dataset to plot wincurve
data.sim <- tibble(play.calling = seq(from=0,to=1, by=.01))
data.sim <- data.sim %>%
  mutate(pred = 1/(1 + exp(-(4.2537 + -4.7542 * play.calling))))


data.sim %>%
  ggplot(aes(play.calling, pred)) +
  geom_line(size = 1, color = "blue") +
  geom_rug(data = game_model_results,
            sides = "b", alpha = .25,color = "red", aes(x = play.calling)) +
  scale_x_continuous(labels = percent_format()) + 
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,1)) + 
  labs(x = "\nAccuracy of generic play calling model",
       y = "Probability of winning\n",
       title = "There is some evidence that predictability can be a bad thing",
       subtitle = "The change in likelihood of win, by accuracy of generalized play calling model,\ncontrolling for team\n")

ggsave("predictability_win.jpeg", device = "jpeg")



###okay, last part of the analysis.  How much are the non-predicted plays worth?

#let's use training set we made for the classification model to determine expected WPA
data_train2 <- data_train

#use linear regression for baseline eWPA
WPA.model <- lm(WPA ~ PlayType2 +
                   pre.prob +
                   as.factor(down) +
                   as.factor(qtr) +
                   TimeSecs +
                   yrdline100 + 
                   ydstogo +
                   ScoreDiff +
                   as.factor(Season) +
                  as.factor(Season) * posteam +
                   posteam + 
                  DefensiveTeam +
                   No_Score_Prob +
                   Field_Goal_Prob +
                   Touchdown_Prob, data = data_train2)
summary(WPA.model)

#limit data to plays when a deviation from prediction occurred
data.expectedWPA <- data.complete.analysis %>%
  filter(accurate == 0)

#Create eWPA and measure success
data.expectedWPA$eWPA <- predict(WPA.model, newdata = data.expectedWPA)
data.expectedWPA <- data.expectedWPA %>%
  mutate(WPA.plus = WPA - eWPA)

#let's plot the cumulative impact of being less predictable by year
data.expectedWPA %>%
  group_by(posteam, Season) %>%
  summarize(WPA.plus1 = mean(WPA.plus, na.rm = TRUE),
            tot.WPA = sum(WPA.plus, na.rm = TRUE)) %>%
  group_by(posteam) %>%
  mutate(avg.season.play = mean(WPA.plus1),
         avg.season.total = mean(tot.WPA)) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(posteam, avg.season.total), tot.WPA)) +
  geom_hline(yintercept = 0) + 
  geom_point(size = 5, alpha = .5, aes(color = as.factor(Season))) +
  geom_point(aes(y = avg.season.total), color = "black", size = 3)+
  scale_fill_viridis_d()+
  scale_y_continuous(labels = percent_format(), 
                     breaks = seq(from = -3, to = 3, by = .5))+
  coord_flip() + 
  labs(x = element_blank(),
       y = "\nCumulative WPA over expected",
       title = "Most teams have benefited (in theory) from deviating from predictability",
       subtitle = "Sum total of WPA minus eWPA for all plays incorrectly predicted, by season and average")+
  theme(legend.title = element_blank(),
        legend.position = "bottom")

ggsave("total_wpa_season.jpeg", height = 9, device = "jpeg")



#And the average effect of deviating per year
data.expectedWPA %>%
  group_by(posteam, Season) %>%
  summarize(WPA.plus1 = mean(WPA.plus, na.rm = TRUE),
            tot.WPA = sum(WPA.plus, na.rm = TRUE)) %>%
  group_by(posteam) %>%
  mutate(avg.season.play = mean(WPA.plus1),
         avg.season.total = mean(tot.WPA)) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(posteam, avg.season.play), WPA.plus1)) +
  geom_hline(yintercept = 0) + 
  geom_point(size = 5, alpha = .5, aes(color = as.factor(Season))) +
  geom_point(aes(y = avg.season.play), color = "black", size = 3)+
  scale_fill_viridis_d()+
  scale_y_continuous(labels = percent_format(accuracy = .1))+
  coord_flip()+
  labs(x = element_blank(),
       y = "\nAverage per play WPA over expected",
       title = "The average unpredictable play adds just a little win probability",
       subtitle = "Average WPA minus eWPA for all plays incorrectly predicted, by season and average")+
  theme(legend.title = element_blank(),
        legend.position = "bottom")

ggsave("average_wpa_season.jpeg", height = 9, device = "jpeg")




