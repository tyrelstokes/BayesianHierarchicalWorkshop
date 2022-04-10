library(tidyverse)

#### Load the data
fts <- read.csv("free_throws.csv")

##### Clean data and organize into player season format
fts$attempt <- 1

fts_player_season <- fts %>% group_by(player,season) %>% summarise(attempts = sum(attempt),makes = sum(shot_made))

fts_player_season <- fts_player_season %>% mutate(made_pct = makes/attempts)

###### Going to use only the 2014-15 and 2015-16 seasons for training
###### and predicting respectively

fts_ps_16 <- fts_player_season %>% filter(season =="2014 - 2015")
fts_ps_15 <- fts_player_season %>% filter(season == "2015 - 2016")

#######
#######
# Create Player ids that will play nicely with the stan model

fts_ps_15$player_id <- c(1:nrow(fts_ps_15))

## Find which players are in the prediction set but not in training
new_players_inds <- which(!(fts_ps_16$player %in% fts_ps_15$player))
new_players <- fts_ps_16$player[new_players_inds]
new_ids <- c((nrow(fts_ps_15)+1):(nrow(fts_ps_15)+length(new_players)))

fts_ps_16$player_id <- unlist(lapply(fts_ps_16$player,function(x){
 if(x %in% fts_ps_15$player){
   fts_ps_15$player_id[which(x == fts_ps_15$player)]
 }else{
   new_ids[which(x == new_players)]
 }
}))

fts_ps_16$in_training <- ifelse((fts_ps_16$player %in% fts_ps_15$player),1,0)

#################################################################
################################################################
### Set the data up to run the stan model

stan_data_re <- list(n_train = nrow(fts_ps_15),n_test = nrow(fts_ps_16), y = fts_ps_15$makes,
                    y_test = fts_ps_16$makes, Attempts = fts_ps_15$attempts, Attempts_test = fts_ps_16$attempts,
                    player_training_int = fts_ps_15$player_id, player_test_int = fts_ps_16$player_id,
                    in_training = fts_ps_16$in_training,np = nrow(fts_ps_15),
                    tau_std_prior = 3, mu_std_prior = 5)

library(rstan)

fit_re <- stan("free_throw_re.stan",data = stan_data_re,chains = 4, iter = 2500)




##############################################################
##############################################################
## Post fitting analysis and plots

pred_probs <-  as.data.frame(summary(fit_re,pars = "predicted_probability")[1]$summary)
pred_probs$player_id <-fts_ps_16$player_id

fts_ps_16$predicted_mean_prob <- pred_probs[,1]
fts_ps_16$predicted_q2p5_prob <- pred_probs[,4]
fts_ps_16$predicted_q975_prob <- pred_probs[,8]


#################################
library(posterior)
library(ggridges)

###############3
## Average probability + between player variance plot
draws <- as_draws_df(extract(fit_re,pars = c("avg_prob","tau")))

pdf <- draws %>% pivot_longer(cols = c("avg_prob","tau"))

p1 <- pdf %>% ggplot(aes(x=value,y = name)) + geom_density_ridges(aes(fill = name))
p1 

#################
## Prediction for unseen player
draws2 <-as.data.frame(extract(fit_re,pars = c("predicted_probability")))

p2 <- draws2 %>% ggplot(aes(x = predicted_probability.1))+geom_density(fill = "dodgerblue",alpha = .5)+
  geom_vline(xintercept = fts_ps_16$predicted_q2p5_prob[1])+geom_vline(xintercept = fts_ps_16$predicted_q975_prob[1])
p2+ xlab("Predicted Probability for unseen Player")

######################
## Compare said predictions to distribution of actual results
df1 <- data.frame(value = draws2[,1],name = "Predicted Density", weight = 1)
df2 <- data.frame(value = fts_ps_16$made_pct[fts_ps_16$in_training ==0], name = "actual results",
                  weight = fts_ps_16$attempts[fts_ps_16$in_training ==0]/sum(fts_ps_16$attempts[fts_ps_16$in_training ==0]))
df3 <- data.frame(value = fts_ps_16$made_pct[fts_ps_16$in_training ==1], name = "actual results,observed group",
                  weight = fts_ps_16$attempts[fts_ps_16$in_training ==1]/sum(fts_ps_16$attempts[fts_ps_16$in_training ==1]))

df <- rbind(df1,df2,df3)


p3 <- df %>% filter(name !="actual results,observed group")%>% ggplot(aes(x= value)) + geom_density(aes(fill = name, weight = weight),alpha = .5)
p3

p4 <- df %>% filter(name !="actual results")%>% ggplot(aes(x= value)) + geom_density(aes(fill = name, weight = weight),alpha = .5)
p4

##########################
### Make the Stein Plot
### First run the fixed effects model

stan_data_fe <- list(n_train = nrow(fts_ps_15),n_test = nrow(fts_ps_16), y = fts_ps_15$makes,
     y_test = fts_ps_16$makes, Attempts = fts_ps_15$attempts, Attempts_test = fts_ps_16$attempts,
     player_training_int = fts_ps_15$player_id, player_test_int = fts_ps_16$player_id,
     in_training = fts_ps_16$in_training,np = nrow(fts_ps_15),
     fe_std_prior = 5)

fit_fe <- stan("free_throw_fe.stan",data = stan_data_fe,chains = 4, iter = 2500)

pred_probs_fe <-   as.data.frame(summary(fit_fe,pars = "predicted_probability")[1]$summary)

pred_probs_fe$player_id <- fts_ps_16$player_id


################################

# Predictions from Random effect and Fixed effect on 2015-16 data

attempts_15 <- unlist(lapply(fts_ps_16$player_id,function(x){
  if(x %in% fts_ps_15$player_id){
    fts_ps_15$attempts[which(x == fts_ps_15$player_id)]
  }else{
    NA
  }
}))

attempts_16 <- unlist(lapply(fts_ps_15$player_id,function(x){
  if(x %in% fts_ps_16$player_id){
    fts_ps_16$attempts[which(x == fts_ps_16$player_id)]
  }else{
    NA
  }
}))

d1 <- data.frame(value = pred_probs$mean,type = "prediction",model = "RE",season = "2015 - 2016",
                 player_id = pred_probs$player_id,training_attempts = attempts_15, prediction_attempts = fts_ps_16$attempts, 
                 std = pred_probs$sd,
                 yaxis = "RE predictions 15-16")
d2 <- data.frame(value = pred_probs_fe$mean,type = "prediction",model = "FE",season = "2015 - 2016",
                 player_id = pred_probs_fe$player_id, training_attempts = attempts_15, prediction_attempts = fts_ps_16$attempts,
                 std = pred_probs_fe$sd,
                 yaxis = "FE predictions 15-16")
# Actual data from the two seasons
d3 <- data.frame(value = fts_ps_15$made_pct,type = "actual",model = "NA",season = "2014 - 2015",
                 player_id = fts_ps_15$player_id, training_attempts = fts_ps_15$attempts, prediction_attempts = attempts_16,
                 std = sqrt(fts_ps_15$made_pct*(1-fts_ps_15$made_pct)/fts_ps_15$attempts),
                 yaxis = "Actual 14-15")

d4 <- data.frame(value = fts_ps_16$made_pct,type = "actual",model = "NA",season = "2015 - 2016",
                 player_id = fts_ps_16$player_id, training_attempts = attempts_15,prediction_attempts = fts_ps_16$attempts,
                 std = sqrt(fts_ps_16$made_pct*(1-fts_ps_16$made_pct)/fts_ps_16$attempts),
                 yaxis = "Actual 15-16")


stein_df <- rbind(d1,d2,d3,d4)

stein_df$weight <- with(stein_df,ifelse(type =="actual",
          ifelse(season =="2014 - 2015",1/training_attempts,1/prediction_attempts),1/training_attempts))


stein_df$yaxis <- factor(stein_df$yaxis, levels = rev(c("Actual 14-15","FE predictions 15-16", "RE predictions 15-16",
                                                    "Actual 15-16")))

group_mean <- summary(fit_re,pars = "avg_prob")[1]$summary

p5 <- stein_df %>% filter(player_id %in% fts_ps_16$player_id[fts_ps_16$in_training==1]) %>%
  filter(model %in% c("FE","RE")) %>% ggplot(aes(x = value, y= yaxis)) +
  geom_point(aes(size = 1/training_attempts, col = model), alpha = .7)+
  geom_line(aes(group = player_id,alpha =1/training_attempts ))+
  scale_color_manual(values = c("firebrick","dodgerblue"))+geom_vline(xintercept = group_mean[1,1])+
  xlab("Predicted Probability")+ylab("")
 
p5 

#################

p6 <- stein_df %>%filter(player_id %in% fts_ps_16$player_id[fts_ps_16$in_training==1]) %>%
  ggplot(aes(x = value, y= yaxis)) +
  geom_point(aes(size = 1/training_attempts, col = model), alpha = .7)+
  geom_line(aes(group = player_id,alpha =weight ))+
  scale_color_manual(values = c("firebrick","dodgerblue","black"))+geom_vline(xintercept = group_mean[1,1])+
  xlab("Predicted Probability")+ylab("")

p6


##################
#################

## Sensitivity to Priors
## Regime 1, Vague Priors

stan_data_fe_vague <- list(n_train = nrow(fts_ps_15),n_test = nrow(fts_ps_16), y = fts_ps_15$makes,
                     y_test = fts_ps_16$makes, Attempts = fts_ps_15$attempts, Attempts_test = fts_ps_16$attempts,
                     player_training_int = fts_ps_15$player_id, player_test_int = fts_ps_16$player_id,
                     in_training = fts_ps_16$in_training,np = nrow(fts_ps_15),
                     fe_std_prior = 100)


fit_fe_vague <- stan("free_throw_fe.stan",data = stan_data_fe_vague,chains = 4, iter = 2500)


stan_data_re_vague <- list(n_train = nrow(fts_ps_15),n_test = nrow(fts_ps_16), y = fts_ps_15$makes,
                     y_test = fts_ps_16$makes, Attempts = fts_ps_15$attempts, Attempts_test = fts_ps_16$attempts,
                     player_training_int = fts_ps_15$player_id, player_test_int = fts_ps_16$player_id,
                     in_training = fts_ps_16$in_training,np = nrow(fts_ps_15),
                     tau_std_prior = 100, mu_std_prior = 100)



fit_re_vague <- stan("free_throw_re.stan",data = stan_data_re_vague,chains = 4, iter = 2500)

##################################
## Make graphs
library(ggpubr)
pred_probs_vague <- as.data.frame(summary(fit_re_vague,pars = "predicted_probability")[1]$summary)
pred_probs_vague$player_id <-fts_ps_16$player_id
pred_probs_vague$actual <- fts_ps_16$made_pct
pred_probs_vague$attempts <- fts_ps_16$attempts

pred_probs_fe_vague <- as.data.frame(summary(fit_fe_vague,pars = "predicted_probability")[1]$summary)
pred_probs_fe_vague$player_id <-fts_ps_16$player_id
pred_probs_fe_vague$actual <- fts_ps_16$made_pct
pred_probs_fe_vague$attempts <- fts_ps_16$attempts

calib_fe <- pred_probs_fe_vague %>% ggplot(aes(x = mean,y=actual)) + geom_smooth(aes(weight = attempts))+
  geom_abline(intercept =0,slope =1,col = "red")+ylab("Actual percentage")+xlab("Predicted Probability")

hist_fe <- pred_probs_fe_vague %>% ggplot(aes(x = mean)) + geom_histogram()+xlab("Predicted Probability")


ggarrange(calib_fe,hist_fe,ncol=1, heights = c(2.5,1))


####################################
####################################

## Tight Prior Experiment

stan_data_fe_tight <- list(n_train = nrow(fts_ps_15),n_test = nrow(fts_ps_16), y = fts_ps_15$makes,
                           y_test = fts_ps_16$makes, Attempts = fts_ps_15$attempts, Attempts_test = fts_ps_16$attempts,
                           player_training_int = fts_ps_15$player_id, player_test_int = fts_ps_16$player_id,
                           in_training = fts_ps_16$in_training,np = nrow(fts_ps_15),
                           fe_std_prior = 0.01)


fit_fe_tight <- stan("free_throw_fe.stan",data = stan_data_fe_tight,chains = 4, iter = 2500)


stan_data_re_tight <- list(n_train = nrow(fts_ps_15),n_test = nrow(fts_ps_16), y = fts_ps_15$makes,
                           y_test = fts_ps_16$makes, Attempts = fts_ps_15$attempts, Attempts_test = fts_ps_16$attempts,
                           player_training_int = fts_ps_15$player_id, player_test_int = fts_ps_16$player_id,
                           in_training = fts_ps_16$in_training,np = nrow(fts_ps_15),
                           tau_std_prior = 0.01, mu_std_prior = 0.01)



fit_re_tight<- stan("free_throw_re.stan",data = stan_data_re_tight,chains = 4, iter = 2500)


######
######
## Make Calibration Plots

pred_probs_tight <- as.data.frame(summary(fit_re_tight,pars = "predicted_probability")[1]$summary)
pred_probs_tight$player_id <-fts_ps_16$player_id
pred_probs_tight$actual <- fts_ps_16$made_pct
pred_probs_tight$attempts <- fts_ps_16$attempts

pred_probs_fe_tight <- as.data.frame(summary(fit_fe_tight,pars = "predicted_probability")[1]$summary)
pred_probs_fe_tight$player_id <-fts_ps_16$player_id
pred_probs_fe_tight$actual <- fts_ps_16$made_pct
pred_probs_fe_tight$attempts <- fts_ps_16$attempts

calib_tight <- pred_probs_fe_tight %>% ggplot(aes(x = mean,y=actual)) + geom_smooth(aes(weight = attempts))+
  geom_abline(intercept =0,slope =1,col = "red")+ylab("Actual percentage")+xlab("Predicted Probability")

hist_tight <- pred_probs_fe_tight  %>% ggplot(aes(x = mean)) + geom_histogram()+xlab("Predicted Probability")


ggarrange(calib_tight,hist_tight,ncol=1, heights = c(2.5,1))


library(gbm)

h1 <- hist(fts_ps_16$predicted_mean_prob, breaks = 30 )
h1$counts <- h1$counts/nrow(fts_ps_16)

plot(h1)
plot(fts_ps_16$predicted_mean_prob,fts_ps_16$made_pct,add =T)+abline(a = 0,b=1,col = "red", add =TRUE)
 


