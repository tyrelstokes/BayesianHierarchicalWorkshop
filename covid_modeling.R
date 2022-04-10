###########################################

covid <- readRDS("covid_aus.Rds")

covid$rate_month <- covid$cases/covid$pop

mnth_dt <- data.frame(year = c(rep(2020,10),rep(2021,12),rep(2022,2)),month = c(c(3:12),c(1:12),c(1:2)),
                      pandemic_month = c(1:24))


covid <- left_join(covid,mnth_dt)


###### Make Table
covid %>% mutate(rate_month = round(rate_month,4))%>%
  head(20)%>%kbl(format = "html") %>% kable_paper(full_width = F,html_font = "helvetica")

#######

### Plot the cases

covid %>% mutate(rate_per_100_000 = rate_month*100000) %>%
  ggplot(aes(x = pandemic_month, y= rate_per_100_000))+geom_line(aes(col = lhd_2010_name))



covid_sample <- covid %>% filter((month ==1)&(year ==2022))


stan_data <- list(y = covid_sample$cases,N = nrow(covid_sample), pop = covid_sample$pop)

fit_mean <- stan("binomial_simple.stan", data = stan_data)


summary(fit_mean)[1]


#sum(covid_sample$cases)/sum(covid_sample$pop)


##################

library(INLA)

prior.fixed <- list(mean.intercept = 0, prec.intercept = .1,
                    mean = 0, prec = 1)

mod <- inla(cases ~ f(as.factor(pandemic_month),model = "ar1"),family = "binomial",Ntrials = pop,data = covid,
            control.fixed = prior.fixed)



predict_data <- rbind(data.frame(lhd_2010_name = districts, month = 3,year = 2022,pandemic_month = 25,cases = NA),
                      data.frame(lhd_2010_name = districts, month = 4,year = 2022,pandemic_month = 26,cases = NA))

predict_data <- left_join(predict_data,population_df)

predict_data <- predict_data %>% filter(lhd_2010_name!="Hotel Quarantine")

covid_data <- rbind(covid,predict_data)

ar_order <- 5
mod2 <- inla(cases ~ f(lhd_2010_name,model = "iid",group = pandemic_month, control.group = list(model = "ar",order =ar_order)),
             family = "binomial",Ntrials = pop,data = covid_data,
             control.predictor=list(link = 1,compute=T),control.compute=list(return.marginals.predictor=TRUE))



getmarginal <- function(covid,predict_data){
  
  ncov <- nrow(covid)
  npred <- nrow(predict_data)
  
  out <- foreach(i=(ncov+1):(ncov+npred),.combine = rbind)%do%{
    
    marg <- mod2$marginals.fitted.values[[i]]
    
    expected_rate <- inla.emarginal(function(x){x},marg)
    
    qs <- inla.qmarginal(c(0.05,0.25,.5,.75,.95),marg)
    
    #smoothed <- as.data.frame(inla.smarginal(marg))
    
    data.frame(District = predict_data$lhd_2010_name[(i-ncov)], pandemic_month = predict_data$pandemic_month[(i-ncov)],
               expected_rate = expected_rate, expectated_cases = expectated_rate*predict_data$pop[(i-ncov)],
               q5_rate = qs[1],q50_rate = qs[3],q95_rate = qs[5])
    
    
  }
 out 
}


marg_data <- getmarginal(covid,predict_data)

ggplot(marg_data,aes(x=District,y=expected_rate,ymin = q5_rate,ymax = q95_rate))+
  geom_crossbar(aes(color=District))+coord_flip()+facet_wrap(~pandemic_month)



mod2$marginals.fitted.values
