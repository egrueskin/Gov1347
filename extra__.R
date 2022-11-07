test$pred_seats<-predict(lm_seats,test,interval='prediction')
test$pred_vote<-predict(lm_vote,test,interval='prediction')
tab<-data.frame(cbind(test[test$year==2022&test$party=='D',c(3,2,22)],
                      test[test$year==2022&test$party=='R',c(3,2,23)]))%>%mutate(pred_vote_D=100-pred_vote,pred_st_R=435-pred_seats)
tab<-data.frame(rbind(tab[,c(1:3,7)]%>% rename('pred_vote'='pred_vote_D'),tab[,c(4:6,8)]%>%
                        rename('pred_seats'='pred_st_R','party'='party.1','year'='year.1')))


mutate(pred_seats=ifelse(party=='R',))
%>%
  
  tab<-data.frame(cbind(test[test$year==2022&test$party=='D',c(3,2,22:23)]%>%
                          mutate(party_2='R',pred_seats_R=435-pred_seats,pred_vote_R=100-pred_vote)
                        tab
                        
\beta_1
                        
                        $$VoteShare=\beta_0+\beta_{1}Incumbent_i+\kappa_{im}SamePartyPres_i*MidtermYear_m+\beta_2*prevvoteshare
                        $$
                          $$VoteShare=\beta_0+\kappa_{im}SamePartyPres_i*MidtermYear_m+\rho_{i}Party_p*CookRating
                        $$
                          $$VoteShare=\beta_0+\beta_1*pollingpercent$$
                        
$$VoteShare=\beta_0+\beta_{1}SamePartyPres_i+\kappa_{im}SamePartyPres_i*MidtermYear_m+
                          \gamma_iSamePartyPres_i*PresidentialApproval+\rho_iSamePartyPres_i*GDPgrowthpct+\\\beta_2*genericballotpct+
                          \beta_3*lagvoteshare
                        $$
                          
                          
                          
                          as_tibble(oh_22[,c(13,9,117:119)])%>%
                          arrange(day_diff)
                        kable()
                        pred_poll_inc <- predict(mod_poll_inc, oh_22, 
                                                 interval = "prediction", level=0.95)
                        pred_poll_inc
                        ,42,117,119)]
oh_22<-tibble(year=c(2022,2022),party=c('R','D'),term=c(16,0),Status=c('Incumbent','Challenger'),prev=c(53.7,46.3),
              MidtermYear=c(1,1),same_party=c(0,1),party_support=c(45.3,44.7),exp_rating=c(4.44,4.44))
oh_22$pred<-predict(mod_poll_inc,oh_22,interval='prediction')
                          
                          
                          $$
                          SeatShare=\beta_0+\beta_{1}SamePartyPres_i+\kappa_{im}SamePartyPres_i*MidtermYear_m+\\
                        \gamma_iSamePartyPres_i*PresidentialApproval+\rho_iSamePartyPres_i*GDPgrowthpct+\\\beta_2*genericballotpct+
                          \beta_3*lagseatshare
                        $$