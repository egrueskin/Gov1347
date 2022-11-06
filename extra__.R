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
                        
                        
                        
                        $$
                          VoteShare=\beta_0+\beta_{1}SamePartyPres_i+\kappa_{im}SamePartyPres_i*MidtermYear_m+\\
                        \gamma_iSamePartyPres_i*PresidentialApproval+\rho_iSamePartyPres_i*GDPgrowthpct+\\\beta_2*genericballotpct+
                          \beta_3*lagvoteshare
                        $$
                          
                          
                          $$
                          SeatShare=\beta_0+\beta_{1}SamePartyPres_i+\kappa_{im}SamePartyPres_i*MidtermYear_m+\\
                        \gamma_iSamePartyPres_i*PresidentialApproval+\rho_iSamePartyPres_i*GDPgrowthpct+\\\beta_2*genericballotpct+
                          \beta_3*lagseatshare
                        $$