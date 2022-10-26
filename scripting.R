
library(stargazer)

inc<-read_csv("/Users/elliegrueskin/Documents/Fall 2022/Gov 1347/Gov1347/data/by_district/inc.csv")

inc_2_total<-inc %>%
mutate(party_wins=ifelse(party==winner_party,1,0),
       incumbent=ifelse(Status=='Incumbent',1,0),
       district_num=substr(st_cd_fips,3,4))%>%
  rename('year'='Year')

cands_22<-read_csv("/Users/elliegrueskin/Documents/Fall 2022/Gov 1347/Gov1347/data/by_district/house_cands.csv")%>%
  mutate(year=2022,president_party='D')%>%mutate(dist_numeric=as.numeric(district),
                                                 district_num=case_when(
                                                   dist_numeric<10~paste0('0',district),
                                                   is.na(dist_numeric)~'00',
                                                   TRUE~district
                                                 ))%>%
  rename(CandidateName=cand,party=cand_party)%>%
  filter(party %in% c('Democratic','Republican'))%>%
  mutate(party=substr(party,1,1))%>%
  dplyr::select(-c(district,dist_numeric))%>%
  dplyr::distinct(CandidateName, .keep_all = TRUE)

exp<-read_csv("/Users/elliegrueskin/Documents/Fall 2022/Gov 1347/Gov1347/data/by_district/expert_rating.csv")%>%
  mutate(dist_numeric=as.numeric(district),
         district_num=case_when(
           dist_numeric<10~paste0('0',district),
           is.na(dist_numeric)~'00',
           TRUE~district))%>%
  dplyr::select(c(year,state,district_num,cook))%>% filter(year!=2022)

cands_22_ <- cands_22 %>% group_by(state,district_num)%>%
  mutate(n=n(),Uncontested=ifelse(n>1,0,1))%>%
  ungroup()

cand<-bind_rows(inc_2_total,cands_22_)

cand_fin<-cand %>%
  group_by(state,district_num,party)%>%
  arrange(year)%>%
  mutate(prev=lag(pv2p),Unc_prev=lag(Uncontested))%>%
  ungroup()
library(MASS)
library(ciTools)
lm1<-glm(party_wins~prev+same_party*MidtermYear+party_support+party,data=cand_fin,family='binomial')
cand_22<-cand_fin %>% filter(year==2022)%>%
  mutate(same_party=ifelse(party=='D',1,0),
         MidtermYear=1,
         party_support=ifelse(party=='D',44.7,45.4))
cand_22$pred<-predict(lm1,cand_22,type='response',interval='prediction')
cand_fin$pred<-predict(lm1,cand_fin)
cand_22 %>% dplyr::select(c('state','district_num','prev','pred')) %>% view()

lm1<-lm(pv2p~prev+same_party*MidtermYear+party_support+party+Unc_prev,data=cand_fin%>% filter(Uncontested==0))
c

# Nationwide

inc<-read_csv("/Users/elliegrueskin/Documents/Fall 2022/Gov 1347/Gov1347/data/by_nation/inc_pop_vote_df.csv")%>%
  mutate(MidtermYear=ifelse(year%%4,1,0)) 
gen_poll<-read_csv("/Users/elliegrueskin/Documents/Fall 2022/Gov 1347/Gov1347/data/by_nation/GenericPolls1942_2020.csv")%>%
  filter(days_until_election<52&year!=2008)%>%
  group_by(year)%>%
  summarise(D=mean(dem),R=mean(rep))%>%
  pivot_longer(cols=c('D','R'),names_to='party',values_to='poll_pct')
gen_poll_2<-read_csv("/Users/elliegrueskin/Documents/Fall 2022/Gov 1347/Gov1347/data/by_nation/GenericPolls1942_2020.csv")%>%
  filter(days_until_election<52&year==2008)%>%
  group_by(year)%>%
  summarise(D=mean(rep),R=mean(dem))%>%
  pivot_longer(cols=c('D','R'),names_to='party',values_to='poll_pct')
gen_poll<-bind_rows(gen_poll,gen_poll_2)
gdp<-read_csv("/Users/elliegrueskin/Documents/Fall 2022/Gov 1347/Gov1347/data/by_nation/GDP_quarterly.csv")%>%
  mutate(Eyr=ifelse(year%%2,0,1)) %>%
  filter(Eyr==1&quarter_yr==3)%>%
  dplyr::select(year,GDP_growth_pct)
library(stringr)
pres<-read_csv("/Users/elliegrueskin/Documents/Fall 2022/Gov 1347/Gov1347/data/by_nation/pres_approval_gallup_1941-2022.csv")%>%
  group_by(year)%>%
  summarise(approval=mean(approve))
inc_gen<-inner_join(inc,gen_poll,by=c('year','party'))
all<-inner_join(inc_gen,gdp,by='year')
all<-inner_join(all,pres,by='year')

all_2<-all %>% 
  arrange(party,year)%>%
  dplyr::mutate(Pres_inc_party=case_when(year==1948~'NA',TRUE~dplyr::lag(president_party,  n=1)))%>%
  dplyr::mutate(Inc_party_pres=ifelse(Pres_inc_party==party,1,0),
         Inc_party_house=ifelse(H_incumbent_party==party,1,0),
         lag_seats=lag(seats),
         lag_pv=lag(majorvote_pct))%>% filter(year!=1948)

ggplot(data=all_2%>% filter(MidtermYear==1),aes(x=approval,y=majorvote_pct))+
  geom_point()+facet_wrap(~Inc_party_pres)+geom_smooth(method='lm')
ggplot(data=all_2%>% filter(MidtermYear==1),aes(x=GDP_growth_pct,y=majorvote_pct))+
  geom_point()+facet_wrap(~Inc_party_pres)+geom_smooth(method='lm')

split1<- sample(c(rep(0, ceiling(0.7 * nrow(all_2))), rep(1, 0.3 * nrow(all_2))))
train <- all_2[split1 == 0, ]  
test <- all_2[split1 == 1, ]  %>%
  add_row(year = 2022, party = 'D',GDP_growth_pct=.6,poll_pct=44.7,approval=41.7,
          Inc_party_pres=1,Inc_party_house=1,MidtermYear=1)%>%
  add_row(year = 2022, party = 'R',GDP_growth_pct=.6,poll_pct=45.3,approval=41.7,
          Inc_party_pres=0,Inc_party_house=0,MidtermYear=1)

train <- all_2 %>% dplyr::sample_frac(0.70)
test  <- dplyr::anti_join(all_2, train, by = c('year','party'))%>%
  add_row(year = 2022, party = 'D',GDP_growth_pct=.6,poll_pct=44.7,approval=41.7,
          Inc_party_pres=1,Inc_party_house=1,MidtermYear=1,lag_seats=222,lag_pv=51.5)%>%
  add_row(year = 2022, party = 'R',GDP_growth_pct=.6,poll_pct=45.3,approval=41.7,
          Inc_party_pres=0,Inc_party_house=0,MidtermYear=1,lag_seats=212,lag_pv=48.5)

lm1<-lm(majorvote_pct~Inc_party_house+Inc_party_pres*MidtermYear+approval:Inc_party_pres+poll_pct,data=train)
lm1<-lm(seats~Inc_party_house+GDP_growth_pct:Inc_party_pres+Inc_party_pres:MidtermYear+approval*Inc_party_pres+poll_pct,data=train)
lm1<-lm(seats~Inc_party_house+Inc_party_pres*(MidtermYear+approval+GDP_growth_pct)+poll_pct,data=train)
lm1<-lm(majorvote_pct~Inc_party_house+Inc_party_pres*(MidtermYear+approval+GDP_growth_pct:Inc_party_pres)+poll_pct,data=train)
lm1<-lm(seats~Inc_party_pres*(MidtermYear+approval+GDP_growth_pct)+poll_pct+lag_seats,data=train)
lm1<-lm(majorvote_pct~Inc_party_pres*(MidtermYear+approval+GDP_growth_pct)+poll_pct+lag_pv,data=train)
lm1<-lm(majorvote_pct~Inc_party_pres*(MidtermYear+approval+GDP_growth_pct)+poll_pct,data=train)

test$pred<-predict(lm1,test,interval='prediction')
ggplot(test,aes(x=majorvote_pct,pred[,"fit"],label=year))+geom_point()+geom_smooth(method=lm)+geom_text()+facet_wrap(~MidtermYear)+geom_abline(slope=1)
