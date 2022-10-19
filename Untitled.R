
inc<-read_csv("/Users/elliegrueskin/Documents/Fall 2022/Gov 1347/Gov1347/data/by_district/incumb_dist_1948-2020.csv")
inc_2<-inc %>% filter(year>2004) %>% 
  select(state,st_cd_fips, year, DemCandidate,RepCandidate,winner_party, RepVotes,DemVotes,RepStatus,DemStatus,RepVotesMajorPercent,DemVotesMajorPercent,president_party)%>%
  mutate(Uncontested=ifelse(DemVotesMajorPercent==0|RepVotesMajorPercent==0,1,0))

# Republican rows
inc_2_rep<-inc_2 %>% select(st_cd_fips, state,year, winner_party,president_party,Uncontested,RepCandidate, RepStatus,RepVotesMajorPercent,RepVotes)
# Dem rows
inc_2_dem<-inc_2 %>% select(st_cd_fips, state,year, winner_party,president_party,Uncontested,DemCandidate, DemStatus,DemVotesMajorPercent,DemVotes)
# Bind together
inc_2_total<-bind_rows(inc_2_rep,inc_2_dem)
inc_2_total<-inc_2_total %>%
  mutate(party=ifelse(is.na(RepVotesMajorPercent),'D','R'),
         CandidateName=ifelse(party=='R',RepCandidate,DemCandidate),
         Status=ifelse(party=='R',RepStatus,DemStatus),
         pv2p=ifelse(party=='R',RepVotesMajorPercent,DemVotesMajorPercent))%>%
  filter(pv2p!=0)%>%
  select(state,st_cd_fips, year,party,winner_party,president_party,Uncontested,
         party,CandidateName,Status,pv2p)%>%
  mutate(party_wins=ifelse(party==winner_party,1,0))



# Unemployment data
unemp<-read_csv('/Users/elliegrueskin/Documents/Fall 2022/Gov 1347/Gov1347/data/by_state/unemployment_state_monthly.csv')

unemp_2 <- unemp %>% 
  filter(Year>2004&Year<2020) %>% 
  filter(!('FIPS Code'%in% c(11,51000)|'State and area'=='Los Angeles County'))%>% 
  mutate(ElectionYear=ifelse(Year%%2,0,1))%>%
  filter(ElectionYear==1)%>%
  filter(Month==10)%>%
  rename('state'='State and area')

#Join datasets together
dat_total_minus_gen<-left_join(unemp_2,inc_2_total,by=c('Year'='year','state'))

# Generic polls
gen_poll<-read_csv('/Users/elliegrueskin/Documents/Fall 2022/Gov 1347/Gov1347/data/by_nation/polls_df.csv')
gen_poll_cl<-gen_poll %>% 
  filter(days_until_election<120)%>%
  group_by(year,party)%>%
  summarise(party_support=mean(support))

#Join datasets together
dat<-left_join(dat_total_minus_gen,gen_poll_cl,by=c('Year'='year','party'))%>%
  mutate(MidtermYear=ifelse(Year%%4,1,0)) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))


dat<-dat %>%
  group_by(CandidateName)%>%
  arrange(Year)%>%
  mutate(term=ifelse(Status=='Incumbent',cumsum(n()),0))%>%
  ungroup()

dat<-dat %>%
  group_by(st_cd_fips,party)%>%
  arrange(Year)%>%
  mutate(prev=lag(pv2p),Unc_prev=lag(Uncontested),
         approval_cent=party_support-44.6)%>%
  ungroup()%>%
  mutate(same_party=ifelse(party==president_party,1,0))

write.csv(dat,'inc.csv')
