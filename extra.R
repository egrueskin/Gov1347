```{r, include=FALSE}

unemp<-read.csv("data/unemployment_national_quarterly_final.csv")
house_vote<-read.csv("data/house_popvote_seats.csv")
RDI<-read.csv("data/RDI_quarterly.csv")%>% filter(quarter_cycle==7)%>%
  mutate(diff=(DSPIC_qt-lag(DSPIC_qt))/lag(DSPIC_qt))
CPI<-read.csv("data/CPI_monthly.csv")%>% mutate(date=as_date(DATE))%>%
  filter(month(date)==10&year(date)%%2)

unemp_2<-unemp %>% 
  filter(quarter_cycle==3)
dat<-inner_join(unemp_2,house_vote,by='year')
dat<-inner_join(dat,RDI,by='year')

dat<-dat%>%
  mutate(R_seatshare=R_seats/(R_seats+D_seats),
         D_seatshare=D_seats/(R_seats+D_seats))%>%
  mutate(Incumbent_vote=ifelse(H_incumbent_party=='R',R_majorvote_pct,D_majorvote_pct),
         Incumbent_share=ifelse(H_incumbent_party=='R',R_seatshare,D_seatshare))

ggplot(data=dat,aes(x=diff,y=H_incumbent_party_majorvote_pct,col=H_incumbent_party))+geom_point()+facet_wrap(~H_incumbent_party)
ggplot(data=dat,aes(x=UNRATE,y=Incumbent_share))+geom_point()+facet_wrap(~H_incumbent_party)

lm1<-lm(H_incumbent_party_majorvote_pct~DSPIC_change_pct+factor(H_incumbent_party),data=dat)

```