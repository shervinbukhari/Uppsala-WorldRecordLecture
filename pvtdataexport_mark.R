library("writexl")

MTdat <- read_excel("/home/shervin/Documents/GitStuff/Uppsala_MTfacestudy/PVTdata_MARK.xls")
MTdat <- na.omit(MTdat)
#LAPSES
MTsum_Lapses <- MTdat  %>% group_by(Session) %>%summarise(Olapse=sum(Olapse), Clapse=sum(Clapse))
write_xlsx(MTsum_Lapses,"/home/shervin/Documents/GitStuff/Uppsala_MTfacestudy/Mark_Lapses.xlsx")

#CALCULATE RS Without invalid trials
MTdat_rs <- MTdat %>% filter(Clapse=="0") #remove over below 100 ms
MTdat_rs <-MTdat_rs %>% filter(Reaction_Time<1000) #keep lapses below
MTdat_rs <- MTdat_rs %>% group_by(Session) %>% summarise(mean_rt=mean(Reaction_Time),mean_rs=mean(response_speed),mean_slowestrs=mean(response_speed[response_speed>=quantile(response_speed, 0.9)]))
MTdat_var <- MTdat %>% filter(MTdat$Olapse=="0" & Clapse=="0")
MTdat_var <- MTdat_var %>% group_by(Session) %>% summarise(RTvariance=var(Reaction_Time))

write_xlsx(MTdat_rs,"/home/shervin/Documents/GitStuff/Uppsala_MTfacestudy/Mark_rsover1000.xlsx")




##
PVT <- read_excel("GitStuff/mswake/SpreadsheetsnJASP/PVT/PVT_DATA_onlyvalid.xlsx")
PVT <- PVT %>% group_by(Time,Condition,G) %>% summarise(mean_rt=mean(rt),se_rt=std.error(rt), mean_rs=mean(rs), se_rs=std.error(rs), mean_rsslowest=mean(rs[rs>=quantile(rs, 0.9)]),se_slowest=std.error(rs[rs>=quantile(rs, 0.9)]))
write_xlsx(PVT,"GitStuff/mswake/SpreadsheetsnJASP/PVT/mean_RT_rs_slowest.xlsx")

PVT <- read_excel("GitStuff/mswake/SpreadsheetsnJASP/PVT/PVT_DATA_all_nofalsestarts.xlsx")
PVT <- PVT %>% group_by(Time,Condition,G) %>% summarise(mean_rs=mean(rs),se_rs=std.error(rs), mean_slowestrs=mean(rs[rs>=quantile(rs, 0.9)]),se_slowest=std.error(rs[rs>=quantile(rs, 0.9)]))
write_xlsx(PVT,"GitStuff/mswake/SpreadsheetsnJASP/PVT/meanrs_basner.xlsx")

##

