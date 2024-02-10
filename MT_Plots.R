#PLOTS MT PVT,KSS and PANAS#
MT_PVT<-read_excel("C:/Users/SHERVIN/Documents/Sleep Science Lab/Project Mark/MTPVT_res.xlsx")
MT_KSSPANAS<- read_excel("C:/Users/SHERVIN/Documents/Sleep Science Lab/Project Mark/KKS_PANAS_MT.xlsx")
MT_PVT$Session=factor(MT_PVT$Session)
MT_PVT$Circadian_Phase=factor(MT_PVT$Circadian_Phase)
MT_PVT$Day=factor(MT_PVT$Day)

MT_KSSPANAS$Session=factor(MT_KSSPANAS$Session)
MT_KSSPANAS$PHASE=factor(MT_KSSPANAS$PHASE)
MT_KSSPANAS$Day=factor(MT_KSSPANAS$Day)

pvt_Xlab<-c("Baseline","Day1. 02:27","Day2. 15:12","Day2. 03:42","Day3. 02:07","Day4. 22:56","Day5. 13:07","Day5. 01:36","Day 6. 04:28","Day 6. 08:22")
Q_Xlab<-c("Baseline","Day1. 22:15","Day1. 10:39","Day2. 22:39","Day2. 09:22","Day3. 21:54","Day3. 10:25","Day4. 18:40","Day4. 08:50","Day5. 21:23", "Day6. 12:01","Day6. 00:14", "Day6. 08:20")
p<-c("#F0E442","#000000")

PVT_plot <- ggplot(MT_PVT,aes(x = Session, y = mean_rs))+
geom_point(aes(x = Session, y = mean_rs, colour=Circadian_Phase), size = 3.5, shape = 20)+
geom_line(aes(x = Session, y = mean_rs,group=1), linetype = 2)+
scale_x_discrete(labels= pvt_Xlab)+
scale_colour_manual(values=p)+
ylab("Response Speed")+
xlab("")+
theme(axis.text.x = element_text(angle = 90),legend.text=element_text(size=10),legend.title=element_text(size=10))+
labs(color="Circadian Phase")
PVT_plot

lapses_plot <- ggplot(MT_PVT,aes(x = Session, y =SUM_lapse))+
  geom_point(aes(x =Session, y =SUM_lapse, colour=Circadian_Phase), size = 3.5, shape = 20)+
  geom_line(aes(x =Session, y = as.numeric(SUM_lapse),group=1), linetype = 2)+
  scale_x_discrete(labels= pvt_Xlab)+
  scale_colour_manual(values=p)+
  theme(legend.position = "none")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 90))+
  ylab("Lapse count")
lapses_plot

slowest_plot <- ggplot(MT_PVT,aes(x = Session, y = mean_slowestrs))+
  geom_point(aes(x =Session, y = mean_slowestrs, colour=Circadian_Phase), size = 3.5, shape = 20)+
  geom_line(aes(x =Session, y = as.numeric(mean_slowestrs),group=1), linetype = 2)+
  scale_x_discrete(labels= pvt_Xlab)+
  scale_colour_manual(values=p)+
  theme(legend.position = "none")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 90))+
  ylab("Slowest Responses")
slowest_plot

rt_plot<-ggplot(MT_PVT,aes(x = Session, y = mean_rt))+
  geom_point(aes(x =Session, y = mean_rt, colour=Circadian_Phase), size = 3.5, shape = 20)+
  geom_line(aes(x =Session, y = as.numeric(mean_rt)), linetype = 2, group=1)+
  scale_x_discrete(labels= pvt_Xlab)+
  scale_colour_manual(values=p)+
  theme(legend.position = "none")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 90))+
  ylab("Reaction Time")
rt_plot

ggarrange(PVT_plot,slowest_plot,lapses_plot,rt_plot, labels=c("A","B","C","D"),common.legend = TRUE, legend="bottom")

KSS_plot <- ggplot(MT_KSSPANAS,aes(x = Session, y = KSS))+
geom_point(aes(x =Session, y = KSS, colour=PHASE), size = 3.5, shape = 20)+
geom_line(aes(x =Session, y = as.numeric(KSS),group=1), linetype = 2)+
scale_colour_manual(values=p)+
ylab("Sleepiness")+
xlab("")+
scale_x_discrete(labels=NULL)+
theme(axis.text.x = element_text(angle = 90),legend.text=element_text(size=10),legend.title=element_text(size=10))+
labs(color="Circadian Phase")
KSS_plot

PANAS_plot <-ggplot() + 
  geom_line(data = MT_KSSPANAS, aes(x =Session, y = as.numeric(POSITIVE_AFFECT),group=1), color = "red", size=1.25)+
  geom_point(data= MT_KSSPANAS,aes(x =Session, y = POSITIVE_AFFECT, colour=PHASE), size = 3.5, shape = 20)+
  geom_line(data = MT_KSSPANAS, aes(x =Session, y = as.numeric(NEGATIVE_AFFECT),group=1), color = "blue", size=1.25)+
  geom_point(data= MT_KSSPANAS,aes(x =Session, y = NEGATIVE_AFFECT, colour=PHASE), size = 3.5, shape = 20)+
scale_colour_manual(values=p)+
ylab("Affective Score")+
scale_x_discrete(labels= Q_Xlab)+
theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position = "none")+
xlab("")
PANAS_plot


ggarrange(KSS_plot, PANAS_plot,labels=c("A","B"),ncol=1,nrow=2,common.legend = TRUE, legend="bottom")