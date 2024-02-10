PVT AND KSS PLOTS
```{r, echo=FALSE}
### MAKING PLOTS OF PVT AND KSS RESULTS OVER TIME
pvt_Xlab<-c("Baseline","Day1. 02:27","Day2. 15:12","Day2. 03:42","Day3. 02:07","Day4. 22:56","Day5. 13:07","Day5. 01:36","Day 6. 04:28","Day 6. 08:22")
Q_Xlab<-c("Baseline","Day1. 22:15","Day1. 10:39","Day2. 22:39","Day2. 09:22","Day3. 21:54","Day3. 10:25","Day4. 18:40","Day4. 08:50","Day5. 21:23", "Day6. 12:01","Day6. 00:14", "Day6. 08:20")
p<-c("#F0E442","#000000")

PVT_plot <- ggplot(PVT_data,aes(x = Elapsed_Time, y = mean_rs1000))+
  geom_point(aes(x = Elapsed_Time, y = mean_rs1000,colour=Circadian_Phase), size = 3.5, shape = 20)+
  geom_line(aes(x = Elapsed_Time, y = mean_rs1000,group=1), linetype = 2)+
  geom_vline(xintercept=c(42.620, 85.950, 129.300), size=1.5)+
  #scale_x_discrete(labels= pvt_Xlab)+
  scale_colour_manual(values=palette)+
  ylab("Response Speed")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 90, size =8),axis.text.y =element_text(size=8),legend.text=element_text(size=8),legend.title=element_text(size=0), axis.title.x=element_text(), axis.title.y=element_text(size=8, face="bold"))+
  scale_x_continuous(breaks=PVT_data$Elapsed_Time)+
  labs(color="Circadian Phase")
#PVT_plot

lapses_plot <- ggplot(PVT_data,aes(x = Elapsed_Time, y =SUM_lapse))+
  geom_point(aes(x =Elapsed_Time, y =SUM_lapse, colour=Circadian_Phase), size = 3.5, shape = 20)+
  geom_line(aes(x =Elapsed_Time, y = as.numeric(SUM_lapse),group=1), linetype = 2)+
  geom_vline(xintercept=c(42.620, 85.950, 129.300), size=1.5)+
  scale_x_continuous(breaks=PVT_data$Elapsed_Time)+
  scale_y_continuous(breaks=c(2,4,6,8,10,12,14,16,18))+
  scale_colour_manual(values=palette)+
  theme(legend.position = "none",axis.text.x = element_text(angle = 90, size=8),axis.text.y=element_text(size=8),axis.title.x=element_text(size=8, face="bold"), axis.title.y=element_text(size=8, face="bold"))+
  xlab("Elapsed Time (Hrs)")+
  ylab("Number of lapses")
#lapses_plot

slowest_plot <- ggplot(PVT_data,aes(x = Elapsed_Time, y = mean_slowestrs))+
  geom_point(aes(x =Elapsed_Time, y = mean_slowestrs, colour=Circadian_Phase), size = 3.5, shape = 20)+
  geom_line(aes(x =Elapsed_Time, y = as.numeric(mean_slowestrs),group=1), linetype = 2)+
  scale_x_discrete(labels= pvt_Xlab)+
  scale_colour_manual(values=palette)+
  theme(legend.position = "none")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 90))+
  ylab("Slowest Responses")
#slowest_plot

rt_plot<-ggplot(PVT_data,aes(x = Elapsed_Time, y = mean_rt))+
  geom_point(aes(x =Elapsed_Time, y = mean_rt, colour=Circadian_Phase), size = 3.5, shape = 20)+
  geom_line(aes(x =Elapsed_Time, y = as.numeric(mean_rt)), linetype = 2, group=1)+
  scale_x_discrete(labels= pvt_Xlab)+
  scale_colour_manual(values=palette)+
  theme(legend.position = "none")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 90))+
  ylab("Reaction Time")
rt_plot

#pvtplotall<-ggarrange(PVT_plot,slowest_plot,lapses_plot,rt_plot, labels=c("A","B","C","D"),common.legend = TRUE, legend="bottom")
pvtplot_rslapse<-ggarrange(PVT_plot,lapses_plot,ncol=1,nrow=2, labels=c("A","B"),font.label=list(size=5),common.legend = TRUE, legend="bottom")



KSS_plot <- ggplot(KSS_data,aes(x = Elapsed_Time, y = KSS))+
  geom_point(aes(x =Elapsed_Time, y = KSS, colour=PHASE), size = 3.5, shape = 20)+
  geom_line(aes(x =Elapsed_Time, y = as.numeric(KSS),group=1), linetype = 2)+
  geom_vline(xintercept=c(42.620, 85.950, 129.300), size=1.5)+
  scale_x_continuous(breaks=KSS_data$Elapsed_Time)+
  scale_colour_manual(values=palette)+
  ylab("Sleepiness")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 90,size=8),legend.text=element_text(size=8),legend.title=element_text(size=0),axis.text.y=element_text(size=8),axis.title.x=element_text(), axis.title.y=element_text(size=8, face="bold"))+
  labs(color="Circadian Phase")
#KSS_plot

PANAS_plot <-ggplot() + 
  geom_line(data = KSS_data, aes(x =Elapsed_Time, y = as.numeric(POSITIVE_AFFECT),group=1), color = "red", size=1.25)+
  geom_point(data= KSS_data,aes(x =Elapsed_Time, y = POSITIVE_AFFECT, colour=PHASE), size = 3.5, shape = 20)+
  geom_line(data = KSS_data, aes(x =Elapsed_Time, y = as.numeric(NEGATIVE_AFFECT),group=1), color = "blue", size=1.25)+
  geom_point(data= KSS_data,aes(x =Elapsed_Time, y = NEGATIVE_AFFECT, colour=PHASE), size = 3.5, shape = 20)+
  geom_vline(xintercept=c(42.620, 85.950, 129.300), size=1.5)+
  scale_x_continuous(breaks=KSS_data$Elapsed_Time)+
  scale_colour_manual(values=palette)+
  ylab("Affective Score")+
  theme(axis.text.x = element_text(angle = 90,size=8),legend.text=element_text(size=5),legend.title=element_text(size=0),axis.text.y=element_text(size=8),axis.title.x=element_text(size=8, face="bold"), axis.title.y=element_text(size=8, face="bold"))+
  xlab("Elapsed Time (Hrs)")
#PANAS_plot


KSS_PANAS_plot<-ggarrange(KSS_plot, PANAS_plot,labels=c("A","B"),font.label=list(size=5), ncol=1,nrow=2,common.legend = TRUE, legend="bottom")
KSS_PANAS_plot
pvtplot_rslapse

```