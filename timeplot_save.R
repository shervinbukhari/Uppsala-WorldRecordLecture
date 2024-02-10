Make a timeline plot
```{r, timelineplot, include=FALSE, echo=FALSE}

timeline$Positions<-as.numeric(timeline$Positions)
event_types <- c("Photograph", "KSS & PANAS", "PVT", "105 min. rest")
event_colors <- c("#1B9E77", "#D95F02", "#7570B3", "#000000")
time_buffer <- 2

time_range <- round(seq(-2, max(145), length.out=38)) # I want to start my timeline at -2 and end at 145 hours, in 38 steps and make round to even numbers.

#month_format <- format(month_date_range, '%b')
#month_df <- data.frame(month_date_range, month_format)





timeline_plot<-ggplot(timeline, aes(x=Time,y=0, col=Event, label=Event, group=Event))+
  geom_hline(yintercept=0, color = "black", size=0.5)+
  geom_segment(data=timeline, aes(y=Positions,yend=0,xend=Time), color='black', size=0.2)+
  geom_point(aes(y=0), size=8)+
  theme_classic()+
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(),
        legend.position = "bottom",legend.text=element_text(size=8))+
  #legend(legend=sapply(event_types,as.expression),fill=event_color)
  scale_color_manual(values=event_colors, labels=event_types, drop = FALSE)+
  geom_text(aes(y=Positions, label=Event),size=8)+
  geom_text(aes(x=time_range,y=-0.1, label=as.factor(time_range)),size=8,vjust=0.5, color='black', angle=90)
timeline_plot



#timeline_plot<-timeline_plot+geom_text(data=year_df, aes(x=year_date_range,y=-0.2,label=year_format, fontface="bold"),size=2.5, color='black')
# Show text for each milestone
#timeline_plot<-timeline_plot+geom_text(aes(y=text_position,label=milestone),size=2.5)
#print(timeline_plot)

#geom_text(data=timeline, aes(x=Time,y=-0.1,label=month_format),size=2.5,vjust=0.5, color='black', angle=90)+



```   