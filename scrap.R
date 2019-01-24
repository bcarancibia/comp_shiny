library(tidyverse)
library(data.table)

dt<- fread(paste0("data/RESCONST-mf.csv"), skip=766)
dt_cat <- fread(paste0("data/RESCONST-mf.csv"), skip=1,nrows=8)

dt_dt  <- fread(paste0("data/RESCONST-mf.csv"), skip=13,nrows=3)

dt_et  <- fread(paste0("data/RESCONST-mf.csv"), skip=20,nrows=3)

dt_geo <- fread(paste0("data/RESCONST-mf.csv"), skip=27,nrows=5)

dt_per  <- fread(paste0("data/RESCONST-mf.csv"), skip=36,nrows=718)[, date:=seq.Date(from=as.Date("1959-01-01"), by="1 month",length.out=718)]

dt <- 
  merge(dt, dt_cat, by="cat_idx") %>%
  merge(dt_dt,by="dt_idx") %>% 
  left_join(dt_et, by="et_idx") %>% 
  merge(dt_geo,by="geo_idx") %>% 
  merge(dt_per,by="per_idx") %>% 
  data.table()

dt <- dt[ ,decade:= paste0(year(date) %/% 10, "0s")]

dt_decade <- dt [, list(vbar=mean(val,na.rm=T)), by = c("decade","cat_idx","cat_desc","dt_idx","dt_desc","et_idx","geo_idx","geo_code","geo_desc")][
  order(geo_idx,dt_idx,cat_idx,decade),
  ]



ggplot(data=dt_decade[dt_idx==1 & cat_idx==7 & geo_idx!=1 & et_idx==0,], aes(x=decade, y=vbar,fill=geo_desc))+
  geom_col(position = position_dodge(width = 0.75),width=0.95)+ 
  theme_minimal()+
  theme(legend.position="none",
        plot.caption=element_text(hjust=0))+
  scale_fill_viridis_d(option="C", end=0.85)+ facet_wrap(~geo_desc)+
  labs(y="",subtitle="Average annual rate for housing units completed (1000s, SAAR)",
       title="U.S. Housing completions in this decade well below historical average")



ggplot(data=dt_decade[dt_idx==1 & cat_idx==7 & geo_idx==1 & et_idx==0,], aes(x=decade, y=vbar,group=geo_desc, color=geo_desc))+
  geom_path(size=1.05)+geom_point(shape=21, fill="white",size=4,stroke=2)+
  theme_minimal()+
  #geom_col(position = position_dodge(width = 0.75),width=0.65)+  #facet_wrap(~geo_desc)+
  scale_color_viridis_d(option="C", end=0.85)+ facet_wrap(~geo_desc)+
  theme(legend.position="none",
        plot.caption=element_text(hjust=0))+
  labs(y="",subtitle="Average annual rate for housing units completed (1000s, SAAR)")

ggplot(data=dt_decade[dt_idx==1 & cat_idx==7 & geo_idx!=1 & et_idx==0,], aes(x=decade, y=vbar,group=geo_desc, color=geo_desc))+
  geom_path(size=1.05)+geom_point(shape=21, fill="white",size=3,stroke=1.5)+
  scale_color_viridis_d(option="C", end=0.85)+ facet_wrap(~geo_desc)+
  theme_minimal()+
  theme(legend.position="none",
        plot.caption=element_text(hjust=0))+
  labs(y="",subtitle="Average annual rate for housing units completed (1000s, SAAR)")


library(cowplot)
df <- left_join(dt[dt_idx==1 & cat_idx==4 & geo_idx==1 & et_idx==0 & year(date)>1959,],dt_decade, on=c("dt_idx","cat_idx","geo_idx","et_idx"))


myxy2<- function(dd, in.df=df){
  x<-filter(in.df,decade==dd)$val
  outdf<- data.frame(
    x=density(x)$x[which.max(density(x)$y)],  #find maximum density (in y dimension)
    y=max(density(x)$y,na.rm=T)
  )
}

df.text <- data.frame(decade=unique(df$decade)) %>% mutate(xy=map(decade,myxy2)) %>% unnest(xy) 
g.dens.plain <- 
  ggplot(data= df,
         aes(x=val, fill=decade,color=decade))+
  geom_density(alpha=0.25)+
  theme_minimal()+
  scale_y_continuous(breaks=NULL,sec.axis=dup_axis())+
  theme(legend.position="none",
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        plot.title=element_text(face="bold",hjust=0),
        plot.caption=element_text(hjust=0))+
  geom_text(data=df.text,aes(label=decade, x=x,y=y),size=8)+
  scale_color_viridis_d(option="C",end=0.9,direction=-1)+
  scale_fill_viridis_d(option="C",end=0.9,direction=-1)+
  labs(x="Monthly housing starts (1000s SAAR)",
       title="Estimated density over monthly values",y="")

myr0 <- function(x, a=0.02){
  geom_ribbon(alpha=a, color=NA, aes(ymin=0,ymax=min(x, val)))    
}

myr <- function(x, a=0.02){
  geom_ribbon(alpha=a, color=NA, aes(ymin=min(x, val )))    
}

g.line<-
  ggplot(data=df ,
         aes(x=date,y=val, ymax= val , fill=decade,color=decade))+
  geom_line()+
  geom_ribbon(alpha=0.5, color=NA, aes(ymin=vbar))+
  map(c(0,pull(df,val) %>% last()) %>% pretty(12), myr, a=0.01)+
  geom_hline(aes(yintercept=last(val)), linetype=2)+
  geom_line(aes(y=vbar),linetype=3)+
  geom_line(size=1.05)+
  theme_minimal(base_size=14)  +
  theme(legend.position="none",
        plot.caption=element_text(hjust=0))+
  scale_x_date(date_breaks="10 years",date_labels="%Y")+
  labs(x="date (monthly)", 
       y="", subtitle="U.S. Total Housing Starts (1000s, SAAR)\nLine monthly value, dark shaded area from decade average to monthly value",
       title="Housing starts stall")+
  scale_color_viridis_d(option="C",end=0.9,direction=-1)+
  scale_fill_viridis_d(option="C",end=0.9,direction=-1)



cowplot::plot_grid(g.line,g.dens.plain,ncol=1,rel_heights=c(3,2))