# Helper functions

## Table of summary data
sum_tab = function(x, save){
  tab = data.frame(
    "Med"=round(as.numeric(summary(x)[3]),1),
    "IQR"=paste("(",round(as.numeric(summary(x)[2]),1), ",",round(as.numeric(summary(x)[5]),1),")"),
    "Min"=round(as.numeric(summary(x)[1]),1),
    "Max"=round(as.numeric(summary(x)[6]),1),
    "Missing"=round(as.numeric(summary(x)[7]),1)
  )
  tab = flextable(tab)
  tab = set_header_labels(tab, 
                          Med="Median", 
                          IQR="Interquartile range",
                          Min="Minimum",
                          Max="Maximum")
  plt = ggplot()+
    gen_grob(tab)+
    plot_layout(nrow=2, heights=c(0,1))
  if (!is_missing(save)){
    ggsave(save, plt, dpi=480)
  }
  plt
}

## Histogram and table

hist_age = function(ages, xlab, ylab, save){
  tab_vals = cut(ages, c(0,51,61,71,81,100), right=F)
  levels(tab_vals)=c(
    "0 to less than 51",
    "51 to less than 61",
    "61 to less than 71",
    "71 to less than 81",
    "81 and over"
  )
  tab_vals = data.frame("vals"=tab_vals)
  tab_vals = proc_freq(tab_vals, "vals")
  tab_vals = set_header_labels(tab_vals, vals=xlab, count=ylab)
  hist_vals = data.frame('vals'=ages)
  plt = ggplot(hist_vals, aes(x=vals))+
    ylab(ylab)+
    xlab(xlab)+
    geom_histogram()+
    gen_grob(tab_vals)+
    plot_layout(nrow=2, heights=c(2,1))
  if (!is_missing(save)){
    ggsave(save, plt, dpi=480)
  }
  plt
}


## Bar and table

bar_tab = function(vals, xlab, ylab, save){
  if (!is.factor(vals)){
    vals = factor(vals)
  }
  tab = data.frame("vals" = vals)
  tab = proc_freq(tab, "vals")
  tab = set_header_labels(tab, vals=xlab, Count=ylab)
  df = data.frame("vals"=tab$body$dataset$vals, "freq"=tab$body$dataset$count)
  df = df[df$vals!="Total",]
  plt = ggplot(data=df, aes(x=vals, y=freq))+
    geom_bar(stat="identity", fill=(1:length(unique(vals)))+1)+
    ylab(ylab)+
    xlab(xlab)+
    scale_x_discrete(labels=function(x) str_wrap(x,width=10))+
    gen_grob(tab)+
    plot_layout(nrow=2, heights=c(2,1))
  if (!is_missing(save)){
    ggsave(save, plt, dpi=480)
  }
  plt
}

bar_tab2 = function(vals1, vals2, xlab, ylab, flab, missing=TRUE, save){
  set_flextable_defaults(na_str="0", nan_str="0") 
  if (!is.factor(vals1)){
    vals1 = factor(vals1)
  }
  if (!is.factor(vals2)){
    vals1 = factor(vals2)
  }
  tab = data.frame(x=vals1, y=vals2)
  colnames(tab)=c(xlab, flab)
  tab = proc_freq(tab, row=xlab, col=flab, 
                  include.row_percent = F, 
                  include.column_percent = F, 
                  include.table_percent = F)
  if (missing){
    bar_x = rep(c(levels(vals1), "Missing"), each=length(unique(vals2)))
    tab2 = table(vals1, vals2, useNA = 'ifany')
    bar_y = as.numeric(t(tab2))
    bar_f = rep(c(levels(vals2),"Missing"), times=length(unique(vals1)))
    dat=data.frame("vals1"=factor(bar_x, levels=c(levels(vals1), "Missing")),
                   "vals2"=factor(bar_f, levels=c(levels(vals2), "Missing")),
                   "freq"=bar_y)
  }
  else {
    bar_x = rep(levels(vals1), each=length(unique(vals2)))
    tab2 = table(vals1, vals2, useNA = 'ifany')
    bar_y = as.numeric(t(tab2))
    bar_f = rep(levels(vals2), times=length(unique(vals1)))
    dat=data.frame("vals1"=factor(bar_x, levels=levels(vals1)),
                   "vals2"=factor(bar_f, levels=levels(vals2)),
                   "freq"=bar_y)
  }

  
  plt = ggplot(dat, aes(fill=vals2, y=freq, x=vals1))+
    geom_bar(position = "dodge", stat="identity")+
    labs(x=xlab, y=ylab, fill=flab)+
    scale_x_discrete(labels=function(x) str_wrap(x,width=10))+
    gen_grob(tab)+
    plot_layout(nrow=2, heights=c(2,1))
  if (!is_missing(save)){
    ggsave(save, plt, dpi=480)
  }
  plt
}

## Assistance use

assist_use = function(dataset, time, save){
  time = as.character(time)
  Use_levels = c(
    "Have not tried it",
    "It helped and I use it always",
    "It helped and I use it sometimes",
    "It helped, but I am not using it now",
    "Tried it, but it was not helpful",
    "Missing"
  )
  Pill = count(dataset, get(paste("Pill.",time, sep="")), .drop=F)$n
  Muse = count(dataset, get(paste("Muse.",time, sep="")),  .drop=F)$n
  Injection=count(dataset, get(paste("PIT.",time, sep="")),  .drop=F)$n
  Vacuum=count(dataset, get(paste("Vac.",time, sep="")),  .drop=F)$n
  Other=count(dataset, get(paste("Other.",time, sep="")),  .drop=F)$n
  df = data.frame(
    Use=Use_levels,
    Pill=Pill,
    Muse=Muse,
    Injection=Injection,
    Vacuum=Vacuum,
    Other=Other
  )
  tab = flextable(df)
  Assistance = c(rep("Pill", 6), 
                 rep("Muse", 6), 
                 rep("Injection", 6),
                 rep("Vacuum", 6), 
                 rep("Other", 6))
  Use = rep(Use_levels, 5)
  Freq = c(Pill, Muse, Injection, Vacuum, Other)
  data = data.frame("Use"=factor(Use, Use_levels), 
                    "Assistance"=factor(Assistance, levels=tab$header$col_keys[2:6]), 
                    "Freq"=Freq)
  plt = ggplot(data, aes(fill=Use, y=Freq, x=Assistance))+
    geom_bar(position = "dodge", stat="identity")+
    ylab("Number of participants")+
    gen_grob(tab)+
    plot_layout(nrow=2, heights=c(2,1))
  if (!is_missing(save)){
    ggsave(save, plt, dpi=480)
  }
  plt
}

## Spaghetti plots

spaghetti = function(df, x, y, group, xlab="Time", ylab="Score", save){
  plt = ggplot(data = df, aes(x = x, y = y, group = group))+
    geom_line(colour="grey")+
    stat_summary(aes(group=1), 
                 fun=median, 
                 fun.min=function(z) {quantile(z,0.25)},
                 fun.max=function(z) {quantile(z,0.75)},
                 lwd=1.2
    )+
    stat_summary(aes(group=1), 
                 fun=median, 
                 fun.min=function(z) {quantile(z,0.25)},
                 fun.max=function(z) {quantile(z,0.75)},
                 geom='line', lwd=1.2
    )+
    scale_x_continuous(breaks = seq(0, 12, by = 3))+
    ylim(0,100)+
    xlab(xlab)+
    ylab(ylab)
  if (!is_missing(save)){
    ggsave(save, plt, dpi=480)
  }
  plt
}

# Binconf in a table

prop_conf = function(x, response, complement=F){
  if (complement){
    flextable(data.frame(binconf(sum(!(x %in% response)), length(x))))
  }
  else {
    flextable(data.frame(binconf(sum(x %in% response), length(x))))
  }
}

prop_conf2 = function(x1, x2, response1, response2, complement=F){
  if (complement){
    flextable(data.frame(binconf(sum((x1 %in% response1) | (x2 %in% response2)), length(x1))))
  }
  else {
    flextable(data.frame(binconf(sum(x1 %in% response1 & x2 %in% response2), length(x1))))
  }
}

# Function spaghetti plots

Uspaghetti = function(all_tmp, save){
  l0 = binconf(length(all_tmp$t0L[all_tmp$t0L==100]),length(all_tmp$t0L))
  l1 = binconf(length(all_tmp$t1L[all_tmp$t1L==100]),length(all_tmp$t0L))
  l3 = binconf(length(all_tmp$t3L[all_tmp$t3L==100]),length(all_tmp$t0L))
  l6 = binconf(length(all_tmp$t6L[all_tmp$t6L==100]),length(all_tmp$t0L))
  l12 = binconf(length(all_tmp$t12L[all_tmp$t12L==100]),length(all_tmp$t0L))
  df_leak = data.frame("Time"=c(0,1,3,6,12),
                       "Proportion"=c(l0[1],l1[1],l3[1],l6[1],l12[1]))
  leak_low = data.frame("Time"=c(0,1,3,6,12), 
                        "Score"=c(l0[2],l1[2],l3[2],l6[2],l12[2]))
  leak_upp = data.frame("Time"=c(0,1,3,6,12), 
                        "Score"=c(l0[3],l1[3],l3[3],l6[3],l12[3]))
  
  
  l0 = binconf(length(all_tmp$t0P[all_tmp$t0P==100]),length(all_tmp$t0L))
  l1 = binconf(length(all_tmp$t1P[all_tmp$t1P==100]),length(all_tmp$t0L))
  l3 = binconf(length(all_tmp$t3P[all_tmp$t3P==100]),length(all_tmp$t0L))
  l6 = binconf(length(all_tmp$t6P[all_tmp$t6P==100]),length(all_tmp$t0L))
  l12 = binconf(length(all_tmp$t12P[all_tmp$t12P==100]),length(all_tmp$t0L))
  df_pad = data.frame("Time"=c(0,1,3,6,12),
                      "Proportion"=c(l0[1],l1[1],l3[1],l6[1],l12[1]))
  pad_low = data.frame("Time"=c(0,1,3,6,12), 
                       "Score"=c(l0[2],l1[2],l3[2],l6[2],l12[2]))
  pad_upp = data.frame("Time"=c(0,1,3,6,12), 
                       "Score"=c(l0[3],l1[3],l3[3],l6[3],l12[3]))
  
  l0 = binconf(length(all_tmp$t0L[all_tmp$t0L==100 & all_tmp$t0P==100]),length(all_tmp$t0L))
  l1 = binconf(length(all_tmp$t1L[all_tmp$t1L==100 & all_tmp$t1P==100]),length(all_tmp$t0L))
  l3 = binconf(length(all_tmp$t3L[all_tmp$t3L==100 & all_tmp$t3P==100]),length(all_tmp$t0L))
  l6 = binconf(length(all_tmp$t6L[all_tmp$t6L==100 & all_tmp$t6P==100]),length(all_tmp$t0L))
  l12 = binconf(length(all_tmp$t12L[all_tmp$t12L==100 & all_tmp$t12P==100]),length(all_tmp$t0L))
  df_leakpad = data.frame("Time"=c(0,1,3,6,12),
                          "Proportion"=c(l0[1],l1[1],l3[1],l6[1],l12[1]))
  lp_low = data.frame("Time"=c(0,1,3,6,12), 
                      "Score"=c(l0[2],l1[2],l3[2],l6[2],l12[2]))
  lp_upp = data.frame("Time"=c(0,1,3,6,12), 
                      "Score"=c(l0[3],l1[3],l3[3],l6[3],l12[3]))
  
  df_med = data.frame("Time"=c(0,1,3,6,12), 
                      "Score"=c(median(all_tmp$U0,na.rm=T)/100,
                                median(all_tmp$U1,na.rm=T)/100,
                                median(all_tmp$U3,na.rm=T)/100,
                                median(all_tmp$U6,na.rm=T)/100,
                                median(all_tmp$U12,na.rm=T)/100)
  )
  df_low = data.frame("Time"=c(0,1,3,6,12), 
                      "Score"=c(quantile(all_tmp$U0,.25,na.rm=T)/100,
                                quantile(all_tmp$U1,.25,na.rm=T)/100,
                                quantile(all_tmp$U3,.25,na.rm=T)/100,
                                quantile(all_tmp$U6,.25,na.rm=T)/100,
                                quantile(all_tmp$U12,.25,na.rm=T)/100)
  )
  df_upp = data.frame("Time"=c(0,1,3,6,12), 
                      "Score"=c(quantile(all_tmp$U0,.75,na.rm=T)/100,
                                quantile(all_tmp$U1,.75,na.rm=T)/100,
                                quantile(all_tmp$U3,.75,na.rm=T)/100,
                                quantile(all_tmp$U6,.75,na.rm=T)/100,
                                quantile(all_tmp$U12,.75,na.rm=T)/100)
  )
  
  colours = c("Leak free"="blue", "Pad free"="orange", "Leak and pad free"="green", "Score"="black")
  plt = ggplot(data=df_leak, aes(x=Time, y=Proportion, color="Leak free"))+
    geom_line(colour="blue",lwd=1.2)+
    scale_x_continuous(breaks = seq(0, 12, by = 3))+
    ylim(0,1)+
    geom_line(data=df_pad, aes(x=Time, y=Proportion, color="Pad free"), colour="orange",lwd=1.2)+
    geom_line(data=df_leakpad, aes(x=Time, y=Proportion, color="Leak and pad free"), colour="green",lwd=1.2)+
    geom_line(data=df_med, aes(x=Time, y=Score, color="Score"), colour="black",lwd=1.2)+
    geom_errorbar(aes(ymin=df_low$Score, ymax=df_upp$Score, color="Score"), width=.2)+
    geom_errorbar(aes(ymin=leak_low$Score, ymax=leak_upp$Score, color="Leak free"), width=.2)+
    geom_errorbar(aes(ymin=pad_low$Score, ymax=pad_upp$Score, color="Pad free"), width=.2)+
    geom_errorbar(aes(ymin=lp_low$Score, ymax=lp_upp$Score, color="Leak and pad free"), width=.2)+
    scale_y_continuous(
      sec.axis=sec_axis(~.*100, name="Score"))+
    labs(color="Legend")+
    scale_color_manual(values=colours)+ theme(legend.position="bottom")
  if (!is_missing(save)){
    ggsave(save, plt, dpi=480)
  }
  plt
}

times_leakfree = function(all_tmp){
  l0 = binconf(length(all_tmp$t0L[all_tmp$t0L==100]),length(all_tmp$t0L))
  l1 = binconf(length(all_tmp$t1L[all_tmp$t1L==100]),length(all_tmp$t0L))
  l3 = binconf(length(all_tmp$t3L[all_tmp$t3L==100]),length(all_tmp$t0L))
  l6 = binconf(length(all_tmp$t6L[all_tmp$t6L==100]),length(all_tmp$t0L))
  l12 = binconf(length(all_tmp$t12L[all_tmp$t12L==100]),length(all_tmp$t0L))
  df_leak = data.frame("Time"=c(0,1,3,6,12),
                       "Proportion"=c(l0[1],l1[1],l3[1],l6[1],l12[1]))
  leak_low = data.frame("Time"=c(0,1,3,6,12), 
                        "Score"=c(l0[2],l1[2],l3[2],l6[2],l12[2]))
  leak_upp = data.frame("Time"=c(0,1,3,6,12), 
                        "Score"=c(l0[3],l1[3],l3[3],l6[3],l12[3]))
  df_s1 = data.frame("Time"=df_leak$Time, 
                     Frequency=c(length(all_tmp$t0L[all_tmp$t0L==100]),
                                 length(all_tmp$t1L[all_tmp$t1L==100]),
                                 length(all_tmp$t3L[all_tmp$t3L==100]),
                                 length(all_tmp$t6L[all_tmp$t6L==100]),
                                 length(all_tmp$t12L[all_tmp$t12L==100])),
                     "Point"=df_leak$Proportion,
                     "Lower"=leak_low$Score,
                     "Upper"=leak_upp$Score)
  flextable(df_s1)
} 

times_padfree = function(all_tmp){
  l0 = binconf(length(all_tmp$t0P[all_tmp$t0P==100]),length(all_tmp$t0L))
  l1 = binconf(length(all_tmp$t1P[all_tmp$t1P==100]),length(all_tmp$t0L))
  l3 = binconf(length(all_tmp$t3P[all_tmp$t3P==100]),length(all_tmp$t0L))
  l6 = binconf(length(all_tmp$t6P[all_tmp$t6P==100]),length(all_tmp$t0L))
  l12 = binconf(length(all_tmp$t12P[all_tmp$t12P==100]),length(all_tmp$t0L))
  df_pad = data.frame("Time"=c(0,1,3,6,12),
                      "Proportion"=c(l0[1],l1[1],l3[1],l6[1],l12[1]))
  pad_low = data.frame("Time"=c(0,1,3,6,12), 
                       "Score"=c(l0[2],l1[2],l3[2],l6[2],l12[2]))
  pad_upp = data.frame("Time"=c(0,1,3,6,12), 
                       "Score"=c(l0[3],l1[3],l3[3],l6[3],l12[3]))
  df_s1 = data.frame("Time"=df_pad$Time, 
                     Frequency=c(length(all_tmp$t0P[all_tmp$t0P==100]),
                                 length(all_tmp$t1P[all_tmp$t1P==100]),
                                 length(all_tmp$t3P[all_tmp$t3P==100]),
                                 length(all_tmp$t6P[all_tmp$t6P==100]),
                                 length(all_tmp$t12P[all_tmp$t12P==100])),
                     "Point"=df_pad$Proportion,
                     "Lower"=pad_low$Score,
                     "Upper"=pad_upp$Score)
  flextable(df_s1)
}

times_leakpadfree = function(all_tmp){
  l0 = binconf(length(all_tmp$t0L[all_tmp$t0L==100 & all_tmp$t0P==100]),length(all_tmp$t0L))
  l1 = binconf(length(all_tmp$t1L[all_tmp$t1L==100 & all_tmp$t1P==100]),length(all_tmp$t0L))
  l3 = binconf(length(all_tmp$t3L[all_tmp$t3L==100 & all_tmp$t3P==100]),length(all_tmp$t0L))
  l6 = binconf(length(all_tmp$t6L[all_tmp$t6L==100 & all_tmp$t6P==100]),length(all_tmp$t0L))
  l12 = binconf(length(all_tmp$t12L[all_tmp$t12L==100 & all_tmp$t12P==100]),length(all_tmp$t0L))
  df_leakpad = data.frame("Time"=c(0,1,3,6,12),
                          "Proportion"=c(l0[1],l1[1],l3[1],l6[1],l12[1]))
  lp_low = data.frame("Time"=c(0,1,3,6,12), 
                      "Score"=c(l0[2],l1[2],l3[2],l6[2],l12[2]))
  lp_upp = data.frame("Time"=c(0,1,3,6,12), 
                      "Score"=c(l0[3],l1[3],l3[3],l6[3],l12[3]))
  df_s1 = data.frame("Time"=df_leakpad$Time, 
                     Frequency=c(
                       length(all_tmp$t0L[all_tmp$t0L==100 & all_tmp$t0P==100]),
                       length(all_tmp$t1L[all_tmp$t1L==100 & all_tmp$t1P==100]),
                       length(all_tmp$t3L[all_tmp$t3L==100 & all_tmp$t3P==100]),
                       length(all_tmp$t6L[all_tmp$t6L==100 & all_tmp$t6P==100]),
                       length(all_tmp$t12L[all_tmp$t12L==100 & all_tmp$t12P==100])
                     ),
                     "Point"=df_leakpad$Proportion,
                     "Lower"=lp_low$Score,
                     "Upper"=lp_upp$Score)
  flextable(df_s1)
}

times_Uscore = function(all_tmp){
  df_tmp = data.frame("Time"=c("Baseline", "Month 1", "Month 3", "Month 6", "Month 12"),
                      "Median"=c(quantile(all_tmp$U0, 0.5, na.rm=T),
                                 quantile(all_tmp$U1, 0.5, na.rm=T),
                                 quantile(all_tmp$U3, 0.5, na.rm=T),
                                 quantile(all_tmp$U6, 0.5, na.rm=T),
                                 quantile(all_tmp$U12, 0.5, na.rm=T)),
                      "Lower IQR"=c(quantile(all_tmp$U0, 0.25, na.rm=T),
                                    quantile(all_tmp$U1, 0.25, na.rm=T),
                                    quantile(all_tmp$U3, 0.25, na.rm=T),
                                    quantile(all_tmp$U6, 0.25, na.rm=T),
                                    quantile(all_tmp$U12, 0.25, na.rm=T)),
                      "Upper IQR"=c(quantile(all_tmp$U0, 0.75, na.rm=T),
                                    quantile(all_tmp$U1, 0.75, na.rm=T),
                                    quantile(all_tmp$U3, 0.75, na.rm=T),
                                    quantile(all_tmp$U6, 0.75, na.rm=T),
                                    quantile(all_tmp$U12, 0.75, na.rm=T))
  )
  flextable(df_tmp)
}

Sspaghetti = function(all_tmp, save){
  l0 = binconf(length(all_tmp$t0L[all_tmp$t0L==100 & all_tmp$t0P=="No"]),length(all_tmp$t0L))
  l1 = binconf(length(all_tmp$t1L[all_tmp$t1L==100 & all_tmp$t1P=="No"]),length(all_tmp$t0L))
  l3 = binconf(length(all_tmp$t3L[all_tmp$t3L==100 & all_tmp$t3P=="No"]),length(all_tmp$t0L))
  l6 = binconf(length(all_tmp$t6L[all_tmp$t6L==100 & all_tmp$t6P=="No"]),length(all_tmp$t0L))
  l12 = binconf(length(all_tmp$t12L[all_tmp$t12L==100 & all_tmp$t12P=="No"]),length(all_tmp$t0L))
  
  df_nat = data.frame("Time"=c(0,1,3,6,12),
                      "Proportion"=c(l0[1],l1[1],l3[1],l6[1],l12[1]))
  nat_low = data.frame("Time"=c(0,1,3,6,12), 
                       "Score"=c(l0[2],l1[2],l3[2],l6[2],l12[2]))
  nat_upp = data.frame("Time"=c(0,1,3,6,12), 
                       "Score"=c(l0[3],l1[3],l3[3],l6[3],l12[3]))
  
  l0 = binconf(length(all_tmp$t0L[all_tmp$t0L==100 & all_tmp$t0P=="Yes"]),length(all_tmp$t0L))
  l1 = binconf(length(all_tmp$t1L[all_tmp$t1L==100 & all_tmp$t1P=="Yes"]),length(all_tmp$t0L))
  l3 = binconf(length(all_tmp$t3L[all_tmp$t3L==100 & all_tmp$t3P=="Yes"]),length(all_tmp$t0L))
  l6 = binconf(length(all_tmp$t6L[all_tmp$t6L==100 & all_tmp$t6P=="Yes"]),length(all_tmp$t0L))
  l12 = binconf(length(all_tmp$t12L[all_tmp$t12L==100 & all_tmp$t12P=="Yes"]),length(all_tmp$t0L))
  
  df_assist = data.frame("Time"=c(0,1,3,6,12),
                         "Proportion"=c(l0[1],l1[1],l3[1],l6[1],l12[1]))
  ass_low = data.frame("Time"=c(0,1,3,6,12), 
                       "Score"=c(l0[2],l1[2],l3[2],l6[2],l12[2]))
  ass_upp = data.frame("Time"=c(0,1,3,6,12), 
                       "Score"=c(l0[3],l1[3],l3[3],l6[3],l12[3]))
  
  df_med = data.frame("Time"=c(0,1,3,6,12), 
                      "Score"=c(median(all_tmp$S0)/100,
                                median(all_tmp$S1)/100,
                                median(all_tmp$S3)/100,
                                median(all_tmp$S6)/100,
                                median(all_tmp$S12)/100)
  )
  df_low = data.frame("Time"=c(0,1,3,6,12), 
                      "Score"=c(quantile(all_tmp$S0,.25)/100,
                                quantile(all_tmp$S1,.25)/100,
                                quantile(all_tmp$S3,.25)/100,
                                quantile(all_tmp$S6,.25)/100,
                                quantile(all_tmp$S12,.25)/100)
  )
  df_upp = data.frame("Time"=c(0,1,3,6,12), 
                      "Score"=c(quantile(all_tmp$S0,.75)/100,
                                quantile(all_tmp$S1,.75)/100,
                                quantile(all_tmp$S3,.75)/100,
                                quantile(all_tmp$S6,.75)/100,
                                quantile(all_tmp$S12,.75)/100)
  )
  
  colours = c("Score"="black", "Assisted"="blue", "Natural"="orange")
  plt = ggplot(data=df_assist, aes(x=Time, y=Proportion, color="Assisted"))+
    geom_line(lwd=1.2)+
    scale_x_continuous(breaks = seq(0, 12, by = 3))+
    ylim(0,1)+
    geom_line(data=df_nat, aes(x=Time, y=Proportion, color="Natural"),lwd=1.2)+
    geom_line(data=df_med, aes(x=Time, y=Score, color="Score"),lwd=1.2)+
    geom_errorbar(aes(ymin=df_low$Score, ymax=df_upp$Score, color="Score"), width=.2)+
    geom_errorbar(aes(ymin=nat_low$Score, ymax=nat_upp$Score, color="Natural"), width=.2)+
    geom_errorbar(aes(ymin=ass_low$Score, ymax=ass_upp$Score, color="Assisted"), width=.2)+
    scale_y_continuous(
      sec.axis=sec_axis(~.*100, name="Score"))+
    labs(color="Legend")+
    scale_color_manual(values=colours)+ theme(legend.position="bottom")
  if (!is_missing(save)){
    ggsave(save, plt, dpi=480)
  }
  plt
}

times_natural = function(all_tmp){
  l0 = binconf(length(all_tmp$t0L[all_tmp$t0L==100 & all_tmp$t0P=="No"]),length(all_tmp$t0L))
  l1 = binconf(length(all_tmp$t1L[all_tmp$t1L==100 & all_tmp$t1P=="No"]),length(all_tmp$t0L))
  l3 = binconf(length(all_tmp$t3L[all_tmp$t3L==100 & all_tmp$t3P=="No"]),length(all_tmp$t0L))
  l6 = binconf(length(all_tmp$t6L[all_tmp$t6L==100 & all_tmp$t6P=="No"]),length(all_tmp$t0L))
  l12 = binconf(length(all_tmp$t12L[all_tmp$t12L==100 & all_tmp$t12P=="No"]),length(all_tmp$t0L))
  
  df_nat = data.frame("Time"=c(0,1,3,6,12),
                      "Proportion"=c(l0[1],l1[1],l3[1],l6[1],l12[1]))
  nat_low = data.frame("Time"=c(0,1,3,6,12), 
                       "Score"=c(l0[2],l1[2],l3[2],l6[2],l12[2]))
  nat_upp = data.frame("Time"=c(0,1,3,6,12), 
                       "Score"=c(l0[3],l1[3],l3[3],l6[3],l12[3]))
  df_s1 = data.frame("Time"=df_nat$Time, 
                     Frequency=c(
                       length(all_tmp$t0L[all_tmp$t0L==100 & all_tmp$t0P=="No"]),
                       length(all_tmp$t1L[all_tmp$t1L==100 & all_tmp$t1P=="No"]),
                       length(all_tmp$t3L[all_tmp$t3L==100 & all_tmp$t3P=="No"]),
                       length(all_tmp$t6L[all_tmp$t6L==100 & all_tmp$t6P=="No"]),
                       length(all_tmp$t12L[all_tmp$t12L==100 & all_tmp$t12P=="No"])
                     ),
                     "Point"=df_nat$Proportion,
                     "Lower"=nat_low$Score,
                     "Upper"=nat_upp$Score)
  flextable(df_s1)
}

times_assist = function(all_tmp){
  l0 = binconf(length(all_tmp$t0L[all_tmp$t0L==100 & all_tmp$t0P=="Yes"]),length(all_tmp$t0L))
  l1 = binconf(length(all_tmp$t1L[all_tmp$t1L==100 & all_tmp$t1P=="Yes"]),length(all_tmp$t0L))
  l3 = binconf(length(all_tmp$t3L[all_tmp$t3L==100 & all_tmp$t3P=="Yes"]),length(all_tmp$t0L))
  l6 = binconf(length(all_tmp$t6L[all_tmp$t6L==100 & all_tmp$t6P=="Yes"]),length(all_tmp$t0L))
  l12 = binconf(length(all_tmp$t12L[all_tmp$t12L==100 & all_tmp$t12P=="Yes"]),length(all_tmp$t0L))
  
  df_assist = data.frame("Time"=c(0,1,3,6,12),
                         "Proportion"=c(l0[1],l1[1],l3[1],l6[1],l12[1]))
  ass_low = data.frame("Time"=c(0,1,3,6,12), 
                       "Score"=c(l0[2],l1[2],l3[2],l6[2],l12[2]))
  ass_upp = data.frame("Time"=c(0,1,3,6,12), 
                       "Score"=c(l0[3],l1[3],l3[3],l6[3],l12[3]))
  df_s1 = data.frame("Time"=df_assist$Time, 
                     Frequency=c(
                       length(all_tmp$t0L[all_tmp$t0L==100 & all_tmp$t0P=="Yes"]),
                       length(all_tmp$t1L[all_tmp$t1L==100 & all_tmp$t1P=="Yes"]),
                       length(all_tmp$t3L[all_tmp$t3L==100 & all_tmp$t3P=="Yes"]),
                       length(all_tmp$t6L[all_tmp$t6L==100 & all_tmp$t6P=="Yes"]),
                       length(all_tmp$t12L[all_tmp$t12L==100 & all_tmp$t12P=="Yes"])
                     ),
                     "Point"=df_assist$Proportion,
                     "Lower"=ass_low$Score,
                     "Upper"=ass_upp$Score)
  flextable(df_s1)
}

times_Sscore = function(all_tmp){
  df_tmp = data.frame("Time"=c("Baseline", "Month 1", "Month 3", "Month 6", "Month 12"),
                      "Median"=c(quantile(all_tmp$S0, 0.5, na.rm=T),
                                 quantile(all_tmp$S1, 0.5, na.rm=T),
                                 quantile(all_tmp$S3, 0.5, na.rm=T),
                                 quantile(all_tmp$S6, 0.5, na.rm=T),
                                 quantile(all_tmp$S12, 0.5, na.rm=T)),
                      "Lower IQR"=c(quantile(all_tmp$S0, 0.25, na.rm=T),
                                    quantile(all_tmp$S1, 0.25, na.rm=T),
                                    quantile(all_tmp$S3, 0.25, na.rm=T),
                                    quantile(all_tmp$S6, 0.25, na.rm=T),
                                    quantile(all_tmp$S12, 0.25, na.rm=T)),
                      "Upper IQR"=c(quantile(all_tmp$S0, 0.75, na.rm=T),
                                    quantile(all_tmp$S1, 0.75, na.rm=T),
                                    quantile(all_tmp$S3, 0.75, na.rm=T),
                                    quantile(all_tmp$S6, 0.75, na.rm=T),
                                    quantile(all_tmp$S12, 0.75, na.rm=T))
  )
  flextable(df_tmp)
}

Lpcp= function(df, save){
  plt = df |>
    pcp_select(A28.0, A28.12) |>
    pcp_scale() |>
    pcp_arrange(method="from-right")|>
    ggplot(aes_pcp()) +
    geom_pcp_boxes(boxwidth=0.1) +
    geom_pcp(aes(colour = A28.12), alpha = 0.1, axiswidth = c(0,0)) +
    guides(colour=guide_legend(override.aes = list(alpha=1))) +
    geom_pcp_labels() +
    scale_x_discrete(expand = expansion(add=0.15),
                     labels=c("A28.0" = "Baseline leakage problem",
                              "A28.12" = "Twelve month leakage problem")) +
    labs(x='Item', y='Proportion', color='Twelve month leakage problem') +
    theme(legend.position="none")
  if (!is_missing(save)){
    ggsave(save, plt, dpi=480)
  }
  plt
}

Upcp= function(df, save){
  plt = df |>
    pcp_select(A34.0, A34.12) |>
    pcp_scale() |>
    pcp_arrange(method="from-right")|>
    ggplot(aes_pcp()) +
    geom_pcp_boxes(boxwidth=0.1) +
    geom_pcp(aes(colour = A34.12), alpha = 0.1, axiswidth = c(0,0)) +
    guides(colour=guide_legend(override.aes = list(alpha=1))) +
    geom_pcp_labels() +
    scale_x_discrete(expand = expansion(add=0.15),
                     labels=c("A34.0" = "Baseline urinary problem",
                              "A34.12" = "Twelve month urinary problem")) +
    labs(x='Item', y='Proportion', color='Twelve month urinary problem') +
    theme(legend.position="none")
  if (!is_missing(save)){
    ggsave(save, plt, dpi=480)
  }
  plt
}

Spcp = function(df, save){
  plt = df |>
    pcp_select(A68.0, A68.12) |>
    pcp_scale() |>
    pcp_arrange(method="from-right")|>
    ggplot(aes_pcp()) +
    geom_pcp_boxes(boxwidth=0.1) +
    geom_pcp(aes(colour = A68.12), alpha = 0.1, axiswidth = c(0,0)) +
    guides(colour=guide_legend(override.aes = list(alpha=1))) +
    geom_pcp_labels() +
    scale_x_discrete(expand = expansion(add=0.15),
                     labels=c("A68.0" = "Baseline sexual problem",
                              "A68.12" = "Twelve month sexual problem")) +
    labs(x='Item', y='Proportion', color='Twelve month sexual problem') +
    theme(legend.position="none")
  if (!is_missing(save)){
    ggsave(save, plt, dpi=480)
  }
  plt
}
