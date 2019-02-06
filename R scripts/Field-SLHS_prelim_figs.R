#Prelim look at para field sublethal expt:


#load libraries

library(readr)
library(Rmisc)
library(ggplot2)
library(dplyr)
library(tidyr)

#------------

#load data


slhs <- read_csv("data files/Field_SLHS_clean_wdead.csv", 
                 col_types = cols(test.temp = col_factor(levels = c("35", "40", "42", "43", "44")), 
                                  treat.hs = col_factor(levels = c("control", "shock")), 
                                  treat.para = col_factor(levels = c("np", "p"))))
View(slhs)


slhs_long <- read_csv("data files/Field_SLHS_clean_wdead_LONG.csv", guess_max = 6000,
                      col_types = cols(test.temp = col_factor(levels = c("35", "40", "42", "43", "44")), 
                                       treat.hs = col_factor(levels = c("control", "shock")), 
                                       treat.para = col_factor(levels = c("np", "p"))))
                                                                                                                                                                                  
View(slhs_long)


#--------------------

#create a clean data frame with no dead individuals 
slhs.cl<-subset(slhs, died.toss=="0")

#------------------

#Quick look at dist of time to wandering amongst treatments; and prop that had emergence at each treat

ttw.sum<-summarySE(slhs.cl, measurevar = "ttwand",
                   groupvars = c("test.temp", "treat.para"),
                   na.rm = TRUE)
ttw.sum



#plot of ttwand and mass.wand, group by treat.para

ttw.plot<-ggplot(slhs.cl, aes(x=ttwand, y=mass.wand, group=treat.para, color=treat.para))
ttw.plot+geom_point(
       )+geom_smooth(method="lm", se=FALSE
       )+facet_wrap(~test.temp)



#plot of ttwand and mass.wand, group by test.temp

ttw.plot2<-ggplot(slhs.cl, aes(x=ttwand, y=mass.wand, group=test.temp, color=test.temp))
ttw.plot2+geom_point(
)+geom_smooth(method="lm", se=FALSE
)+facet_wrap(~treat.para)



#box plot of ttwand

ttw.boxplot<-ggplot(slhs.cl, aes(x=test.temp, y=ttwand, group=interaction(test.temp, treat.para),fill=treat.para))
ttw.boxplot+geom_boxplot(
)+scale_fill_manual(values = c("grey", "red"),
                    breaks=c("np", "p"),
                    name="Para treatment",
                    label=c("NP", "P"))



#box plot of mass wand

slhs.cl$mass.wand[slhs.cl$mass.wand=="0"]<-NA

massw.boxplot<-ggplot(slhs.cl, aes(x=test.temp, y=mass.wand, 
                                   group=interaction(test.temp, treat.para),fill=treat.para))
massw.boxplot+geom_boxplot(
)+scale_fill_manual(values = c("grey", "red"),
                    breaks=c("np", "p"),
                    name="Para treatment",
                    label=c("NP", "P"))


#-------------------

#Plotting mass X age for para treatment
  ##for individuals

#removing individual with typo in mass

slhs.plng<-subset(slhs.plng, mass < 60000)

#logging mass

slhs.plng$log.mass<-log(slhs.plng$mass)

indmass.plot<-ggplot(slhs.plng, aes(x=age, y=log.mass, group=interaction(id, test.temp), color=test.temp))
indmass.plot+geom_line(size=1
)+geom_point(aes(shape=stage), size=3
)+facet_wrap(~mongo)


#Plotting the mean mass at each measurement

#SummarySE of mass

lm.sum<-summarySE(slhs.plng, measurevar = "log.mass",
                  groupvars = c("test.temp", "instar"),
                  na.rm=TRUE)
lm.sum


age.sum<-summarySE(slhs.plng, measurevar = "age",
                   groupvars = c("test.temp", "instar"),
                   na.rm=TRUE)
age.sum

lm.sum$age<-age.sum[,4]
lm.sum$age.se<-age.sum[,6]


lmsum.plot<-ggplot(lm.sum, aes(x=age, y=log.mass, group=test.temp, color=test.temp))
lmsum.plot+geom_point(
)+geom_line()



