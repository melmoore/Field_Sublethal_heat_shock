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


slhs_lng <- read_csv("data files/Field_SLHS_clean_wdead_LONG.csv", guess_max = 6000,
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

slhs.cl$para.pup[is.na(slhs.cl$para.pup)]<-0
slhs.cl$para.pup<-as.factor(slhs.cl$para.pup)

ttw.plot<-ggplot(slhs.cl, aes(x=ttwand, y=mass.wand, group=treat.para, color=treat.para))
ttw.plot+geom_point(aes(shape=para.pup)
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


#------------------

#calculating proportion of each "outcome" for each temp and para treatment
  
slhs.cl$mass.wand[is.na(slhs.cl$mass.wand)]<-0
slhs.cl$wander<-ifelse(slhs.cl$mass.wand>0, 1, 0)

outcome<-slhs.cl %>% group_by(test.temp, treat.para) %>% tally(wander)
View(outcome)


#find total number in each treatment
tot.n<-slhs.cl %>% count(test.temp, treat.para)

#add total number to dataframe with number of outcomeers
outcome$tot.n<-tot.n$n  

#rename outcome$n to outcome$wander to avoid confusion
outcome<-rename(outcome, wander=n)

#calculate proportion of outcomeers
outcome$prop.wand<-outcome$wander/outcome$tot.n

#calculate the number in each treatment that had wasp emergence
slhs.cl$date.em.j[is.na(slhs.cl$date.em.j)]<-0
slhs.cl$emergence<-ifelse(slhs.cl$date.em.j>0, 1, 0)

emerge<-slhs.cl %>% group_by(test.temp, treat.para) %>% tally(emergence)
View(emerge)

#add emergence data to outcome dataframe
outcome$emerge<-emerge$n

#calculate proportion emergence
outcome$prop.emerge<-outcome$emerge/outcome$tot.n


#Make a mongo sorting column specifically for mongos that were not wanderers
slhs.cl$mongo.calc<-ifelse(slhs.cl$mongo==1 & slhs.cl$mass.wand==0, 1, 0)


#calculate the number in each treatment that were culled (assumed mongos)
mongo<-slhs.cl %>% group_by(test.temp, treat.para) %>% tally(mongo.calc)
View(mongo)

#add mongo data to outcome dataframe
outcome$mongo<-mongo$n

#calculate the proportion of mongos in each treatment
outcome$prop.mongo<-outcome$mongo/outcome$tot.n



#Making a long data frame of the outcome data
oc_lng<-gather(outcome, outcome, prop, prop.wand, prop.emerge, prop.mongo)



#Plotting prop outcomes for each test temperature and para treatment

outcome.plot<-ggplot(oc_lng,aes(x=test.temp ,y=prop, fill=outcome))
outcome.plot+geom_bar(position="fill",stat="identity"
)+scale_fill_manual(values=c("#95D840", "#1F9F88", "#440D54"),
                    breaks=c("prop.emerge", "prop.mongo", "prop.wand"),
                    labels=c("Emergence", "Mongo", "Wandering"),
                    name="Outcome"
)+labs(x="Test temperature", y="Proportion"
)+facet_wrap(~treat.para
)+theme(text = element_text(family=("Cambria")),
        strip.background = element_rect(colour="black",linetype = "solid",fill="white",
                                        size = 1),
        strip.text = element_text(size=30),
        axis.line.x=element_line(colour = 'black', size = 1),
        axis.line.y=element_line(colour = 'black', size = 1),
        axis.ticks = element_line(colour = 'black', size = 1),
        axis.ticks.length = unit(2, "mm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        legend.key.width=unit(15,"mm"),
        legend.key.height = unit(10,"mm"),
        legend.text=element_text(size=18),
        legend.title=element_text(size=20),
        legend.background = element_rect(color="black",linetype="solid",size=1))


#-------------------


#Plotting mass X age for para treatment
  ##for individuals

#removing individual with typo in mass
which(slhs_lng$mass > 60000)
slhs_lng<-slhs_lng[-816,]
slhs_lng<-slhs_lng[-5971,]

#Make 0s in mass column == NAs 
slhs_lng$mass[slhs_lng$mass==0]<-NA

#logging mass
slhs_lng$log.mass<-log(slhs_lng$mass)

#removing dead individuals
slhs_lngc<-subset(slhs_lng, died.toss==0)



#individuals plot with test temp by color and panel by para
indmass.plot<-ggplot(slhs_lngc, aes(x=age, y=log.mass, group=interaction(id, test.temp), color=test.temp))
indmass.plot+geom_line(size=1
)+geom_point(size=3
)+facet_wrap(~treat.para)


#individuals plot with para by color and panel by test temp
indmass.plot2<-ggplot(slhs_lngc, aes(x=age, y=log.mass, group=interaction(id, treat.para), color=treat.para))
indmass.plot2+geom_line(size=1
)+geom_point(size=3
)+facet_wrap(~test.temp)


#-------------------


#Plotting mass X age for para treatment
##means

#SummarySE of mass

lm.sum<-summarySE(slhs_lngc, measurevar = "log.mass",
                  groupvars = c("test.temp", "treat.para", "instar"),
                  na.rm=TRUE)
lm.sum

#subsetting to only the 3, 4, 5 and end data, to try and make more meaningful plot
lm.sum.neat<-subset(lm.sum, instar=="3" | instar=="4" | instar=="5" | instar=="end")


age.sum<-summarySE(slhs_lngc, measurevar = "age",
                   groupvars = c("test.temp", "treat.para","instar"),
                   na.rm=TRUE)
age.sum

#subsetting to only the 3, 4, 5 and end data, to try and make more meaningful plot
age.sum.neat<-subset(age.sum, instar=="3" | instar=="4" | instar=="5" | instar=="end")


lm.sum.neat$age<-age.sum.neat[,5]
lm.sum.neat$age.se<-age.sum.neat[,7]


#Color by test temp, panel by para
lmsum.plot<-ggplot(lm.sum.neat, aes(x=age, y=log.mass, group=test.temp,
                               color=test.temp))
lmsum.plot+geom_point(aes(shape=instar),
                      size=3
)+geom_line(size=1.2
)+facet_wrap(~treat.para)

#Color by para, panel by test temp
lmsum.plot2<-ggplot(lm.sum.neat, aes(x=age, y=log.mass, group=treat.para,
                                    color=treat.para))
lmsum.plot2+geom_point(aes(shape=treat.para),
                       size=3
)+geom_line(aes(linetype=treat.para),
            size=1.2
)+geom_errorbar(aes(ymin=log.mass-se, ymax=log.mass+se),
                width=.4, size=1
)+geom_errorbarh(aes(xmin=age-age.se, xmax=age+age.se),
                 height=.4, size=1
)+scale_color_manual(values=c("black", "red"),
                     breaks=c("np", "p"),
                     label=c("NP", "P"),
                     name="Parasitized"
)+scale_linetype_manual(values=c("solid", "dashed"),
                        breaks=c("np", "p"),
                        label=c("NP", "P"),
                        name="Parasitized"
)+scale_shape_manual(values=c(16, 17),
                     breaks=c("np", "p"),
                     label=c("NP", "P"),
                     name="Parasitized"
)+facet_wrap(~test.temp)

#--------------------------

#plotting the change in mass during heat shock

#scatterplot
dmass.plot<-ggplot(slhs.cl, aes(x=test.temp, y=delta.mass.test, color=treat.para))
dmass.plot+geom_jitter()

#boxplot
dmass.plot2<-ggplot(slhs.cl, aes(x=test.temp, y=delta.mass.test, fill=treat.para))
dmass.plot2+geom_boxplot(
)+scale_fill_manual(values=c("grey", "orangered3"),
                    breaks=c("np", "p"),
                    label=c("NP", "P"),
                    name="Parasitization")


#plotting with dead individuals included
#boxplot

#making died.toss a factor for plotting purposes
slhs$died.toss<-as.factor(slhs$died.toss)

#load surv data frame to add labels with sample size
surv <- read_csv("data files/field-slhs_treat-surv-table.csv")

#create a surv.n column with the number that survived in each treatment
surv$surv.n<-surv$tot.n-surv$died.n

#make a long dataframe from surv data set
surv_lng<-gather(surv, died, n, surv.n, died.n)

#rename surv_lng column names so they match the aesthetics of the plot
surv_lng<-rename(surv_lng, delta.mass.test=n)

#make a died.toss column to match aesthetics of plot
surv_lng$died.toss<-ifelse(surv_lng$died=="surv.n", 0, 1)

#make surv_lng$died.toss and surv_lng$test.temp factors so they will plot appropriately
surv_lng$died.toss<-as.factor(surv_lng$died.toss)
surv_lng$test.temp<-as.factor(surv_lng$test.temp)

#Making a column in surv_lng with y axis data that will match the label with the boxplot
  ##there should be an easier way to do this, but I'm tired of looking
surv_lng$ypos<-c(100, 45, 45, 30, 10, 55, 40, 16, 19, 10,
                25, 25, 18, 10, 12, 30, 20, 10, 10, 8)


dmass.dead.plot<-ggplot(slhs, aes(x=test.temp, y=delta.mass.test, 
                                  group=interaction(test.temp, died.toss),
                                  fill=died.toss))
dmass.dead.plot+geom_boxplot(
)+scale_fill_manual(values=c("grey", "orangered3"),
                    breaks=c("0", "1"),
                    label=c("Lived", "Died"),
                    name="Survival"
)+facet_wrap(~treat.para
)+geom_text(data=surv_lng,aes(x=test.temp, y=ypos, label = n, fontface="bold"),
            position = position_dodge(.7))



#plotting delta.mass.test by mass at start of test

initmass.plot<-ggplot(slhs, aes(x=mass.in.test, y=delta.mass.test, color=died.toss))
initmass.plot+geom_point(
)+scale_color_manual(values=c("black", "red"),
                     breaks=c("0", "1"),
                     label=c("Lived", "Died"),
                     name="Survived"
)+geom_smooth(method=lm, se=FALSE
)+facet_wrap(test.temp~treat.para)



#plotting mean delta.mass.test 

dmass.sum<-summarySE(slhs, measurevar = "delta.mass.test",
                     groupvars = c("test.temp", "treat.para", "died.toss"),
                     na.rm = TRUE)
dmass.sum


dmass.mn.plot<-ggplot(dmass.sum, aes(x=test.temp, y=delta.mass.test,
                                     group=treat.para, color=treat.para))
dmass.mn.plot+geom_point(aes(shape=treat.para),
                         size=4
)+geom_line(aes(linetype=treat.para),
            size=1.5
)+geom_errorbar(aes(ymin=delta.mass.test-se, ymax=delta.mass.test+se),
                width=.3, size=1
)+scale_color_manual(values=c("black", "red"),
                     breaks=c("np", "p"),
                     label=c("NP", "P"),
                     name="Parasitized"
)+scale_linetype_manual(values=c("solid", "dashed"),
                        breaks=c("np", "p"),
                        label=c("NP", "P"),
                        name="Parasitized"
)+scale_shape_manual(values=c(16, 17),
                     breaks=c("np", "p"),
                     label=c("NP", "P"),
                     name="Parasitized"
)+facet_wrap(~died.toss)



#plotting mean initial mass by temp

initmass.sum<-summarySE(slhs, measurevar = "mass.in.test",
                        groupvars = c("test.temp", "treat.para", "died.toss"),
                        na.rm = TRUE)
initmass.sum

mninmass.plot<-ggplot(initmass.sum, aes(x=test.temp, y=mass.in.test,
                                        group=treat.para, color=treat.para))
mninmass.plot+geom_point(aes(shape=treat.para),
                         size=4
)+geom_line(aes(linetype=treat.para),
            size=1.5
)+geom_errorbar(aes(ymin=mass.in.test-se, ymax=mass.in.test+se),
                width=.3, size=1
)+scale_color_manual(values=c("black", "red"),
                     breaks=c("np", "p"),
                     label=c("NP", "P"),
                     name="Parasitized"
)+scale_linetype_manual(values=c("solid", "dashed"),
                        breaks=c("np", "p"),
                        label=c("NP", "P"),
                        name="Parasitized"
)+scale_shape_manual(values=c(16, 17),
                     breaks=c("np", "p"),
                     label=c("NP", "P"),
                     name="Parasitized"
)+facet_wrap(~died.toss)




#---------------------

#plot last point, time and mass at end for parasitized

#subset to parasitized treatment only
slhs_para<-subset(slhs_lngc, treat.para=="p")

#subset to only end data point
slhs_para<-subset(slhs_para, instar=="end")

#Make useful stage column for what the end point was (em, mong, wand)
slhs_para$date.wand.j[is.na(slhs_para$date.wand.j)]<-0
slhs_para$date.em.j[is.na(slhs_para$date.em.j)]<-0
slhs_para$date.cull.j[is.na(slhs_para$date.cull.j)]<-0

slhs_para$stage<-ifelse(slhs_para$date.wand.j>0, "wand",
                        ifelse(slhs_para$date.em.j>0, "em",
                               ifelse(slhs_para$date.cull.j, "cull", "0")))

slhs_para$stage[slhs_para$stage=="0"]<-NA

#plot age and mass at end data point
end.plot<-ggplot(slhs_para, aes(x=age, y=mass, group=test.temp, color=test.temp))
end.plot+geom_point(aes(shape=stage),
                    size=5
)+geom_smooth(method=lm, se=FALSE)



massend.sum<-summarySE(slhs_para, measurevar = "mass",
                   groupvars = c("test.temp", "stage"),
                   na.rm = TRUE)
massend.sum

massend.sum<-subset(massend.sum, N!=0)


ageend.sum<-summarySE(slhs_para, measurevar = "age",
                      groupvars = c("test.temp", "stage"),
                      na.rm = TRUE)
ageend.sum

ageend.sum<-subset(ageend.sum, N!=0)


massend.sum$age<-ageend.sum[,4]
massend.sum$age.se<-ageend.sum[,6]


mnend.plot<-ggplot(massend.sum, aes(x=age, y=mass, group=stage, color=stage))
mnend.plot+geom_point(size=5
)+facet_wrap(~test.temp)



#boxplots of mass and age

massend.boxplot<-ggplot(slhs_para, aes(x=test.temp, y=mass, fill=stage))
massend.boxplot+geom_boxplot(
)+scale_fill_manual(values=c("#95D840", "#1F9F88", "#891BA9"),
                    breaks=c("em", "cull", "wand"),
                    labels=c("Emerged", "Culled", "Wandered"),
                    name="Outcome"
)+labs(x="Test temperature", y="Mass at End")
       
       

ageend.boxplot<-ggplot(slhs_para, aes(x=test.temp, y=age, fill=stage))
ageend.boxplot+geom_boxplot(
)+scale_fill_manual(values=c("#95D840", "#1F9F88", "#891BA9"),
                    breaks=c("em", "cull", "wand"),
                    labels=c("Emerged", "Culled", "Wandered"),
                    name="Outcome"
)+labs(x="Test temperature", y="Age at End")



