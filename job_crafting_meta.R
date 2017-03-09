# Frederick and VanderWeele

# Variable Key ####
# str <- increasing structural resources
# soc <- increasing social resources
# cha <- increasing challenging demands
# hin <- decreasing hindering demands
# res <- increasing resources (cf Petrou et al. (2012) combine Tims et al. (2012) two resources into one)


## Load library and define helper functions  #########################
library(metafor)
library(ggplot2)

#set working directory
setwd("~/harvard/work/job_crafting/r/")
# clear workspace
rm(list=ls())

# Define helper functions
d2r <- function(d){r <- d / sqrt( d^2 + 4); r} # Cohen's d to r conversion. using a <- 4 for n1==n2
f2r <- function(f){r <- tanh(f); r} #
r2d <- function(r){d <- (2*r) / sqrt(1-r^2); d} #

# Define Studies and Variables as data sets #####
# study<- first author and year of study
# d<- calculated effect size
# n<- sample size
# t<- time lag between collection points (in days; using 30 as one month)
work<- data.frame(study=character(), d=double(), n=integer(), t=integer(), stringsAsFactors = FALSE)
workNoHin<- data.frame(study=character(), d=double(), n=integer(), t=integer(), stringsAsFactors = FALSE)
workStr<- data.frame(study=character(), d=double(), n=integer(), t=integer(), stringsAsFactors = FALSE)
workSoc<- data.frame(study=character(), d=double(), n=integer(), t=integer(), stringsAsFactors = FALSE)
workCha<- data.frame(study=character(), d=double(), n=integer(), t=integer(), stringsAsFactors = FALSE)
workHin<- data.frame(study=character(), d=double(), n=integer(), t=integer(), stringsAsFactors = FALSE)


# Demerouti et al 2015 "Productive..."  ####
demerouti_2015_n <- 95 # 95 employees sampled
demerouti_2015_t <- 1
  
# general 
g_res_var <- 0.61^2
g_cha_var <- 1.00^2
g_hin_var <- 0.61^2
g_res_cha_cor <- 0.53
g_res_hin_cor <- 0.16
g_cha_hin_cor <- 0.20

(g_jc_var_of_sum <- g_res_var + g_cha_var + g_hin_var  + 2*(g_res_cha_cor + g_res_hin_cor + g_cha_hin_cor))
(g_sd_sum <- sqrt(g_jc_var_of_sum))
(g_sd_avg <- g_sd_sum / 3)

#Compute standardized effect for daily (averaged) work engagement

# Day-level (Table 4 for model)
d_res_beta <- 0.072 # day-level seeking resources beta
d_cha_beta <- 0.121 # day-level seeking challenging demands beta
d_hin_beta <--0.205 # day-level reducing hindering demands beta

# Table 1 for sd, cors
d_res_var <- 0.56^2
d_cha_var <- 0.58^2
d_hin_var <- 0.52^2
d_res_cha_cor <- 0.18
d_res_hin_cor <- 0.15
d_cha_hin_cor <- 0.06

(d_jc_var_of_sum <- d_res_var + d_cha_var + d_hin_var + 2*(d_res_cha_cor + d_res_hin_cor + d_cha_hin_cor))
(d_sd_sum <- sqrt(d_jc_var_of_sum))
(d_sd_avg <- d_sd_sum / 3)

d_we_sd <- 0.57 #day-level work engagement sd

(d_res<- (d_res_beta * d_sd_avg) / d_we_sd)
(d_cha<- (d_cha_beta * d_sd_avg) / d_we_sd)
(d_hin<- (d_hin_beta * d_sd_avg) / d_we_sd)

demerouti_2015_res2we <- d_res
demerouti_2015_cha2we <- d_cha
demerouti_2015_hin2we <- d_hin

demerouti_2015_d <- demerouti_2015_res2we + demerouti_2015_cha2we + demerouti_2015_hin2we
demerouti_2015_d_noHin <- demerouti_2015_res2we + demerouti_2015_cha2we

study<- 'Demerouti 2015'
(work<- rbind(work,data.frame(study=study,n=demerouti_2015_n,d=demerouti_2015_d,t=demerouti_2015_t)))
(workNoHin<- rbind(workNoHin,data.frame(study=study,n=demerouti_2015_n,d=demerouti_2015_d_noHin,t=demerouti_2015_t)))
(workCha<- rbind(workCha,data.frame(study=study,n=demerouti_2015_n,d=demerouti_2015_cha2we,t=demerouti_2015_t)))
(workHin<- rbind(workHin,data.frame(study=study,n=demerouti_2015_n,d=demerouti_2015_hin2we,t=demerouti_2015_t)))



# Harju et al. 2016 "Can job ..." ####
# standardized regression coefficients (Fig. 1)
harju_2016_n <- 1630
harju_2016_t <- 365*3 # 3year time lag
harju_2016_cha2we <- 0.06 #increasing challenging demands to work engagement

(work<- rbind(work,data.frame(study='Harju 2016',n=harju_2016_n,d=harju_2016_cha2we,t=harju_2016_t)))
(workNoHin<- rbind(workNoHin, data.frame(study="Harju 2016",n=harju_2016_n, d=harju_2016_cha2we, t=harju_2016_t)))
(workCha<- rbind(workCha, data.frame(study="Harju 2016",n=harju_2016_n, d=harju_2016_cha2we, t=harju_2016_t)))



## Petrou et al. 2012 "Crafting a job..." ####
petrou_2012_n <- 95 
petrou_2012_t <- 1

#between subjects results
petrou_2012_res2we <- 0.26 # seeking resources --> work engagement

#w/in subjects results
petrou_2012_cha2we_win <- 0.15 # seeking challenging demands within-subject
petrou_2012_hin2we_win <--0.10 # reducing hindering demands within-subject

petrou_2012_d <- petrou_2012_res2we + petrou_2012_cha2we_win + petrou_2012_hin2we_win
petrou_2012_d_noHin <- petrou_2012_cha2we_win

(work<- rbind(work, data.frame(study='Petrou 2012',d=petrou_2012_d,n=petrou_2012_n,t=petrou_2012_t)))
(workNoHin<- rbind(workNoHin, data.frame(study='Petrou 2012',d=petrou_2012_d_noHin,n=petrou_2012_n,t=petrou_2012_t)))
(workCha<- rbind(workCha, data.frame(study='Petrou 2012',d=petrou_2012_cha2we_win,n=petrou_2012_n,t=petrou_2012_t)))



# Petrou et al. 2016 "Crafting the change..." ####
petrou_2016_n <- 368
petrou_2016_t <- 365 # 1-year lags; 3 time points

# Standardized coefficients (Table 3; Fig. 2)
petrou_2016_hin2we <--0.14 
petrou_2016_res2we <- 0.17 

petrou_2016_d <- sum(petrou_2016_hin2we,petrou_2016_res2we)
petrou_2016_d_noHin <- petrou_2016_res2we

(work<- rbind(work,data.frame(study='Petrou 2016',n=petrou_2016_n,d=petrou_2016_d, t=petrou_2016_t)))
(workNoHin<- rbind(workNoHin,data.frame(study='Petrou 2016',n=petrou_2016_n,d=petrou_2016_d_noHin, t=petrou_2016_t)))
(workHin<- rbind(workHin,data.frame(study='Petrou 2016',n=petrou_2016_n,d=petrou_2016_hin2we, t=petrou_2016_t)))

## Tims et al. 2013 "The Impact of..." ####
# NB: same dataset for Tims 2013 & 2015 studies
# NB: using standarized estimates for fully mediated models
tims_2013_n <- 288
tims_2013_t <- 30 # 1-month lag; three time points
  
# increasing structural resources (Fig 1)
tims_2013_str2we <- 0.65 #work engagement
# increasing social resources (Fig 2)
tims_2013_soc2we <- 0.45 #work engagement
# increasing challenging demands (Fig 3)
tims_2013_cha2we <- 0.02 #work engagement
# decreasing hindering demands (Fig 4)
#tims_2013_hin2we <- no path


(tims_2013_we<- sum(c(tims_2013_cha2we, tims_2013_soc2we, tims_2013_str2we)))
(tims_2013_we_no_hin<- sum(c(tims_2013_cha2we, tims_2013_soc2we, tims_2013_str2we)))

(work<- rbind(work,data.frame(study='Tims 2013',n=tims_2013_n,d=tims_2013_we, t=tims_2013_t)))
(workNoHin<- rbind(workNoHin,data.frame(study='Tims 2013',n=tims_2013_n,d=tims_2013_we_no_hin, t=tims_2013_t)))
(workSoc<- rbind(workSoc,data.frame(study='Tims 2013',n=tims_2013_n,d=tims_2013_soc2we, t=tims_2013_t)))
(workStr<- rbind(workStr,data.frame(study='Tims 2013',n=tims_2013_n,d=tims_2013_str2we, t=tims_2013_t)))
(workCha<- rbind(workCha,data.frame(study='Tims 2013',n=tims_2013_n,d=tims_2013_cha2we, t=tims_2013_t)))



# Tims et al. 2015 "Job Crafting and job performance..." ####
# Uses same data as Tims 2013
tims_2015_n <- 288
tims_2015_t <- 30 # 1-month; three time points
  
tims_2015_jrjd2we <- 0.20 # actual crafting of job resources & challenging demands @T2 to work engagement @T3
tims_2015_hin2we <- -0.06 # actual crafting of hindering demands @T2 to work engagement @T3

tims_2015_we<- tims_2015_hin2we + tims_2015_jrjd2we

(work<- rbind(work,data.frame(study='Tims 2015',n=tims_2015_n,d=tims_2015_we,t=tims_2015_t)))
(workNoHin<- rbind(workNoHin,data.frame(study='Tims 2015',n=tims_2015_n,d=tims_2015_jrjd2we,t=tims_2015_t)))
(workHin<- rbind(workHin,data.frame(study='Tims 2015',n=tims_2015_n,d=tims_2015_hin2we,t=tims_2015_t)))



# Van Wingerdon et al. 2015 "The Impact of..." ####
vanwingerdon_2015_n <- 102
vanwingerdon_2015_t <- 28 # specified 4-weeks

# calculated from corr matrix
# use SDs of job crafting @T1
sd_soc <- 0.49 #increasing social job resources
sd_str <- 0.50 #increasing structural job resources
sd_cha <- 0.58 #increasing challenging job demands
sd_hin <- 0.46 #decreasing hindering job demands
# Use SDs of outcome vars @T2
sd_eng <- 1.02 #work engagement
sd_per <- 0.30 #in-role work performance

# corr@T1 * SDs
soc_str <- 0.25*sd_soc*sd_str
soc_cha <- 0.33*sd_soc*sd_cha
soc_hin <- 0.22*sd_soc*sd_hin
str_cha <- 0.45*sd_str*sd_cha
str_hin <- 0.01*sd_str*sd_hin
cha_hin <- 0.20*sd_cha*sd_hin

(var_sum_jc_with_hin <- sd_soc^2 + sd_str^2 + sd_cha^2 + sd_hin^2 + 2*sum(c(soc_str, soc_cha, soc_hin, str_cha, str_hin,cha_hin)))
(var_sum_jc_without_hin <- sd_soc^2 + sd_str^2 + sd_cha^2 + 2*sum(c(soc_str, soc_cha, str_cha)))

sd_sum_with_hin <- sqrt(var_sum_jc_with_hin)
sd_sum_without_hin <- sqrt(var_sum_jc_without_hin)

sd_avg_with_hin <- sd_sum_with_hin / 4
sd_avg_without_hin <- sd_sum_without_hin / 3

(work_effect_with_hind <- (1.064*sd_avg_with_hin)/1.02)
(work_effect_without_hind <- (1.064*sd_avg_without_hin)/1.02)

(work<- rbind(work,data.frame(study='Van Wingerdon 2015',n=vanwingerdon_2015_n,d=work_effect_with_hind,t=vanwingerdon_2015_t)))
#NB: no betas for work engagement from individual job crafting factors


# Vogel et al. 2015 "Engaged and Productive..." ####
vogel_2015_n <- 193
vogel_2015_t <- 21 # specified as three weeks between T1 & T2, which are the two employee measured points

vogel_2015_jc2je <- 0.31 #Table 2. Step 1 model
jc_sd <- 0.76 # job crafting SD from Table 1
je_sd <- 0.78 # job engagement SD from Table 1

(vogel_2015_d <- vogel_2015_jc2je*(jc_sd / je_sd))

(work<- rbind(work,data.frame(study='Vogel 2015',n=vogel_2015_n,d=vogel_2015_d, t=vogel_2015_t)))



# Vogt et al. 2016 "The Consequences of..." ####
vogt_2016_n <- 940
vogt_2016_t <- 30*3 # 3-month lag; three time points

vogt_2016_jc2we_t12 <- 0.12
vogt_2016_jc2we_t23 <- 0.13
vogt_2016_jc2we <- mean(c(vogt_2016_jc2we_t12, vogt_2016_jc2we_t23))

(work<- rbind(work,data.frame(study='Vogt 2015',n=vogt_2016_n,d=vogt_2016_jc2we, t=vogt_2016_t)))



# Work Engagement Analysis ####
# Using Tims 2013 & not 2015

# Using random effects model with ZCOR input
# NB: The results of rma function are ZCOR output values (i.e., r-values)

# Without Tims 2015
x1<-subset(work, work$study!='Tims 2015')
g1<- escalc(measure="ZCOR", ri= d2r(x1$d), ni=x1$n, method="REML")
summary(g1)
m1<- rma(yi, vi, data= g1, method="REML")
summary(m1)

# Without Tims 2015, without hindering measures
x2<-subset(workNoHin, workNoHin$study!='Tims 2015')
g2<- escalc(measure="ZCOR", ri= d2r(x2$d), ni=x2$n, method="REML")
summary(g2)
m2<- rma(yi, vi, data= g2, method="REML")
summary(m2)


# Challenging
x3<- workCha
g3<- escalc(measure="ZCOR", ri= d2r(x3$d), ni=x3$n, method="REML")
summary(g3)
m3<- rma(yi, vi, data= g3, method="REML")
summary(m3)


# Hindering
x4<- workHin
g4<- escalc(measure="ZCOR", ri= d2r(x4$d), ni=x4$n, method="REML")
summary(g4)
m4<- rma(yi, vi, data= g4, method="REML")
summary(m4)


## Plotting ################
# Create new data.frame for plotting main models
# NB: shouldn't need new frames, but ¯\_(ツ)_/¯
m<- c('m1','m2')
xs<- as.numeric(1:2)
b<- c(m1$b, m2$b)
u<- c(m1$ci.ub, m2$ci.ub)
l<- c(m1$ci.lb, m2$ci.lb)
df<- data.frame(m=m, x=xs, b=b, u=u, l=l)

p<- ggplot(df, aes(x=xs, y=b) ) +
  geom_hline(aes(yintercept=0),linetype='dotted') +
  geom_point(x=xs, y=b) + geom_errorbar(aes(x=xs, ymin=l, ymax=u), width=0.2) +
  expand_limits(y=c(0,0.1), x=(c(0,3))) +
  ylab('Effect Size (r value)') +
  scale_x_discrete(name="Model",limits=c(1,2),labels=c("1"="Full", "2"="NoHind")) +
  theme_bw()
p

# Create new data.frame for plotting individual factor models
m<- c('m3','m4')
xs<- as.numeric(1:2)
b<- c(m3$b, m4$b)
u<- c(m3$ci.ub, m4$ci.ub)
l<- c(m3$ci.lb, m4$ci.lb)
df<- data.frame(m=m, x=xs, b=b, u=u, l=l)

p<- ggplot(df, aes(x=xs, y=b) ) +
  geom_hline(aes(yintercept=0),linetype='dotted') +
  geom_point(x=xs, y=b) + geom_errorbar(aes(x=xs, ymin=l, ymax=u), width=0.2) +
  expand_limits(y=c(0,0.1), x=(c(0,3))) +
  ylab('Effect Size (r value)') +
  scale_x_discrete(name="Model",limits=c(1,2),labels=c("1"="Chal", "2"="Hind")) +
  theme_bw()
p




### Confounding Analysis Below ####
# NB: this uses the d values instead of the estimated r values

# Effects Sizes & Samples Sizes
df<- subset(work, study!="Tims 2015")
df$lab<-1:8

# Plot effect size (d) vs sample size
p2<- ggplot(df, aes(x=n, y=d))
p2 + geom_point() + geom_text(aes(label=lab),hjust=0, vjust=0) + #,hjust=0,vjust=0)
  expand_limits(y=c(0,1.2), x=c(0,2000)) +
  theme_bw() +
  ylab('Effect Size (d)') +
  xlab('Sample Size')

# Plot effect size (d) vs time lag (in days)
p2<- ggplot(df, aes(x=t, y=d))
p2 + geom_point() + geom_text(aes(label=lab),hjust=0, vjust=0) + #,hjust=0,vjust=0)
  expand_limits(y=c(0,1.2), x=c(0,1100)) +
  theme_bw() +
  ylab('Effect Size (d)') +
  xlab('Time lag (days)')

# linear regression of sample size
g1<- lm(d~n, data=df)
summary(g1) #not significant

# linear regression of time lag
g2<- lm(d~t, data=df)
summary(g2) #not significant




