setwd("D:/METOPEL UAS/TALITA METOPEL")
library(readxl)
library(tidyverse)
library(kableExtra)
read_excel("AREN.xlsx")
dat <- read_excel("AREN.xlsx")
kbl(dat) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


# Plot 
# regresi
reg1<-lm(gula~tebu+aren+lebaran,data=dat)
summary(reg1)

dat$m<-resid(reg1)
ggplot(data=dat, aes(x = m, y = gula, shape = as.factor(lebaran))) +
  geom_point(color = "red", size = 1.5)+theme_minimal()

dat$m<-resid(reg1)
ggplot(data=dat, aes(x = m, y = tebu, shape = as.factor(lebaran))) +
  geom_point(color = "navy", size = 1.5)+theme_minimal()

dat$m<-resid(reg1)
ggplot(data=dat, aes(x = m, y = aren, shape = as.factor(lebaran))) +
  geom_point(color = "blue", size = 1.5)+theme_minimal()

## buat variabel baru namanya SS di mana SS=1 kalau S="sakit"
dat$hari.biasa<-ifelse(dat$lebaran=="0",1 , 0) 
reg2<-lm(gula~tebu+aren+hari.biasa, data=dat)
summary(reg2)
