# import datasets
a_df <- read.csv(file = '/PATH/TO/FILE.csv', fileEncoding="UTF-8-BOM")
b_df <- read.csv(file = '/PATH/TO/FILE.csv', fileEncoding="UTF-8-BOM")
c_df <- read.csv(file = '/PATH/TO/FILE.csv', fileEncoding="UTF-8-BOM")
d_df <- read.csv(file = '/PATH/TO/FILE.csv', fileEncoding="UTF-8-BOM")
e_df <- read.csv(file = '/PATH/TO/FILE.csv', fileEncoding="UTF-8-BOM")
f_df <- read.csv(file = '/PATH/TO/FILE.csv', fileEncoding="UTF-8-BOM")
g_df <- read.csv(file = '/PATH/TO/FILE.csv', fileEncoding="UTF-8-BOM")

options(timeout=100)

# import libraries
library(rms)
#devtools::install_github("NightingaleHealth/ggforestplot")
library(ggforestplot)
library(tibble)
library(ggplot2)
library(ggforce)
library(forcats)

# create datadist object
ddist_a = datadist(a_df)
ddist_b = datadist(b_df)
ddist_c = datadist(c_df)
ddist_d = datadist(d_df)
ddist_e = datadist(e_df)
ddist_f = datadist(f_df)
ddist_g = datadist(g_df)
options(datadist_a='ddist')
options(datadist_b='ddist')
options(datadist_c='ddist')
options(datadist_d='ddist')
options(datadist_e='ddist')
options(datadist_f='ddist')
options(datadist_g='ddist')

# create surv object for OS and CSS
os_a <- Surv(time = a_df$SurvivalMonths, event = a_df$Death)
os_b <- Surv(time = b_df$SurvivalMonths, event = b_df$Death)
os_c <- Surv(time = c_df$SurvivalMonths, event = c_df$Death)
os_d <- Surv(time = d_df$SurvivalMonths, event = d_df$Death)
os_e <- Surv(time = e_df$SurvivalMonths, event = e_df$Death)
os_f <- Surv(time = f_df$SurvivalMonths, event = f_df$Death)
os_g <- Surv(time = g_df$SurvivalMonths, event = g_df$Death)

css_a <- Surv(time = a_df$SurvivalMonths, event = a_df$AIHWHNCCauseOfDeath)
css_b <- Surv(time = b_df$SurvivalMonths, event = b_df$AIHWHNCCauseOfDeath)
css_c <- Surv(time = c_df$SurvivalMonths, event = c_df$AIHWHNCCauseOfDeath)
css_d <- Surv(time = d_df$SurvivalMonths, event = d_df$AIHWHNCCauseOfDeath)
css_e <- Surv(time = e_df$SurvivalMonths, event = e_df$AIHWHNCCauseOfDeath)
css_f <- Surv(time = f_df$SurvivalMonths, event = f_df$AIHWHNCCauseOfDeath)
css_g <- Surv(time = g_df$SurvivalMonths, event = g_df$AIHWHNCCauseOfDeath)

# univariate CPH models for OS and CSS

# age
a_cph_age_os <- cph(os_a ~ Age, data = a_df, 
                    x = TRUE, y = TRUE, surv = TRUE)
a_cph_age_os

b_cph_age_os <- cph(os_b ~ Age, data = b_df, 
                    x = TRUE, y = TRUE, surv = TRUE)
b_cph_age_os

c_cph_age_os <- cph(os_c ~ Age, data = c_df, 
                    x = TRUE, y = TRUE, surv = TRUE)
c_cph_age_os

d_cph_age_os <- cph(os_d ~ Age, data = d_df, 
                    x = TRUE, y = TRUE, surv = TRUE)
d_cph_age_os

e_cph_age_os <- cph(os_e ~ Age, data = e_df, 
                    x = TRUE, y = TRUE, surv = TRUE)
e_cph_age_os

f_cph_age_os <- cph(os_f ~ Age, data = f_df, 
                    x = TRUE, y = TRUE, surv = TRUE)
f_cph_age_os

g_cph_age_os <- cph(os_g ~ Age, data = g_df, 
                    x = TRUE, y = TRUE, surv = TRUE)
g_cph_age_os

# gender
table(a_df$Gender)
a_df$Gender <- relevel(factor(a_df$Gender), ref="Male")
a_cph_gender_os <- cph(os_a ~ Gender, data = a_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
a_cph_gender_os

table(b_df$Gender)
b_df$Gender <- relevel(factor(b_df$Gender), ref="Male")
b_cph_gender_os <- cph(os_b ~ Gender, data = b_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
b_cph_gender_os

table(c_df$Gender)
c_df$Gender <- relevel(factor(c_df$Gender), ref="Male")
c_cph_gender_os <- cph(os_c ~ Gender, data = c_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
c_cph_gender_os

table(d_df$Gender)
d_df$Gender <- relevel(factor(d_df$Gender), ref="Male")
d_cph_gender_os <- cph(os_d ~ Gender, data = d_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
d_cph_gender_os

table(e_df$Gender)
e_df$Gender <- relevel(factor(e_df$Gender), ref="Male")
e_cph_gender_os <- cph(os_e ~ Gender, data = e_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
e_cph_gender_os

table(f_df$Gender)
f_df$Gender <- relevel(factor(f_df$Gender), ref="Male")
f_cph_gender_os <- cph(os_f ~ Gender, data = f_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
f_cph_gender_os

table(g_df$Gender)
g_df$Gender <- relevel(factor(g_df$Gender), ref="Male")
g_cph_gender_os <- cph(os_g ~ Gender, data = g_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
g_cph_gender_os

# primary site
table(a_df$TumourSite)
a_df$TumourSite <- relevel(factor(a_df$TumourSite), ref="Oropharynx")
a_cph_site_os <- cph(os_a ~ TumourSite, data = a_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
a_cph_site_os

table(b_df$TumourSite)
b_df$TumourSite <- relevel(factor(b_df$TumourSite), ref="Oropharynx")
b_cph_site_os <- cph(os_b ~ TumourSite, data = b_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
b_cph_site_os

table(c_df$TumourSite)
c_df$TumourSite <- relevel(factor(c_df$TumourSite), ref="Oropharynx")
c_cph_site_os <- cph(os_c ~ TumourSite, data = c_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
c_cph_site_os

table(d_df$TumourSite)
d_df$TumourSite <- relevel(factor(d_df$TumourSite), ref="Oropharynx")
d_cph_site_os <- cph(os_d ~ TumourSite, data = d_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
d_cph_site_os

table(e_df$TumourSite)
e_df$TumourSite <- relevel(factor(e_df$TumourSite), ref="Oropharynx")
e_cph_site_os <- cph(os_e ~ TumourSite, data = e_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
e_cph_site_os

table(f_df$TumourSite)
f_df$TumourSite <- relevel(factor(f_df$TumourSite), ref="Oropharynx")
f_cph_site_os <- cph(os_f ~ TumourSite, data = f_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
f_cph_site_os

table(g_df$TumourSite)
g_df$TumourSite <- relevel(factor(g_df$TumourSite), ref="Oropharynx")
g_cph_site_os <- cph(os_g ~ TumourSite, data = g_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
g_cph_site_os

# T classification
table(a_df$Tclassification)
a_df$Tclassification <- relevel(factor(a_df$Tclassification), ref="T2")
a_cph_tstage_os <- cph(os_a ~ Tclassification, data = a_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
a_cph_tstage_os

table(b_df$Tclassification)
b_df$Tclassification <- relevel(factor(b_df$Tclassification), ref="T2")
b_cph_tstage_os <- cph(os_b ~ Tclassification, data = b_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
b_cph_tstage_os

table(c_df$Tclassification)
c_df$Tclassification <- relevel(factor(c_df$Tclassification), ref="T2")
c_cph_tstage_os <- cph(os_c ~ Tclassification, data = c_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
c_cph_tstage_os

table(d_df$Tclassification)
d_df$Tclassification <- relevel(factor(d_df$Tclassification), ref="T2")
d_cph_tstage_os <- cph(os_d ~ Tclassification, data = d_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
d_cph_tstage_os

#table(e_df$Tclassification)
#e_df$Tclassification <- relevel(factor(e_df$Tclassification), ref="T2")
#e_cph_tstage_os <- cph(os_e ~ Tclassification, data = e_df, 
                     #x = TRUE, y = TRUE, surv = TRUE)
#e_cph_tstage_os

table(f_df$Tclassification)
f_df$Tclassification <- relevel(factor(f_df$Tclassification), ref="T2")
f_cph_tstage_os <- cph(os_f ~ Tclassification, data = f_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
f_cph_tstage_os

table(g_df$Tclassification)
g_df$Tclassification <- relevel(factor(g_df$Tclassification), ref="T2")
g_cph_tstage_os <- cph(os_g ~ Tclassification, data = g_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
g_cph_tstage_os

# N classification
table(a_df$Nclassification)
a_df$Nclassification <- relevel(factor(a_df$Nclassification), ref="N2")
a_cph_nstage_os <- cph(os_a ~ Nclassification, data = a_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
a_cph_nstage_os

table(b_df$Nclassification)
b_df$Nclassification <- relevel(factor(b_df$Nclassification), ref="N1")
b_cph_nstage_os <- cph(os_b ~ Nclassification, data = b_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
b_cph_nstage_os

table(c_df$Nclassification)
c_df$Nclassification <- relevel(factor(c_df$Nclassification), ref="N2")
c_cph_nstage_os <- cph(os_c ~ Nclassification, data = c_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
c_cph_nstage_os

table(d_df$Nclassification)
d_df$Nclassification <- relevel(factor(d_df$Nclassification), ref="N0")
d_cph_nstage_os <- cph(os_d ~ Nclassification, data = d_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
d_cph_nstage_os

#table(e_df$Nclassification)
#e_df$Nclassification <- relevel(factor(e_df$Nclassification), ref="N2")
#e_cph_nstage_os <- cph(os_e ~ Nclassification, data = e_df, 
#                       x = TRUE, y = TRUE, surv = TRUE)
#e_cph_nstage_os

table(f_df$Nclassification)
f_df$Nclassification <- relevel(factor(f_df$Nclassification), ref="N1")
f_cph_nstage_os <- cph(os_f ~ Nclassification, data = f_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
f_cph_nstage_os

table(g_df$Nclassification)
g_df$Nclassification <- relevel(factor(g_df$Nclassification), ref="N2")
g_cph_nstage_os <- cph(os_g ~ Nclassification, data = g_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
g_cph_nstage_os

# EQD2T
a_cph_dose_os <- cph(os_a ~ EQD2T, data = a_df, 
                    x = TRUE, y = TRUE, surv = TRUE)
a_cph_dose_os

b_cph_dose_os <- cph(os_b ~ EQD2T, data = b_df, 
                      x = TRUE, y = TRUE, surv = TRUE)
b_cph_dose_os

c_cph_dose_os <- cph(os_c ~ EQD2T, data = c_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
c_cph_dose_os

d_cph_dose_os <- cph(os_d ~ EQD2T, data = d_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
d_cph_dose_os

e_cph_dose_os <- cph(os_e ~ EQD2T, data = e_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
e_cph_dose_os

f_cph_dose_os <- cph(os_f ~ EQD2T, data = f_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
f_cph_dose_os

g_cph_dose_os <- cph(os_g ~ EQD2T, data = g_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
g_cph_dose_os

# age
a_cph_age_css <- cph(css_a ~ Age, data = a_df, 
                    x = TRUE, y = TRUE, surv = TRUE)
a_cph_age_css

b_cph_age_css <- cph(css_b ~ Age, data = b_df, 
                    x = TRUE, y = TRUE, surv = TRUE)
b_cph_age_css

c_cph_age_css <- cph(css_c ~ Age, data = c_df, 
                    x = TRUE, y = TRUE, surv = TRUE)
c_cph_age_css

d_cph_age_css <- cph(css_d ~ Age, data = d_df, 
                    x = TRUE, y = TRUE, surv = TRUE)
d_cph_age_css

e_cph_age_css <- cph(css_e ~ Age, data = e_df, 
                    x = TRUE, y = TRUE, surv = TRUE)
e_cph_age_css

f_cph_age_css <- cph(css_f ~ Age, data = f_df, 
                    x = TRUE, y = TRUE, surv = TRUE)
f_cph_age_css

g_cph_age_css <- cph(css_g ~ Age, data = g_df, 
                    x = TRUE, y = TRUE, surv = TRUE)
g_cph_age_css

# gender
table(a_df$Gender)
a_df$Gender <- relevel(factor(a_df$Gender), ref="Male")
a_cph_gender_css <- cph(css_a ~ Gender, data = a_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
a_cph_gender_css

table(b_df$Gender)
b_df$Gender <- relevel(factor(b_df$Gender), ref="Male")
b_cph_gender_css <- cph(css_b ~ Gender, data = b_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
b_cph_gender_css

table(c_df$Gender)
c_df$Gender <- relevel(factor(c_df$Gender), ref="Male")
c_cph_gender_css <- cph(css_c ~ Gender, data = c_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
c_cph_gender_css

table(d_df$Gender)
d_df$Gender <- relevel(factor(d_df$Gender), ref="Male")
d_cph_gender_css <- cph(css_d ~ Gender, data = d_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
d_cph_gender_css

table(e_df$Gender)
e_df$Gender <- relevel(factor(e_df$Gender), ref="Male")
e_cph_gender_css <- cph(css_e ~ Gender, data = e_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
e_cph_gender_css

table(f_df$Gender)
f_df$Gender <- relevel(factor(f_df$Gender), ref="Male")
f_cph_gender_css <- cph(css_f ~ Gender, data = f_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
f_cph_gender_css

table(g_df$Gender)
g_df$Gender <- relevel(factor(g_df$Gender), ref="Male")
g_cph_gender_css <- cph(css_g ~ Gender, data = g_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
g_cph_gender_css

# primary site
table(a_df$TumourSite)
a_df$TumourSite <- relevel(factor(a_df$TumourSite), ref="Oropharynx")
a_cph_site_css <- cph(css_a ~ TumourSite, data = a_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
a_cph_site_css

table(b_df$TumourSite)
b_df$TumourSite <- relevel(factor(b_df$TumourSite), ref="Oropharynx")
b_cph_site_css <- cph(css_b ~ TumourSite, data = b_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
b_cph_site_css

table(c_df$TumourSite)
c_df$TumourSite <- relevel(factor(c_df$TumourSite), ref="Oropharynx")
c_cph_site_css <- cph(css_c ~ TumourSite, data = c_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
c_cph_site_css

table(d_df$TumourSite)
d_df$TumourSite <- relevel(factor(d_df$TumourSite), ref="Oropharynx")
d_cph_site_css <- cph(css_d ~ TumourSite, data = d_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
d_cph_site_os

table(e_df$TumourSite)
e_df$TumourSite <- relevel(factor(e_df$TumourSite), ref="Oropharynx")
e_cph_site_css <- cph(css_e ~ TumourSite, data = e_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
e_cph_site_css

table(f_df$TumourSite)
f_df$TumourSite<- relevel(factor(f_df$TumourSite), ref="Oropharynx")
f_cph_site_css <- cph(css_f ~ TumourSite, data = f_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
f_cph_site_css

table(g_df$TumourSite)
g_df$TumourSite <- relevel(factor(g_df$TumourSite), ref="Oropharynx")
g_cph_site_css <- cph(css_g ~ TumourSite, data = g_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
g_cph_site_css

# T classification
table(a_df$Tclassification)
a_df$Tclassification <- relevel(factor(a_df$Tclassification), ref="T2")
a_cph_tstage_css <- cph(css_a ~ Tclassification, data = a_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
a_cph_tstage_css

table(b_df$Tclassification)
b_df$Tclassification <- relevel(factor(b_df$Tclassification), ref="T2")
b_cph_tstage_css <- cph(css_b ~ Tclassification, data = b_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
b_cph_tstage_css

table(c_df$Tclassification)
c_df$Tclassification <- relevel(factor(c_df$Tclassification), ref="T2")
c_cph_tstage_css <- cph(css_c ~ Tclassification, data = c_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
c_cph_tstage_css

table(d_df$Tclassification)
d_df$Tclassification <- relevel(factor(d_df$Tclassification), ref="T2")
d_cph_tstage_css <- cph(css_d ~ Tclassification, data = d_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
d_cph_tstage_css

#table(e_df$Tclassification)
#e_df$Tclassification <- relevel(factor(e_df$Tclassification), ref="T2")
#e_cph_tstage_os <- cph(os_e ~ Tclassification, data = e_df, 
#                       x = TRUE, y = TRUE, surv = TRUE)
#e_cph_tstage_os

table(f_df$Tclassification)
f_df$Tclassification <- relevel(factor(f_df$Tclassification), ref="T2")
f_cph_tstage_css <- cph(css_f ~ Tclassification, data = f_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
f_cph_tstage_css

table(g_df$Tclassification)
g_df$Tclassification <- relevel(factor(g_df$Tclassification), ref="T2")
g_cph_tstage_css <- cph(css_g ~ Tclassification, data = g_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
g_cph_tstage_css

# N classification
table(a_df$Nclassification)
a_df$Nclassification <- relevel(factor(a_df$Nclassification), ref="N2")
a_cph_nstage_css <- cph(css_a ~ Nclassification, data = a_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
a_cph_nstage_css

table(b_df$Nclassification)
b_df$Nclassification <- relevel(factor(b_df$Nclassification), ref="N1")
b_cph_nstage_css <- cph(css_b ~ Nclassification, data = b_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
b_cph_nstage_css

table(c_df$Nclassification)
c_df$Nclassification <- relevel(factor(c_df$Nclassification), ref="N2")
c_cph_nstage_css <- cph(css_c ~ Nclassification, data = c_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
c_cph_nstage_css

table(d_df$Nclassification)
d_df$Nclassification <- relevel(factor(d_df$Nclassification), ref="N0")
d_cph_nstage_css <- cph(css_d ~ Nclassification, data = d_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
d_cph_nstage_css

#table(e_df$Nclassification)
#e_df$Nclassification <- relevel(factor(e_df$Nclassification), ref="N2")
#e_cph_nstage_css <- cph(css_e ~ Nclassification, data = e_df, 
#                       x = TRUE, y = TRUE, surv = TRUE)
#e_cph_nstage_css

table(f_df$Nclassification)
f_df$Nclassification <- relevel(factor(f_df$Nclassification), ref="N1")
f_cph_nstage_css <- cph(css_f ~ Nclassification, data = f_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
f_cph_nstage_css

table(g_df$Nclassification)
g_df$Nclassification <- relevel(factor(g_df$Nclassification), ref="N2")
g_cph_nstage_css <- cph(css_g ~ Nclassification, data = g_df, 
                       x = TRUE, y = TRUE, surv = TRUE)
g_cph_nstage_css

# EQD2T
a_cph_dose_css <- cph(css_a ~ EQD2T, data = a_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
a_cph_dose_css

b_cph_dose_css <- cph(css_b ~ EQD2T, data = b_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
b_cph_dose_css

c_cph_dose_css <- cph(css_c ~ EQD2T, data = c_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
c_cph_dose_css

d_cph_dose_css <- cph(css_d ~ EQD2T, data = d_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
d_cph_dose_css

e_cph_dose_css <- cph(css_e ~ EQD2T, data = e_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
e_cph_dose_css

f_cph_dose_css <- cph(css_f ~ EQD2T, data = f_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
f_cph_dose_css

g_cph_dose_css <- cph(css_g ~ EQD2T, data = g_df, 
                     x = TRUE, y = TRUE, surv = TRUE)
g_cph_dose_css

# multivariable CPH
a_cph_mv_os <- cph(os_a ~ Age + Gender + TumourSite + Tclassification + Nclassification + EQD2T, 
                  data = a_df, 
                  x = TRUE, y = TRUE,
                  se.fit = TRUE,
                  #time.inc = 12,
                  surv = TRUE)

a_cph_mv_os
exp(coef(a_cph_mv_os))
exp(confint(a_cph_mv_os))

b_cph_mv_os <- cph(os_b ~ Age + Gender + TumourSite + Tclassification + Nclassification + EQD2T, 
                   data = b_df, 
                   x = TRUE, y = TRUE,
                   se.fit = TRUE,
                   #time.inc = 12,
                   surv = TRUE)

b_cph_mv_os
exp(coef(b_cph_mv_os))
exp(confint(b_cph_mv_os))

c_cph_mv_os <- cph(os_c ~ Age + Gender + TumourSite + Tclassification + Nclassification + EQD2T, 
                   data = c_df, 
                   x = TRUE, y = TRUE,
                   se.fit = TRUE,
                   #time.inc = 12,
                   surv = TRUE)

c_cph_mv_os
exp(coef(c_cph_mv_os))
exp(confint(c_cph_mv_os))

d_cph_mv_os <- cph(os_d ~ Age + Gender + TumourSite + Tclassification + Nclassification + EQD2T, 
                   data = d_df, 
                   x = TRUE, y = TRUE,
                   se.fit = TRUE,
                   #time.inc = 12,
                   surv = TRUE)

d_cph_mv_os
exp(coef(d_cph_mv_os))
exp(confint(d_cph_mv_os))

e_cph_mv_os <- cph(os_e ~ Age + Gender + TumourSite + EQD2T, 
                   data = e_df, 
                   x = TRUE, y = TRUE,
                   se.fit = TRUE,
                   #time.inc = 12,
                   surv = TRUE)

e_cph_mv_os
exp(coef(e_cph_mv_os))
exp(confint(e_cph_mv_os))

f_cph_mv_os <- cph(os_f ~ Age + Gender + TumourSite + Tclassification + Nclassification + EQD2T, 
                   data = f_df, 
                   x = TRUE, y = TRUE,
                   se.fit = TRUE,
                   #time.inc = 12,
                   surv = TRUE)

f_cph_mv_os
exp(coef(f_cph_mv_os))
exp(confint(f_cph_mv_os))

g_cph_mv_os <- cph(os_g ~ Age + Gender + TumourSite + Tclassification + Nclassification + EQD2T, 
                   data = g_df, 
                   x = TRUE, y = TRUE,
                   se.fit = TRUE,
                   #time.inc = 12,
                   surv = TRUE)

g_cph_mv_os
exp(coef(g_cph_mv_os))
exp(confint(g_cph_mv_os))

a_cph_mv_css <- cph(css_a ~ Age + Gender + TumourSite + Tclassification + Nclassification + EQD2T, 
                   data = a_df, 
                   x = TRUE, y = TRUE,
                   se.fit = TRUE,
                   #time.inc = 12,
                   surv = TRUE)

a_cph_mv_css
exp(coef(a_cph_mv_css))
exp(confint(a_cph_mv_css))

b_cph_mv_css <- cph(css_b ~ Age + Gender + TumourSite + Tclassification + Nclassification + EQD2T, 
                   data = b_df, 
                   x = TRUE, y = TRUE,
                   se.fit = TRUE,
                   #time.inc = 12,
                   surv = TRUE)

b_cph_mv_css
exp(coef(b_cph_mv_css))
exp(confint(b_cph_mv_css))

c_cph_mv_css <- cph(css_c ~ Age + Gender + TumourSite + Tclassification + Nclassification + EQD2T, 
                   data = c_df, 
                   x = TRUE, y = TRUE,
                   se.fit = TRUE,
                   #time.inc = 12,
                   surv = TRUE)

c_cph_mv_css
exp(coef(c_cph_mv_css))
exp(confint(c_cph_mv_css))

d_cph_mv_css <- cph(css_d ~ Age + Gender + TumourSite + Tclassification + Nclassification + EQD2T, 
                   data = d_df, 
                   x = TRUE, y = TRUE,
                   se.fit = TRUE,
                   #time.inc = 12,
                   surv = TRUE)

d_cph_mv_css
exp(coef(d_cph_mv_css))
exp(confint(d_cph_mv_css))

e_cph_mv_css <- cph(css_e ~ Age + Gender + TumourSite + EQD2T, 
                   data = e_df, 
                   x = TRUE, y = TRUE,
                   se.fit = TRUE,
                   #time.inc = 12,
                   surv = TRUE)

e_cph_mv_css
exp(coef(e_cph_mv_css))
exp(confint(e_cph_mv_css))

f_cph_mv_css <- cph(css_f ~ Age + Gender + TumourSite + Tclassification + Nclassification + EQD2T, 
                   data = f_df, 
                   x = TRUE, y = TRUE,
                   se.fit = TRUE,
                   #time.inc = 12,
                   surv = TRUE)

f_cph_mv_css
exp(coef(f_cph_mv_css))
exp(confint(f_cph_mv_css))

g_cph_mv_css <- cph(css_g ~ Age + Gender + TumourSite + Tclassification + Nclassification + EQD2T, 
                   data = g_df, 
                   x = TRUE, y = TRUE,
                   se.fit = TRUE,
                   #time.inc = 12,
                   surv = TRUE)

g_cph_mv_css
exp(coef(g_cph_mv_css))
exp(confint(g_cph_mv_css))

# plot forest plot
df_os <- read.csv(file = '/PATH/TO/FILE.csv', fileEncoding="UTF-8-BOM")
df_hr_os <- as_tibble(df_os)
df_hr_os$Class <- factor(df_hr_os$Class, levels = c("Age", "Gender", "Primary site", "T classification", "N classification", "Radiotherapy"))
df_hr_os$Hospital <- factor(df_hr_os$Hospital, levels = c("Hospital A", "Hospital B", "Hospital C", "Hospital D", "Hospital E", "Hospital F", "Hospital G"))
df_hr_os$Hospital <- factor(df_hr_os$Hospital, levels = rev(levels(df_hr_os$Hospital)))

# create forest plot
ggforestplot::forestplot(
  df = df_hr_os,
  estimate = Coef,
  name = Factor,
  se = SE,
  pvalue = pvalue,
  psignif = 0.05,
  ci = 0.95,
  logodds = TRUE,
  xlab = "Multivariable hazard ratio (95% CI)",
  #title = "",
  #xtickbreaks = c(0.25, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0),
  colour = Hospital
  #shape = Hospital
) + theme(
  legend.position = c(.93, .25),
  legend.background = element_rect(fill = "white", color = "black"),
  legend.key.size = unit(0.1, 'cm')
) + ggforce::facet_col(
  facets = ~Class,
  scales = "free_y",
  space = "free"
) + facet_wrap(
  facets = ~Class, 
  scales = "free_y",
  ncol = 3
) 

df_css <- read.csv(file = '/PATH/TO/FILE.csv', fileEncoding="UTF-8-BOM")
df_hr_css <- as_tibble(df_css)
df_hr_css$Class <- factor(df_hr_css$Class, levels = c("Age", "Gender", "Primary site", "T classification", "N classification", "Radiotherapy"))
df_hr_css$Hospital <- factor(df_hr_css$Hospital, levels = c("Hospital A", "Hospital B", "Hospital C", "Hospital D", "Hospital E", "Hospital F", "Hospital G"))
df_hr_css$Hospital <- factor(df_hr_css$Hospital, levels = rev(levels(df_hr_css$Hospital)))

# create forest plot
ggforestplot::forestplot(
  df = df_hr_css,
  estimate = Coef,
  name = Factor,
  se = SE,
  pvalue = pvalue,
  psignif = 0.05,
  ci = 0.95,
  logodds = TRUE,
  xlab = "Multivariable hazard ratio (95% CI)",
  #title = "",
  #xtickbreaks = c(0.25, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0),
  colour = Hospital
  #shape = Hospital
) + theme(
  legend.position = c(.93, .25),
  legend.background = element_rect(fill = "white", color = "black"),
  legend.key.size = unit(0.1, 'cm')
) + ggforce::facet_col(
  facets = ~Class,
  scales = "free_y",
  space = "free"
) + facet_wrap(
  facets = ~Class, 
  scales = "free_y",
  ncol = 3
)