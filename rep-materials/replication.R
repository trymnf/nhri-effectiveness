#               REPLICATION MATERIAL FOR MASTER THESIS:  
# 
#                   Coping or Hoping Mechanisms
# 
# Evaluating The Effectiveness of National Human Rights Institutions
# 
#                               ### 
# Trym Nohr Fjørtoft 
# 19 December 2016 
#  
# contact: trymnf@gmail.com


# LOADING PACKAGES --------------------------------------------------------

library(panelAR)
library(stargazer)
library(multiwayvcov)
library(rms)
library(lmtest)
library(separationplot)
library(effects)
library(plm)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(Zelig)
library(dplyr)
library(data.table)
library(DataCombine)
library(xtable)
library(pglm)
library(car)
library(VGAM)
library(pander)
library(erer)

library(maps)
library(mapdata)
library(rworldmap)
library(countrycode)
library(ggExtra)
library(RColorBrewer)
library(wesanderson)
library(separationplot)
library(ordinal)

# Importing data ----------------------------------------------------------

dt.na <- read.csv("finaldata-na.csv")
dtny  <- read.csv("dtny-last.csv")  


# Set theme ---------------------------------------------------------------

theme_set(theme_tufte(base_size = 12, base_family = "sans"))


# Baseline models  ---------------------------------------------------------

phys.nhri.plm <- plm(ciri_int.lead ~ nhri + iccprrat +
                       p_polity2 + 
                       leadinterstateconflict + leadinternalconflict + 
                       loggdp + logingo +
                       wdi_trade + logpop +
                       p_durable + LJI + ciri_physint, 
                     data = dt.na, effect = "twoways", 
                     index = c("iso3numeric", "year"))
pn.SEs <- sqrt(diag(plm::vcovHC(phys.nhri.plm, method = "arellano", cluster = "group")))

civ.nhri.plm <- plm(ciri_civ.lead ~ nhri + iccprrat +
                      p_polity2 + 
                      leadinterstateconflict  + leadinternalconflict + 
                      loggdp + logingo +
                      wdi_trade + logpop +
                      p_durable + LJI + ciri_empinx_new, 
                    data = dt.na, effect = "twoways", 
                    index = c("iso3numeric", "year"), x = T, y = T)
civn.SEs <- sqrt(diag(plm::vcovHC(civ.nhri.plm, cluster = "group", method = "arellano")))

phys.accnew.plm <- plm(ciri_int.lead ~ 
                         astatus.new +
                         bstatus.new + 
                         cstatus.new + 
                         iccprrat +
                         p_polity2 + 
                         leadinterstateconflict  + 
                         leadinternalconflict + 
                         loggdp + logingo +
                         wdi_trade + logpop +
                         p_durable + LJI + ciri_physint, 
                       data = dt.na, effect = "twoways", 
                       index = c("iso3numeric", "year"))

paccnew.SEs <- sqrt(diag(vcovHC(phys.accnew.plm, cluster = "group", method = "arellano")))

civ.accnew.plm <- plm(ciri_civ.lead ~ astatus.new + bstatus.new + cstatus.new + iccprrat +
                        p_polity2 + 
                        leadinterstateconflict  + leadinternalconflict + 
                        loggdp + logingo + 
                        wdi_trade + logpop +
                        p_durable + LJI + ciri_empinx_new, 
                      data = dt.na, effect = "twoways", index = c("iso3numeric", "year"))

civaccnew.SEs <- sqrt(diag(plm::vcovHC(civ.accnew.plm, cluster = "group", method = "arellano")))

cedaw.std2 <- lrm(leadwop_red ~ nhri + 
                    + cedawrat + 
                    p_polity2 + logingo +
                    leadinterstateconflict  + leadinternalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + as.numeric(wop_red) + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.na, na.action = "na.omit")

clust.std <- robcov(cedaw.std2, cluster = dt.na$iso3numeric)

cedaw.anew <- lrm(leadwop_red ~ astatus.new + bstatus.new + cstatus.new
                  + cedawrat + 
                    p_polity2 + logingo +
                    leadinterstateconflict  + leadinternalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + wop_red + 
                    nineties +  naughts + tens + west, 
                  y = T, x = T,
                  data = dt.na) 

clust.anew <- robcov(cedaw.anew, cluster = dt.na$iso3numeric)

wecon.nhri.lrm <- lrm(leadwec_red ~ nhri + cedawrat + 
                        p_polity2 + logingo +
                        leadinterstateconflict  + leadinternalconflict + 
                        loggdp + 
                        wdi_trade + logpop +
                        p_durable + LJI + as.numeric(wec_red) + 
                        nineties +  naughts + tens + west, 
                      y = T, x = T,
                      data = dt.na) 
clust.wecon.nhri.lrm <- robcov(wecon.nhri.lrm, cluster = dt.na$iso3numeric)

wecon.a <- lrm(leadwec_red ~ astatus.new + bstatus.new + cstatus.new + cedawrat + 
                 p_polity2 + logingo +
                 leadinterstateconflict  + leadinternalconflict + 
                 loggdp + 
                 wdi_trade + logpop +
                 p_durable + LJI + as.numeric(wec_red) + 
                 nineties +  naughts + tens + west, 
               y = T, x = T,
               data = dt.na) 

clust.wecon.a <- robcov(wecon.a, cluster = dt.na$iso3numeric)


# Table 5.1 ---------------------------------------------------------------

expvar <- c("NHRI$_{t-1}$",
            "A status$_{t-1}$",
            "B status$_{t-1}$",
            "C status$_{t-1}$", 
            "ICCPR ratification$_{t-1}$",  
            "CEDAW ratification$_{t-1}$",
            "Polity IV Democracy$_{t-1}$",
            "Inter-state conflict",
            "Internal conflict",
            "GDP/capita (ln)$_{t-1}$",
            "INGO (ln)$_{t-1}$",
            "Trade, \\% of GDP$_{t-1}$",
            "Population (ln)$_{t-1}$",
            "Regime durability$_{t-1}$",
            "Judicial independence$_{t-1}$",
            "Physical integrity$_{t-1}$", 
            "Civil liberties$_{t-1}$",
            "W. political rights$_{t-1}$",
            "W. economic rights$_{t-1}$",
            "Western country dummy")

stargazer(phys.nhri.plm, phys.accnew.plm, civ.nhri.plm, civ.accnew.plm, clust.std, clust.anew, 
          clust.wecon.nhri.lrm, clust.wecon.a, 
          type  = "text", 
          column.sep.width = "0pt",
          omit  = c("y>", "nineties", "naughts", "tens"),
          se = list(pn.SEs, paccnew.SEs, civn.SEs, civaccnew.SEs),
          add.lines = 
            list(c("Fixed effects\\textsuperscript{a}", "Yes", "Yes", "Yes", "Yes", "No", "No", "No", "No"),
                 c("Decade dummies", "No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes")),
          notes = c("\\textsuperscript{a} Country- and year fixed effects.", 
                    "Standard errors clustered by country (in parentheses)."), 
          title = 
            "Fixed effects and ordered logit estimates for the effect of NHRIs on domestic human rights scores, 1981--2011", 
          label = "base",
          font.size  = "scriptsize",
          no.space = T, align = T,
          dep.var.labels = c("Physical integrity",
                             "Civil liberties",
                             "Women's political", 
                             "Women's economic"),
          dep.var.caption = "\\textit{Dependent variable (rights area):}", 
          float.env = "sidewaystable", 
          omit.stat = c("f", "chi2"), 
          out   = c("table-1-last.tex",
                    "table-1-last.html"),
          covariate.labels = expvar)


getwd()


# Testing AB grouping  ----------------------------------------------------

phys.abnew.plm <- plm(ciri_int.lead ~ 
                        ab_accred + 
                        iccprrat +
                        p_polity2 + 
                        leadinterstateconflict  + 
                        leadinternalconflict + 
                        loggdp + logingo +
                        wdi_trade + logpop +
                        p_durable + LJI + ciri_physint, 
                      data = dt.na, effect = "twoways", 
                      index = c("iso3numeric", "year"))
stargazer(phys.abnew.plm, out = "ab-phys.tex", no.space = T, 
          title = "Accreditation status grouped as A and B")



# Complaints (table 5.2) --------------------------------------------------------------

phys.comp.plm <- plm(ciri_int.lead ~ comp + nocomp + iccprrat +
                       p_polity2 + 
                       leadinterstateconflict  + leadinternalconflict + 
                       loggdp + logingo +
                       wdi_trade + logpop +
                       p_durable + LJI + ciri_physint, 
                     data = dt.na, effect = "twoways", model = "within", 
                     index = c("iso3numeric", "year"))
pcp <- coeftest(phys.comp.plm, vcov=vcovHC(phys.comp.plm, cluster = "group", method = "arellano"))
sepcp <- sqrt(diag(vcovHC(phys.comp.plm, cluster = "group",method = "arellano")))

civ.comp.plm <- plm(ciri_civ.lead ~ comp + nocomp + iccprrat +
                      p_polity2 + 
                      leadinterstateconflict  + leadinternalconflict + 
                      loggdp + logingo +
                      wdi_trade + logpop +
                      p_durable + LJI + ciri_empinx_new, 
                    data = dt.na, effect = "twoways", 
                    index = c("iso3numeric", "year"))
ccp   <- coeftest(civ.comp.plm, vcov=vcovHC(civ.comp.plm, cluster = "group", method = "arellano"))
seccp <- sqrt(diag(vcovHC(civ.comp.plm, cluster = "group", method = "arellano")))

cedaw.comp.west <- lrm(leadwop_red ~ comp + nocomp + cedawrat + 
                         p_polity2 + logingo +
                         leadinterstateconflict  + leadinternalconflict + 
                         loggdp + 
                         wdi_trade + logpop +
                         p_durable + LJI + as.numeric(wop_red) + west + 
                         nineties +  naughts + tens, 
                       y = T, x = T,
                       data = dt.na) 
wop.comp.west.clust <-  robcov(cedaw.comp.west, cluster = dt.na$iso3numeric)


# With western country-control
wecon.comp.west <- lrm(leadwec_red ~ comp + nocomp + cedawrat + 
                         p_polity2 + logingo +
                         leadinterstateconflict  + leadinternalconflict + 
                         loggdp + 
                         wdi_trade + logpop +
                         p_durable + LJI + as.numeric(wec_red) + 
                         nineties +  naughts + tens + west,  
                       y = T, x = T,
                       data = dt.na) 
wec.comp.west.clust <-  robcov(wecon.comp.west, cluster = dt.na$iso3numeric)

rms::vif(wec.comp.west.clust)
table(dt.na$west, dt.na$nocomp)


## TABLE OUT: COMPLAINTS MECHANISM ## 

labelscomp <- c("Punishment$_{t-1}$", 
                "No punishment$_{t-1}$",
                "ICCPR ratification$_{t-1}$",  
                "CEDAW ratification$_{t-1}$",
                "Polity IV Democracy$_{t-1}$",
                "Inter-state conflict",
                "Internal conflict",
                "GDP/capita (ln)$_{t-1}$",
                "INGO (ln)$_{t-1}$",
                "Trade, \\% of GDP$_{t-1}$",
                "Population (ln)$_{t-1}$",
                "Regime durability$_{t-1}$",
                "Judicial independence$_{t-1}$",
                "Physical integrity$_{t-1}$",
                "Civil liberties$_{t-1}$",
                "Women's political$_{t-1}$",
                "Women's economic$_{t-1}$",
                "Western country dummy")

stargazer(phys.comp.plm, civ.comp.plm, wop.comp.west.clust, wec.comp.west.clust, 
          se = list(sepcp, seccp),
          type  = "text",
          omit  = c("y>", "nineties", "naughts", "tens"),
          notes = c("\\textsuperscript{a} Country- and year fixed effects.", 
                    "Intercepts not reported. Standard errors clustered on country (in parentheses)"),
          title = "Fixed effects and ordinal logistic regression of the effect of NHRI complaints procedure on human rights scores",
          add.lines = list(c("Fixed effects\\textsuperscript{a}", "Yes", "Yes", "No", "No"),
                           c("Decade dummies", "No", "No", "Yes", "Yes")),
          out  = "complaints.tex",
          no.space = T, align = T,
          font.size = "footnotesize", 
          label = "complaints", 
          dep.var.labels = c("Physical integrity",
                             "Civil liberties",
                             "Women's political",
                             "Women's economic"),
          dep.var.caption = "Dependent variable (Rights area):", 
          covariate.labels = labelscomp)


# Empty cells (table 5.3) -------------------------------------------------

pander(table(dt.na$wop_red, dt.na$nocomp))

# Without No-Complaints dummy (Appendix table A 3) ------------------------

wop.comp.test <- lrm(leadwop_red ~ comp + cedawrat + 
                       p_polity2 + logingo +
                       leadinterstateconflict  + leadinternalconflict + 
                       loggdp + 
                       wdi_trade + logpop +
                       p_durable + LJI + as.numeric(wop_red) + 
                       nineties +  naughts + tens + west,
                     data = dt.na) 

wecon.comp.test <- lrm(leadwec_red ~ comp + cedawrat + 
                         p_polity2 + logingo +
                         leadinterstateconflict  + leadinternalconflict + 
                         loggdp + 
                         wdi_trade + logpop +
                         p_durable + LJI + as.numeric(wec_red) + 
                         nineties +  naughts + tens + west,
                       data = dt.na) 


stargazer(wop.comp.test, wecon.comp.test, type = "text",
          out = "womens-wo-nocomp.tex", 
          title = "Models without 'No Complaints'-dummy, women's political and economic rights", 
          label = "wo-nocomp", 
          column.sep.width = "0pt",
          no.space = T, 
          font.size = "small")


# Punishment (table 5.4) --------------------------------------------------------------

phys.punish.plm <- plm(ciri_int.lead ~ punish + nopunish + iccprrat +
                         p_polity2 + 
                         leadinterstateconflict  + leadinternalconflict + 
                         loggdp + logingo +
                         wdi_trade + logpop +
                         p_durable + LJI + ciri_physint, 
                       data = dt.na, effect = "twoways", 
                       index = c("iso3numeric", "year"))
ppp   <- coeftest(phys.punish.plm, vcov=vcovHC(phys.punish.plm, cluster = "group", method = "arellano"))
seppp <- sqrt(diag(vcovHC(phys.punish.plm, cluster = "group", method = "arellano")))

civ.punish.plm <- plm(ciri_civ.lead ~ punish + nopunish + iccprrat +
                        p_polity2 + 
                        leadinterstateconflict  + leadinternalconflict + 
                        loggdp + logingo +
                        wdi_trade + logpop +
                        p_durable + LJI + ciri_empinx_new, 
                      data = dt.na, effect = "twoways", 
                      index = c("iso3numeric", "year"))
cpp   <- coeftest(civ.punish.plm, vcov=vcovHC(civ.punish.plm, cluster = "group", method = "arellano"))
secpp <- sqrt(diag(vcovHC(civ.punish.plm, cluster = "group", method = "arellano")))

cedaw.punish <- lrm(leadwop_red ~ punish + nopunish + cedawrat + 
                      p_polity2 + logingo +
                      leadinterstateconflict  + leadinternalconflict + 
                      loggdp + 
                      wdi_trade + logpop +
                      p_durable + LJI + as.numeric(wop_red) + 
                      nineties +  naughts + tens + west, 
                    y = T, x = T,
                    data = dt.na) 
wop.punish.clust <- robcov(cedaw.punish, cluster = dt.na$iso3numeric)

wecon.punish <- lrm(leadwec_red ~ punish + nopunish + cedawrat + 
                      p_polity2 + logingo +
                      leadinterstateconflict  + leadinternalconflict + 
                      loggdp + 
                      wdi_trade + logpop + 
                      p_durable + LJI + as.numeric(wec_red) + 
                      nineties +  naughts + tens + west,
                    y = T, x = T,
                    data = dt.na) 
wec.punish.clust <-  robcov(wecon.punish, cluster = dt.na$iso3numeric)

## TABLE OUT: PUNISHMENT ## 

labelspun <- c("Punishment$_{t-1}$", "No punishment$_{t-1}$", labelscomp[-c(1,2)])

stargazer(phys.punish.plm, civ.punish.plm, wop.punish.clust, wec.punish.clust, 
          se  = list(seppp, secpp),
          type  = "text",
          omit  = c("y>", "nineties", "naughts", "tens"),
          notes = c("\\textsuperscript{a} Country- and year fixed effects.", 
                    "Intercepts omitted from table. Standard errors clustered on country (in parentheses)"),
          title = "Fixed effects and ordinal logistic regression of the effect of NHRI punishment capacity on human rights scores",
          add.lines = list(c("Fixed effects\\textsuperscript{a}", "Yes", "Yes", "No", "No"),
                           c("Decade dummies", "No", "No", "Yes", "Yes")),
          out  = "punishment.tex",
          no.space = T, align = T, 
          font.size = "footnotesize", 
          label = "punishment",
          dep.var.labels = c("Physical integrity",
                             "Civil liberties",
                             "Women's political",
                             "Women's economic"),
          dep.var.caption = "Dependent variable (Rights area):", 
          covariate.labels = labelspun) 


# Marginal Effects (table 5.5 ) --------------------------------------------------------


cedaw.apolr <- polr(as.factor(leadwop_red) ~ astatus.new + bstatus.new + cstatus.new
                    + cedawrat + 
                      p_polity2 + logingo +
                      leadinterstateconflict  + leadinternalconflict + 
                      loggdp + 
                      wdi_trade + logpop +
                      p_durable + LJI + wop_red + 
                      nineties +  naughts + tens + west, 
                    data = dt.na, method = "logistic", Hess = TRUE) 
# Eqivalent to lrm model cedaw.a

marginaleffs <- ocME(cedaw.apolr, rev.dum = F)
margeffAST <- rbind(marginaleffs$out$ME.0[1,],
                    marginaleffs$out$ME.1[1,],
                    marginaleffs$out$ME.2[1,])

rownames(margeffAST) <- c("0", "1","2")
pander(cbind(margeffAST))


# Women’s rights Fixed Effects --------------------------------------------

#  Making additive variable 

dt.na$leadwomsam <- as.numeric(as.character(dt.na$lead.ciri_wopol)) + as.numeric(as.character(dt.na$lead.ciri_wecon))
dt.na$womsam <- dt.na$ciri_wopol + dt.na$ciri_wecon

# NHRI
cedaw.plm <- plm(leadwomsam  ~ nhri + 
                   + cedawrat + 
                   p_polity2 + logingo +
                   leadinterstateconflict  + leadinternalconflict + 
                   loggdp + 
                   wdi_trade + logpop +
                   p_durable + LJI + womsam +
                   nineties + naughts + tens, 
                 y = T, x = T,
                 data = dt.na, effect = "individual", model = "within", index = c("iso3numeric", "year"))

cedwses <- sqrt(diag(plm::vcovHC(cedaw.plm, cluster = "group", method = "arellano")))

# A status

astat.plm <- plm(leadwomsam  ~ astatus + bstatus + cstatus + 
                   + cedawrat + 
                   p_polity2 + logingo +
                   leadinterstateconflict  + leadinternalconflict + 
                   loggdp + 
                   wdi_trade + logpop +
                   p_durable + LJI + womsam +
                   nineties + naughts + tens, 
                 y = T, x = T,
                 data = dt.na, effect = "individual", model = "within", index = c("iso3numeric", "year"))



astses <- sqrt(diag(plm::vcovHC(astat.plm, cluster = "group", method = "arellano")))


labelsfix  <- c("NHRI$_{t-1}$", 
                "A status$_{t-1}$",
                "B status$_{t-1}$",
                "C status$_{t-1}$",
                "CEDAW ratification$_{t-1}$",
                "Polity IV Democracy$_{t-1}$",
                "Inter-state conflict",
                "Internal conflict",
                "GDP/capita (ln)$_{t-1}$",
                "INGO (ln)$_{t-1}$",
                "Trade, \\% of GDP$_{t-1}$",
                "Population (ln)$_{t-1}$",
                "Regime durability$_{t-1}$",
                "Judicial independence$_{t-1}$",
                "Women's rights$_{t-1}$",
                "1990s",
                "2000s", 
                "2010s")

stargazer(cedaw.plm, astat.plm, type = "text", omit = "iso3numeric", 
          out = "rob-wom-fixeff.tex", 
          title = "Fixed effects OLS regression on combined women's rights index", 
          label = "womfixeff", 
          covariate.labels = labelsfix, 
          se = list(cedwses, astses), 
          font.size = "footnotesize",
          no.space = TRUE,
          column.sep.width = "0pt",
          align = TRUE,
          notes = c("Fixed effects OLS estimation with country-clustered", 
                    "Arellano-type robust standard errors (in parentheses)"))


# Hausman Test (table 5.7) ------------------------------------------------

# Random Effects alternative
civ.re <- plm(ciri_civ.lead ~ nhri + iccprrat +
                p_polity2 + 
                leadinterstateconflict  + leadinternalconflict + 
                loggdp + INGO_final +
                wdi_trade + logpop +
                p_durable + LJI + ciri_empinx_new, 
              data = dt.na, model = "random", 
              index = c("iso3numeric", "year"))

civptest <- phtest(civ.nhri.plm, civ.re) # Outcome from hausman test clearly shows that a FE model is preferred. 
pander(civptest, caption = "Hausman test of Random vs Fixed Effects")


# Prais-Winsten -----------------------------------------------------------

# Making function to print table
artable <- function(obj, file, caption, coefnames){
  coefs <- obj$coefficients
  ses <- sqrt(diag(obj$vcov))
  
  longresults <- matrix(NA,nrow=length(obj$coefficients)*2,ncol=2)
  coef_place <- seq(1,nrow(longresults),2)
  se_place <- seq(2,nrow(longresults),2)
  
  longresults[coef_place,2] <- round(coefs,3)
  longresults[coef_place,1] <- coefnames
  longresults[se_place,2] <- paste0("(",round(ses,3),")")
  longresults[se_place,1] <- " "
  longresults <- rbind(longresults,
                       cbind("N",length(resid(obj))),
                       cbind("R2",round(obj$r2,2)))
  colnames(longresults) <- c("Coefficient", "Beta (se)")
  
  print(xtable(longresults,align="llr", caption = caption),
        hline.after=c(0,nrow(longresults)-2),
        file=file,include.rownames = FALSE)
  longresults
}

ar1.alt <- panelAR(ciri_int.lead ~ nhri + p_polity2 + 
                     iccprrat +
                     leadinterstateconflict  + internalconflict + 
                     loggdp + logingo + 
                     wdi_trade + logpop +
                     p_durable + coldwar, 
                   data = dt.na, panelVar = "iso3numeric", timeVar = "year",
                   panelCorrMethod = "pcse", autoCorr = "psar1",
                   rhotype = "scorr")


coefs <- ar1.alt$coefficients
ses <- sqrt(diag(ar1.alt$vcov))

longresults <- matrix(NA,nrow=length(ar1.alt$coefficients)*2,ncol=2)
coef_place <- seq(1,nrow(longresults),2)
se_place <- seq(2,nrow(longresults),2)

longresults[coef_place,2] <- round(coefs,3)
longresults[coef_place,1] <- names(ar1.alt$coefficients)
longresults[se_place,2] <- paste0("(",round(ses,3),")")
longresults[se_place,1] <- " "
longresults <- rbind(longresults,
                     cbind("N",length(resid(ar1.alt))),
                     cbind("R2",round(ar1.alt$r2,2)))
colnames(longresults) <- c("Coefficient", "Beta (se)")


civ.nhri.panelar <- panelAR(ciri_civ.lead ~ nhri + p_polity2 + 
                              iccprrat +
                              leadinterstateconflict  + internalconflict + 
                              loggdp + logingo + 
                              wdi_trade + logpop +
                              p_durable + coldwar, 
                            data = dt.na, panelVar = "iso3numeric", timeVar = "year",
                            panelCorrMethod = "pcse", autoCorr = "psar1",
                            rhotype = "scorr")

civlong <- artable(obj = civ.nhri.panelar, file = "civ-panelar.tex",
                   caption = "Civil Liberties, AR(1)", coefnames = c("(Intercept)",
                                                                     "NHRI",
                                                                     "Polity IV Democracy",
                                                                     "ICCPR ratification",
                                                                     "Inter-state conf.",
                                                                     "Internal conf.",
                                                                     "GDP/capita (ln)",
                                                                     "INGO membership (ln)",
                                                                     "Trade (% of GDP)",
                                                                     "Population (ln)",
                                                                     "Regime durability",
                                                                     "Cold war"))

togetherresults <- cbind(civlong, longresults[,2])

stargazer(togetherresults, out = "sammen_ar1_stargazer.tex", type = "text",
          notes = c("Results from a two-step Prais-Winsten Feasible Generalized Least Squares (FGLS)", 
                    "with panel-specific AR(1) type autocorrelation coefficients. Panel-corrected standard errors",
                    "robust to heteroskedasticity- and contemporaneous correlation (in parentheses)"),
          title = "AR(1) Prais-Winsten models",
          label = "ar1pw", 
          header = F, 
          font.size = "footnotesize",
          column.sep.width = "0pt")



# MAKING FIGURES ----------------------------------------------------------


# Fig 2.1: Map ------------------------------------------------------------


#join data to a map
dtny$yearformal[dtny$iso3numeric == 250] <- 1947

tilkart <- subset(dtny, year == 2011, select = c("iso3numeric", "cname", "yearformal", "iccstatus.new", "nhri"))
tilkart2 <- subset(dtny, year == 1999, select = c("iso3numeric", "cname", "yearformal", "iccstatus.new", "nhri"))

tilkart$iso3alpha <- countrycode(tilkart$iso3numeric, "iso3n", "iso3c")
tilkart2$iso3alpha <- countrycode(tilkart2$iso3numeric, "iso3n", "iso3c")

tilkart$decade <- ifelse(tilkart$yearformal < 1970, "< 1970s", NA)
tilkart$decade <- ifelse(tilkart$yearformal >= 1970 & tilkart$yearformal <1980, "1970s", tilkart$decade)
tilkart$decade <- ifelse(tilkart$yearformal >= 1980 & tilkart$yearformal <1990, "1980s", tilkart$decade)
tilkart$decade <- ifelse(tilkart$yearformal >= 1990 & tilkart$yearformal <2000, "1990s", tilkart$decade)
tilkart$decade <- ifelse(tilkart$yearformal >= 2000 & tilkart$yearformal <2010, "2000s", tilkart$decade)
tilkart$decade <- ifelse(tilkart$yearformal >= 2010, "2010s", tilkart$decade)


tilkart$decade <- as.factor(tilkart$decade)


gtdMap <- joinCountryData2Map( tilkart, 
                               nameJoinColumn="iso3alpha", 
                               joinCode="ISO3", verbose = T)

gtdMap2 <- joinCountryData2Map( tilkart2, 
                                nameJoinColumn="iso3alpha", 
                                joinCode="ISO3", verbose = T)

decadescale <- rev(brewer.pal(6, "Greens"))
mypalette<-brewer.pal(9,"Greens")
decadescale2 <- rev(mypalette)[1:6]
image(1:9,1,as.matrix(1:9),col=decadescale2,xlab="Greens (sequential)",
      ylab="",xaxt="n",yaxt="n",bty="n")
sPDF <- gtdMap[-which(getMap()$ADMIN=="Antarctica"),]
sPDF <- spTransform(sPDF, CRS=CRS("+proj=robin +ellps=WGS84"))

png(file = "nhri-establ-map.png", width = 8, height = 5, units = "in", res = 300)
par(mar=c(0,0,1,0), cex = 0.9, bty= "n")
mapParams <- mapCountryData( sPDF, 
                             nameColumnToPlot='decade', 
                             catMethod='categorical', 
                             numCats=100, colourPalette = decadescale2, 
                             addLegend = F, mapTitle = "")

# mapParams$legendTitle <- "Decade"
do.call(addMapLegendBoxes, c(mapParams,x='bottom',title="Decade established",horiz=TRUE, bty = "n"))

dev.off()


# Fig 2.2. ----------------------------------------------------------------

office.typ <- table(dtny$officetype, dtny$year)

molty <- melt(office.typ)

stack <- ggplot(molty, aes(Var2, value, color = as.factor(Var1), pch= as.factor(Var1)))


pdf(file = "officetype.pdf", width = 8, height = 4)
stack +  geom_vline(xintercept = 1991, col = "grey10", lty = 3) + 
  geom_point(size = 2) + geom_line() + theme_tufte(base_family = "sans", base_size = 12) + 
  scale_color_brewer(palette = "Set1", name = "Office type",
                     labels = c("Ombudsperson", "Human Rights\n Commission")) + 
  scale_shape_tableau(name = "Office type",
                      labels = c("Ombudsperson", "Human Rights\n Commission"),
                      palette = "filled") + theme(legend.position="none") + 
  xlab("Year") + ylab("No. of institutions") + 
  annotate("text", x = 1996, y = 50, label = "Human Rights\nCommission", hjust = 0) + 
  annotate("text", x = 2007, y = 36, label = "Human Rights\nOmbudsperson") + 
  annotate("text", x = 1990.5, y = 36, label = "1991: Paris Principles\n adopted", hjust = 1)
dev.off()


# Fig. 2.3  ---------------------------------------------------------------

# Making GGPLOT stacked barcharts -----------------------------------------

NHRIsum <- tapply(dtny$nhri,
                  dtny$year, FUN=sum, na.rm=F)

molt.sum <- melt(NHRIsum)
par(cex = 0.8, bty = "n", mfrow=c(1,1))

# New Plot 
NHRIcat.new <-  table(dtny$iccstatus.newer, 
                      dtny$year)
NHRIcat.new <- NHRIcat.new[-nrow(NHRIcat.new),]
moltcat <- melt(NHRIcat.new)

NHRIcat.sum <-  colSums(NHRIcat.new)
molt.sum.2 <- melt(NHRIcat.sum)
molt.sum.2$Var1 <- rownames(molt.sum.2)

mermol <- merge(moltcat, molt.sum.2, by.x = "Var2", by.y = "Var1")

pal.bar <- rev(brewer.pal(4, "RdYlGn"))
pal.bar[5] <- "white"

pal.bar.wes <- wes_palette("Darjeeling", 5, "discrete")
barplot(c(1:5), col = pal.bar.wes)

# par(mfrow=c(3,1), mar=c(4,4,1,1))

catgg <- ggplot(mermol, aes(Var2, value.x, fill = Var1)) + 
  geom_bar(stat = "identity", aes(y = value.y), fill = "grey85", position = "dodge", width = 1) +
  geom_bar(stat = "identity") + 
  theme_tufte(base_family = "sans", base_size = 12) + 
  scale_fill_brewer(palette = "YlGn", direction = -1, 
                    guide = guide_legend(reverse=TRUE, title = "Accreditation status")) + 
  annotate("text", x = 1998, y = 80, label ="Total NHRIs", hjust = 1) + ylab("No. of institutions") + 
  xlab("Year")

comp <- table(dtny$complaint, 
              dtny$year)
rownames(comp) <- c("No", "Yes")

compmolt <- melt(comp)
mercomp <- merge(comp, molt.sum.2, by.x = "Var2", by.y = "Var1")
mercomp$Var2 <- as.character(mercomp$Var2)
mercomp$Var2 <- as.numeric(mercomp$Var2)

compgg <- ggplot(mercomp, aes(Var2, Freq, fill = Var1)) + 
  geom_bar(stat = "identity", aes(y = value), fill = "grey85", position = "dodge", width = 1) +
  geom_bar(stat = "identity") + 
  theme_tufte(base_family = "sans", base_size = 12) + 
  scale_fill_brewer(type = "div", palette = 7, direction = 1, 
                    guide = guide_legend(reverse=TRUE, title = "Individual complaints")) + 
  annotate("text", x = 1998, y = 80, label ="Total NHRIs", hjust = 1) + 
  ylab("No. of institutions") + 
  xlab("Year")

estby <- table(dtny$establishedby, dtny$year)
rownames(estby) <- c("Treaty", "Constitution", "Executive Decree", "Legistlation", "Judiciary")

estmolt <- melt(estby)
merest <- merge(estmolt, molt.sum.2, by.x = "Var2", by.y = "Var1")

estgg <- ggplot(merest, aes(Var2, value.x, fill = Var1)) +
  geom_bar(stat = "identity", aes(y = value.y), fill = "grey85", position = "dodge", width = 1) +
  geom_bar(stat = "identity") + theme_tufte(base_family = "sans", base_size = 12) + 
  scale_fill_brewer(type = "qual", palette = 3, direction = -1, 
                    guide = guide_legend(reverse=TRUE, title = "Established by")) + 
  annotate("text", x = 1998, y = 80, label ="Total NHRIs", hjust = 1) + 
  ylab("No. of institutions") + 
  xlab("Year")
estgg

gA <- ggplotGrob(catgg)
gB <- ggplotGrob(compgg)
gC <- ggplotGrob(estgg)

pdf(file = "accstat-ggplot.pdf", width = 8, height = 8)
grid::grid.newpage()
grid::grid.draw(rbind(gA, gB, gC))
dev.off() 

# Note: Due to updates in ggplot, the direction of the bars are reversed, 
# but the content is the same as in the original figure. 



# Fig 4.1 (population) ----------------------------------------------------

pdf(file = "pop-hist.pdf", width = 8, height = 3)
plot1 <- qplot(dtny$wdi_pop, fill = I("darkred"), xlab = "Population", geom = "density", bw = "SJ") + 
  ylab("Estimated density") + geom_rug(size = .1, alpha = .5)
plot2 <- qplot(log(dtny$wdi_pop), fill = I("skyblue"), xlab = "log(Population)", geom = "density", bw = "SJ") + 
  ylab("Estimated density") + geom_rug(size =.1, alpha = .5)
grid.arrange(plot1, plot2, ncol=2)
dev.off()


# Fig 4.2 (INGO) ----------------------------------------------------------

rect <- data.frame(xmin=1985, xmax=1987, ymin=-Inf, ymax=Inf)

ingA <- ggplot(dtny, aes(x = year, y = INGO_final_old, group = iso3numeric))
ing1 <- ingA + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                         fill="red",
                         alpha=0.1,
                         inherit.aes = FALSE) + geom_line(alpha = 0.2) + theme_minimal() + 
  scale_color_discrete(guide = F) + ylab(NULL) + 
  geom_smooth(aes(group = 1), fill = "blue") + labs(caption = "Original 1986")

ingB <- ggplot(dtny, aes(x = year, y = INGO_final_86, group = iso3numeric))
ing2 <- ingB + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                         fill="red",
                         alpha=0.1,
                         inherit.aes = FALSE) + 
  geom_line(alpha = 0.2) + 
  scale_color_discrete(name = "NHRI", labels = c("No", "Yes")) + 
  ylab(NULL) + 
  geom_smooth(aes(group = 1), fill = "blue") + labs(caption = "Interpolated 1986")

ing3 <- ggMarginal(ing2, margins = "y", size = 10, bw = "SJ")

ingosam <- grid.arrange(ing1, ing3, ncol=2, left = "# of INGOs")
ggsave(ingosam, filename = "ingo-summary.pdf", width = 8,height = 3)



# Fig 4.3 (Liberia) -------------------------------------------------------

qplot(dt.na$year[dt.na$cname == "Liberia"], dt.na$ciri_physint[dt.na$cname == "Liberia"], geom = "line") + 
  #  geom_line(data = subset(dt.na, cname == "Liberia"), aes(x = year, y = ciri_empinx_new), linetype = 2) +
  geom_vline(xintercept = 2003, linetype = 3) + xlab("Year") + ylab("CIRI Physical Integrity") + 
  ggtitle("Liberia") + annotate("text", x = 2002.5, y = 6, label = "2003:\n End of civil war", hjust = 1) + 
  #  annotate("text", x = 2011, y = 10, label = "Civil\nliberties", hjust = 0) + 
  #  annotate("text", x = 2011, y = 5, label = "Physical\nintegrity", hjust = 0) + 
  ylim(0,8) + geom_rug(col = "darkred", sides = "b")

ggsave(filename = "influential-liberia.pdf", width = 4.5, height = 2.5) 


# Fig 5.1  ----------------------------------------------------------------

pdf(file = "rights-time.pdf", width = 6, height = 12)

par(mar = c(5,5,3,5), bty = "n", mfrow = c(3,1))
meanwopol <- tapply(dtny$ciri_wopol, dtny$year, FUN = mean, na.rm = T)
sdwopol <- tapply(dtny$ciri_wopol, dtny$year, FUN = sd, na.rm = T)
Nwopol <- with(dtny, aggregate(ciri_wopol, list(year), function(x) { sum(!is.na(x)) }))
sewopol <- sdwopol/sqrt(Nwopol[,2])

plot(names(meanwopol), meanwopol, type = "n", ylim = c(0,3), lty = 1, lwd = 2, 
     col = "darkred", main = "Women's rights, 1980-2011", 
     ylab = "CIRI Women's rights", xlab = "Year")

for(i in 1:length(meanwopol)){ segments(x0 = as.numeric(names(meanwopol[i])), 
                                        y0 = (meanwopol[i] + (1.96*sewopol[i])), 
                                        y1 = (meanwopol[i] - (1.96 * sewopol[i])),
                                        lwd = 2, col = "black") 
}

points(names(meanwopol), meanwopol, type = "p", ylim = c(0,3), lty = 1, lwd = 2,
       pch = 21,
       col = "darkred", bg = "white", main = "Women's rights, 1980-2014", 
       ylab = "Mean score, CIRI", xlab = "Year")

meanwecon <- tapply(dtny$ciri_wecon, dtny$year, FUN = mean, na.rm = T)
sdwecon <- tapply(dtny$ciri_wecon, dtny$year, FUN = sd, na.rm = T)
Nwecon <- with(dtny, aggregate(ciri_wecon, list(year), function(x) { sum(!is.na(x)) }))
sewecon <- sdwecon/sqrt(Nwecon[,2])

for(i in 1:length(meanwecon)){ segments(x0 = as.numeric(names(meanwecon[i])), 
                                        y0 = (meanwecon[i] + (1.96*sewecon[i])), 
                                        y1 = (meanwecon[i] - (1.96 * sewecon[i])),
                                        lwd = 2, col = "black") 
}

points(names(meanwecon), meanwecon, type = "p", ylim = c(0,3), lty = 1, lwd = 2, 
       pch = 21, col = "skyblue", bg = "white")

par(new = T)
plot(names(NHRIsum), NHRIsum, type = "l", ylim = c(0,200), axes = F, ylab = NA, xlab = NA,
     lwd = 2, lty = 2)
par(new = T)
CEDAWsum <- tapply(dtny$cedawrat, dtny$year, FUN = sum, na.rm =T)
plot(names(CEDAWsum), CEDAWsum, type = "l", ylim = c(0,200), axes = F, ylab = NA, xlab = NA, 
     lwd = 2, lty = 2, col = "darkgrey")
axis(side = 4, lwd = 1, lty = 2)
mtext(side = 4, line = 3, "Number of states")
par(cex = 0.8)
text(2015, 155, "Avg. CIRI Women's\n political rights", pos = 2, col = "darkred")
text(2015, 200, "Global CEDAW ratification", pos = 2, col = "darkgrey")
text(1990, 10,"Global NHRI adoption", pos = 4) 
text(2015, 50, "Avg. CIRI Women's\n economic rights", pos = 2, col = "black")

# Mean ICCPR --------------------------------------------------------------

meaniccpr <- tapply(dtny$ciri_physint, dtny$year, FUN = mean, na.rm = T)
sdiccpr <- tapply(dtny$ciri_physint, dtny$year, FUN = sd, na.rm = T)
Niccpr <- with(dtny, aggregate(ciri_physint, list(year), function(x) { sum(!is.na(x)) }))
seiccpr <- sdiccpr/sqrt(Niccpr[,2])

plot(names(meaniccpr), meaniccpr, type = "n", ylim = c(0,8), lty = 1, lwd = 2, pch = 16,
     col = "darkred", main = "Physical integrity rights, 1980-2011",
     ylab = "CIRI Physical Integrity Rights", xlab = "Year")

for(i in 1:length(meaniccpr)){ segments(x0 = as.numeric(names(meaniccpr[i])), 
                                        y0 = (meaniccpr[i] + (1.96*seiccpr[i])), 
                                        y1 = (meaniccpr[i] - (1.96 * seiccpr[i])),
                                        lwd = 2, col = "black") 
}

points(names(meaniccpr), meaniccpr, type = "p", ylim = c(0,8), lty = 1, lwd = 2, pch = 21,
       col = "darkred", bg="white", main = "Physical integrity rights, 1980-2014",
       ylab = "CIRI Physical Integrity Rights", xlab = "Year")

x <- line(meaniccpr)

par(new = T)
plot(names(NHRIsum), NHRIsum, type = "l", ylim = c(0,200), axes = F, ylab = NA, xlab = NA,
     lwd = 2, lty = 2)
par(new = T)
ICCPRsum <- tapply(dtny$iccprrat, dtny$year, FUN = sum, na.rm =T)
plot(names(ICCPRsum), ICCPRsum, type = "l", ylim = c(0,200), axes = F, ylab = NA, xlab = NA, 
     lwd = 2, lty = 2, col = "darkgrey")
axis(side = 4, lwd = 1, lty = 2)
mtext(side = 4, line = 3, "Number of states")
par(cex = 0.8)
text(1980, 152, "Avg. CIRI physical\n integrity rights", pos = 4, col = "darkred")
text(2015, 175, "Global ICCPR ratification", pos = 2, col = "darkgrey")
text(2015, 90,"Global NHRI adoption", pos = 2) 

# Mean empinx -------------------------------------------------------------

meanempinx_new <- tapply(dtny$ciri_empinx_new, dtny$year, FUN = mean, na.rm = T)
sdempinx_new <- tapply(dtny$ciri_empinx_new, dtny$year, FUN = sd, na.rm = T)
Nempinx_new <- with(dtny, aggregate(ciri_empinx_new, list(year), function(x) { sum(!is.na(x)) }))
seempinx_new <- sdempinx_new/sqrt(Nempinx_new[,2])

plot(names(meanempinx_new), meanempinx_new, type = "n", ylim = c(0,14), lty = 1, lwd = 2, 
     col = "darkred", main = "Civil and political rights 1980-2011", 
     ylab = "CIRI Empowerment index", xlab = "Year")

for(i in 1:length(meanempinx_new)){ segments(x0 = as.numeric(names(meanempinx_new[i])), 
                                             y0 = (meanempinx_new[i] + (1.96*seempinx_new[i])), 
                                             y1 = (meanempinx_new[i] - (1.96 * seempinx_new[i])),
                                             lwd = 2, col = "black") 
}

points(names(meanempinx_new), meanempinx_new, type = "p", ylim = c(0,14), lty = 1, lwd = 2,
       pch = 21,
       col = "darkred", bg="white", main = "Civil and political rights 1980-2011", 
       ylab = "Mean score, CIRI", xlab = "Year")

par(new = T)
plot(names(NHRIsum), NHRIsum, type = "l", ylim = c(0,200), axes = F, ylab = NA, xlab = NA,
     lwd = 2, lty = 2)
par(new = T)
ICCPRsum <- tapply(dtny$iccprrat, dtny$year, FUN = sum, na.rm =T)
plot(names(ICCPRsum), ICCPRsum, type = "l", ylim = c(0,200), axes = F, ylab = NA, xlab = NA, 
     lwd = 2, lty = 2, col = "darkgrey")
axis(side = 4, lwd = 1, lty = 2)
mtext(side = 4, line = 3, "Number of states")
par(cex = 0.8)
text(1980, 152, "Avg. CIRI civil and\n political rights", pos = 4, col = "darkred")
text(2015, 175, "Global ICCPR ratification", pos = 2, col = "darkgrey")
text(2015, 90,"Global NHRI adoption", pos = 2) 

dev.off()



# Fig 5.2 (NHRI) ----------------------------------------------------------

pdf(file = "sammen-time.pdf", width = 8, height = 6)
par(bty = "n", mfrow=c(2,2), mar = c(4.5,4.5,4,2), cex = 0.8)

wopol.nhri <- tapply(dtny$ciri_wopol[dtny$nhri == 1], dtny$year[dtny$nhri == 1], FUN = mean, na.rm = T)
sdwopol <- tapply(dtny$ciri_wopol[dtny$nhri == 1], dtny$year[dtny$nhri == 1], FUN = sd, na.rm = T)
Nwopol <- with(dtny, aggregate(ciri_wopol[dtny$nhri == 1], list(year[dtny$nhri == 1]), function(x) { sum(!is.na(x)) }))
sewopol <- sdwopol/sqrt(Nwopol[,2])

plot(names(wopol.nhri), wopol.nhri, type = "n", ylim = c(0,3), lwd = 3, col = "darkred",
     ylab = "CIRI Women's political rights", xlab = "Year", 
     main = "CIRI women's political rights and NHRIs")

for(i in 1:length(wopol.nhri)){ segments(x0 = as.numeric(names(wopol.nhri[i])), 
                                         y0 = (wopol.nhri[i] + (1.96*sewopol[i])), 
                                         y1 = (wopol.nhri[i] - (1.96 * sewopol[i])),
                                         lwd = 2, col = "darkgrey") 
}

points(names(wopol.nhri), wopol.nhri, ylim = c(0,3), col = "darkred", pch = 21, bg ="white")

wopol.nhri0 <- tapply(dtny$ciri_wopol[dtny$nhri == 0], dtny$year[dtny$nhri == 0], FUN = mean, na.rm = T)
sdwopol0 <- tapply(dtny$ciri_wopol[dtny$nhri == 0], dtny$year[dtny$nhri == 0], FUN = sd, na.rm = T)
# Niccpr <- for(i in c(1981:2015)){print(sum(!is.na(dtny$ciri_wopol[dtny$year == i])))}
Nwopol0 <- with(dtny, aggregate(ciri_wopol[dtny$nhri == 0], list(year[dtny$nhri == 0]), function(x) { sum(!is.na(x)) }))
sewopol0 <- sdwopol0/sqrt(Nwopol0[,2])

for(i in 1:length(wopol.nhri0)){ segments(x0 = as.numeric(names(wopol.nhri0[i])), 
                                          y0 = (wopol.nhri0[i] + (1.96*sewopol0[i])), 
                                          y1 = (wopol.nhri0[i] - (1.96 * sewopol0[i])),
                                          lwd = 2, col = "darkgrey") 
}

points(names(wopol.nhri0), wopol.nhri0, ylim = c(0,3), col = "skyblue", pch = 21, bg ="white")

text(2012, 1.2, "Countries w/o NHRI", pos = 2, col = "black")
text(2012, 2.5, "Countries with NHRI", pos = 2, col = "darkred")


# WECON NHRI -------------------------------------------------------------------


wecon.nhri <- tapply(dtny$ciri_wecon[dtny$nhri == 1], dtny$year[dtny$nhri == 1], FUN = mean, na.rm = T)
sdwecon <- tapply(dtny$ciri_wecon[dtny$nhri == 1], dtny$year[dtny$nhri == 1], FUN = sd, na.rm = T)
Nwecon <- with(dtny, aggregate(ciri_wecon[dtny$nhri == 1], list(year[dtny$nhri == 1]), function(x) { sum(!is.na(x)) }))
sewecon <- sdwecon/sqrt(Nwecon[,2])

plot(names(wecon.nhri), wecon.nhri, type = "n", ylim = c(0,3), lwd = 3, col = "darkred",
     ylab = "CIRI Women's economic rights", xlab = "Year", 
     main = "CIRI women's economic rights and NHRIs")

# for(i in 1:length(wecon.nhri)){ segments(x0 = as.numeric(names(wecon.nhri[i])), 
#                                          y0 = (wecon.nhri[i] + (1.96*sewecon[i])), 
#                                          y1 = (wecon.nhri[i] - (1.96 * sewecon[i])),
#                                          lwd = 2, col = "darkgrey") 
# }

lines(names(wecon.nhri), wecon.nhri, ylim = c(0,3), col = "darkred", pch = 21, bg ="white", lwd = 3)

wecon.nhri0 <- tapply(dtny$ciri_wecon[dtny$nhri == 0], dtny$year[dtny$nhri == 0], FUN = mean, na.rm = T)
sdwecon0 <- tapply(dtny$ciri_wecon[dtny$nhri == 0], dtny$year[dtny$nhri == 0], FUN = sd, na.rm = T)
# Niccpr <- for(i in c(1981:2015)){print(sum(!is.na(dtny$ciri_wecon[dtny$year == i])))}
Nwecon0 <- with(dtny, aggregate(ciri_wecon[dtny$nhri == 0], list(year[dtny$nhri == 0]), function(x) { sum(!is.na(x)) }))
sewecon0 <- sdwecon0/sqrt(Nwecon0[,2])

# for(i in 1:length(wecon.nhri0)){ segments(x0 = as.numeric(names(wecon.nhri0[i])), 
#                                           y0 = (wecon.nhri0[i] + (1.96*sewecon0[i])), 
#                                           y1 = (wecon.nhri0[i] - (1.96 * sewecon0[i])),
#                                           lwd = 2, col = "darkgrey") 
# }

lines(names(wecon.nhri0), wecon.nhri0, ylim = c(0,3), col = "skyblue", pch = 21, bg ="white", lwd = 3)

text(1985, .9, "Countries w/o NHRI", pos = 4, col = "black")
text(1985, 2.3, "Countries with NHRI", pos = 4, col = "darkred")

# NHRI vs no NHRI -- ICCPR ---------------------------------------------------------
# par(opar)
# pdf(file = "Arbeidsfiler/physint-time.pdf", width = 10, height = 4)
# par(bty = "n", mfcol=c(1,2), mar = c(4.5,4.5,4,2))
physint.nhri <- tapply(dtny$ciri_physint[dtny$nhri == 1], dtny$year[dtny$nhri == 1], FUN = mean, na.rm = T)
plot(names(physint.nhri), physint.nhri, type = "l", ylim = c(0,8), lwd = 3, col = "darkred", pch = 20,
     ylab = "CIRI physical integrity rights", xlab = "Year", 
     main = "CIRI physical integrity rights and NHRIs")

physint.nhri0 <- tapply(dtny$ciri_physint[dtny$nhri == 0], dtny$year[dtny$nhri == 0], FUN = mean, na.rm = T)
lines(names(physint.nhri0), physint.nhri0, type = "l", ylim = c(0,8), lwd = 3, col = "skyblue")

text(1995, 2, "Countries w/o NHRI", pos = 3, col = "black")
arrows(1995, 3, 1995, 4.3, lwd = 2, col = "black", length = 0.15)
text(1995, 7, "Countries with NHRI", pos = 3, col = "darkred")
arrows(1995, 7, 1995, 5.5, lwd = 2, col = "darkred", length = 0.15)


# Empinx NHRI ------------------------------------------------------------

empinx_new.nhri <- tapply(dtny$ciri_empinx_new[dtny$nhri == 1], dtny$year[dtny$nhri == 1], FUN = mean, na.rm = T)
sdempinx_new.nhri <- tapply(dtny$ciri_empinx_new[dtny$nhri == 1], dtny$year[dtny$nhri == 1], FUN = sd, na.rm = T)
Nempinx_new.nhri <- with(dtny, aggregate(ciri_empinx_new[dtny$nhri == 1], list(year[dtny$nhri == 1]), function(x) { sum(!is.na(x)) }))
seempinx_new.nhri <- sdempinx_new.nhri/sqrt(Nempinx_new.nhri[,2])

empinx_new.nhri0 <- tapply(dtny$ciri_empinx_new[dtny$nhri == 0], dtny$year[dtny$nhri == 0], FUN = mean, na.rm = T)
sdempinx_new.nhri0 <- tapply(dtny$ciri_empinx_new[dtny$nhri == 0], dtny$year[dtny$nhri == 0], FUN = sd, na.rm = T)
Nempinx_new.nhri0 <- with(dtny, aggregate(ciri_empinx_new[dtny$nhri == 0], list(year[dtny$nhri == 0]), function(x) { sum(!is.na(x)) }))
seempinx_new.nhri0 <- sdempinx_new.nhri/sqrt(Nempinx_new.nhri[,2])


plot(names(empinx_new.nhri), empinx_new.nhri, type = "n", ylim = c(0,14), lwd = 3, col = "darkred", pch = 20,
     ylab = "CIRI civil and political rights", xlab = "Year", 
     main = "CIRI civil and political rights and NHRIs")

for(i in 1:length(empinx_new.nhri)){ segments(x0 = as.numeric(names(empinx_new.nhri[i])), 
                                              y0 = (empinx_new.nhri[i] + (1.96*seempinx_new.nhri[i])), 
                                              y1 = (empinx_new.nhri[i] - (1.96 * seempinx_new.nhri[i])),
                                              lwd = 2, col = "darkgrey") 
}

for(i in 1:length(empinx_new.nhri0)){ segments(x0 = as.numeric(names(empinx_new.nhri0[i])), 
                                               y0 = (empinx_new.nhri0[i] + (1.96*seempinx_new.nhri0[i])), 
                                               y1 = (empinx_new.nhri0[i] - (1.96 * seempinx_new.nhri0[i])),
                                               lwd = 2, col = "darkgrey") 
}

points(names(empinx_new.nhri), empinx_new.nhri, type = "p", ylim = c(0,14), 
       col = "darkred", bg = "white", pch = 21,
       ylab = "CIRI civil and political rights", xlab = "Year", 
       main = "CIRI civil and political rights and NHRIs")


points(names(empinx_new.nhri0), empinx_new.nhri0, type = "p", ylim = c(0,14),
       col = "skyblue", bg = "white", pch = 21)

text(1995, 4, "Countries w/o NHRI", pos = 3, col = "black")
# arrows(1995, 3, 1995, 4.3, lwd = 2, col = "skyblue", length = 0.15)
text(2000, 12, "Countries with NHRI", pos = 3, col = "darkred")
# arrows(1995, 13, 1995, 11, lwd = 2, col = "darkred", length = 0.15)


dev.off()


# Fig 5.3 (accstat) -------------------------------------------------------
pal.acc.lines <- c("darkblue", "indianred1", "black")

pdf(file = "accstat-new-lines.pdf", width = 8.2, height = 6.5, pointsize = 11)
par(mfcol = c(2,2), bty = "n")
wopol.nhriA <- tapply(dtny$ciri_wopol[dtny$iccstatus.newer == "A"], dtny$year[dtny$iccstatus.newer == "A"],
                      FUN = mean, na.rm = T)
labtext <- "Women's political rights"
plot(names(wopol.nhriA), wopol.nhriA, type = "l", ylim = c(0,3), lwd = 3, 
     col = pal.acc.lines[1], xlim = c(1980, 2013), xlab = "Year", ylab = labtext, main = labtext)


wopol.nhriNA <- tapply(dtny$ciri_wopol[dtny$iccstatus.newer == "No Accreditation" | dtny$iccstatus.newer == "C"], 
                       dtny$year[dtny$iccstatus.newer == "No Accreditation" | dtny$iccstatus.newer == "C"], FUN = mean, na.rm = T)
# lines(names(wopol.nhriNA), wopol.nhriNA, type = "l", ylim = c(0,3), lwd = 2, col = pal.acc.lines[3], lty = 1)

wopol.nhriNO <- tapply(dtny$ciri_wopol[dtny$nhri == 0], dtny$year[dtny$nhri == 0], FUN = mean, na.rm = T)
lines(names(wopol.nhriNO), wopol.nhriNO, type = "l", ylim = c(0,3), lwd = 3, col = pal.acc.lines[2], lty = 1)

lines(names(wopol.nhri), wopol.nhri, type = "l", ylim = c(0,3), lwd = 1, col = pal.acc.lines[3], lty = 3)

legend(col = pal.acc.lines, 
       legend = c("A status", "No NHRI", "All NHRI"),
       lty = c(1, 1, 3), x = "bottomleft", lwd = 2, bty  = "n")
abline(v = 1998, lty = 2, col = "grey50")
text(1998, 2.5, "1998:\nAccreditation\nstarted", pos = 2)

physint.nhriA <- tapply(dtny$ciri_physint[dtny$iccstatus.newer == "A"], dtny$year[dtny$iccstatus.newer == "A"],
                        FUN = mean, na.rm = T)
labtext <- "Physical integrity rights"
plot(names(physint.nhriA), physint.nhriA, type = "l", ylim = c(0,8), lwd = 3, 
     col = "darkblue", xlim = c(1980, 2013), xlab = "Year", ylab = labtext, main = labtext)

physint.nhriB <- tapply(dtny$ciri_physint[dtny$iccstatus.newer == "B"], dtny$year[dtny$iccstatus.newer == "B"], FUN = mean, na.rm = T)
# lines(names(physint.nhriB), physint.nhriB, type = "l", ylim = c(0,8), lwd = 1, col = "red", lty = 2)

physint.nhriNA <- tapply(dtny$ciri_physint[dtny$iccstatus.newer == "No Accreditation" | dtny$iccstatus.newer == "C"], 
                         dtny$year[dtny$iccstatus.newer == "No Accreditation" | dtny$iccstatus.newer == "C"], FUN = mean, na.rm = T)
# lines(names(physint.nhriNA), physint.nhriNA, type = "l", ylim = c(0,8), lwd = 1, col = "green", lty = 3)

physint.nhriNO <- tapply(dtny$ciri_physint[dtny$nhri == 0], dtny$year[dtny$nhri == 0], FUN = mean, na.rm = T)
lines(names(physint.nhriNO), physint.nhriNO, type = "l", ylim = c(0,8), lwd = 3, col = "indianred1", lty = 1)

lines(names(physint.nhri), physint.nhri, type = "l", ylim = c(0,8), lwd = 1, col = "black", lty = 3)

legend(col = c("darkblue",  "indianred1", "black"), 
       legend = c("A status", "No NHRI", "All NHRI"),
       lty = c(1, 1, 3), x = "bottomleft", lwd = 2, bty  = "n")
abline(v = 1998, lty = 2, col = "grey50")
text(1998, 7, "1998:\nAccreditation\nstarted", pos = 2)

wecon.nhriA <- tapply(dtny$ciri_wecon[dtny$iccstatus.newer == "A"], dtny$year[dtny$iccstatus.newer == "A"],
                      FUN = mean, na.rm = T)
labtext <- "Women's economic rights"
plot(names(wecon.nhriA), wecon.nhriA, type = "l", ylim = c(0,3), lwd = 3, 
     col = "darkblue", xlim = c(1980, 2013), xlab = "Year", ylab = labtext, main = labtext)

wecon.nhriB <- tapply(dtny$ciri_wecon[dtny$iccstatus.newer == "B"], dtny$year[dtny$iccstatus.newer == "B"], FUN = mean, na.rm = T)
# lines(names(wecon.nhriB), wecon.nhriB, type = "l", ylim = c(0,3), lwd = 1, col = "red", lty = 2)

wecon.nhriNA <- tapply(dtny$ciri_wecon[dtny$iccstatus.newer == "No Accreditation" | dtny$iccstatus.newer == "C"], 
                       dtny$year[dtny$iccstatus.newer == "No Accreditation" | dtny$iccstatus.newer == "C"], FUN = mean, na.rm = T)
# lines(names(wecon.nhriNA), wecon.nhriNA, type = "l", ylim = c(0,3), lwd = 2, col = "green", lty = 1)

wecon.nhriNO <- tapply(dtny$ciri_wecon[dtny$nhri == 0], dtny$year[dtny$nhri == 0], FUN = mean, na.rm = T)
lines(names(wecon.nhriNO), wecon.nhriNO, type = "l", ylim = c(0,3), lwd = 3, col = "indianred1", lty = 1)

wecon.nhriALL <- tapply(dtny$ciri_wecon[dtny$nhri == 1], dtny$year[dtny$nhri == 1], FUN = mean, na.rm = T)
lines(names(wecon.nhriALL), wecon.nhriALL, type = "l", ylim = c(0,3), lwd = 1, col = "black", lty = 3)

legend(col = c("darkblue", "indianred1", "black"), 
       legend = c("A status", "No NHRI", "All NHRI"),
       lty = c(1, 1, 3), x = "bottomleft", lwd = 2, bty  = "n")
abline(v = 1998, lty = 2, col = "grey50")
text(1998, 2.5, "1998:\nAccreditation\nstarted", pos = 2)

# CivPol

empinx_new.nhriA <- tapply(dtny$ciri_empinx_new[dtny$iccstatus.newer == "A"], dtny$year[dtny$iccstatus.newer == "A"],
                           FUN = mean, na.rm = T)
labtext <- "Civil and political rights"
plot(names(empinx_new.nhriA), empinx_new.nhriA, type = "l", ylim = c(0,14), lwd = 3, 
     col = "darkblue", xlim = c(1980, 2013), xlab = "Year", ylab = labtext, main = labtext)

empinx_new.nhriB <- tapply(dtny$ciri_empinx_new[dtny$iccstatus.newer == "B"], dtny$year[dtny$iccstatus.newer == "B"], FUN = mean, na.rm = T)
# lines(names(empinx_new.nhriB), empinx_new.nhriB, type = "l", ylim = c(0,14), lwd = 1, col = "red", lty = 2)

empinx_new.nhriNA <- tapply(dtny$ciri_empinx_new[dtny$iccstatus.newer == "No Accreditation" | dtny$iccstatus.newer == "C"], 
                            dtny$year[dtny$iccstatus.newer == "No Accreditation" | dtny$iccstatus.newer == "C"], FUN = mean, na.rm = T)
# lines(names(empinx_new.nhriNA), empinx_new.nhriNA, type = "l", ylim = c(0,14), lwd = 2, col = "green", lty = 1)

empinx_new.nhriNO <- tapply(dtny$ciri_empinx_new[dtny$nhri == 0], dtny$year[dtny$nhri == 0], FUN = mean, na.rm = T)
lines(names(empinx_new.nhriNO), empinx_new.nhriNO, type = "l", ylim = c(0,14), lwd = 3, col = "indianred1", lty = 1)

empinx.nhriALL <- tapply(dtny$ciri_empinx_new[dtny$nhri == 1], dtny$year[dtny$nhri == 1], FUN = mean, na.rm = T)
lines(names(empinx.nhriALL), empinx.nhriALL, type = "l", ylim = c(0,3), lwd = 1, col = "black", lty = 3)

legend(col = c("darkblue", "indianred1", "black"), 
       legend = c("A status", "No NHRI", "All NHRI"),
       lty = c(1, 1, 3), x = "bottomleft", lwd = 2, bty  = "n")
abline(v = 1998, lty = 2, col = "grey50")
text(1998, 13, "1998: Accreditation\nstarted", pos = 2)

dev.off()



# Fig 5.4  ----------------------------------------------------------------
# Thanks to Øyvind Stiansen for the simulation code


civ.nhri.lm <- lm(ciri_civ.lead ~ nhri + iccprrat +
                    p_polity2 + 
                    leadinterstateconflict  + internalconflict + 
                    loggdp + logingo +
                    wdi_trade + logpop +
                    p_durable + LJI + ciri_empinx_new + 
                    as.factor(year) + as.factor(iso3numeric), 
                  data = dt.na, x = T)

# Using lm model because the plm model doesn't work as well with the simulation functions

simb <- mvrnorm(n=1000, 
                mu=civ.nhri.lm$coefficients, 
                Sigma=vcovHC(civ.nhri.lm, cluster = dt.na$iso3numeric, method = "arellano"))

# the next step is to choose a variable to study the effect of
# we are interested in llngdppc and will look at the predicted
# democracy depending on where on the range on this variable 
# a country is. 

range.nhri <-seq(min(civ.nhri.lm$x[,"nhri"]),
                 max(civ.nhri.lm$x[,"nhri"]), by=1)

## the third step is to defining our values on the other variables

snitt <-  apply(civ.nhri.lm$x, 2, mean)

## generating a matrix where each row contains all the values of median, but nhri varies
set.x <-NA
for( i in 1:length(range.nhri)){
  set.x <-rbind(set.x, snitt) 
}
set.x <-set.x[2:nrow(set.x),]
set.x[,2] <- range.nhri

# we can then multiply the simulated betas with 
# our set of x-values. 

x.beta <-  set.x %*% t(simb)

x.df <- x.beta
rownames(x.df) <- c(0,1)
x.df <- melt(x.df)

ggplot(x.df, aes(x = value, fill = factor(Var1))) + geom_density(alpha = .7, bw = "SJ")
# Interesting alternative way to see the predicted values


### Because we have an ols model expected y = beta*x, so: 

exp.y <-x.beta ##In other models, you will have to replace this with a formula


## We can use the median value of y in each row as the expected value, and then the variation
## around that median as our 95-% confidence intervals: 
quantile.values <- apply(X = exp.y, MARGIN = 1, FUN = quantile, probs = c(.025,.5,.975))

plot.points<- cbind(range.nhri, t(quantile.values))

### Finally: we can make a nice figure: 

plot.points <- data.frame(plot.points)


pdf(file = "civlib-predict.pdf", width = 7, height = 4)

gem2 <- ggplot(plot.points, aes(x = factor(range.nhri), y = X50., ymin = X2.5., ymax = X97.5.)) + 
  geom_hline(yintercept = plot.points[1,2], lty = 2, alpha = .8) +
  geom_rect(ymin = 8.5, ymax = 9.5, xmin = -Inf, xmax = Inf,
            fill = "grey", alpha = 0.1) + 
  geom_point(aes(color = factor(range.nhri))) + 
  geom_errorbar(aes(color = factor(range.nhri)), width = .4) + ylim(0,14) + 
  xlab("NHRI") + ylab("Civil Liberties score") + 
  scale_color_brewer(palette = "Set1") + 
  guides(color=F) + ggtitle("Full: Y[0,14]")

gem3 <- gem2 + coord_cartesian(ylim = c(8.5, 9.5)) + ggtitle(label =  "Y[8.5,9.5]")

grid.arrange(gem2, gem3,  nrow = 1) 

dev.off()


# Fig 5.5  ----------------------------------------------------------------

ci1 <- confint.default(clust.std, level = 0.90)

oddsrat1 <- exp(cbind(OR = coef(clust.std), ci1))
oddsrat1 <- as.data.frame(oddsrat1)
colnames(oddsrat1) <- c("OR", "low", "high")

oddsrat1$expvar <- ifelse(oddsrat1$low > 1, "darkgreen", "black")
oddsrat1$expvar <- ifelse(oddsrat1$high < 1, "darkred", oddsrat1$expvar)
oddsrat1$OR <- round(oddsrat1$OR, 3)
oddsrat1 # 1.5 times higher odds to be in a higher category
rownames(oddsrat1) <- c("1", "2",
                        "NHRI presence",
                        "CEDAW ratification",
                        "Democracy", 
                        "INGO",
                        "Inter-state conflict",
                        "Internal conflict",
                        "ln(GDP/capita)",
                        "Trade, % of GDP",
                        "ln(Population) (thousands)",
                        "Regime durability",
                        "Judicial independence", 
                        "Women's political rights",
                        "1990s",
                        "2000s",
                        "2010s",
                        "Western country")

oddsrat1 <- oddsrat1[-c(1,2, 14 ,15,16,17,18),]

ci <- confint.default(clust.anew, level = 0.90)

oddsrat <- exp(cbind(OR = coef(clust.anew), ci))
oddsrat <- as.data.frame(oddsrat)
colnames(oddsrat) <- c("OR", "low", "high")

oddsrat$expvar <- ifelse(oddsrat$low > 1, "darkgreen", "black")
oddsrat$expvar <- ifelse(oddsrat$high < 1, "darkred", oddsrat$expvar)
oddsrat$OR <- round(oddsrat$OR, 3)
oddsrat # Astatus 1.85 times more likely to be in a higher category. 
rownames(oddsrat) <- c("1", "2", 
                       "A status",
                       "B status", 
                       "C status",
                       "CEDAW ratification",
                       "Polity IV Democracy",
                       "INGO",
                       "Inter-state conflict",
                       "Internal conflict",
                       "ln(GDP/capita)",
                       "Trade, % of GDP",
                       "ln(Population) (thousands)",
                       "Regime durability",
                       "Judicial independence", 
                       "Women's political rights",
                       "1990s",
                       "2000s",
                       "2010s", 
                       "Western country")

oddsrat <- oddsrat[-c(1,2,17,18,19,20,21),]

samm <- rbind(oddsrat["A status",], 
              oddsrat["B status",], 
              oddsrat["C status",] , 
              oddsrat1["NHRI presence",])

nhri.acc <- ggplot(samm, aes(x = factor(rownames(samm), levels = c(
  "C status", 
  "B status", 
  "A status",
  "NHRI presence"), ordered = T), 
  y = OR, 
  ymin = low, 
  ymax = high, 
  color = expvar,
  label = OR)) + 
  geom_hline(yintercept = 1, lty = 2, alpha = .6) +
  geom_pointrange() + coord_flip(ylim = c(0,4)) +
  theme(legend.position="none") + scale_color_manual(values = c("black", "darkgreen", "darkred")) +
  xlab("Coefficients") + ylab("Odds Ratio") + 
  geom_text(vjust = 0, nudge_x = .23)

ggsave("coef-nhri-acc.pdf", width = 8, height = 4)



# Fig 5.6  ----------------------------------------------------------------

snitt2 <-  apply(clust.anew$x, 2, mean)

snitt3 <- t(data.frame(snitt2))
snitt3 <- snitt3[rep(seq_len(nrow(snitt3)), 2),]
rownames(snitt3) <- 1:nrow(snitt3)

newdat <- data.frame(
  astatus.new = c(0,1))
newdat <- cbind(newdat, snitt3[,-1])

newdat <- cbind(newdat, predict(clust.anew, newdat, type = "fitted"))

`y>=0` <- 1 - (newdat$`y>=1` + newdat$`y>=2`)
newdat <- cbind(newdat, `y>=0`)

lnewdat <- data.table::melt(newdat, id.vars = c(1:18), 
                            variable.name = "Level", value.name="Probability")

lnewdat$Level <- ordered(lnewdat$Level, levels = c("y>=0", "y>=1", "y>=2"))
lnewdat$Level <-factor(lnewdat$Level, levels=rev(levels(lnewdat$Level)))

ggplot(lnewdat, aes(x = astatus.new, y = Probability, fill = Level, label = round(Probability, 3))) +
  geom_bar(stat = "identity") + scale_x_continuous(breaks=c(0,1), labels = c("No", "Yes")) + 
  xlab("A Status") + ylab("Cumulative Probability of Response") + 
  scale_fill_brewer(type = "div", 
                    palette = "RdYlGn", labels = c("Y = 2",
                                                   "Y = 1",
                                                   "Y = 0"),
                    name = "Response\nlevels") 

ggsave(filename = "predprob-astat-new.pdf", width = 6, height = 3.5)



# Time lags (figure 5.7 and tables in Appendix) --------------------------------------------------------------

### MAKING LAGGED DATASET ### 
dt.nalags <- plm.data(dt.na, indexes = c("iso3numeric", "year"))

# save(dl, file = "savedl.RData")

dt.nalags$nhri1 <- lag(dt.nalags$nhri, n = 1)
dt.nalags$nhri2 <- lag(dt.nalags$nhri, n = 2)
dt.nalags$nhri3 <- lag(dt.nalags$nhri, n = 3)
dt.nalags$nhri4 <- lag(dt.nalags$nhri, n = 4)
dt.nalags$nhri5 <- lag(dt.nalags$nhri, n = 5)
dt.nalags$nhri6 <- lag(dt.nalags$nhri, n = 6)
dt.nalags$nhri7 <- lag(dt.nalags$nhri, n = 7)

dt.nalags$astatus1 <- lag(dt.nalags$astatus.new, n = 1)
dt.nalags$astatus2 <- lag(dt.nalags$astatus.new, n = 2)
dt.nalags$astatus3 <- lag(dt.nalags$astatus.new, n = 3)
dt.nalags$astatus4 <- lag(dt.nalags$astatus.new, n = 4)
dt.nalags$astatus5 <- lag(dt.nalags$astatus.new, n = 5)
dt.nalags$astatus6 <- lag(dt.nalags$astatus.new, n = 6)
dt.nalags$astatus7 <- lag(dt.nalags$astatus.new, n = 7)

dt.nalags <- slide(dt.nalags, Var = "ciri_wopol", TimeVar = "year", GroupVar = "iso3numeric", 
                   slideBy = -1, NewVar = "wop.lag")
dt.nalags <- slide(dt.nalags, Var = "ciri_physint", TimeVar = "year", GroupVar = "iso3numeric", 
                   slideBy = -1, NewVar = "phys.lag")
dt.nalags <- slide(dt.nalags, Var = "ciri_empinx_new", TimeVar = "year", GroupVar = "iso3numeric", 
                   slideBy = -1, NewVar = "civ.lag")
dt.nalags <- slide(dt.nalags, Var = "ciri_wecon", TimeVar = "year", GroupVar = "iso3numeric", 
                   slideBy = -1, NewVar = "wec.lag")


dt.nalags$leadwop_dum <- car::recode(dt.nalags$lead.ciri_wopol, "c(0,1)=0; c(2, 3)=1")
dt.nalags$leadwec_dum <- car::recode(dt.nalags$lead.ciri_wecon, "c(0,1)=0; c(2,3)=1")
dt.nalags$leadwop_red <- car::recode(dt.nalags$lead.ciri_wopol, "c(0,1)=0; 2=1; 3=2")
dt.nalags$wop_red <- car::recode(dt.nalags$ciri_wopol, "c(0,1)=0; 2=1; 3=2")
dt.nalags$leadwec_red <- car::recode(dt.nalags$lead.ciri_wecon, "c(0,1)=0; 2=1; 3=2")
dt.nalags$wec_red <- car::recode(dt.nalags$ciri_wecon, "c(0,1)=0; 2=1; 3=2")
dt.nalags$woplag_red  <- car::recode(dt.nalags$wop.lag, "c(0,1)=0; 2=1; 3=2")
dt.nalags$weclag_red  <- car::recode(dt.nalags$wec.lag, "c(0,1)=0; 2=1; 3=2")

dt.nalags$leadwop_dum <- as.numeric(as.character(dt.nalags$leadwop_dum))
dt.nalags$leadwec_dum <- as.numeric(as.character(dt.nalags$leadwec_dum))
dt.nalags$leadwop_red <- factor(dt.nalags$leadwop_red, ordered = T)
dt.nalags$wop_red     <- factor(dt.nalags$wop_red, ordered = T)
dt.nalags$leadwec_red <- factor(dt.nalags$leadwec_red, ordered = T)
dt.nalags$wec_red     <- factor(dt.nalags$wec_red, ordered = T)


dt.nalags <- na.omit(dt.nalags)

### MAKING MODELS ### 

laglabs <- paste0(rep("$t-",9), 0:8, "$")

physlag0 <- plm(ciri_physint ~ nhri + iccprrat + # Tilsvarer t-2 (fordi lead-DV)
                  p_polity2 + 
                  interstateconflict  + internalconflict + 
                  loggdp + logingo +
                  wdi_trade + logpop +
                  p_durable + LJI + phys.lag, 
                data = dt.nalags, effect = "twoways", 
                index = c("iso3numeric", "year"))

physlag1 <- plm(ciri_int.lead ~ nhri + iccprrat + # Tilsvarer t-2 (fordi lead-DV)
                  p_polity2 + 
                  leadinterstateconflict  + leadinternalconflict + 
                  loggdp + logingo +
                  wdi_trade + logpop +
                  p_durable + LJI + ciri_physint, 
                data = dt.nalags, effect = "twoways", 
                index = c("iso3numeric", "year"))

physlag2 <- plm(ciri_int.lead ~ nhri1 + iccprrat + # Tilsvarer t-2 (fordi lead-DV)
                  p_polity2 + 
                  leadinterstateconflict  + leadinternalconflict + 
                  loggdp + logingo +
                  wdi_trade + logpop +
                  p_durable + LJI + ciri_physint, 
                data = dt.nalags, effect = "twoways", 
                index = c("iso3numeric", "year"))

physlag3 <- plm(ciri_int.lead ~ nhri2 + iccprrat + # Tilsvarer t-2 (fordi lead-DV)
                  p_polity2 + 
                  leadinterstateconflict  + leadinternalconflict + 
                  loggdp + logingo +
                  wdi_trade + logpop +
                  p_durable + LJI + ciri_physint, 
                data = dt.nalags, effect = "twoways", 
                index = c("iso3numeric", "year"))

physlag4 <- plm(ciri_int.lead ~ nhri3 + iccprrat + # Tilsvarer t-2 (fordi lead-DV)
                  p_polity2 + 
                  leadinterstateconflict  + leadinternalconflict + 
                  loggdp + logingo +
                  wdi_trade + logpop +
                  p_durable + LJI + ciri_physint, 
                data = dt.nalags, effect = "twoways", 
                index = c("iso3numeric", "year"))

physlag5 <- plm(ciri_int.lead ~ nhri4 + iccprrat + # Tilsvarer t-2 (fordi lead-DV)
                  p_polity2 + 
                  leadinterstateconflict  + leadinternalconflict + 
                  loggdp + logingo +
                  wdi_trade + logpop +
                  p_durable + LJI + ciri_physint, 
                data = dt.nalags, effect = "twoways", 
                index = c("iso3numeric", "year"))

physlag6 <- plm(ciri_int.lead ~ nhri5 + iccprrat + # Tilsvarer t-2 (fordi lead-DV)
                  p_polity2 + 
                  leadinterstateconflict  + leadinternalconflict + 
                  loggdp + logingo +
                  wdi_trade + logpop +
                  p_durable + LJI + ciri_physint, 
                data = dt.nalags, effect = "twoways", 
                index = c("iso3numeric", "year"))

physlag7 <- plm(ciri_int.lead ~ nhri6 + iccprrat + # Tilsvarer t-2 (fordi lead-DV)
                  p_polity2 + 
                  leadinterstateconflict  + leadinternalconflict + 
                  loggdp + logingo +
                  wdi_trade + logpop +
                  p_durable + LJI + ciri_physint, 
                data = dt.nalags, effect = "twoways", 
                index = c("iso3numeric", "year"))

physlag8 <- plm(ciri_int.lead ~ nhri7 + iccprrat + # Tilsvarer t-2 (fordi lead-DV)
                  p_polity2 + 
                  leadinterstateconflict  + leadinternalconflict + 
                  loggdp + logingo +
                  wdi_trade + logpop +
                  p_durable + LJI + ciri_physint, 
                data = dt.nalags, effect = "twoways", 
                index = c("iso3numeric", "year"))

stargazer(physlag0, physlag1, physlag2,physlag3,physlag4,physlag5,physlag6,physlag7, physlag8, 
          out = "physlag-full.tex", 
          no.space = T, 
          column.sep.width = "0pt",
          font.size = "footnotesize", 
          title = "Physical integrity rights, time-lags", 
          omit.stat = "f", 
          column.labels = laglabs)

coefs_p <- c(physlag0$coefficients[1], physlag1$coefficients[1], physlag2$coefficients[1],physlag3$coefficients[1],physlag4$coefficients[1],
             physlag5$coefficients[1],physlag6$coefficients[1],physlag7$coefficients[1], physlag8$coefficients[1])

int_p <- rbind(confint(physlag0)[1,], 
               confint(physlag1)[1,], 
               confint(physlag2)[1,],
               confint(physlag3)[1,],
               confint(physlag4)[1,],
               confint(physlag5)[1,],
               confint(physlag6)[1,],
               confint(physlag7)[1,],
               confint(physlag8)[1,])

lagtab_p <- cbind(coefs_p, int_p)
colnames(lagtab_p) <- c("coefs", "low", "high")
rownames(lagtab_p) <- 0:8
lagtab_p <- data.frame(lagtab_p)


# ggplot phys -------------------------------------------------------------


physlag <- ggplot(lagtab_p, aes(x = rownames(lagtab_p), y= coefs, ymin = low, ymax = high, group = NA)) + 
  geom_ribbon(alpha = .5, fill = pal.bar.wes[5]) + geom_line() + geom_point() + # geom_pointrange() + 
  geom_hline(yintercept = 0, lty = 2, col = "black") + 
  ylab("Linear coefficient") + 
  xlab("") + 
  ylim(-0.25, 0.26)  + ggtitle("Physical integrity rights")
physlag
ggsave(physlag, filename = "physlag.pdf", width = 8, height = 5)

## Civil liberties ------
civ.lag0 <- plm(ciri_empinx_new ~ nhri + iccprrat +
                  p_polity2 + 
                  interstateconflict  + internalconflict + 
                  loggdp + logingo +
                  wdi_trade + logpop +
                  p_durable + LJI + civ.lag, 
                data = dt.nalags, effect = "twoways", 
                index = c("iso3numeric", "year"), x = T)

civ.lag1 <- plm(ciri_civ.lead ~ nhri + iccprrat +
                  p_polity2 + 
                  leadinterstateconflict  + leadinternalconflict + 
                  loggdp + logingo +
                  wdi_trade + logpop +
                  p_durable + LJI + ciri_empinx_new, 
                data = dt.nalags, effect = "twoways", 
                index = c("iso3numeric", "year"), x = T)

civ.lag2 <- plm(ciri_civ.lead ~ nhri1 + iccprrat +
                  p_polity2 + 
                  leadinterstateconflict  + leadinternalconflict + 
                  loggdp + logingo +
                  wdi_trade + logpop +
                  p_durable + LJI + ciri_empinx_new, 
                data = dt.nalags, effect = "twoways", 
                index = c("iso3numeric", "year"), x = T)

civ.lag3 <- plm(ciri_civ.lead ~ nhri2 + iccprrat +
                  p_polity2 + 
                  leadinterstateconflict  + leadinternalconflict + 
                  loggdp + logingo +
                  wdi_trade + logpop +
                  p_durable + LJI + ciri_empinx_new, 
                data = dt.nalags, effect = "twoways", 
                index = c("iso3numeric", "year"), x = T)

civ.lag4 <- plm(ciri_civ.lead ~ nhri3 + iccprrat +
                  p_polity2 + 
                  leadinterstateconflict  + leadinternalconflict + 
                  loggdp + logingo +
                  wdi_trade + logpop +
                  p_durable + LJI + ciri_empinx_new, 
                data = dt.nalags, effect = "twoways", 
                index = c("iso3numeric", "year"), x = T)

civ.lag5 <- plm(ciri_civ.lead ~ nhri4 + iccprrat +
                  p_polity2 + 
                  leadinterstateconflict  + leadinternalconflict + 
                  loggdp + logingo +
                  wdi_trade + logpop +
                  p_durable + LJI + ciri_empinx_new, 
                data = dt.nalags, effect = "twoways", 
                index = c("iso3numeric", "year"), x = T)

civ.lag6 <- plm(ciri_civ.lead ~ nhri5 + iccprrat +
                  p_polity2 + 
                  leadinterstateconflict  + leadinternalconflict + 
                  loggdp + logingo +
                  wdi_trade + logpop +
                  p_durable + LJI + ciri_empinx_new, 
                data = dt.nalags, effect = "twoways", 
                index = c("iso3numeric", "year"), x = T)

civ.lag7 <- plm(ciri_civ.lead ~ nhri6 + iccprrat +
                  p_polity2 + 
                  leadinterstateconflict  + leadinternalconflict + 
                  loggdp + logingo +
                  wdi_trade + logpop +
                  p_durable + LJI + ciri_empinx_new, 
                data = dt.nalags, effect = "twoways", 
                index = c("iso3numeric", "year"), x = T)

civ.lag8 <- plm(ciri_civ.lead ~ nhri7 + iccprrat +
                  p_polity2 + 
                  leadinterstateconflict  + leadinternalconflict + 
                  loggdp + logingo +
                  wdi_trade + logpop +
                  p_durable + LJI + ciri_empinx_new, 
                data = dt.nalags, effect = "twoways", 
                index = c("iso3numeric", "year"), x = T)

stargazer(civ.lag0, civ.lag1, civ.lag2,civ.lag3,civ.lag4,civ.lag5,civ.lag6,civ.lag7, civ.lag8,
          out = "civlag-full.tex", 
          no.space = T, 
          column.sep.width = "0pt",
          font.size = "footnotesize", 
          title = "Civil liberties, time-lags", 
          omit.stat = "f", 
          column.labels = laglabs)

coefs <- c(civ.lag0$coefficients[1], civ.lag1$coefficients[1], civ.lag2$coefficients[1],civ.lag3$coefficients[1],civ.lag4$coefficients[1],
           civ.lag5$coefficients[1],civ.lag6$coefficients[1],civ.lag7$coefficients[1],civ.lag8$coefficients[1])

int <- rbind(confint(civ.lag0)[1,], 
             confint(civ.lag1)[1,], 
             confint(civ.lag2)[1,],
             confint(civ.lag3)[1,],
             confint(civ.lag4)[1,],
             confint(civ.lag5)[1,],
             confint(civ.lag6)[1,],
             confint(civ.lag7)[1,],
             confint(civ.lag8)[1,])


# ggplot civ --------------------------------------------------------------



lagtab <- cbind(coefs, int)
colnames(lagtab) <- c("coefs", "low", "high")
rownames(lagtab) <- c(0:8)
lagtab <- data.frame(lagtab)

civlag <- ggplot(lagtab, aes(x = rownames(lagtab), y= coefs, ymin = low, ymax = high, group = NA)) + 
  geom_ribbon(alpha = .5, fill = pal.bar.wes[5]) + geom_line() + geom_point() + # geom_pointrange() + 
  geom_hline(yintercept = 0, lty = 2, col = "black") + 
  ylab("Linear coefficient") + 
  xlab("") +
  ylim(-0.4, 0.15) + 
  ggtitle("Civil liberties")
civlag
ggsave(civlag, filename = "civlag.pdf", width = 8, height = 5)

## WOPOL ----
cedaw.lag0 <- lrm(as.factor(wop_red) ~ nhri + cedawrat + 
                    p_polity2 + logingo + 
                    interstateconflict  + internalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + wop.lag + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.nalags)
cedawlag0.clust <-  robcov(cedaw.lag0, cluster = dt.nalags$iso3numeric)

cedaw.lag1 <- lrm(leadwop_red ~ nhri + cedawrat + 
                    p_polity2 + logingo + 
                    leadinterstateconflict  + leadinternalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + wop_red + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.nalags)
cedawlag1.clust <-  robcov(cedaw.lag1, cluster = dt.nalags$iso3numeric)

cedaw.lag2 <- lrm(leadwop_red ~ nhri1 + cedawrat + 
                    p_polity2 + logingo + 
                    leadinterstateconflict  + leadinternalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + wop_red + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.nalags)
cedawlag2.clust <-  robcov(cedaw.lag2, cluster = dt.nalags$iso3numeric)

cedaw.lag3 <- lrm(leadwop_red ~ nhri2 + cedawrat + 
                    p_polity2 + logingo + 
                    leadinterstateconflict  + leadinternalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + wop_red + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.nalags)
cedawlag3.clust <-  robcov(cedaw.lag3, cluster = dt.nalags$iso3numeric)


cedaw.lag4 <- lrm(leadwop_red ~ nhri3 + cedawrat + 
                    p_polity2 + logingo + 
                    leadinterstateconflict  + leadinternalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + wop_red + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.nalags)
cedawlag4.clust <-  robcov(cedaw.lag4, cluster = dt.nalags$iso3numeric)


cedaw.lag5 <- lrm(leadwop_red ~ nhri4 + cedawrat + 
                    p_polity2 + logingo + 
                    leadinterstateconflict  + leadinternalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + wop_red + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.nalags)
cedawlag5.clust <-  robcov(cedaw.lag5, cluster = dt.nalags$iso3numeric)


cedaw.lag6 <- lrm(leadwop_red ~ nhri5 + cedawrat + 
                    p_polity2 + logingo + 
                    leadinterstateconflict  + leadinternalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + wop_red + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.nalags)
cedawlag6.clust <-  robcov(cedaw.lag6, cluster = dt.nalags$iso3numeric)


cedaw.lag7 <- lrm(leadwop_red ~ nhri6 + cedawrat + 
                    p_polity2 + logingo + 
                    leadinterstateconflict  + leadinternalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + wop_red + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.nalags)
cedawlag7.clust <-  robcov(cedaw.lag7, cluster = dt.nalags$iso3numeric)

cedaw.lag8 <- lrm(leadwop_red ~ nhri7 + cedawrat + 
                    p_polity2 + logingo + 
                    leadinterstateconflict  + leadinternalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + wop_red + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.nalags)
cedawlag8.clust <-  robcov(cedaw.lag8, cluster = dt.nalags$iso3numeric)

stargazer(cedaw.lag0, cedaw.lag1, cedaw.lag2,cedaw.lag3,cedaw.lag4,cedaw.lag5,cedaw.lag6,cedaw.lag7, cedaw.lag8, 
          out = "woplag-full.tex", 
          no.space = T, 
          column.sep.width = "0pt",
          font.size = "footnotesize", 
          title = "Women's political rights, time-lags", 
          omit.stat = c("f", "chi2"),
          column.labels = laglabs)

coefs_wop <- c(cedawlag0.clust$coefficients[3], 
               cedawlag1.clust$coefficients[3], 
               cedawlag2.clust$coefficients[3],
               cedawlag3.clust$coefficients[3],
               cedawlag4.clust$coefficients[3],
               cedawlag5.clust$coefficients[3],
               cedawlag6.clust$coefficients[3],
               cedawlag7.clust$coefficients[3], 
               cedawlag8.clust$coefficients[3])

int_wop <- rbind(confint.default(cedawlag0.clust)[3,],
                 confint.default(cedawlag1.clust)[3,], 
                 confint.default(cedawlag2.clust)[3,],
                 confint.default(cedawlag3.clust)[3,],
                 confint.default(cedawlag4.clust)[3,],
                 confint.default(cedawlag5.clust)[3,],
                 confint.default(cedawlag6.clust)[3,],
                 confint.default(cedawlag7.clust)[3,],
                 confint.default(cedawlag8.clust)[3,])


# ggplot wopol ------------------------------------------------------------



lagtab_wop <- cbind(coefs_wop, int_wop)
colnames(lagtab_wop) <- c("coefs", "low", "high")
rownames(lagtab_wop) <- c(0:8)
lagtab_wop <- data.frame(lagtab_wop)
# lagtab_wop <- exp(lagtab_wop)
cedawlag <- ggplot(lagtab_wop, aes(x = rownames(lagtab_wop), y= coefs, ymin = low, ymax = high, group = NA)) + 
  geom_ribbon(alpha = .5,fill = pal.bar.wes[5]) + geom_line() + geom_point() + # geom_pointrange() + 
  geom_hline(yintercept = 0, lty = 2, col = "black") + 
  ylab("Logit coefficient") + 
  xlab("Time lag in years")  + ggtitle("Women's political rights")
cedawlag
ggsave(cedawlag, filename = "cedawlag.pdf", width = 8, height = 5)

## WECON ----

wecon.lag0 <- lrm(wec_red ~ nhri + cedawrat + 
                    p_polity2 + logingo + 
                    interstateconflict  + internalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + weclag_red + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.nalags)
wecon.lag0.clust <-  robcov(wecon.lag0, cluster = dt.nalags$iso3numeric)


wecon.lag1 <- lrm(leadwec_red ~ nhri + cedawrat + 
                    p_polity2 + logingo + 
                    leadinterstateconflict  + leadinternalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + wec_red + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.nalags)
wecon.lag1.clust <-  robcov(wecon.lag1, cluster = dt.nalags$iso3numeric)

wecon.lag2 <- lrm(leadwec_red ~ nhri1 + cedawrat + 
                    p_polity2 + logingo + 
                    leadinterstateconflict  + leadinternalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + wec_red + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.nalags)
wecon.lag2.clust <-  robcov(wecon.lag2, cluster = dt.nalags$iso3numeric)

wecon.lag3 <- lrm(leadwec_red ~ nhri2 + cedawrat + 
                    p_polity2 + logingo + 
                    leadinterstateconflict  + leadinternalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + wec_red + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.nalags)
wecon.lag3.clust <-  robcov(wecon.lag3, cluster = dt.nalags$iso3numeric)


wecon.lag4 <- lrm(leadwec_red ~ nhri3 + cedawrat + 
                    p_polity2 + logingo + 
                    leadinterstateconflict  + leadinternalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + wec_red + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.nalags)
wecon.lag4.clust <-  robcov(wecon.lag4, cluster = dt.nalags$iso3numeric)


wecon.lag5 <- lrm(leadwec_red ~ nhri4 + cedawrat + 
                    p_polity2 + logingo + 
                    leadinterstateconflict  + leadinternalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + wec_red + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.nalags)
wecon.lag5.clust <-  robcov(wecon.lag5, cluster = dt.nalags$iso3numeric)


wecon.lag6 <- lrm(leadwec_red ~ nhri5 + cedawrat + 
                    p_polity2 + logingo + 
                    leadinterstateconflict  + leadinternalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + wec_red + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.nalags)
wecon.lag6.clust <-  robcov(wecon.lag6, cluster = dt.nalags$iso3numeric)


wecon.lag7 <- lrm(leadwec_red ~ nhri6 + cedawrat + 
                    p_polity2 + logingo + 
                    leadinterstateconflict  + leadinternalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + wec_red + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.nalags)
wecon.lag7.clust <-  robcov(wecon.lag7, cluster = dt.nalags$iso3numeric)

wecon.lag8 <- lrm(leadwec_red ~ nhri7 + cedawrat + 
                    p_polity2 + logingo + 
                    leadinterstateconflict  + leadinternalconflict + 
                    loggdp + 
                    wdi_trade + logpop +
                    p_durable + LJI + wec_red + 
                    nineties +  naughts + tens + west, y = T, x = T,
                  data = dt.nalags)
wecon.lag8.clust <-  robcov(wecon.lag8, cluster = dt.nalags$iso3numeric)

stargazer(wecon.lag0, wecon.lag1, wecon.lag2,wecon.lag3,wecon.lag4,wecon.lag5,wecon.lag6,wecon.lag7, wecon.lag8,
          out = "weconlag-full.tex", 
          no.space = T, 
          column.sep.width = "0pt",
          font.size = "footnotesize", 
          title = "Women's economic rights, time-lags", 
          omit.stat = c("f", "chi2"), 
          column.labels = laglabs)


coefs_wec <- c(wecon.lag0.clust$coefficients[3], 
               wecon.lag1.clust$coefficients[3], 
               wecon.lag2.clust$coefficients[3],
               wecon.lag3.clust$coefficients[3],
               wecon.lag4.clust$coefficients[3],
               wecon.lag5.clust$coefficients[3],
               wecon.lag6.clust$coefficients[3],
               wecon.lag7.clust$coefficients[3], 
               wecon.lag8.clust$coefficients[3])

int_wec <- rbind(confint.default(wecon.lag0.clust)[3,], 
                 confint.default(wecon.lag1.clust)[3,], 
                 confint.default(wecon.lag2.clust)[3,],
                 confint.default(wecon.lag3.clust)[3,],
                 confint.default(wecon.lag4.clust)[3,],
                 confint.default(wecon.lag5.clust)[3,],
                 confint.default(wecon.lag6.clust)[3,],
                 confint.default(wecon.lag7.clust)[3,],
                 confint.default(wecon.lag8.clust)[3,])


# ggplot wecon ------------------------------------------------------------

lagtab_wec <- cbind(coefs_wec, int_wec)
colnames(lagtab_wec) <- c("coefs", "low", "high")
rownames(lagtab_wec) <- c(0:8)
lagtab_wec <- data.frame(lagtab_wec)
# lagtab_wec <- exp(lagtab_wec)
weconlag <- ggplot(lagtab_wec, aes(x = rownames(lagtab_wec), y= coefs, ymin = low, ymax = high, group = NA)) + 
  geom_ribbon(alpha = .5, fill = pal.bar.wes[5]) + geom_line() + geom_point() + # geom_pointrange() + 
  geom_hline(yintercept = 0, lty = 2, col = "black") +  ylab("Logit coefficient") + 
  xlab("Time lag in years") + ggtitle("Women's economic rights")
weconlag

### PLOT OUT ### 

grid_lags <- grid.arrange(physlag, civlag, cedawlag, weconlag, 
                          left = "NHRI coefficients with 95 % confidence intervals")
ggsave(grid_lags, filename = "grid-4-lags.pdf", width = 8, height = 5)


# Resid plots -------------------------------------------------------------
respc <- ggplot(dt.na,
                aes(y = resid(civ.nhri.lm), x = year, group = as.factor(iso3numeric)))
grobresc <- respc + geom_point(alpha = .8, col = "#5BBCD6") + 
  geom_line(stat="smooth", 
            method = "loess", 
            se = F, alpha = 1) + 
  scale_color_brewer(palette = "Set1", 
                     name = "Western country") + 
  geom_hline(yintercept = 0, col = "white", lty = 2) + 
  geom_hline(yintercept = c(-2, 2), 
             col = "black",
             linetype = "dashed") + 
  ylim(-4,4) + 
  ggtitle("Civil liberties (model 3)") + ylab(NULL) + 
  theme(legend.position="bottom")

# Country-level facets 
indresciv <- respc + geom_point(alpha = .5, col = "darkorange") + 
  geom_line(stat="smooth", method = "loess", se = F, alpha = 1, col = "black") + 
  facet_wrap(facets = as.factor("iso3numeric")) + theme(axis.title.x=element_blank(),
                                                        axis.text.x=element_blank(),
                                                        axis.ticks.x=element_blank())
ggsave("countryresids-civ.pdf", plot = indresciv,
       device = "pdf", width = 210, height = 240, units = "mm")


# Same for physint 

robj <- resid(phys.nhri.plm)
robj <- as.numeric(robj)

respa <- ggplot(dt.na, aes(y = robj, x = year, group = as.factor(iso3numeric)))


indres <- respa + geom_point(alpha = .5, col = "darkorange") + 
  geom_line(stat="smooth", method = "loess", se = F, alpha = 1, col = "black") + 
  facet_wrap(facets = as.factor("iso3numeric")) + theme(axis.title.x=element_blank(),
                                                        axis.text.x=element_blank(),
                                                        axis.ticks.x=element_blank())
ggsave("countryresids.pdf", plot = indres,
       device = "pdf", width = 210, height = 240, units = "mm")

grobres <- respa + geom_point(alpha = .8, col = "#5BBCD6") + 
  geom_line(stat="smooth", method = "loess", se = F, alpha = 1) + scale_color_brewer(palette = "Set1") + 
  geom_hline(yintercept = 0, col = "white", lty = 2) + ylim(-4,4) + geom_hline(yintercept = c(-2, 2), 
                                                                               col = "black",
                                                                               linetype = "dashed") +
  ggtitle("Physical integrity (model 1)") + ylab("Residuals") + theme(legend.position="none")

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(grobresc)

p3 <- grid.arrange(arrangeGrob(grobres + theme(legend.position="none"),
                               grobresc + theme(legend.position="none"),
                               nrow=1))

ggsave(p3, filename = "resid-time-civnhri.pdf", width = 8, height = 5)


# Fig 5.9 -----------------------------------------------------------------

pdf(file = "timeseries.pdf", width = 6, height = 3)
barplot(sort(table(dt.na$iso3numeric)), border = "grey", col = "grey", xlab = "ISO 3 country code",
        ylab = "Length of time series")
dev.off()


# Fig 5.10 (with full figures and LR tests of parallel odds in Appendix) -----

nomtest <- clm(factor(leadwop_red) ~ nhri + cedawrat + 
                 logingo + 
                 leadinternalconflict + 
                 leadinterstateconflict +
                 loggdp + 
                 wdi_trade + logpop +
                 p_durable + LJI + wop_red + 
                 nineties + naughts + tens + west,
               data = dt.na)

nomtest.a <- clm(factor(leadwop_red) ~ astatus.new + 
                   bstatus.new + cstatus.new + cedawrat + 
                   logingo + 
                   leadinternalconflict + 
                   leadinterstateconflict +
                   loggdp + 
                   wdi_trade + logpop +
                   p_durable + LJI + wop_red + 
                   nineties + naughts + tens + west,
                 data = dt.na)

nomtest.wec <- clm(factor(leadwec_red) ~ nhri + cedawrat + 
                     logingo + 
                     leadinternalconflict + 
                     leadinterstateconflict +
                     loggdp + 
                     wdi_trade + logpop +
                     p_durable + LJI + wec_red + 
                     nineties + naughts + tens + west,
                   data = dt.na)

nomtest.weca <- clm(factor(leadwec_red) ~ astatus.new + 
                      bstatus.new + cstatus.new + cedawrat + 
                      logingo + 
                      leadinternalconflict + 
                      leadinterstateconflict +
                      loggdp + 
                      wdi_trade + logpop +
                      p_durable + LJI + wec_red + 
                      nineties + naughts + tens + west,
                    data = dt.na)


stargazer(nominal_test(nomtest), type = "text", 
          summary = F, out = "nomtest-pol.tex", 
          title = "Likelihood ratio test of proportional odds, main model 5")
stargazer(nominal_test(nomtest.a), type = "text", 
          summary = F, out = "nomtest-pola.tex", 
          title = "Likelihood ratio test of proportional odds, main model 6")
stargazer(nominal_test(nomtest.wec), type = "text", 
          summary = F, out = "nomtest-econ.tex", 
          title = "Likelihood ratio test of proportional odds, main model 7")
stargazer(nominal_test(nomtest.weca), type = "text", 
          summary = F, out = "nomtest-econa.tex", 
          title = "Likelihood ratio test of proportional odds, main model 8")

# plot nhri wop -----
pdf("partest-nhri-wop.pdf", width = 8, height = 10)
par(mfrow=c(5, 4))
plot.xmean.ordinaly(leadwop_red ~ nhri + cedawrat + 
                      logingo + 
                      leadinternalconflict + 
                      leadinterstateconflict +
                      loggdp + 
                      wdi_trade + logpop +
                      p_durable + LJI + wop_red + 
                      nineties + naughts + tens + west,
                    data = dt.na)
dev.off()

pdf("partest-ast-wop.pdf", width = 8, height = 10)
par(mfrow=c(5, 4))
plot.xmean.ordinaly(leadwop_red ~ astatus.new + 
                      bstatus.new + cstatus.new + cedawrat + 
                      logingo + 
                      leadinternalconflict + 
                      leadinterstateconflict +
                      loggdp + 
                      wdi_trade + logpop +
                      p_durable + LJI + wop_red + 
                      nineties + naughts + tens + west,
                    data = dt.na)

dev.off()

pdf("partest-nhri-wec.pdf", width = 8, height = 10)
par(mfrow=c(5, 4))
plot.xmean.ordinaly(leadwec_red ~ nhri + cedawrat + 
                      logingo + 
                      leadinternalconflict + 
                      leadinterstateconflict +
                      loggdp + 
                      wdi_trade + logpop +
                      p_durable + LJI + wec_red + 
                      nineties + naughts + tens + west,
                    data = dt.na)
dev.off()

pdf("partest-ast-wec.pdf", width = 8, height = 10)
par(mfrow=c(5, 4))
plot.xmean.ordinaly(leadwec_red ~ astatus.new + 
                      bstatus.new + cstatus.new + cedawrat + 
                      logingo + 
                      leadinternalconflict + 
                      leadinterstateconflict +
                      loggdp + 
                      wdi_trade + logpop +
                      p_durable + LJI + wec_red + 
                      nineties + naughts + tens + west,
                    data = dt.na)

dev.off()



# 5.11 Separation plots ---------------------------------------------------

# Sepplot 1 
predo <- predict(clust.std, type = "fitted.ind")
colnames(predo) <- c(0:2)
sp.categorical(predo, clust.std$y, file = "std-wop.pdf", show.expected = T)

# Sepplot 2 
pred2 <- predict(clust.anew, type = "fitted.ind")
colnames(pred2) <- c(0:2)
sp.categorical(pred2, clust.anew$y, file = "astat-wop.pdf", show.expected = T)

# Sepplot 3
pred3 <- predict(clust.wecon.nhri.lrm, type = "fitted.ind")
colnames(pred3) <- c(0:2)
sp.categorical(pred3, clust.wecon.nhri.lrm$y, file = "std-wec.pdf", show.expected = T)

# Sepplot 4
pred4 <- predict(clust.wecon.a, type = "fitted.ind")
colnames(pred4) <- c(0:2)
sp.categorical(pred4, clust.wecon.a$y, file = "astat-wec.pdf", show.expected = T)



# 5.12 Judicial Independence ----------------------------------------------

ljitest <- ggplot(dt.na, aes(x = year, y = LJI, group = iso3numeric, color = p_polity2))
ljitest + geom_line(alpha = .6) + geom_smooth(aes(group = 1), col = "chocolate", fill = "chocolate", method = "loess", alpha = .5) +
  scale_color_continuous(name = "Polity IV\ndemocracy") +  ylab("Judicial Independence")

ggsave(filename = "judint-times.pdf", width = 5, height = 3)

# 6.1 NORWAY DYNAMICS ---------------------------------------------------------

nor1 <- ggplot(subset(dt.na, cname == "Norway"), aes(x = year, y = ciri_wopol)) + geom_line(col = "darkblue") + 
  geom_line(aes(y = ciri_wecon), col = "darkred", lty = 2) + ylim(0,3) + 
  annotate("text", x = 1995, y = 3, vjust = 1.5, label = "Women's political rights") + 
  annotate("text", x = 1995, y = 2, vjust = 1.5, label = "Women's economic rights") + 
  ylab("CIRI scales") + xlab("Year")

nor2 <- ggplot(subset(dt.na, cname == "Norway"), aes(x = year, y = ciri_physint)) + geom_line(col = "darkblue") + 
  geom_line(aes(y = ciri_empinx_new), col = "darkred") + annotate("text", x = 2010, y = 12, label = "Civil liberties",
                                                                  vjust = 1, hjust = 1) + 
  annotate("text", x = 2010, y = 8, label = "Physical integrity rights", vjust = -1, hjust = 1) + 
  ylim(0,14) + ylab("CIRI indexes") + xlab("Year")

norsam <- grid.arrange(nor1, nor2, nrow = 1)
ggsave(norsam, filename = "norway.pdf", width = 8, height = 3)
