# ---- load-packages --------------------------------------------------------
library("tidyverse")
library("foreign")
library("knitr")
library("kableExtra")
library("reshape2")
source("~/adroseHelperScripts/R/afgrHelpFunc.R")
library("lme4")
library("insight")


# ---- set-wd ----------------------------------------------------------------------
## This is to be run when not knitting the R MD file
setwd("./HW2/scripts/")

# ---- load-data ----------------------------------------------------------------------
in.dat <- foreign::read.spss("../data/abuse.sav", to.data.frame = T, use.value.labels = F)

# ---- q-1-a ----------------------------------------------------------------------
in.dat$schnum <- factor(in.dat$schnum)
in.dat[which(in.dat$schnum %in% c(1:10)),] %>% ggplot(., aes(y=alcuse, x=nhsup, group=schnum, color=schnum)) +
  geom_point(position = 'jitter') +
  geom_smooth(method = "lm", se = FALSE, fullrange=TRUE) +
  theme_bw()

# ---- q-1-b ----------------------------------------------------------------------
## Now train all models so we can grab the intercepts and slopes
all.mods <- list()
all.loop <- unique(in.dat$schnum)
for(i in all.loop){
  mod.tmp <- lm(alcuse ~ nhsup, data=in.dat[which(in.dat$schnum==i),])
  # Now export the object
  all.mods[[i]] <- mod.tmp
}
## Now grab all coefficients
all.params <- data.frame(matrix(unlist(lapply(all.mods, coef)), ncol = 2, byrow = T))
colnames(all.params) <- c("Intercept", "Slope")
## Now plot these
to.plot <- melt(all.params)
# Now get the summary stats for these values
mean.vals <- summarySE(data = to.plot, measurevar = "value", groupvars = "variable")
tmp <- to.plot %>% ggplot(., aes(y = value, x=variable)) +
  geom_violin() +
  theme_bw()
# Now create a variable with the mean values for each specific coef
mean.inter <- mean.vals$value[1]
mean.slope <- mean.vals$value[2]
# Now do the sd
sd.inter <- mean.vals$sd[1]
sd.slope <- mean.vals$sd[2]
## Now return a table with the mean and standard deviation values
mean.vals %>% kable()


# ---- q-2-a ----------------------------------------------------------------------
mod.one <- lme4::lmer(alcuse ~ (1|schnum), data=in.dat)

# ---- q-2-b-1 ----------------------------------------------------------------------
fixef(mod.one) %>% kable()

# ---- q-2-b-2 ----------------------------------------------------------------------
ranef(mod.one) %>% kable()

# ---- q-2-c ----------------------------------------------------------------------
vals <- summary(mod.one)
## Calculate the ICC by taking the ratio of the variance of the intercept over all of the variance
var.vals <- as.data.frame(VarCorr(mod.one, comp=c("Variance","Std.Dev.")))

ICC.value <- var.vals$vcov[1] / sum(var.vals$vcov)

# ---- q-3-a ----------------------------------------------------------------------
in.dat$nhsupZ <- scale(in.dat$nhsup)[,]
in.dat$nhsupZG <- ave(in.dat$nhsup, in.dat$schnum, FUN=function(x) scale(x, scale = F))
mod.one.sd <- lme4::lmer(alcuse ~ nhsupZG + (nhsupZG|schnum), data=in.dat)


# ---- q-3-b ----------------------------------------------------------------------
var.vals <- lme4::VarCorr(mod.one.sd, comp="V")
var.val.int <- as.data.frame(var.vals$schnum)[1,1]

# ---- q-3-c ----------------------------------------------------------------------
var.val.slope <- as.data.frame(var.vals$schnum)[2,2]

# ---- q-3-d ----------------------------------------------------------------------
covar.val <- as.data.frame(var.vals$schnum)[1,2]

# ---- q-3-e ----------------------------------------------------------------------
# Now I need to find the total amount of variation explained by the mixed effects - so I need to find the variance of the mixed effects
# take the sum of all variance, and then find the ratio of these two values
num.1 <- var.val.int + var.val.slope
denom <- get_variance_residual(mod.one.sd) + num.1
var.explained <- num.1 / denom

# ---- q-4-a-1 ----------------------------------------------------------------------
in.dat$dropoutZ <- in.dat$dropout - mean(in.dat$dropout)
mod.two <- lme4::lmer(alcuse ~ dropoutZ * nhsupZG + (nhsupZG|schnum), data =in.dat)
mod.two.gp <- lmerTest::lmer(alcuse ~ dropoutZ * nhsupZG + (nhsupZG|schnum), data =in.dat)
fixef(mod.two) %>% kable()

# ---- q-4-a-2 ----------------------------------------------------------------------
ranef(mod.two) %>% as.data.frame(.) %>% 
  pivot_wider(., id_cols=c("grp"), values_from="condval", names_from="term") %>% 
  kable()


# ---- q-5-a ----------------------------------------------------------------------
mod.three <- lme4::lmer(alcuse ~  nhsup + gender + (nhsup+gender|schnum), data=in.dat)

# ---- q-5-a-1 ----------------------------------------------------------------------
fixef(mod.three) %>% kable()

# ---- q-5-a-2 ----------------------------------------------------------------------
ranef(mod.three) %>% as.data.frame(.) %>% 
  pivot_wider(., id_cols=c("grp"), values_from="condval", names_from="term") %>% 
  kable()

# ---- q-5-a-3 ----------------------------------------------------------------------
VarCorr(mod.three)$schnum %>% kable()
