library('tidyverse')
library('ggplot2')
library('lme4')
library(lmerTest)
library(emmeans)
library(data.table)
library(ggplot2)

# First, import the data
Q1 <- read_csv("../data/Q1.csv")
Q2 <- read_csv("../data/Q2.csv")
M2 <- read_csv("../data/M2.csv")

G1 <- read_csv("../data/G1.csv")
G2 <- read_csv("../data/G2.csv")


# convert time variables back into milliseconds. Not important, but seems standard
Q1$RT <- Q1$RT*1000
Q1$WT <- Q1$WT*1000
Q1$CT <- Q1$CT*1000
Q2$RT <- Q2$RT*1000
Q2$WT <- Q2$WT*1000
Q2$CT <- Q2$CT*1000
M2$RT <- M2$RT*1000

# In Experiment 1, we pre-registered two slightly different experiments. Analyze separately:
Q1a <- Q1 %>% filter(base %in% c(2, 3, 4, 5))
Q1b <- Q1 %>% filter(base %in% c(4, 8, 10))

# This document contains our pre-registered analyses. Annotations reflect one major difference,
# which is increased Experiment 1a sample size, requested by conference reviewers. Also, we will check if
# our pre-registered time DV (RT + (WT / word_length)) differs from simple reaction time (RT).
# Finally, it contains an exploratory model probing effects of different rules of composition.


## Experiment 1a - Analysis 15.1   #######################
# maximal models:
CT_Q1a_max <- lmer( data=Q1a, CT ~ base + ( 1 + num | GID )) # NO CONVERGE
AC_Q1a_max <- glmer(data=Q1a, correct ~ base + ( 1 + num | GID ), family='binomial') # SINGULAR

# final models:
CT_Q1a <- lmer( data=Q1a, CT ~ base + ( 1 | GID ))
RT_Q1a <- lmer( data=Q1a, RT ~ base + ( 1 | GID )) # similar to CT
AC_Q1a <- glmer(data=Q1a, correct ~ base + ( 1 | GID ), family='binomial')

sink(file = "../results/models/expt1a.txt")
summary(CT_Q1a)
message("#########################################\n next \n#########################################")
summary(AC_Q1a)
sink(file = NULL)
#######################


## Experiment 1b - Analysis 15.1   #######################
# maximal models:
CT_Q1b_max <- lmer( data=Q1b, CT ~ base + ( 1 + num | GID )) # NO CONVERGE
AC_Q1b_max <- glmer(data=Q1b, correct ~ base + ( 1 + num | GID ), family='binomial') # converges

# final models (identical):
CT_Q1b <- lmer( data=Q1b, CT ~ base + ( 1 | GID ))
RT_Q1b <- lmer( data=Q1b, RT ~ base + ( 1 | GID )) # similar to CT
AC_Q1b <- glmer(data=Q1b, correct ~ base + ( 1 | GID ), family='binomial') # remove random slope for consistency

sink(file = "../results/models/expt1b.txt")
summary(CT_Q1b)
message("#########################################\n next \n#########################################")
summary(AC_Q1b)
sink(file = NULL)
#######################


## Experiment 1 - Analysis 15.1   #######################
# maximal models:
CT_Q1_max <- lmer( data=Q1, CT ~ base + ( 1 + num | GID )) # NO CONVERGE
AC_Q1_max <- glmer(data=Q1, correct ~ base + ( 1 + num | GID ), family='binomial') # NO CONVERGE

# final models:
CT_Q1 <- lmer( data=Q1, CT ~ base + ( 1 | GID ))
RT_Q1 <- lmer( data=Q1, RT ~ base + ( 1 | GID )) # similar to CT
AC_Q1 <- glmer(data=Q1, correct ~ base + ( 1 | GID ), family='binomial')

sink(file = "../results/models/expt1.txt")
summary(CT_Q1)
message("#########################################\n next \n#########################################")
summary(AC_Q1)
sink(file = NULL)

# check it out with base as a factor
Q1f <- Q1
Q1f$base <- as.factor(Q1f$base)
Q1f$base <- relevel(factor(Q1f$base), ref = "3")

CT_Q1f <- lmer( data=Q1f, CT ~ base + ( 1 | GID ))
AC_Q1f <- glmer(data=Q1f, correct ~ base + ( 1 | GID ), family='binomial')
summary(CT_Q1f)
summary(AC_Q1f)


#######################


## Experiment 1 - Exploratory (Rules) #######################
# the full models both had 'number' as a random slope, but rulesCT didn't converge, so it was removed. Output was qualitatively similar. For rulesAC, output *did* change when the random slope was removed, which seems to suggest it should be left in (it converges)
rulesCT <- lmer(data=Q1,  CT ~      base + add + mult + exp + numSyls + (1 | GID))
rulesAC <- glmer(data=Q1, correct ~ base + add + mult + exp + numSyls + (1 + num | GID), family='binomial')
summary(rulesCT)
summary(rulesAC)

sink(file = "../results/models/expt1_rules.txt")
summary(rulesCT)
message("#########################################\n next \n#########################################")
summary(rulesAC)
sink(file = NULL)
#######################




## Experiment 2 - Analysis 15.1   #######################
# maximal models:
CT_Q2_max <- lmer( data=Q2, CT      ~ cond * num * freq + (1 | GID)) # converges
AC_Q2_max <- glmer(data=Q2, correct ~ cond * num * freq + (1 | GID), family='binomial') # NO CONVERGE

# final models:
CT_Q2 <-      lmer( data=Q2, CT     ~ cond * freq + num + (1 | GID))
RT_Q2 <-      lmer( data=Q2, RT     ~ cond * freq + num + (1 | GID)) # only freq now non-sig
AC_Q2 <-     glmer(data=Q2, correct ~ cond * freq + num + (1 | GID), family='binomial')

summary(AC_Q2)

sink(file = "../results/models/expt2_15.1.txt")
summary(CT_Q2)
message("#########################################\n next \n#########################################")
summary(AC_Q2)
sink(file = NULL)
#######################


## Experiment 2 - Analysis 15.2   #######################
# maximal models:
RT_M2a_max <-  lmer(data=M2, RT ~      cond * numsum * freq + (1 | GID)) # converges
AC_M2a_max <- glmer(data=M2, correct ~ cond * numsum * freq + (1 | GID), family='binomial') # NO CONVERGE

# final models:
RT_M2a <-  lmer(data=M2, RT ~      cond * numsum * freq + (1 | GID))
AC_M2a <- glmer(data=M2, correct ~ cond * numsum + freq + (1 | GID), family='binomial')

sink(file = "../results/models/expt2_15.2.txt")
summary(RT_M2a)
message("#########################################\n next \n#########################################")
summary(AC_M2a)
sink(file = NULL)
#######################


## Experiment 2 - Analysis 15.2.1 #######################
# maximal (unaugmented) models:
RT_M2b_max <-  lmer(data=M2, RT ~      cond * numsum * numdif + (1 | GID)) # converges
AC_M2b_max <- glmer(data=M2, correct ~ cond * numsum * numdif + (1 | GID), family='binomial') # NO CONVERGE

# final (unaugmented) models (to compare):
RT_M2b <-  lmer(data=M2, RT ~      cond + numsum + numdif + (1 | GID)) # same as glmer, for consistency
AC_M2b <- glmer(data=M2, correct ~ cond + numsum + numdif + (1 | GID), family='binomial')

# word-length models (based on final unaugmented models; to compare):
RT_M2b_wl <-  lmer(data=M2, RT ~      syllDiff * (cond + numdif) + numsum + (1 | GID))
AC_M2b_wl <- glmer(data=M2, correct ~ syllDiff * (cond + numdif) + numsum + (1 | GID), family='binomial')
summary(RT_M2b_wl)
summary(AC_M2b_wl)

# final model comparisons:
compRT <- anova(RT_M2b,RT_M2b_wl)
compAC <- anova(AC_M2b,AC_M2b_wl)

sink(file = "../results/models/expt2_15.2.1.txt")
summary(RT_M2b)
message("#########################################\n next \n#########################################")
summary(AC_M2b)
message("#########################################\n next \n#########################################")
summary(RT_M2b_wl)
message("#########################################\n next \n#########################################")
summary(AC_M2b_wl)
message("#########################################\n next \n#########################################")
message("#########################################\n\n#########################################")
compRT
message("#########################################\n next \n#########################################")
compAC
sink(file = NULL)
#######################


## Experiment 1 - Post-Hoc: Base Size on Generalization Strategy #######################
summary(glmer(data=G1, exact_match ~ base + ( 1 + num | GID ), family='binomial'))

# base-8 appears to have driven the effect
G1no8 <- G1[G1$base != 8,]
summary(glmer(data=G1no8, exact_match ~ base + ( 1 + num | GID ), family='binomial'))


# Pairs of numbers with the same components, but flipped order (and thus rule-types)
q1r <- read_csv("../data/revision/q1r.csv")
q1r$CT <- q1r$CT*1000

m_q1rCT <- lmer(data=q1r,  CT ~      base + mult + (1 | GID))
m_q1rAC <- glmer(data=q1r, correct ~ base + mult + (1 | GID), family='binomial')
summary(m_q1rCT)
summary(m_q1rAC)

# n.b. here the correlation between numsum and freq
summary(lmer(data=M2, RT ~      cond + numsum + freq + (1 | GID)))

# Examine effects of cardinality for each condition
M_relevel <- M2
M_relevel$cond <- as.factor(M_relevel$cond)
M_relevel$cond <- relevel(M_relevel$cond, ref = "control")
mA <- lmer(data=M_relevel, RT ~      cond * numsum + (1 | GID))
mB <- glmer(data=M_relevel, correct ~    cond * numsum + (1 | GID), family='binomial')
summary(mA)

M_relevel <- M2
M_relevel$cond <- as.factor(M_relevel$cond)
M_relevel$cond <- relevel(M_relevel$cond, ref = "control")
mA <- lmer(data=M_relevel, RT ~      cond * numsum + (1 | GID))
mB <- glmer(data=M_relevel, correct ~    cond * numsum + (1 | GID), family='binomial')
summary(mA)
summary(mB)

M_relevel <- M2
M_relevel$cond <- as.factor(M_relevel$cond)
M_relevel$cond <- relevel(M_relevel$cond, ref = "no_syntax")
mA <- lmer(data=M_relevel, RT ~      cond * numsum + (1 | GID))
mB <- glmer(data=M_relevel, correct ~    cond * numsum + (1 | GID), family='binomial')
summary(mA)
summary(mB)

M_relevel <- M2
M_relevel$cond <- as.factor(M_relevel$cond)
M_relevel$cond <- relevel(M_relevel$cond, ref = "no_count")
mA <- lmer(data=M_relevel, RT ~      cond * numsum + (1 | GID))
mB <- glmer(data=M_relevel, correct ~    cond * numsum + (1 | GID), family='binomial')
summary(mA)
summary(mB)


# We should investigate the NDE more clearly
ndeRT <- lmer(data=M_relevel, RT ~      cond * numdif + numsum + (1 | GID))
ndeAC <- glmer(data=M_relevel, correct ~ cond * numdif + numsum + (1 | GID), family='binomial')
simple_slopes <- emtrends(ndeRT, ~ cond, var = "numdif", lmerTest.limit = 6000, pbkrtest.limit = 6000)
summary(simple_slopes)
test(simple_slopes)

simple_slopes <- emtrends(ndeAC, ~ cond, var = "numdif", lmerTest.limit = 6000, pbkrtest.limit = 6000)
summary(simple_slopes)
test(simple_slopes)

# compare the effect of numdif between conditions:
summary(ndeRT)
summary(ndeAC)

# does adding word length improve model fit?
wlRT <- lmer(data=M2, RT ~      cond + numdif + numsum + syllDiff + (1 | GID))
wlAC <- glmer(data=M2, correct ~ cond + numdif + numsum + syllDiff + (1 | GID), family='binomial')
anova(RT_M2b,wlRT)
anova(AC_M2b,wlAC)

# break it down by condition
wl_rt <- lmer(data=M2,  RT ~      cond * (syllDiff + numdif) + numsum + (1 | GID))
wl_ac <- glmer(data=M2, correct ~ cond * (syllDiff + numdif) + numsum + (1 | GID), family='binomial')

simple_slopes <- emtrends(wl_rt, ~ cond, var = "syllDiff", lmerTest.limit = 6000, pbkrtest.limit = 6000)
summary(simple_slopes)
test(simple_slopes)

simple_slopes <- emtrends(wl_ac, ~ cond, var = "syllDiff", lmerTest.limit = 6000, pbkrtest.limit = 6000)
summary(simple_slopes)
test(simple_slopes)

simple_slopes <- emtrends(wl_rt, ~ cond, var = "numdif", lmerTest.limit = 6000, pbkrtest.limit = 6000)
summary(simple_slopes)
test(simple_slopes)

simple_slopes <- emtrends(wl_ac, ~ cond, var = "numdif", lmerTest.limit = 6000, pbkrtest.limit = 6000)
summary(simple_slopes)
test(simple_slopes)





