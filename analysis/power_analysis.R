# Following along with this:
# https://humburg.github.io/Power-Analysis/simr_power_analysis.html

# Another reference:
# https://cran.r-project.org/web/packages/simr/vignettes/fromscratch.html

# install.packages('simr')
library(simr)
simrOptions(progress=FALSE)

# how many of each factor level is there?
N = 20        # 20
bases = 3     # 3
trials = 65   # 65

# set up the number of factors for each column
base <- c(4, 8, 10)
subj <- factor(1:(N*bases))
trial <- factor(1:trials)

# repeat the factor levels
base_full <- c(rep(base, each=N*trials))
subj_full <- c(rep(subj, each=trials))
trial_full <- c(rep(trial, N))

# put them together into dataframe
covars <- data.frame(base=base_full, subject=subj_full, trial=trial_full)

# First, let's take some data we already have, model it, and extract expected model parameters
study1 <- read.csv('numsyn1Data.csv')
syls <- data.frame(apply(study1,2,nchar))$response / 2
study1$numSyls <- syls
study1$RT <-  as.numeric(study1$RT)
study1$writingTime <-  as.numeric(study1$writingTime)
study1$trialNum <-  as.numeric(study1$trialNum)
study1$timeDV <- study1$RT + (study1$writingTime / study1$numSyls)
model1 <- lmer( timeDV ~ base*trialNum + ( 1 | gameID ), data=study1)

# now let's visualize it
summary(model1)

# FIRST, fill in effects with what we got previously
## fixed effects: (Intercept), base, trial, base:trial  
fixed <- c(2356.361, 65.901, 1.978)#, -1.293 )
## Random intercepts for participants
rand <- list(145694.89 ) # Std.Dev. is sqrt of this number
## residual variance
res <- 1263.4

# SECOND, replace 'base' coef with an anticipated H1 effect
## fixed effects: (Intercept), base, trial, base:trial  
fixed <- c(2356.361, 45, 1.978)#, -1.293 )
## Random intercepts for participants
rand <- list(145694.89 ) # Std.Dev. is sqrt of this number
## residual variance
res <- 1263.4

# now we make a new model with the expected effects, etc.
model <- makeLmer(y ~ base+trial + (1|subject),
                  fixef=fixed, VarCorr=rand, sigma=res, data=covars)

# simular data from such a model 100 times
sim_treat <- powerSim(model, nsim=100, test = fcompare(y~trial))

# what if we had more subjects per condition?
model_ext <- extend(model, along="subject", n=120) 

# plot the curve of how much power we'd have to detect expected effects, for every sample size
p_curve_treat <- powerCurve(model_ext, test=fcompare(y~trial), along="subject")
plot(p_curve_treat)



