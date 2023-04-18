set.seed(12345)
library(brms)
library(readr)
library(bayesplot)
library(bayestestR)
library(BayesFactor)
library(ggplot2)
library(rstanarm)
library(loo)
library(see)
library(sjPlot)
library(tidyverse)
library(emmeans)

df.cloze <- read_csv2("df.cloze.csv")
summary(df.cloze)

#create subsets and Vars#
df.c1 <- subset(df.cloze, cond == 'c1')
c1_c2 <- as.data.frame(table(df.c1$pred.cloze))                  # Summarize data
c1_c2 
df.c3 <- subset(df.cloze, cond == 'c3')
c3_c4 <- as.data.frame(table(df.c3$pred.cloze))                  # Summarize data
c3_c4  
df.c5 <- subset(df.cloze, cond == 'c5')
c5_c6 <- as.data.frame(table(df.c5$pred.cloze))                  # Summarize data
c5_c6 
df.c7 <- subset(df.cloze, cond == 'c7')
c7_c8 <- as.data.frame(table(df.c7$pred.cloze))                 # Summarize data
c7_c8                                                           # Print summarized data
df.cond <- rbind(c1_c2, c3_c4, c5_c6, c7_c8)
df.cond$Var2 <- as.factor(c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"))
df.cond$lambda <- as.numeric(((df.cond$Freq)/116))
df.cond$Var4 <- as.numeric(((df.cond$Freq)-(df.cond$lambda))) #Freq - Freq Mean 
sample_n(df.cond, 4)

##plot number of observations per condition, grouped by Cloze Predictability#
p1 <- ggplot(df.cond, aes(x = Var2, y = Freq, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label= Freq), vjust = 0) +
  xlab("Condición") +
  ylab("Observaciones por Condición") +
  scale_fill_hue(name="Predictibilidad Cloze", # Legend label, use darker colors
                breaks=c("alta", "baja"),
                labels=c("ALTA", "BAJA"))
p1

dev.print(jpeg, "p1.OBSxCOND.jpeg", res=900, height=8, width=16, units="in")

p2 <- ggplot(df.cond, aes(x = Var2, y = lambda, fill = Var1)) +  # Plot with values on top
  geom_bar(stat = "identity") +
  geom_text(aes(label= format(lambda, digits=3, decimal.mark   = ".")), vjust = 0) + 
  xlab("Condición") +
  ylab("Índice Predictibilidad Cloze por Condición") +
  scale_fill_hue(name="Índice Pred. Cloze",
                 breaks=c("alta", "baja"),
                 labels=c("ALTA", "BAJA"))
p2

dev.print(jpeg, "p2.CLOZE.PROBxCOND.jpeg", res=900, height=8, width=16, units="in")

##Bayesian GLMER model fitting##
color_scheme_set("brightblue")
fit_glmer1 <- stan_glmer(Freq ~ Var2 + offset(log(lambda)) + (1|Var2), #no intercept, no random slope
                data = df.cond,
                family = poisson(),
                cores = 5,
                chains = 10, 
                iter = 10000,
                warmup = 1000,
                diagnostic_file = file.path(tempdir(), "df.csv")
                )
#bridge_1 <- bridgesampling::bridge_sampler(fit_glmer1)

fit_glmer2 <- update(fit_glmer1, formula = . ~ Var2 + Var1 + offset(log(lambda)) + (1|Var2))
#bridge_2 <- bridgesampling::bridge_sampler(fit_glmer2)

fit_glmer3 <- update(fit_glmer1, formula = . ~ Var2 + Var1 + offset(log(lambda)) + (1+Var1|Var2))
#bridge_3 <- bridgesampling::bridge_sampler(fit_glmer3)

fit_glmer4 <- update(fit_glmer1, formula = . ~ Var2 + Var1 + offset(log(lambda)) + (1|Var2) + (1|Var1))
#bridge_3 <- bridgesampling::bridge_sampler(fit_glmer3)

fit_glmer5 <- update(fit_glmer1, formula = . ~ Var2 + Var1 + offset(log(lambda)) + (1+Var1|Var2) + (1|Var1))
#bridge_3 <- bridgesampling::bridge_sampler(fit_glmer3)

summary(fit_glmer5)

#Compare models with loo
loo1 <- loo(fit_glmer1, cores = 5)
loo2 <- loo(fit_glmer2, cores = 5)
loo3 <- loo(fit_glmer3, cores = 5) 
loo4 <- loo(fit_glmer4, cores = 5) #selected model
loo5 <- loo(fit_glmer5, cores = 5)

comp <- loo_compare(loo1, loo2, loo3, loo4, loo5)
comp

#elpd_diff se_diff
#fit_glmer2  0.0       0.0   
#fit_glmer3  0.0       0.3   
#fit_glmer5 -0.4       0.4   
#fit_glmer4 -0.4       0.4   
#fit_glmer1 -0.8       0.5

#Plot Posterior Predictive Intervals
ppc_intervals(
  y = df.cond$Var4,
  yrep = posterior_predict(fit_glmer3), #selected model
  x = df.cond$lambda,
  prob = 0.5,
  prob_outer = 0.9
) +
  labs(
    x = "Predictibilidad Cloze",
    y = "Frecuencia Promedio de Futuras Observaciones",
    title = "Intervalos de Predicción (PI 90%) \nvs Índice Cloze Calculado por Condición") + 
  panel_bg(fill = "gray95", color = NA) +
  grid_lines(color = "white") 

dev.print(jpeg, "PIcloze_selectedmodel.jpeg", res=900, height=8, width=16, units="in")

#BF GLMER model evaluation##
BF_glmer3 <- bayesfactor_parameters(fit_glmer3, null= 0)
plot(BF_glmer3)
effectsize::interpret_bf(exp(BF_glmer3$log_BF[9]), include_value = TRUE)
dev.print(jpeg, "1_BF_GLMER.jpeg", res=900, height=8, width=16, units="in")
#[1] "extreme evidence (BF = 119.97) in favour of"
#(Rules: Jeffrey, 1961)
comparison <- bayesfactor_models(fit_glmer1, fit_glmer2, fit_glmer3, fit_glmer4, fit_glmer5, denominator = fit_glmer1)

##GLMER Posterior Predictive Distributions##
plot_title0 <- ggplot2::ggtitle("Distribuciones posteriores",
                               "con mediana e intervalos predictivos (90%)")
pp_check(fit_glmer3, plotfun = "stat", stat = "mean") + plot_title0
dev.print(jpeg, "1_pp_check_GLMER.jpeg", res=900, height=8, width=16, units="in")

##Bayesian GLM object fitting##
bayesplot::color_scheme_set("brightblue")
fit_glm <- stan_glm(Freq ~ 0 + Var2,
                    data = df.cond,
                    family = poisson(link="log"), #The Poisson distribution is defined by a single parameter, lambda (λ), which is the mean number of occurrences during an observation unit. A rate of occurrence is simply the mean count per standard observation period.
                    cores = 5,
                    chains = 10, 
                    iter = 10000,
                    warmup = 1000,
                    diagnostic_file = file.path(tempdir(), "df.csv")
)
plot(fit_glm)
em_condition <- emmeans(fit_glm, ~ Var2)
hyps <- c("c1 > c2 & c3 > c4",
          "c5 > c6 & c7 > c8"
)

##GLM Posterior predictive probability distributions##
plot_title <- ggplot2::ggtitle("Distribuciones posteriores",
                               "con media y SD e intervalos predictivos (90%)")
pp_check(fit_glm, plotfun = "stat_2d", stat = c("mean", "sd")) +
  plot_title
dev.print(jpeg, "1_pp_check_GLM.jpeg", res=900, height=8, width=16, units="in")

bayesplot::mcmc_areas(fit_glm, 
                      pars = c("Var2c1",
                               "Var2c2",
                               "Var2c3",
                               "Var2c4",
                               "Var2c5",
                               "Var2c6",
                               "Var2c7",
                               "Var2c8"),
                      prob = 0.9) + plot_title
dev.print(jpeg, "1_bayesplot_GLM_Var2.jpeg", res=900, height=8, width=16, units="in")

#BF GLM model evaluation##
BF_glm <- bayesfactor_parameters(fit_glm, null = 0)
BF_glm
effectsize::interpret_bf(exp(BF_glm$log_BF[3]), include_value = TRUE)
dev.print(jpeg, "1_BF_glm_parameters.jpeg", res=900, height=8, width=16, units="in")

#BF GLM restricted hypothesis##
posterior <- posterior_predict(fit_glmer5)
b <- bayesfactor_restricted(em_condition, hypothesis = hyps, prior = fit_glm)
as.numeric(b)
if (require("see") && require("patchwork")) {
  i <- attr(b, "bool_results")[["posterior"]]
  
  see::plots(
    plot(estimate_density(posterior)),
    # distribution **conditional** on the restrictions
    plot(estimate_density(posterior[i[[hyps[1]]], ])) + ggplot2::ggtitle(hyps[1]),
    plot(estimate_density(posterior[i[[hyps[2]]], ])) + ggplot2::ggtitle(hyps[2]),
    guides = "collect"
  )
}
dev.print(jpeg, "1_BF_GLM_restricted.jpeg", res=900, height=8, width=16, units="in")

md0 <- brm(Freq ~ 1 + Var1 + (1|Var2), #intercept, random slopes
           data = df.cond,
           family = poisson(),
           cores = 5,
           chains = 10, 
           iter = 10000,
           warmup = 1000,
           diagnostic_file = file.path(tempdir(), "df.csv")
)
summary(md0)
conditional_effects(md0)

md1 <- brm(Freq ~ 1 + Var1 + offset(log(lambda)) + (1|Var2), #intercept, random slopes
                                data = df.cond,
                                family = poisson(),
                                cores = 5,
                                chains = 10, 
                                iter = 10000,
                                warmup = 1000,
                                diagnostic_file = file.path(tempdir(), "df.csv")
)
summary(md1)
conditional_effects(md1)

dev.print(jpeg, "1_conditional_effects_Var2.jpeg", res=900, height=8, width=16, units="in")
md2 <- brm(Freq ~ 1 + Var1 + offset(log(lambda)) + (1+Var1|Var2), #intercept, random slopes
           data = df.cond,
           family = poisson(),
           cores = 5,
           chains = 10, 
           iter = 10000,
           warmup = 1000,
           diagnostic_file = file.path(tempdir(), "df.csv")
)
summary(md2)
conditional_effects(md2)

#Compare models with loo
loo_md0 <- loo(md0, cores = 5)
loo_md1 <- loo(md1, cores = 5)
loo_md2 <- loo(md2, cores = 5) 

loo_comp <- loo_compare(loo_md0, loo_md1, loo_md2)
loo_comp

h <- 'exp(Intercept + Var1baja * 1) = exp(Intercept + Var1baja * 0)' 
hypothesis(md2, h)

Var1baja_h <- 'exp(Intercept + Var1baja * 0) = 0' 
hypothesis(md2, Var1baja_h)

Var1alta_h <- 'exp(Intercept + Var1baja * 1) = 0'
hypothesis(md2, Var1alta_h)

pp_check(md2, nsample = 1000,
         type = 'ecdf_overlay')

dev.print(jpeg, "1_ppcheck_CLOZE.jpeg", res=900, height=8, width=16, units="in")

