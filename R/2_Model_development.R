library(lmtest)
library(nlme)
library(lme4)
library(tidyverse)
library(dplyr)

theme_fig <- function () { 
  theme_bw() %+replace% 
    theme(plot.title = element_text(size=16, hjust = 0.5, margin = margin(t = 15, r = 0, b = 15, l = 0), face = "bold",lineheight=1.3),
          plot.subtitle = element_text(size=12, hjust = 0.5, margin = margin(t = 0, r = 0, b = 15, l = 0)),
          plot.caption = element_text(size=12, hjust = 1, margin = margin(t = 15, r = 0, b = 0, l = 0)),
          axis.text.y = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 90),
          axis.title.y = element_text(size=12, angle = 90, margin = margin(t = 0, r = 15, b = 0, l = 25)),
          axis.title.x = element_text(size=12, margin = margin(t = 15, r = 0, b = 0, l = 0)),
          panel.border = element_blank(),
          legend.position = "right",
          panel.ontop = FALSE,
          panel.grid.major = element_line(colour = "grey90"),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          legend.title=element_text(size=12), 
          legend.text=element_text(size=12)
    )
}

# Train models 
# 1 Create training set
training <- Data_master_weekly_3 %>%
  filter(time < 105)

training$id <- as.numeric(as.factor(training$geo_merge))

# 2 Generate complete a data set
training_rmv <- training[!is.na(training$pct_pos_tminus2) &
                           !is.na(training$pct_pos_tminus3) &
                           !is.na(training$test_rate_tminus2) &
                           !is.na(training$pct_cum_doses_tminus2) &
                           !is.na(training$pct_cum_inc_tminus2) &
                           !is.na(training$avg_pct_pos_past_2_4wk),]

# 3 Build model using training data
# PART I. Univariate model
# 1(a) % pos_tminus2

glm_1a_pct_pos <- glm(pct_pos ~ pct_pos_tminus2,
                      data = training_rmv, 
                      family = gaussian(), 
                      na.action = na.exclude)

summary(glm_1a_pct_pos)

glm_1c_pct_pos_3 <- glm(pct_pos ~ pct_pos_tminus3,
                        data = training_rmv, 
                        family = gaussian(), 
                        na.action = na.exclude)

summary(glm_1c_pct_pos_3)

AR2_1a_pct_pos <- gls(pct_pos ~ pct_pos_tminus2, 
                      data = training_rmv,
                      corr = corARMA(form=~1|id, p=2), 
                      method="REML", 
                      na.action = na.exclude)

summary(AR2_1a_pct_pos)

me_1a_pct_pos <- lme(pct_pos ~ pct_pos_tminus2,
                     random = ~ 1 | id,
                     method = "REML",
                     data = training_rmv)
summary(me_1a_pct_pos)

# 1(b) Avg % pos t-2 to t

glm_1b_pct_pos_2_4wk <- glm(pct_pos ~  avg_pct_pos_past_2_4wk,
                            data = training_rmv, 
                            family = gaussian(), 
                            na.action = na.exclude)

summary(glm_1b_pct_pos_2_4wk)

AR2_1b_pct_pos_2_4wk <- gls(pct_pos ~ avg_pct_pos_past_2_4wk, 
                            data = training_rmv,
                            corr = corARMA(form=~1|id, p=2), 
                            method="REML", 
                            na.action = na.exclude)

summary(AR2_1b_pct_pos_2_4wk)

me_1b_pct_pos_2_4wk <- lme(pct_pos ~ avg_pct_pos_past_2_4wk,
                           random = ~ 1 | id,
                           method = "REML",
                           data = training_rmv)
summary(me_1b_pct_pos_2_4wk)

# 2 Test t 
glm_2_test_rate <- glm(pct_pos ~ test_rate_tminus2,
                       data = training_rmv, 
                       family = gaussian(), 
                       na.action = na.exclude)
summary(glm_2_test_rate)

AR2_2_test_rate <- gls(pct_pos ~ test_rate_tminus2, 
                       data = training_rmv,
                       corr = corARMA(form=~1|id, p=2), 
                       method="REML", 
                       na.action = na.exclude)

summary(AR2_2_test_rate)

me_2_test_rate <- lme(pct_pos ~ test_rate_tminus2,
                      random = ~ 1 | id,
                      method = "REML",
                      data = training_rmv)
summary(me_2_test_rate)

# 3 Cum inc t
glm_3_cum_inc <- glm(pct_pos ~ pct_cum_inc_tminus2,
                     data = training_rmv, 
                     family = gaussian(), 
                     na.action = na.exclude)
summary(glm_3_cum_inc)

AR2_3_cum_inc <- gls(pct_pos ~ pct_cum_inc_tminus2, 
                     data = training_rmv,
                     corr = corARMA(form=~1|id, p=2), 
                     method="REML", 
                     na.action = na.exclude)
summary(AR2_3_cum_inc)

me_3_cum_inc <- lme(pct_pos ~ pct_cum_inc_tminus2,
                    random = ~ 1 | id,
                    method = "REML",
                    data = training_rmv)
summary(me_3_cum_inc)

# 4 % Vax t
glm_4_cum_doses <- glm(pct_pos ~ pct_cum_doses_tminus2,
                       data = training_rmv, 
                       family = gaussian(), 
                       na.action = na.exclude)

AR2_4_cum_doses <- gls(pct_pos ~ pct_cum_doses_tminus2,
                       data = training_rmv,
                       corr = corARMA(form=~1|id, p=2), 
                       method="REML", 
                       na.action = na.exclude)

summary(AR2_4_cum_doses)

me_4_cum_doses <- lme(pct_pos ~ pct_cum_doses_tminus2,
                      random = ~ 1 | id,
                      method = "REML",
                      data = training_rmv, 
                      na.action = na.exclude)
summary(me_4_cum_doses)

# Tabulate the univariate asso
univariate_result <- rbind(
  predictors = c("Coef",
                 "t-value",
                 "p-value",
                 "Coef",
                 "t-value",
                 "p-value",
                 "Coef",
                 "t-value",
                 "p-value"),
  data.frame(
    estimate = round(c(coef(summary(glm_1a_pct_pos))[2,1],
                       coef(summary(glm_1b_pct_pos_2_4wk))[2,1], 
                       coef(summary(glm_2_test_rate))[2,1], 
                       coef(summary(glm_3_cum_inc))[2,1], 
                       coef(summary(glm_4_cum_doses))[2,1]),4),
    t = round(c(coef(summary(glm_1a_pct_pos))[2,"t value"],
                coef(summary(glm_1b_pct_pos_2_4wk))[2,"t value"],
                coef(summary(glm_2_test_rate))[2,"t value"], 
                coef(summary(glm_3_cum_inc))[2,"t value"], 
                coef(summary(glm_4_cum_doses))[2,"t value"]),4),
    p = round(c(coef(summary(glm_1a_pct_pos))[2,"Pr(>|t|)"],
                coef(summary(glm_1b_pct_pos_2_4wk))[2,"Pr(>|t|)"],
                coef(summary(glm_2_test_rate))[2,"Pr(>|t|)"], 
                coef(summary(glm_3_cum_inc))[2,"Pr(>|t|)"], 
                coef(summary(glm_4_cum_doses))[2,"Pr(>|t|)"]),4),
    AR2_estimate = round(c(coef(summary(AR2_1a_pct_pos))[2,1],
                           coef(summary(AR2_1b_pct_pos_2_4wk))[2,1], 
                           coef(summary(AR2_2_test_rate))[2,1], 
                           coef(summary(AR2_3_cum_inc))[2,1], 
                           coef(summary(AR2_4_cum_doses))[2,1]),4),
    AR2_t = round(c(coef(summary(AR2_1a_pct_pos))[2,"t-value"],
                    coef(summary(AR2_1b_pct_pos_2_4wk))[2,"t-value"],
                    coef(summary(AR2_2_test_rate))[2,"t-value"], 
                    coef(summary(AR2_3_cum_inc))[2,"t-value"], 
                    coef(summary(AR2_4_cum_doses))[2,"t-value"]),4),
    AR2_p = round(c(coef(summary(AR2_1a_pct_pos))[2,"p-value"],
                    coef(summary(AR2_1b_pct_pos_2_4wk))[2,"p-value"],
                    coef(summary(AR2_2_test_rate))[2,"p-value"], 
                    coef(summary(AR2_3_cum_inc))[2,"p-value"], 
                    coef(summary(AR2_4_cum_doses))[2,"p-value"]),4),
    me_estimate = round(c(coef(summary(me_1a_pct_pos))[2,1],
                          coef(summary(me_1b_pct_pos_2_4wk))[2,1], 
                          coef(summary(me_2_test_rate))[2,1], 
                          coef(summary(me_3_cum_inc))[2,1], 
                          coef(summary(me_4_cum_doses))[2,1]),4),
    me_t = round(c(coef(summary(me_1a_pct_pos))[2,"t-value"],
                   coef(summary(me_1b_pct_pos_2_4wk))[2,"t-value"],
                   coef(summary(me_2_test_rate))[2,"t-value"], 
                   coef(summary(me_3_cum_inc))[2,"t-value"], 
                   coef(summary(me_4_cum_doses))[2,"t-value"]),4),
    me_p = round(c(coef(summary(me_1a_pct_pos))[2,"p-value"],
                   coef(summary(me_1b_pct_pos_2_4wk))[2,"p-value"],
                   coef(summary(me_2_test_rate))[2,"p-value"], 
                   coef(summary(me_3_cum_inc))[2,"p-value"], 
                   coef(summary(me_4_cum_doses))[2,"p-value"]),4)
  ) 
)

rownames(univariate_result) <- c("",
                                 "% tested pos t",
                                 "Average % tested pos t-2 to t", 
                                 "Tests per 100 t", 
                                 "Cumulative incidence t", 
                                 "% fully vaccinated t")

colnames(univariate_result) <- c(" ", 
                                 "OLS", 
                                 "  ",
                                 "   ", 
                                 "", 
                                 "AR(2)",
                                 "    ",
                                 "Random $b_i$",
                                 "     ")

write.csv(univariate_result, "R/Tables/TableS2.csv")

# PART II. Multivariable models  
# Naming: 1: % pos, 2: % pos + test per 100, 3: all var (a) PLUS replacing % pos with Avg % pos t-2 to 2 (b) 
# 1a. Simplest model w/ pos t
glm_1a_tminus2 <- glm(pct_pos ~ pct_pos_tminus2,
                      data = training_rmv, 
                      family = gaussian(), 
                      na.action = na.exclude)

summary(glm_1a_tminus2)

AR2_1a_tminus2 <- gls(pct_pos ~ pct_pos_tminus2,
                      data = training_rmv,
                      corr = corARMA(form=~1|id, p=2), 
                      method="REML", 
                      na.action = na.exclude)

summary(AR2_1a_tminus2)

me_1a_simplest <- lmer(pct_pos ~ (1 | id) + pct_pos_tminus2,
                       REML = TRUE,
                       data = training_rmv)
summary(me_1a_simplest)

# 2 Reduced model w/ pos t + test per 100
glm_2a_sim_tminus2 <- glm(pct_pos ~ pct_pos_tminus2 + test_rate_tminus2,
                          data = training_rmv, 
                          family = gaussian(), 
                          na.action = na.exclude)
summary(glm_2a_sim_tminus2)

AR2_2a_sim_tminus2 <- gls(pct_pos ~ pct_pos_tminus2 + test_rate_tminus2,
                          data = training_rmv,
                          corr = corARMA(form=~1|id, p=2), 
                          method="REML", 
                          na.action = na.exclude)

me_2a_sim_tminus2 <- lmer(pct_pos ~ (1 | id) + pct_pos_tminus2 + test_rate_tminus2,
                          REML = TRUE,
                          data = training_rmv) 

# 3a. Full model w/ pos t
glm_3a_comp_tminus2 <- glm(pct_pos ~ pct_pos_tminus2 + test_rate_tminus2 + pct_cum_doses_tminus2 + pct_cum_inc_tminus2,
                           data = training_rmv, 
                           family = gaussian(), 
                           na.action = na.exclude)

summary(glm_3a_comp_tminus2)

AR2_3a_comp_tminus2 <- gls(pct_pos ~ pct_pos_tminus2 + test_rate_tminus2 + pct_cum_doses_tminus2 + pct_cum_inc_tminus2,
                           data = training_rmv, 
                           corr = corARMA(form=~1|id, p=2), 
                           method="REML", 
                           na.action = na.exclude)

summary(AR2_3a_comp_tminus2)

me_3a_comp_tminus2 <- lmer(pct_pos ~ (1 | id) + pct_pos_tminus2 + test_rate_tminus2 + pct_cum_doses_tminus2 + pct_cum_inc_tminus2,
                           REML = TRUE,
                           data = training_rmv)  

summary(me_3a_comp_tminus2)

# 1b Simplest model w/ pos t-2 to t
glm_1b_tminus2_4 <- glm(pct_pos ~ avg_pct_pos_past_2_4wk,
                        data = training_rmv, 
                        family = gaussian(), 
                        na.action = na.exclude)

summary(glm_1b_tminus2_4)

AR2_1b_tminus2_4 <- gls(pct_pos ~ avg_pct_pos_past_2_4wk,
                        data = training_rmv,
                        corr = corARMA(form=~1|id, p=2), 
                        method="REML", 
                        na.action = na.exclude)

summary(AR2_1b_tminus2_4)

me_1b_tminus2_4 <- lmer(pct_pos ~ (1 | id) + avg_pct_pos_past_2_4wk,
                        REML = TRUE,
                        data = training_rmv)  
summary(me_1b_tminus2_4)

# 2b Reduced model w/ pos t-2 to t 
glm_2b_sim_tminus2_4 <- glm(pct_pos ~ avg_pct_pos_past_2_4wk + test_rate_tminus2,
                            data = training_rmv, 
                            family = gaussian(), 
                            na.action = na.exclude)

summary(glm_2b_sim_tminus2_4)

AR2_2b_sim_tminus2_4 <- gls(pct_pos ~ avg_pct_pos_past_2_4wk + test_rate_tminus2,
                            data = training_rmv,
                            corr = corARMA(form=~1|id, p=2), 
                            method="REML", 
                            na.action = na.exclude)

summary(AR2_2b_sim_tminus2_4)

me_2b_sim_tminus2_4 <- lmer(pct_pos ~ (1 | id) + avg_pct_pos_past_2_4wk + test_rate_tminus2,
                            REML = TRUE,
                            data = training_rmv)

summary(me_2b_sim_tminus2_4)


# 3b Full model w/ pos t-2 to t
glm_3b_comp_tminus2_4 <- glm(pct_pos ~ avg_pct_pos_past_2_4wk + test_rate_tminus2 + pct_cum_doses_tminus2 + pct_cum_inc_tminus2,
                             data = training_rmv, 
                             family = gaussian(), 
                             na.action = na.exclude)

summary(glm_3b_comp_tminus2_4)

AR2_3b_comp_tminus2_4 <- gls(pct_pos ~ avg_pct_pos_past_2_4wk + test_rate_tminus2 + pct_cum_doses_tminus2 + pct_cum_inc_tminus2,
                             data = training_rmv,
                             corr = corARMA(form=~1|id, p=2), 
                             method="REML", 
                             na.action = na.exclude)

summary(AR2_3b_comp_tminus2_4)

me_3b_comp_tminus2_4 <- lmer(pct_pos ~ (1 | id) + avg_pct_pos_past_2_4wk + test_rate_tminus2 + pct_cum_doses_tminus2 + pct_cum_inc_tminus2,
                             REML = TRUE,
                             data = training_rmv)

summary(me_3b_comp_tminus2_4)

# Tabulate multivariate regression results 
multivariate_result_dim <- list(estimate = c("Coef",
                                             "Lower",
                                             "Upper"),
                                model = c("Simplest",
                                          "Reduced",
                                          "Full"),
                                variables = c("1. % Positive", 
                                              "2. Tests per 100",
                                              "3. Cumulative \n% fully vaccinated",
                                              "4. Cumulative \nincidence per 100"),
                                methods = c("Ordinary \nleast squares",
                                            "2nd-order \n autoregressive",
                                            "Community-specific \n intercept"))

multivariate_result <- array(NA, lengths(multivariate_result_dim), multivariate_result_dim)

for(k in 1:3){
  for(i in 1:3){
    if(k == 1) {
      if(i == 1){
        multivariate_result[k,1,1,i] <- coef(summary(glm_1a_tminus2))[2,1]
        multivariate_result[k,2,1,i] <- coef(summary(glm_2a_sim_tminus2))[2,1]
        multivariate_result[k,2,2,i] <- coef(summary(glm_2a_sim_tminus2))[3,1]
        multivariate_result[k,3,1,i] <- coef(summary(glm_3a_comp_tminus2))[2,1]
        multivariate_result[k,3,2,i] <- coef(summary(glm_3a_comp_tminus2))[3,1]
        multivariate_result[k,3,3,i] <- coef(summary(glm_3a_comp_tminus2))[4,1]
        multivariate_result[k,3,4,i] <- coef(summary(glm_3a_comp_tminus2))[5,1]
      }
      if(i == 2){
        multivariate_result[k,1,1,i] <- coef(summary(AR2_1a_tminus2))[2,1]
        multivariate_result[k,2,1,i] <- coef(summary(AR2_2a_sim_tminus2))[2,1]
        multivariate_result[k,2,2,i] <- coef(summary(AR2_2a_sim_tminus2))[3,1]
        multivariate_result[k,3,1,i] <- coef(summary(AR2_3a_comp_tminus2))[2,1]
        multivariate_result[k,3,2,i] <- coef(summary(AR2_3a_comp_tminus2))[3,1]
        multivariate_result[k,3,3,i] <- coef(summary(AR2_3a_comp_tminus2))[4,1]
        multivariate_result[k,3,4,i] <- coef(summary(AR2_3a_comp_tminus2))[5,1]
      }
      if(i == 3){
        multivariate_result[k,1,1,i] <- coef(summary(me_1a_simplest))[2,1]
        multivariate_result[k,2,1,i] <- coef(summary(me_2a_sim_tminus2))[2,1]
        multivariate_result[k,2,2,i] <- coef(summary(me_2a_sim_tminus2))[3,1]
        multivariate_result[k,3,1,i] <- coef(summary(me_3a_comp_tminus2))[2,1]
        multivariate_result[k,3,2,i] <- coef(summary(me_3a_comp_tminus2))[3,1]
        multivariate_result[k,3,3,i] <- coef(summary(me_3a_comp_tminus2))[4,1]
        multivariate_result[k,3,4,i] <- coef(summary(me_3a_comp_tminus2))[5,1]
      }
    }
    
    if(k == 2) {
      if(i == 1){
        multivariate_result[k,1,1,i] <- confint(glm_1a_tminus2)[2,1]
        multivariate_result[k,2,1,i] <- confint(glm_2a_sim_tminus2)[2,1]
        multivariate_result[k,2,2,i] <- confint(glm_2a_sim_tminus2)[3,1]
        multivariate_result[k,3,1,i] <- confint(glm_3a_comp_tminus2)[2,1]
        multivariate_result[k,3,2,i] <- confint(glm_3a_comp_tminus2)[3,1]
        multivariate_result[k,3,3,i] <- confint(glm_3a_comp_tminus2)[4,1]
        multivariate_result[k,3,4,i] <- confint(glm_3a_comp_tminus2)[5,1]
      }
      if(i == 2){
        multivariate_result[k,1,1,i] <- confint(AR2_1a_tminus2)[2,1]
        multivariate_result[k,2,1,i] <- confint(AR2_2a_sim_tminus2)[2,1]
        multivariate_result[k,2,2,i] <- confint(AR2_2a_sim_tminus2)[3,1]
        multivariate_result[k,3,1,i] <- confint(AR2_3a_comp_tminus2)[2,1]
        multivariate_result[k,3,2,i] <- confint(AR2_3a_comp_tminus2)[3,1]
        multivariate_result[k,3,3,i] <- confint(AR2_3a_comp_tminus2)[4,1]
        multivariate_result[k,3,4,i] <- confint(AR2_3a_comp_tminus2)[5,1]
      }
      if(i == 3){
        multivariate_result[k,1,1,i] <- confint(me_1a_simplest)[4,1]
        multivariate_result[k,2,1,i] <- confint(me_2a_sim_tminus2)[4,1]
        multivariate_result[k,2,2,i] <- confint(me_2a_sim_tminus2)[5,1]
        multivariate_result[k,3,1,i] <- confint(me_3a_comp_tminus2)[4,1]
        multivariate_result[k,3,2,i] <- confint(me_3a_comp_tminus2)[5,1]
        multivariate_result[k,3,3,i] <- confint(me_3a_comp_tminus2)[6,1]
        multivariate_result[k,3,4,i] <- confint(me_3a_comp_tminus2)[7,1]
      }
    }
    if(k == 3) {
      if(i == 1){
        multivariate_result[k,1,1,i] <- confint(glm_1a_tminus2)[2,2]
        multivariate_result[k,2,1,i] <- confint(glm_2a_sim_tminus2)[2,2]
        multivariate_result[k,2,2,i] <- confint(glm_2a_sim_tminus2)[3,2]
        multivariate_result[k,3,1,i] <- confint(glm_3a_comp_tminus2)[2,2]
        multivariate_result[k,3,2,i] <- confint(glm_3a_comp_tminus2)[3,2]
        multivariate_result[k,3,3,i] <- confint(glm_3a_comp_tminus2)[4,2]
        multivariate_result[k,3,4,i] <- confint(glm_3a_comp_tminus2)[5,2]
      }
      if(i == 2){
        multivariate_result[k,1,1,i] <- confint(AR2_1a_tminus2)[2,2]
        multivariate_result[k,2,1,i] <- confint(AR2_2a_sim_tminus2)[2,2]
        multivariate_result[k,2,2,i] <- confint(AR2_2a_sim_tminus2)[3,2]
        multivariate_result[k,3,1,i] <- confint(AR2_3a_comp_tminus2)[2,2]
        multivariate_result[k,3,2,i] <- confint(AR2_3a_comp_tminus2)[3,2]
        multivariate_result[k,3,3,i] <- confint(AR2_3a_comp_tminus2)[4,2]
        multivariate_result[k,3,4,i] <- confint(AR2_3a_comp_tminus2)[5,2]
      }
      if(i == 3){
        multivariate_result[k,1,1,i] <- confint(me_1a_simplest)[4,2]
        multivariate_result[k,2,1,i] <- confint(me_2a_sim_tminus2)[4,2]
        multivariate_result[k,2,2,i] <- confint(me_2a_sim_tminus2)[5,2]
        multivariate_result[k,3,1,i] <- confint(me_3a_comp_tminus2)[4,2]
        multivariate_result[k,3,2,i] <- confint(me_3a_comp_tminus2)[5,2]
        multivariate_result[k,3,3,i] <- confint(me_3a_comp_tminus2)[6,2]
        multivariate_result[k,3,4,i] <- confint(me_3a_comp_tminus2)[7,2]
      }
    }
  }
}

multivariate_result <- round(multivariate_result, 3)

# Seperate point estimate and the 95% CI
multivariate_result_df <- as.data.frame.table(multivariate_result[1,,,])
multivariate_result_95ci <- as.data.frame.table(multivariate_result[2:3,,,]) %>%
  pivot_wider(names_from = "estimate", values_from = "Freq") %>%
  left_join(multivariate_result_df, by = c("variables", "methods", "model"))

ggplot(data = na.omit(multivariate_result_df), aes(x = Freq, y = variables, col = variables, label = Freq)) +
  geom_point() + 
  geom_text(hjust= - 0.2, vjust= - 1, show.legend = FALSE, size=4) +
  geom_errorbar(data = na.omit(multivariate_result_95ci), aes(xmin=Lower,xmax=Upper, y = variables, col = variables), width=0)+
  theme_fig_3() +
  facet_grid(model~methods, scales = "free", space = "free", switch = "y")+
  labs(y="Variable",
       x="Effect size",
       col = "Variable") +
  lims(x = c(-1, 1)) +
  geom_vline(aes(xintercept = 0), col = "grey") +
  scale_y_discrete(limits=rev) +
  theme(strip.text.y = element_text(size=16, angle = 0),
        strip.text.x = element_text(size=16, angle = 0),
        strip.background = element_rect(colour=NA, fill=NA),
        axis.line.x.bottom = element_line(color = 'black'),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=16, angle = 0, hjust = 0.95),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=16),
        legend.position = "none") -> multivariate_result_plot

multivariate_result_plot

ggsave("R/Figures/FigS3_multivariate_pct_pos.png", 
       plot = multivariate_result_plot,
       width = 13,
       height = 7,
       dpi = 300)

multivariate_result_dim_tminus2_4 <- list(estimate = c("Coef",
                                                       "Lower",
                                                       "Upper"),
                                          model = c("Simplest",
                                                    "Reduced",
                                                    "Full"),
                                          variables = c("1. Average 3-week \n% positive", 
                                                        "2. Tests per 100",
                                                        "3. Cumulative \n% fully vaccinated",
                                                        "4. Cumulative \nincidence per 100"),
                                          methods = c("Ordinary \nleast squares",
                                                      "2nd-order \n autoregressive",
                                                      "Community-specific \n intercept"))

multivariate_result_tminus2_4 <- array(NA, lengths(multivariate_result_dim_tminus2_4), multivariate_result_dim_tminus2_4)

for(k in 1:3){
  for(i in 1:3){
    if(k == 1) {
      if(i == 1){
        multivariate_result_tminus2_4[k,1,1,i] <- coef(summary(glm_1b_tminus2_4))[2,1]
        multivariate_result_tminus2_4[k,2,1,i] <- coef(summary(glm_2b_sim_tminus2_4))[2,1]
        multivariate_result_tminus2_4[k,2,2,i] <- coef(summary(glm_2b_sim_tminus2_4))[3,1]
        multivariate_result_tminus2_4[k,3,1,i] <- coef(summary(glm_3b_comp_tminus2_4))[2,1]
        multivariate_result_tminus2_4[k,3,2,i] <- coef(summary(glm_3b_comp_tminus2_4))[3,1]
        multivariate_result_tminus2_4[k,3,3,i] <- coef(summary(glm_3b_comp_tminus2_4))[4,1]
        multivariate_result_tminus2_4[k,3,4,i] <- coef(summary(glm_3b_comp_tminus2_4))[5,1]
      }
      if(i == 2){
        multivariate_result_tminus2_4[k,1,1,i] <- coef(summary(AR2_1b_tminus2_4))[2,1]
        multivariate_result_tminus2_4[k,2,1,i] <- coef(summary(AR2_2b_sim_tminus2_4))[2,1]
        multivariate_result_tminus2_4[k,2,2,i] <- coef(summary(AR2_2b_sim_tminus2_4))[3,1]
        multivariate_result_tminus2_4[k,3,1,i] <- coef(summary(AR2_3b_comp_tminus2_4))[2,1]
        multivariate_result_tminus2_4[k,3,2,i] <- coef(summary(AR2_3b_comp_tminus2_4))[3,1]
        multivariate_result_tminus2_4[k,3,3,i] <- coef(summary(AR2_3b_comp_tminus2_4))[4,1]
        multivariate_result_tminus2_4[k,3,4,i] <- coef(summary(AR2_3b_comp_tminus2_4))[5,1]
      }
      if(i == 3){
        multivariate_result_tminus2_4[k,1,1,i] <- coef(summary(me_1b_tminus2_4))[2,1]
        multivariate_result_tminus2_4[k,2,1,i] <- coef(summary(me_2b_sim_tminus2_4))[2,1]
        multivariate_result_tminus2_4[k,2,2,i] <- coef(summary(me_2b_sim_tminus2_4))[3,1]
        multivariate_result_tminus2_4[k,3,1,i] <- coef(summary(me_3b_comp_tminus2_4))[2,1]
        multivariate_result_tminus2_4[k,3,2,i] <- coef(summary(me_3b_comp_tminus2_4))[3,1]
        multivariate_result_tminus2_4[k,3,3,i] <- coef(summary(me_3b_comp_tminus2_4))[4,1]
        multivariate_result_tminus2_4[k,3,4,i] <- coef(summary(me_3b_comp_tminus2_4))[5,1]
      }
    }
    
    if(k == 2) {
      if(i == 1){
        multivariate_result_tminus2_4[k,1,1,i] <- confint(glm_1b_tminus2_4)[2,1]
        multivariate_result_tminus2_4[k,2,1,i] <- confint(glm_2b_sim_tminus2_4)[2,1]
        multivariate_result_tminus2_4[k,2,2,i] <- confint(glm_2b_sim_tminus2_4)[3,1]
        multivariate_result_tminus2_4[k,3,1,i] <- confint(glm_3b_comp_tminus2_4)[2,1]
        multivariate_result_tminus2_4[k,3,2,i] <- confint(glm_3b_comp_tminus2_4)[3,1]
        multivariate_result_tminus2_4[k,3,3,i] <- confint(glm_3b_comp_tminus2_4)[4,1]
        multivariate_result_tminus2_4[k,3,4,i] <- confint(glm_3b_comp_tminus2_4)[5,1]
      }
      if(i == 2){
        multivariate_result_tminus2_4[k,1,1,i] <- confint(AR2_1b_tminus2_4)[2,1]
        multivariate_result_tminus2_4[k,2,1,i] <- confint(AR2_2b_sim_tminus2_4)[2,1]
        multivariate_result_tminus2_4[k,2,2,i] <- confint(AR2_2b_sim_tminus2_4)[3,1]
        multivariate_result_tminus2_4[k,3,1,i] <- confint(AR2_3b_comp_tminus2_4)[2,1]
        multivariate_result_tminus2_4[k,3,2,i] <- confint(AR2_3b_comp_tminus2_4)[3,1]
        multivariate_result_tminus2_4[k,3,3,i] <- confint(AR2_3b_comp_tminus2_4)[4,1]
        multivariate_result_tminus2_4[k,3,4,i] <- confint(AR2_3b_comp_tminus2_4)[5,1]
      }
      if(i == 3){
        multivariate_result_tminus2_4[k,1,1,i] <- confint(me_1b_tminus2_4)[4,1]
        multivariate_result_tminus2_4[k,2,1,i] <- confint(me_2b_sim_tminus2_4)[4,1]
        multivariate_result_tminus2_4[k,2,2,i] <- confint(me_2b_sim_tminus2_4)[5,1]
        multivariate_result_tminus2_4[k,3,1,i] <- confint(me_3b_comp_tminus2_4)[4,1]
        multivariate_result_tminus2_4[k,3,2,i] <- confint(me_3b_comp_tminus2_4)[5,1]
        multivariate_result_tminus2_4[k,3,3,i] <- confint(me_3b_comp_tminus2_4)[6,1]
        multivariate_result_tminus2_4[k,3,4,i] <- confint(me_3b_comp_tminus2_4)[7,1]
      }
    }
    if(k == 3) {
      if(i == 1){
        multivariate_result_tminus2_4[k,1,1,i] <- confint(glm_1b_tminus2_4)[2,2]
        multivariate_result_tminus2_4[k,2,1,i] <- confint(glm_2b_sim_tminus2_4)[2,2]
        multivariate_result_tminus2_4[k,2,2,i] <- confint(glm_2b_sim_tminus2_4)[3,2]
        multivariate_result_tminus2_4[k,3,1,i] <- confint(glm_3b_comp_tminus2_4)[2,2]
        multivariate_result_tminus2_4[k,3,2,i] <- confint(glm_3b_comp_tminus2_4)[3,2]
        multivariate_result_tminus2_4[k,3,3,i] <- confint(glm_3b_comp_tminus2_4)[4,2]
        multivariate_result_tminus2_4[k,3,4,i] <- confint(glm_3b_comp_tminus2_4)[5,2]
      }
      if(i == 2){
        multivariate_result_tminus2_4[k,1,1,i] <- confint(AR2_1b_tminus2_4)[2,2]
        multivariate_result_tminus2_4[k,2,1,i] <- confint(AR2_2b_sim_tminus2_4)[2,2]
        multivariate_result_tminus2_4[k,2,2,i] <- confint(AR2_2b_sim_tminus2_4)[3,2]
        multivariate_result_tminus2_4[k,3,1,i] <- confint(AR2_3b_comp_tminus2_4)[2,2]
        multivariate_result_tminus2_4[k,3,2,i] <- confint(AR2_3b_comp_tminus2_4)[3,2]
        multivariate_result_tminus2_4[k,3,3,i] <- confint(AR2_3b_comp_tminus2_4)[4,2]
        multivariate_result_tminus2_4[k,3,4,i] <- confint(AR2_3b_comp_tminus2_4)[5,2]
      }
      if(i == 3){
        multivariate_result_tminus2_4[k,1,1,i] <- confint(me_1b_tminus2_4)[4,2]
        multivariate_result_tminus2_4[k,2,1,i] <- confint(me_2b_sim_tminus2_4)[4,2]
        multivariate_result_tminus2_4[k,2,2,i] <- confint(me_2b_sim_tminus2_4)[5,2]
        multivariate_result_tminus2_4[k,3,1,i] <- confint(me_3b_comp_tminus2_4)[4,2]
        multivariate_result_tminus2_4[k,3,2,i] <- confint(me_3b_comp_tminus2_4)[5,2]
        multivariate_result_tminus2_4[k,3,3,i] <- confint(me_3b_comp_tminus2_4)[6,2]
        multivariate_result_tminus2_4[k,3,4,i] <- confint(me_3b_comp_tminus2_4)[7,2]
      }
    }
  }
}

multivariate_result_tminus2_4 <- round(multivariate_result_tminus2_4, 3)

multivariate_result_tminus2_4_df <- as.data.frame.table(multivariate_result_tminus2_4[1,,,])

multivariate_result_tminus2_4_95ci <- as.data.frame.table(multivariate_result_tminus2_4[2:3,,,]) %>%
  pivot_wider(names_from = "estimate", values_from = "Freq") %>%
  left_join(multivariate_result_tminus2_4_df, by = c("variables", "methods", "model"))


ggplot(data = na.omit(multivariate_result_tminus2_4_df), aes(x = Freq, y = variables, col = variables, label = Freq)) +
  geom_point() + 
  geom_text(hjust= - 0.2, vjust= - 1, show.legend = FALSE, size=4) +
  geom_errorbar(data = na.omit(multivariate_result_tminus2_4_95ci), aes(xmin=Lower,xmax=Upper, y = variables, col = variables), width=0)+
  theme_fig_3() +
  facet_grid(model~methods, scales = "free", space = "free", switch = "y")+
  labs(y="Variable",
       x="Effect size",
       col = "Variable") +
  lims(x = c(-1, 1)) +
  geom_vline(aes(xintercept = 0), col = "grey") +
  scale_y_discrete(limits=rev) +
  theme(strip.text.y = element_text(size=16, angle = 0),
        strip.text.x = element_text(size=16, angle = 0),
        strip.background = element_rect(colour=NA, fill=NA),
        axis.line.x.bottom = element_line(color = 'black'),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=16, angle = 0, hjust = 0.95),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=16),
        legend.position = "none") -> multivariate_result_tminus2_4_plot

multivariate_result_tminus2_4_plot

ggsave("R/Figures/FigS4_multivariate_avg_3wk.png", 
       plot = multivariate_result_tminus2_4_plot, 
       width = 13, 
       height = 7,
       dpi = 300)
