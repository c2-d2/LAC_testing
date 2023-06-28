library(RColorBrewer)
library(latex2exp)
set.seed(123)
my_palette = brewer.pal(5, "Set1")[1:5]

# 0 Some descriptive statistics
# Characteristics of cities/communities in the training set
length(unique(training_rmv$geo_merge))
training_rmv %>% select(test_rate, pct_pos) %>% summary()

# Characteristics of cities/communities in the testing set
length(unique(test_clean$geo_merge))
test_clean %>% select(test_rate, pct_pos) %>% summary()

# 1 areas with top 10/20/.../100% dectile of positivity that were correctly predicted by each model
# Take mean # correct over the weeks
accurate <- apply(correct,c(2,3),mean, na.rm=TRUE)
accurate_pct <- accurate/accurate[,20] 

# Add in the extremes
accurate_pct[1,20] <- 1
accurate_pct[1,-20] <- 0 # replace NaN with 0
accurate_pct %>%
  as.data.frame.table() -> accurate_pct_df

# Tidy as a data frame for plotting
accurate_pct_df$model <- as.character(accurate_pct_df$model)

accurate_pct_dt <- accurate_pct_df %>% 
  mutate( var = ifelse(grepl("tminus2_4", model, fixed = TRUE), "Avg %pos t-2 to t",
                ifelse(grepl("tminus2", model, fixed = TRUE), "%pos t",
                ifelse(grepl("True", model, fixed = TRUE), "True values t+2",
                       "Random selection")))) %>%
  mutate( assumption = ifelse(grepl("glm_", model, fixed = TRUE), "OLS",
                       ifelse(grepl("AR2_", model, fixed = TRUE), "AR(2)", 
                              "Random intercept"))) %>%
  mutate( sim = ifelse(grepl("_simplest_", model, fixed = TRUE), "Simplest",
                ifelse(grepl("_sim_", model, fixed = TRUE), "Reduced", 
                       "Full")))

# Plot the correct % classified
accurate_pct_dt %>%
  mutate(top=as.factor(paste0("Top ", 100-as.numeric(as.character(percentile)), "%"))) -> accurate_pct_dt

## Rearraging Top 0% Top 10% Top 100% Top 20% Top 30% Top 40% Top 50% Top 60% Top 70% ... Top 90%
## as Top 0% Top 10% Top 20% Top 30% Top 40% Top 50% Top 60% Top 70% ... Top 90% Top 100% 
accurate_pct_dt$top <- factor(accurate_pct_dt$top, levels=levels(accurate_pct_dt$top)[c(1,2,4:11,3)])

### PART I. Models using %pos tminus2
accurate_pct_dt %>%
  filter(var %nin% "Avg %pos t-2 to t") -> accurate_pct_dt_tminus2

# OLS
accurate_pct_dt_tminus2$model <- as.character(accurate_pct_dt_tminus2$model)

accurate_pct_dt_tminus2 %>%
  filter(ifelse(grepl("glm", model, fixed = TRUE), TRUE,
                ifelse(grepl("Random", model, fixed = TRUE), TRUE, 
                       ifelse(grepl("True", model, fixed = TRUE), TRUE, FALSE)))) %>%
  mutate(legend= case_when(model=="glm_1a_simplest_tminus2" ~ "1. Simplest (% pos) 2-week lag",
                           model=="glm_1c_simplest_tminus3" ~ "2. Simplest (% pos) 3-week lag",
                           model=="glm_2a_sim_tminus2" ~ "3. Reduced (% pos + tests per 100)",
                           model=="glm_3a_comp_tminus2" ~"4. Full",
                           model=="Random" ~ "5. Random selection",
                           model=="True" ~ "6. True % pos at week t+2") )-> FigS5A1_data

FigS5A1_data %>%
  filter(!is.na(var),
         !is.na(legend)) %>%
  ggplot() +
  geom_line(aes(y=Freq, x=top, col=legend, group=legend), lwd=1) +
  theme_fig_3() +
  labs(y="Mean proportion of areas correctly predicted",
       x="Threshold",
       col=" ",
       title="Ordinary least squares") +
  theme(legend.position = c(0.7, 0.2),
        legend.text=element_text(size=12),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.background = element_rect(fill = "transparent")) +
  scale_color_manual(values = brewer.pal(6, "Set2")[c(1,6,2:5)],
                     labels=c("1. Simplest (% pos) 2-week lag",
                              "2. Simplest (% pos) 3-week lag",
                              "3. Reduced (% pos + tests per 100)",
                              "4. Full",
                              "5. Random selection",
                              unname(TeX(c("6. True % pos t+2"))))
  ) -> FigS5A1


# AR(2)
accurate_pct_dt_tminus2 %>%
  filter(ifelse(grepl("AR", model, fixed = TRUE), TRUE,
                ifelse(grepl("Random", model, fixed = TRUE), TRUE, 
                       ifelse(grepl("True", model, fixed = TRUE), TRUE, FALSE)))) %>%
  mutate(legend= case_when(grepl("_simplest_",model) ~ "1. Simplest (% pos) 2-week lag",
                           grepl("_sim_",model) ~ "2. Reduced (% pos + tests per 100)",
                           grepl("_comp_",model) ~ "3. Full",
                           model=="Random" ~ "4. Random selection",
                           model=="True" ~ "5. True % pos t+2")) -> FigS5A2_data

FigS5A2_data %>%
  ggplot() +
  geom_line(aes(y=Freq, x=top, col=legend, group=legend), lwd=1) +
  theme_fig_3() +
  labs(y="Mean proportion of areas correctly predicted",
       x="Threshold",
       col=" ",
       title="2nd-order autoregressive") +
  theme(legend.position = c(0.7, 0.2),
        legend.text=element_text(size=12),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.background = element_rect(fill = "transparent")) +
  scale_color_manual(values = brewer.pal(5, "Set2")) -> FigS5A2

# ME

accurate_pct_dt_tminus2 %>%
  filter(ifelse(grepl("me", model, fixed = TRUE), TRUE,
                ifelse(grepl("Random", model, fixed = TRUE), TRUE, 
                       ifelse(grepl("True", model, fixed = TRUE), TRUE, FALSE)))) %>%
  mutate(legend= case_when(grepl("_simplest_",model) ~ "1. Simplest (% pos) 2-week lag",
                           grepl("_sim_",model) ~ "2. Reduced (% pos + tests per 100)",
                           grepl("_comp_",model) ~ "3. Full",
                           model=="Random" ~ "4. Random selection",
                           model=="True" ~ "5. True % pos at week t+2")) -> FigS5A3_data

FigS5A3_data %>%
  ggplot() +
  geom_line(aes(y=Freq, x=top, col=legend, group=legend), lwd=1) +
  theme_fig_3() +
  labs(y="Mean proportion of areas correctly predicted",
       x="Threshold",
       col=" ",
       title="Community-specific intercepts") +
  theme(legend.position = c(0.7, 0.2),
        legend.text=element_text(size=12),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.background = element_rect(fill = "transparent")) +
  scale_color_manual(values = brewer.pal(5, "Set2")) -> FigS5A3

egg::ggarrange(FigS5A1, FigS5A2, FigS5A3, ncol=3) -> FigS5A
ggsave("R/Figures/FigS5A.png", FigS5A, width=22, height = 7, dpi = 300)

# PART II. tminus2 to tminus4
accurate_pct_dt %>%
  filter(var %nin% "%pos t") -> accurate_pct_dt_tminus2to4

# OLS
accurate_pct_dt_tminus2to4 %>%
  filter(model %nin% "glm_1c_simplest_tminus3") %>%
  filter(ifelse(grepl("glm", model, fixed = TRUE), TRUE,
                ifelse(grepl("Random", model, fixed = TRUE), TRUE, 
                       ifelse(grepl("True", model, fixed = TRUE), TRUE, FALSE)))) %>%
  mutate(legend= case_when(grepl("_simplest_",model)  ~ "1. Simplest (Avg %pos t-2 to t)",
                           grepl("_sim_",model) ~ "2. Reduced (Avg %pos t-2 to t + tests per 100)",
                           grepl("_comp_",model) ~ "3. Full",
                           model=="Random" ~ "4. Random selection",
                           model=="True" ~ "5. True % pos at week t+2")) -> FigS5B1_data
FigS5B1_data %>%
  ggplot() +
  geom_line(aes(y=Freq, x=top, col=legend, group=legend), lwd=1) +
  theme_fig_3() +
  labs(y="Mean proportion of areas correctly predicted",
       x="Threshold",
       col=" ",
       title="Ordinary least squares") +
  theme(legend.position = c(0.7, 0.2),
        legend.text=element_text(size=12),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.background = element_rect(fill = "transparent")) +
  scale_color_manual(values = brewer.pal(5, "Set2")) -> FigS5B1

# AR(2)
accurate_pct_dt_tminus2to4 %>%
  filter(ifelse(grepl("AR", model, fixed = TRUE), TRUE,
                ifelse(grepl("Random", model, fixed = TRUE), TRUE, 
                       ifelse(grepl("True", model, fixed = TRUE), TRUE, FALSE)))) %>%
  mutate(legend= case_when(grepl("_simplest_",model)  ~ "1. Simplest (Avg %pos t-2 to t)",
                           grepl("_sim_",model) ~ "2. Reduced (Avg %pos t-2 to t + tests per 100)",
                           grepl("_comp_",model) ~ "3. Full",
                           model=="Random" ~ "4. Random selection",
                           model=="True" ~ "5. True % pos at week t+2")) -> FigS5B2_data
FigS5B2_data %>%
  ggplot() +
  geom_line(aes(y=Freq, x=top, col=legend, group=legend), lwd=1) +
  theme_fig_3() +
  labs(y="Mean proportion of areas correctly predicted",
       x="Threshold",
       col=" ",
       title="2nd-order autoregressive") +
  theme(legend.position = c(0.7, 0.2),
        legend.text=element_text(size=12),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.background = element_rect(fill = "transparent")) +
  scale_color_manual(values = brewer.pal(5, "Set2")) -> FigS5B2

# ME

accurate_pct_dt_tminus2to4 %>%
  filter(ifelse(grepl("me", model, fixed = TRUE), TRUE,
                ifelse(grepl("Random", model, fixed = TRUE), TRUE, 
                       ifelse(grepl("True", model, fixed = TRUE), TRUE, FALSE)))) %>%
  mutate(legend= case_when(grepl("_simplest_",model)  ~ "1. Simplest (Avg %pos t-2 to t)",
                           grepl("_sim_",model) ~ "2. Reduced (Avg %pos t-2 to t + tests per 100)",
                           grepl("_comp_",model) ~ "3. Full",
                           model=="Random" ~ "4. Random selection",
                           model=="True" ~ "5. True % pos at week t+2")) -> FigS5B3_data
FigS5B3_data %>%
  ggplot() +
  geom_line(aes(y=Freq, x=top, col=legend, group=legend), lwd=1) +
  theme_fig_3() +
  labs(y="Mean proportion of areas correctly predicted",
       x="Threshold",
       col=" ",
       title="Community-specific intercepts") +
  theme(legend.position = c(0.7, 0.2),
        legend.text=element_text(size=11),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.background = element_rect(fill = "transparent")) +
  scale_color_manual(values = brewer.pal(5, "Set2")) -> FigS5B3

egg::ggarrange(FigS5B1, FigS5B2, FigS5B3, ncol=3) -> FigS5B
ggsave("R/Figures/FigS5B.png", FigS5B, width=22, height = 7, dpi = 300)

# Table S1
performance <- data.frame("Mean add cases" = round(apply(add_pos_10_1,2,sum)/length(vad_weeks), digits=0),
                          "Relative mean add cases (True=1)" = round(apply(add_pos_10_1,2,sum)/sum(add_pos_10_1[,20]), digits=3),
                          "Highest cases detected = 1" = (-apply(add_pos_10_1,2,sum)) %>% rank(),
                          "Mean NNT" = round(apply(NNT_all_ts_10_1,2,mean), digits=1), 
                          "Relative mean NNT (True=1)" = round(apply(NNT_all_ts_10_1,2,sum)/sum(NNT_all_ts_10_1[,20]), digits=3),
                          "Most efficient = 1" = apply(NNT_all_ts_10_1,2,mean) %>% rank())

write.csv(performance, "R/Tables/TableS3_all_model_performance.csv")

# Figures ----------------------------------------------------------------------
# Figure 1 Additional cases under the best model, simplest model, 
# random selection and perfect knowledge of future % pos 
# Figure 2 NNT

Fig1_legends <- c("1. Best model",
                  "2. Simplest model",
                  "Random selection",
                 "True % pos")

# Create emply dataframe to store results
Fig2_NNT_ts_0_1 <- Fig1_inc_ts_10_1 <- Fig1_pct_inc_ts_10_1 <- Fig1_add_pos_ts_10_1 <- Fig2_NNT_ts_10_1 <- array(0, lengths(list(week = as.character(unique(test_weeks$epi_week)), model = Fig1_legends)),
                                                                                                                            list(week = as.character(unique(test_weeks$epi_week)), model = Fig1_legends))
for(i in 1:length(vad_weeks)){
  Fig1_add_pos_ts_10_1[i,] <- LA_add_pos_all_ts[[length(vad_weeks)-i+1]][1,2,c(15,1,21,20)]
  Fig2_NNT_ts_10_1[i,] <- targeted_NNT_hyp_all_ts[[length(vad_weeks)-i+1]][1,2,c(15,1,21,20)]
  Fig1_pct_inc_ts_10_1[i,] <- pct_inc_targeted_areas_ts[[length(vad_weeks)-i+1]][1,2,c(15,1,21,20)]
  Fig1_inc_ts_10_1[i,] <- add_test_all_ts_10_1[i,c(15,1,21,20)]
}

Fig1_add_pos_ts_10_1 %>%
  as.data.frame.table() -> Fig1_data 

Fig1_data %>% 
  pivot_wider(names_from = "model", values_from = "Freq") -> Fig1_data_wide

theme_fig_3 <- function () { 
  theme_bw() %+replace% 
    theme(plot.title = element_text(size=24, hjust = 0.5, margin = margin(t = 15, r = 0, b = 15, l = 0), face = "bold",lineheight=1.3),
          plot.subtitle = element_text(size=18, hjust = 0.5, margin = margin(t = 0, r = 0, b = 15, l = 0)),
          plot.caption = element_text(size=18, hjust = 1, margin = margin(t = 15, r = 0, b = 0, l = 0)),
          axis.text.y = element_text(size=18),
          axis.text.x = element_text(size=18, angle = 90),
          axis.title.y = element_text(size=18, angle = 90, margin = margin(t = 0, r = 15, b = 0, l = 25)),
          axis.title.x = element_text(size=18, margin = margin(t = 15, r = 0, b = 0, l = 0)),
          panel.border = element_blank(),
          legend.position = "right",
          panel.ontop = FALSE,
          panel.grid.major = element_line(colour = "grey90"),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          legend.title=element_text(size=18), 
          legend.text=element_text(size=18)
    )
}

ggplot() +
  geom_line(data=Fig1_data_wide, aes(y=`1. Best model`, 
                                       x=week, 
                                       group=1, 
                                       col="1. Best model"), lwd=1) +
  geom_line(data=Fig1_data_wide, aes(y=`2. Simplest model`, 
                                       x=week, 
                                       group=1, 
                                       col="2. Simplest model"), lwd=1) +
  geom_line(data=Fig1_data_wide, aes(y=`True % pos`, 
                                       x=week, 
                                       group=1, 
                                       col="Perfect knowledge"), lwd=1) +
  geom_line(data=Fig1_data_wide, aes(y=`Random selection`, 
                                       x=week, 
                                       group=1, 
                                       col="Random selection"), lwd=1) +
  geom_ribbon(data=Fig1_data_wide, aes(ymin=`Random selection`,
                                         ymax=`True % pos`,
                                         x=week,
                                         group=1),
              fill = "grey50",
              alpha = 0.1) +
  theme_fig_3() +
  labs(y = "Additional cases (Count)",
       x = "Week",
       color = " ") +
  theme(plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        strip.text.x = element_text(size=18, angle = 0),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18),
        legend.text=element_text(size=18),
        strip.background = element_rect(colour=NA, fill=NA),
        legend.position = "right")+ 
  guides(col=guide_legend(nrow=4,byrow=TRUE)) -> Fig1

ggsave("R/Figures/Fig1.png", plot = Fig1, width = 16, height = 8, dpi = 300)

# NNT
Fig2_data <- Fig2_NNT_ts_10_1 %>%
  as.data.frame.table() 
colnames(Fig2_data) <- c("week","model","Freq")

Fig2_data %>% 
  pivot_wider(names_from = "model", values_from = "Freq") -> Fig2_data_wide

ggplot() +
  geom_line(data=Fig2_data_wide, aes(y=`1. Best model`, 
                                       x=week, 
                                       group=1, 
                                       col="1. Best model"), lwd=1) +
  geom_line(data=Fig2_data_wide, aes(y=`2. Simplest model`, 
                                       x=week, 
                                       group=1, 
                                       col="2. Simplest model"), lwd=1) +
  geom_line(data=Fig2_data_wide, aes(y=`True % pos`, 
                                       x=week, 
                                       group=1, 
                                       col="Perfect knowledge"), lwd=1) +
  geom_line(data=Fig2_data_wide, aes(y=`Random selection`, 
                                       x=week, 
                                       group=1, 
                                       col="Random selection"), lwd=1) +
  geom_ribbon(data=Fig2_data_wide, aes(ymin=`Random selection`,
                                         ymax=`True % pos`,
                                         x=week,
                                         group=1),
              fill = "grey50",
              alpha = 0.1) +
  theme_fig_3() +
  labs(y = "NNT in the selected communities (Count)",
       x = "Week",
       color = " ") +
  theme(strip.text.x = element_text(size=12, angle = 0),
        strip.background = element_rect(colour=NA, fill=NA),
        legend.position = "right") + 
  guides(col=guide_legend(nrow=4,byrow=TRUE)) -> Fig2

ggsave("R/Figures/Fig2.png", Fig2, width = 16, height = 8, dpi = 300)

