# P300 Force Inhibition Study - Nguyen, Albrecht, Lipp, & Marinovic - Under Revision #

rm(list=ls()) #clears environment
options(scipen=999) #suppress scientific notation

require(openxlsx) 
require(data.table)
require(lmerTest) 
require(ggplot2) 
require(Rmisc) 
require(emmeans) 
require(jtools)
require(psych) 
require(ggplot2) 
require(extrafont) 
require(gridExtra)
require(r2glmm)
# font_import()
loadfonts(device = "win")

# load data
data_folder <- dirname(rstudioapi::getSourceEditorContext()$path)
all_data_exp1 <- data.table(read.xlsx(paste(data_folder, "all_data_exp1.xlsx", sep = "/")))
all_data_exp2 <- data.table(read.xlsx(paste(data_folder, "all_data_exp2.xlsx", sep = "/")))

# set condition variables as factors
all_data_exp1$Condition <- factor(all_data_exp1$Condition)
all_data_exp2$Condition <- factor(all_data_exp2$Condition)
all_data_exp1$Condition2 <- factor(all_data_exp1$Condition2)
all_data_exp2$Condition2 <- factor(all_data_exp2$Condition2)

### Nunber of trials retained ###
ntrials <- aggregate(Trial ~ Condition + Subject_ID, data = all_data_exp1, length)
names(ntrials)[names(ntrials) == "Trial"] <- "Number.of.Trials"
paste0("Average number of Valid Go trials:", round(mean(ntrials$Number.of.Trials[ntrials$Condition == "Valid Go"]), digits = 2), " SD = ", round(sd(ntrials$Number.of.Trials[ntrials$Condition == "Valid Go"]), digits = 2))
paste0("Average number of Valid Ignore Go trials:", round(mean(ntrials$Number.of.Trials[ntrials$Condition == "Valid Ignore Go"]), digits = 2), " SD = ", round(sd(ntrials$Number.of.Trials[ntrials$Condition == "Valid Ignore Go"]), digits = 2))
paste0("Average number of Successful Stop trials:", round(mean(ntrials$Number.of.Trials[ntrials$Condition == "Successful Stop"]), digits = 2), " SD = ", round(sd(ntrials$Number.of.Trials[ntrials$Condition == "Successful Stop"]), digits = 2))
paste0("Average number of Failed Stop trials:", round(mean(ntrials$Number.of.Trials[ntrials$Condition == "Failed Stop"]), digits = 2), " SD = ", round(sd(ntrials$Number.of.Trials[ntrials$Condition == "Failed Stop"]), digits = 2))

ntrials <- aggregate(Trial ~ Condition + Subject_ID, data = all_data_exp2, length)
names(ntrials)[names(ntrials) == "Trial"] <- "Number.of.Trials"
paste0("Average number of Go trials:", round(mean(ntrials$Number.of.Trials[ntrials$Condition == "Valid Go"]), digits = 2), " SD = ", round(sd(ntrials$Number.of.Trials[ntrials$Condition == "Valid Go"]), digits = 2))
paste0("Average number of Successful Stop trials:", round(mean(ntrials$Number.of.Trials[ntrials$Condition == "Successful Nogo"]), digits = 2), " SD = ", round(sd(ntrials$Number.of.Trials[ntrials$Condition == "Successful Nogo"]), digits = 2))
paste0("Average number of Failed Stop trials:", round(mean(ntrials$Number.of.Trials[ntrials$Condition == "Failed Nogo"]), digits = 2), " SD = ", round(sd(ntrials$Number.of.Trials[ntrials$Condition == "Failed Nogo"]), digits = 2))

######################
## Behavioural Data ##
######################

#########################
### Movement Onset RT ### 
#########################

# Exp. 1
data2use <- subset(all_data_exp1, Condition %in% c("Valid Go", "Failed Stop"))
data2use <- aggregate(RT_movement_onset ~ Condition + Subject_ID, data = data2use, mean)
data2use$Condition <- factor(data2use$Condition, levels = c("Valid Go", "Failed Stop")) 
rt_model_exp1 <- lmer(RT_movement_onset ~ Condition + (1|Subject_ID), data = data2use) #lmm
emmeans(rt_model_exp1, list(pairwise ~ Condition), adjust = "tukey") #t-ratio
# plot
summary4plot <- summarySEwithin(data2use, measurevar = "RT_movement_onset", withinvars = c("Condition"), idvar="Subject_ID", conf.interval = 0.95) #variances invalid, compute manually
CI_1 <- describe(data2use$RT_movement_onset[data2use$Condition == "Valid Go"])$se*1.96
CI_2 <- describe(data2use$RT_movement_onset[data2use$Condition == "Failed Stop"])$se*1.96
CI <- rbind(CI_1, CI_2)
rt_plot_exp1 <- ggplot(data = data2use, aes(x = Condition, y = RT_movement_onset)) +
  geom_jitter(alpha = 0.3, width = 0.2, size = 2, aes(color = Condition)) + #individual points
  scale_colour_manual(values=c("palegreen3","red")) +
  geom_errorbar(data = summary4plot, aes(ymin = RT_movement_onset-CI, ymax = RT_movement_onset+CI), colour = "black", width=.1, size = 0.75, position = position_dodge(.5)) + #confidence interval
  geom_point(data = summary4plot, aes(x = Condition, y = RT_movement_onset), size = 2) + #grand-averaged points
  geom_point(data = summary4plot, colour = "white",size = 1, alpha = 0.5) +
  geom_abline(intercept = 0, slope = 0, col = "black", linetype = 'dashed', size = 1, alpha = 0.2) +
  ggtitle("") +
  xlab("") +
  ylab("Movement Onset (ms)")+
  theme_classic() +
  theme(legend.position = "none", text = element_text(size=12, family="Calibri"), plot.title = element_text(hjust = 0.5))
rt_plot_exp1

# Exp. 2
data2use <- subset(all_data_exp2, Condition %in% c("Valid Go", "Failed Nogo"))
data2use <- aggregate(RT_movement_onset ~ Condition + Subject_ID, data = data2use, mean)
data2use$Condition <- factor(data2use$Condition, levels = c("Valid Go", "Failed Nogo")) 
rt_model_exp2 <- lmer(RT_movement_onset ~ Condition + (1|Subject_ID), data = data2use) #lmm
emmeans(rt_model_exp2, list(pairwise ~ Condition), adjust = "tukey") #t-ratio
# plot
summary4plot <- summarySEwithin(data2use, measurevar = "RT_movement_onset", withinvars = c("Condition"), idvar="Subject_ID", conf.interval = 0.95) #variances invalid, compute manually
CI_1 <- describe(data2use$RT_movement_onset[data2use$Condition == "Valid Go"])$se*1.96
CI_2 <- describe(data2use$RT_movement_onset[data2use$Condition == "Failed Nogo"])$se*1.96
CI <- rbind(CI_1, CI_2)
rt_plot_exp2 <- ggplot(data = data2use, aes(x = Condition, y = RT_movement_onset)) +
  geom_jitter(alpha = 0.3, width = 0.2, size = 2, aes(color = Condition)) + #individual points
  scale_colour_manual(values=c("palegreen3","red")) +
  geom_errorbar(data = summary4plot, aes(ymin = RT_movement_onset-CI, ymax = RT_movement_onset+CI), colour = "black", width=.1, size = 0.75, position = position_dodge(.5)) + #confidence interval
  geom_point(data = summary4plot, aes(x = Condition, y = RT_movement_onset), size = 2) + #grand-averaged points
  geom_point(data = summary4plot, colour = "white",size = 1, alpha = 0.5) +
  ggtitle("") +
  xlab("") +
  ylab("Movement Onset (ms)")+
  theme_classic() +
  theme(legend.position = "none", text = element_text(size=12, family="Calibri"), plot.title = element_text(hjust = 0.5))
rt_plot_exp2

##################
### Peak Force ###
##################

# Exp. 1
data2use <- subset(all_data_exp1, Condition %in% c("Valid Go", "Failed Stop"))
data2use <- aggregate(Peak_force ~ Condition + Subject_ID, data = data2use, mean)
data2use$Condition <- factor(data2use$Condition, levels = c("Valid Go", "Failed Stop")) 
peak_force_model_exp1 <- lmer(Peak_force ~ Condition + (1|Subject_ID), data = data2use) #lmm
emmeans(peak_force_model_exp1, list(pairwise ~ Condition), adjust = "tukey") #t-ratio
# plot
summary4plot <- summarySEwithin(data2use, measurevar = "Peak_force", withinvars = c("Condition"), idvar="Subject_ID", conf.interval = 0.95) #variances invalid, compute manually
CI_1 <- describe(data2use$Peak_force[data2use$Condition == "Valid Go"])$se*1.96
CI_2 <- describe(data2use$Peak_force[data2use$Condition == "Failed Stop"])$se*1.96
CI <- rbind(CI_1, CI_2)
peak_force_plot_exp1 <- ggplot(data = data2use, aes(x = Condition, y = Peak_force)) +
  geom_jitter(alpha = 0.3, width = 0.2, size = 2, aes(color = Condition)) + #individual points
  scale_colour_manual(values=c("palegreen3","red")) +
  geom_errorbar(data = summary4plot, aes(ymin = Peak_force-CI, ymax = Peak_force+CI), colour = "black", width=.1, size = 0.75, position = position_dodge(.5)) + #confidence interval
  geom_point(data = summary4plot, aes(x = Condition, y = Peak_force), size = 2) + #grand-averaged points
  geom_point(data = summary4plot, colour = "white",size = 1, alpha = 0.5) +
  geom_abline(intercept = 0, slope = 0, col = "black", linetype = 'dashed', size = 1, alpha = 0.2) +
  ggtitle("") +
  xlab("") +
  ylab("Peak Force (N)")+
  theme_classic() +
  theme(legend.position = "none", text = element_text(size=12, family="Calibri"), plot.title = element_text(hjust = 0.5))
peak_force_plot_exp1

# Exp. 2
data2use <- subset(all_data_exp2, Condition %in% c("Valid Go", "Failed Nogo"))
data2use <- aggregate(Peak_force ~ Condition + Subject_ID, data = data2use, mean)
data2use$Condition <- factor(data2use$Condition, levels = c("Valid Go", "Failed Nogo")) 
peak_force_model_exp2 <- lmer(Peak_force ~ Condition + (1|Subject_ID), data = data2use) #lmm
emmeans(peak_force_model_exp2, list(pairwise ~ Condition), adjust = "tukey") #t-ratio
# plot
summary4plot <- summarySEwithin(data2use, measurevar = "Peak_force", withinvars = c("Condition"), idvar="Subject_ID", conf.interval = 0.95) #variances invalid, compute manually
CI_1 <- describe(data2use$Peak_force[data2use$Condition == "Valid Go"])$se*1.96
CI_2 <- describe(data2use$Peak_force[data2use$Condition == "Failed Nogo"])$se*1.96
CI <- rbind(CI_1, CI_2)
peak_force_plot_exp2 <- ggplot(data = data2use, aes(x = Condition, y = Peak_force)) +
  geom_jitter(alpha = 0.3, width = 0.2, size = 2, aes(color = Condition)) + #individual points
  scale_colour_manual(values=c("palegreen3","red")) +
  geom_errorbar(data = summary4plot, aes(ymin = Peak_force-CI, ymax = Peak_force+CI), colour = "black", width=.1, size = 0.75, position = position_dodge(.5)) + #confidence interval
  geom_point(data = summary4plot, aes(x = Condition, y = Peak_force), size = 2) + #grand-averaged points
  geom_point(data = summary4plot, colour = "white",size = 1, alpha = 0.5) +
  geom_abline(intercept = 0, slope = 0, col = "black", linetype = 'dashed', size = 1, alpha = 0.2) +
  ggtitle("") +
  xlab("") +
  ylab("Peak Force (N)")+
  theme_classic() +
  theme(legend.position = "none", text = element_text(size=12, family="Calibri"), plot.title = element_text(hjust = 0.5))
peak_force_plot_exp2

### Predicting Force with RT on Failed Inhibitions at the trial-Level ###
# Exp. 1
data2use <- as.data.table(subset(all_data_exp1, Condition %in% c("Failed Stop")))
data2use[,sRT_movement_onset := scale(RT_movement_onset), by = Subject_ID]
data2use[,sPeak_force := scale(Peak_force), by = Subject_ID]
force_rt_model_exp1 <- lmer(sRT_movement_onset ~ sPeak_force + (1|Subject_ID), data = data2use)
anova(force_rt_model_exp1)
summ(force_rt_model_exp1)
# Exp. 2
data2use <- as.data.table(subset(all_data_exp2, Condition %in% c("Failed Nogo")))
data2use[,sRT_movement_onset := scale(RT_movement_onset), by = Subject_ID]
data2use[,sPeak_force := scale(Peak_force), by = Subject_ID]
force_rt_model_exp2 <- lmer(sRT_movement_onset ~ sPeak_force + (1|Subject_ID), data = data2use)
anova(force_rt_model_exp2)
summ(force_rt_model_exp2)

##########################
### Peak Force Latency ###
##########################

# Exp. 1
data2use <- subset(all_data_exp1, Condition %in% c("Valid Go", "Failed Stop"))
data2use <- aggregate(Peak_force_latency ~ Condition + Subject_ID, data = data2use, mean)
data2use$Condition <- factor(data2use$Condition, levels = c("Valid Go", "Failed Stop")) 
peak_force_latency_model_exp1 <- lmer(Peak_force_latency ~ Condition + (1|Subject_ID), data = data2use) #lmm
emmeans(peak_force_latency_model_exp1, list(pairwise ~ Condition), adjust = "tukey") #t-ratio
describe(data2use$Peak_force_latency[data2use$Condition == "Valid Go"])$sd
describe(data2use$Peak_force_latency[data2use$Condition == "Failed Stop"])$sd
# plot
summary4plot <- summarySEwithin(data2use, measurevar = "Peak_force_latency", withinvars = c("Condition"), idvar="Subject_ID", conf.interval = 0.95) #variances invalid, compute manually
CI_1 <- describe(data2use$Peak_force_latency[data2use$Condition == "Valid Go"])$se*1.96
CI_2 <- describe(data2use$Peak_force_latency[data2use$Condition == "Failed Stop"])$se*1.96
CI <- rbind(CI_1, CI_2)
peak_force_latency_plot_exp1 <- ggplot(data = data2use, aes(x = Condition, y = Peak_force_latency)) +
  geom_jitter(alpha = 0.3, width = 0.2, size = 2, aes(color = Condition)) + #individual points
  scale_colour_manual(values=c("palegreen3","red")) +
  geom_errorbar(data = summary4plot, aes(ymin = Peak_force_latency-CI, ymax = Peak_force_latency+CI), colour = "black", width=.1, size = 0.75, position = position_dodge(.5)) + #confidence interval
  geom_point(data = summary4plot, aes(x = Condition, y = Peak_force_latency), size = 2) + #grand-averaged points
  geom_point(data = summary4plot, colour = "white",size = 1, alpha = 0.5) +
  geom_abline(intercept = 0, slope = 0, col = "black", linetype = 'dashed', size = 1, alpha = 0.2) +
  ggtitle("") +
  xlab("") +
  ylab("Peak Force Latency (ms)")+
  theme_classic() +
  theme(legend.position = "none", text = element_text(size=12, family="Calibri"), plot.title = element_text(hjust = 0.5))
peak_force_latency_plot_exp1

# Exp. 2
data2use <- subset(all_data_exp2, Condition %in% c("Valid Go", "Failed Nogo"))
data2use <- aggregate(Peak_force_latency ~ Condition + Subject_ID, data = data2use, mean)
data2use$Condition <- factor(data2use$Condition, levels = c("Valid Go", "Failed Nogo")) 
peak_force_latency_model_exp2 <- lmer(Peak_force_latency ~ Condition + (1|Subject_ID), data = data2use) #lmm
emmeans(peak_force_latency_model_exp2, list(pairwise ~ Condition), adjust = "tukey") #t-ratio
describe(data2use$Peak_force_latency[data2use$Condition == "Valid Go"])$sd
describe(data2use$Peak_force_latency[data2use$Condition == "Failed Nogo"])$sd
#plot
summary4plot <- summarySEwithin(data2use, measurevar = "Peak_force_latency", withinvars = c("Condition"), idvar="Subject_ID", conf.interval = 0.95) #variances invalid, compute manually
CI_1 <- describe(data2use$Peak_force_latency[data2use$Condition == "Valid Go"])$se*1.96
CI_2 <- describe(data2use$Peak_force_latency[data2use$Condition == "Failed Nogo"])$se*1.96
CI <- rbind(CI_1, CI_2)
peak_force_latency_plot_exp2 <- ggplot(data = data2use, aes(x = Condition, y = Peak_force_latency)) +
  geom_jitter(alpha = 0.3, width = 0.2, size = 2, aes(color = Condition)) + #individual points
  scale_colour_manual(values=c("palegreen3","red")) +
  geom_errorbar(data = summary4plot, aes(ymin = Peak_force_latency-CI, ymax = Peak_force_latency+CI), colour = "black", width=.1, size = 0.75, position = position_dodge(.5)) + #confidence interval
  geom_point(data = summary4plot, aes(x = Condition, y = Peak_force_latency), size = 2) + #grand-averaged points
  geom_point(data = summary4plot, colour = "white",size = 1, alpha = 0.5) +
  geom_abline(intercept = 0, slope = 0, col = "black", linetype = 'dashed', size = 1, alpha = 0.2) +
  ggtitle("") +
  xlab("") +
  ylab("Peak Force Latency (ms)")+
  theme_classic() +
  theme(legend.position = "none", text = element_text(size=12, family="Calibri"), plot.title = element_text(hjust = 0.5))
peak_force_latency_plot_exp2

######################################
### Peak rate of force development ###
######################################

# Exp. 1
data2use <- subset(all_data_exp1, Condition %in% c("Valid Go", "Failed Stop"))
data2use <- aggregate(Peak_rate_of_force_development ~ Condition + Subject_ID, data = data2use, mean)
data2use$Condition <- factor(data2use$Condition, levels = c("Valid Go", "Failed Stop")) 
peak_rate_force_development_model_exp1 <- lmer(Peak_rate_of_force_development ~ Condition + (1|Subject_ID), data = data2use) #lmm
emmeans(peak_rate_force_development_model_exp1, list(pairwise ~ Condition), adjust = "tukey") #t-ratio
# plot
summary4plot <- summarySEwithin(data2use, measurevar = "Peak_rate_of_force_development", withinvars = c("Condition"), idvar="Subject_ID", conf.interval = 0.95) #variances invalid, compute manually
CI_1 <- describe(data2use$Peak_rate_of_force_development[data2use$Condition == "Valid Go"])$se*1.96
CI_2 <- describe(data2use$Peak_rate_of_force_development[data2use$Condition == "Failed Stop"])$se*1.96
describe(data2use$Peak_rate_of_force_development[data2use$Condition == "Valid Go"])$sd
describe(data2use$Peak_rate_of_force_development[data2use$Condition == "Failed Stop"])$sd
CI <- rbind(CI_1, CI_2)
Peak_rate_force_development_plot_exp1 <- ggplot(data = data2use, aes(x = Condition, y = Peak_rate_of_force_development)) +
  geom_jitter(alpha = 0.3, width = 0.2, size = 2, aes(color = Condition)) + #individual points
  scale_colour_manual(values=c("palegreen3","red")) +
  geom_errorbar(data = summary4plot, aes(ymin = Peak_rate_of_force_development-CI, ymax = Peak_rate_of_force_development+CI), colour = "black", width=.1, size = 0.75, position = position_dodge(.5)) + #confidence interval
  geom_point(data = summary4plot, aes(x = Condition, y = Peak_rate_of_force_development), size = 2) + #grand-averaged points
  geom_point(data = summary4plot, colour = "white",size = 1, alpha = 0.5) +
  geom_abline(intercept = 0, slope = 0, col = "black", linetype = 'dashed', size = 1, alpha = 0.2) +
  ggtitle("") +
  xlab("") +
  ylab("Peak Rate of Force Development (N/s)")+
  theme_classic() +
  theme(legend.position = "none", text = element_text(size=12, family="Calibri"), plot.title = element_text(hjust = 0.5))
Peak_rate_force_development_plot_exp1

# Exp. 2
data2use <- subset(all_data_exp2, Condition %in% c("Valid Go", "Failed Nogo"))
data2use <- aggregate(Peak_rate_of_force_development ~ Condition + Subject_ID, data = data2use, mean)
data2use$Condition <- factor(data2use$Condition, levels = c("Valid Go", "Failed Nogo")) 
Peak_rate_force_development_model_exp2 <- lmer(Peak_rate_of_force_development ~ Condition + (1|Subject_ID), data = data2use) #lmm
emmeans(Peak_rate_force_development_model_exp2, list(pairwise ~ Condition), adjust = "tukey") #t-ratio
#plot
summary4plot <- summarySEwithin(data2use, measurevar = "Peak_rate_of_force_development", withinvars = c("Condition"), idvar="Subject_ID", conf.interval = 0.95) #variances invalid, compute manually
CI_1 <- describe(data2use$Peak_rate_of_force_development[data2use$Condition == "Valid Go"])$se*1.96
CI_2 <- describe(data2use$Peak_rate_of_force_development[data2use$Condition == "Failed Nogo"])$se*1.96
describe(data2use$Peak_rate_of_force_development[data2use$Condition == "Valid Go"])$sd
describe(data2use$Peak_rate_of_force_development[data2use$Condition == "Failed Nogo"])$sd
CI <- rbind(CI_1, CI_2)
Peak_rate_force_development_plot_exp2 <- ggplot(data = data2use, aes(x = Condition, y = Peak_rate_of_force_development)) +
  geom_jitter(alpha = 0.3, width = 0.2, size = 2, aes(color = Condition)) + #individual points
  scale_colour_manual(values=c("palegreen3","red")) +
  geom_errorbar(data = summary4plot, aes(ymin = Peak_rate_of_force_development-CI, ymax = Peak_rate_of_force_development+CI), colour = "black", width=.1, size = 0.75, position = position_dodge(.5)) + #confidence interval
  geom_point(data = summary4plot, aes(x = Condition, y = Peak_rate_of_force_development), size = 2) + #grand-averaged points
  geom_point(data = summary4plot, colour = "white",size = 1, alpha = 0.5) +
  geom_abline(intercept = 0, slope = 0, col = "black", linetype = 'dashed', size = 1, alpha = 0.2) +
  ggtitle("") +
  xlab("") +
  ylab("Peak Rate of Force Development (N/s)")+
  theme_classic() +
  theme(legend.position = "none", text = element_text(size=12, family="Calibri"), plot.title = element_text(hjust = 0.5))
Peak_rate_force_development_plot_exp2

#################################
### Electrophysiological Data ###
#################################

####################
### P3 Amplitude ###
####################
# Exp. 1
data2use <- as.data.table(subset(all_data_exp1, Condition %in% c("Successful Stop", "Failed Stop")))
data2use$Condition <- factor(data2use$Condition, levels = c("Successful Stop", "Failed Stop")) 
data2use <- aggregate(P3_amplitude ~ Condition + Subject_ID, data = data2use, mean)
P3_model_exp1 <- lmerTest::lmer(P3_amplitude ~ Condition + (1|Subject_ID), data = data2use)
anova(P3_model_exp1)
emmeans(P3_model_exp1,list(pairwise ~ Condition), adjust = "tukey")
describe(data2use$P3_amplitude[data2use$Condition == "Successful Stop"])$sd
describe(data2use$P3_amplitude[data2use$Condition == "Failed Stop"])$sd
# plot
summary4plot <- summarySEwithin(data2use, measurevar = "P3_amplitude", withinvars = c("Condition"), idvar="Subject_ID", conf.interval = 0.95) #variances invalid, compute manually
CI_1 <- describe(data2use$P3_amplitude[data2use$Condition == "Successful Stop"])$se*1.96
CI_2 <- describe(data2use$P3_amplitude[data2use$Condition == "Failed Stop"])$se*1.96
CI <- rbind(CI_1, CI_2)
P3_amplitude_plot_exp1 <- ggplot(data = data2use, aes(x = Condition, y = P3_amplitude)) +
  geom_jitter(alpha = 0.3, width = 0.2, size = 2, aes(color = Condition)) + #individual points
  scale_colour_manual(values=c("black","red")) +
  geom_errorbar(data = summary4plot, aes(ymin = P3_amplitude-CI, ymax = P3_amplitude+CI), colour = "black", width=.1, size = 0.75, position = position_dodge(.5)) + #confidence interval
  geom_point(data = summary4plot, aes(x = Condition, y = P3_amplitude), size = 2) + #grand-averaged points
  geom_point(data = summary4plot, colour = "white",size = 1, alpha = 0.5) +
  # geom_abline(intercept = 0, slope = 0, col = "black", linetype = 'dashed', size = 1, alpha = 0.2) +
  ggtitle("") +
  xlab("") +
  ylab(expression(paste("P300 Amplitude ( ", mu,V/mm^2, ")"))) +
  theme_classic() +
  theme(legend.position = "none", text = element_text(size=12, family="Calibri"), plot.title = element_text(hjust = 0.5))
P3_amplitude_plot_exp1

# Exp. 2
data2use <- as.data.table(subset(all_data_exp2, Condition %in% c("Successful Nogo", "Failed Nogo")))
data2use$Condition <- factor(data2use$Condition, levels = c("Successful Nogo", "Failed Nogo")) 
data2use <- aggregate(P3_amplitude ~ Condition + Subject_ID, data = data2use, mean)
P3_model_exp2 <- lmerTest::lmer(P3_amplitude ~ Condition + (1|Subject_ID), data = data2use)
anova(P3_model_exp2)
emmeans(P3_model_exp2,list(pairwise ~ Condition), adjust = "tukey")
describe(data2use$P3_amplitude[data2use$Condition == "Successful Nogo"])$sd
describe(data2use$P3_amplitude[data2use$Condition == "Failed Nogo"])$sd
# plot
summary4plot <- summarySEwithin(data2use, measurevar = "P3_amplitude", withinvars = c("Condition"), idvar="Subject_ID", conf.interval = 0.95) #variances invalid, compute manually
CI_1 <- describe(data2use$P3_amplitude[data2use$Condition == "Successful Nogo"])$se*1.96
CI_2 <- describe(data2use$P3_amplitude[data2use$Condition == "Failed Nogo"])$se*1.96
CI <- rbind(CI_1, CI_2)
P3_amplitude_plot_exp2 <- ggplot(data = data2use, aes(x = Condition, y = P3_amplitude)) +
  geom_jitter(alpha = 0.3, width = 0.2, size = 2, aes(color = Condition)) + #individual points
  scale_colour_manual(values=c("black","red")) +
  geom_errorbar(data = summary4plot, aes(ymin = P3_amplitude-CI, ymax = P3_amplitude+CI), colour = "black", width=.1, size = 0.75, position = position_dodge(.5)) + #confidence interval
  geom_point(data = summary4plot, aes(x = Condition, y = P3_amplitude), size = 2) + #grand-averaged points
  geom_point(data = summary4plot, colour = "white",size = 1, alpha = 0.5) +
  # geom_abline(intercept = 0, slope = 0, col = "black", linetype = 'dashed', size = 1, alpha = 0.2) +
  ggtitle("") +
  xlab("") +
  ylab(expression(paste("P300 Amplitude ( ", mu,V/mm^2, ")"))) +
  theme_classic() +
  theme(legend.position = "none", text = element_text(size=12, family="Calibri"), plot.title = element_text(hjust = 0.5))
P3_amplitude_plot_exp2

########################
### P3 Onset Latency ###
########################

# Exp. 1
all_data_exp1$P3_difference_onset[all_data_exp1$Subject_ID %in% c("P005FR18", "P010FR19", "P011FR25", "P023MR27", "P026FR26")] <- NA
data2use <- aggregate(P3_difference_onset ~ Condition + Subject_ID, data = all_data_exp1, mean, na.rm = TRUE)
data2use$Condition <- factor(data2use$Condition, levels = c("Successful Stop", "Failed Stop"))
data2use$P3_difference_onset <- data2use$P3_difference_onset - 825
P3_onset_model_exp1 <- lmer(P3_difference_onset ~ Condition + (1|Subject_ID), data = data2use)
anova(P3_onset_model_exp1)
emmeans(P3_onset_model_exp1, list(pairwise ~ Condition), adjust = "tukey")
describe(data2use$P3_difference_onset[data2use$Condition == "Successful Stop"])$sd
describe(data2use$P3_difference_onset[data2use$Condition == "Failed Stop"])$sd
describe(data2use$P3_difference_onset[data2use$Condition == "Successful Stop"])$mean
describe(data2use$P3_difference_onset[data2use$Condition == "Failed Stop"])$mean
# r2beta(P3_onset_model_exp1, method = "kr")
# plot
summary4plot <- summarySEwithin(data2use, measurevar = "P3_difference_onset", withinvars = c("Condition"), idvar="Subject_ID", conf.interval = 0.95) #variances invalid, compute manually
CI_1 <- describe(data2use$P3_difference_onset[data2use$Condition == "Successful Stop"])$se*1.96
CI_2 <- describe(data2use$P3_difference_onset[data2use$Condition == "Failed Stop"])$se*1.96
CI <- rbind(CI_1, CI_2)
P3_onset_plot_exp1 <- ggplot(data = data2use, aes(x = Condition, y = P3_difference_onset)) +
  geom_jitter(alpha = 0.3, width = 0.2, size = 2, aes(color = Condition)) + #individual points
  scale_colour_manual(values=c("black","red")) +
  geom_errorbar(data = summary4plot, aes(ymin = P3_difference_onset-CI, ymax = P3_difference_onset+CI), colour = "black", width=.1, size = 0.75, position = position_dodge(.5)) + #confidence interval
  geom_point(data = summary4plot, aes(x = Condition, y = P3_difference_onset), size = 2) + #grand-averaged points
  geom_point(data = summary4plot, colour = "white",size = 1, alpha = 0.5) +
  geom_abline(intercept = 176.71, slope = 0, col = "green", linetype = 'dashed', size = 1, alpha = 0.2) +
  geom_abline(intercept = 264.3396, slope = 0, col = "red", linetype = 'dashed', size = 1, alpha = 0.2) +
  ggtitle("") +
  xlab("") +
  ylab("P300 Onset Latency (ms)")+
  theme_classic() +
  theme(legend.position = "none", text = element_text(size=12, family="Calibri"), plot.title = element_text(hjust = 0.5))
P3_onset_plot_exp1

# Exp. 2
all_data_exp2$P3_difference_onset[all_data_exp2$Subject_ID %in% c("P008FR19", "P028MR30", "P031MR21", "P034FR21")] <- NA
data2use <- aggregate(P3_difference_onset ~ Condition + Subject_ID, data = all_data_exp2, mean, na.rm = TRUE)
data2use$Condition <- factor(data2use$Condition, levels = c("Successful Nogo", "Failed Nogo"))
P3_onset_model_exp2 <- lmer(P3_difference_onset ~ Condition + (1|Subject_ID), data = data2use)
anova(P3_onset_model_exp2)
emmeans(P3_onset_model_exp2, list(pairwise ~ Condition), adjust = "tukey")
describe(data2use$P3_difference_onset[data2use$Condition == "Successful Nogo"])$sd
describe(data2use$P3_difference_onset[data2use$Condition == "Failed Nogo"])$sd
describe(data2use$P3_difference_onset[data2use$Condition == "Successful Nogo"])$mean
describe(data2use$P3_difference_onset[data2use$Condition == "Failed Nogo"])$mean
# r2beta(P3_onset_model_exp2, method = "kr")
# plot
summary4plot <- summarySEwithin(data2use, measurevar = "P3_difference_onset", withinvars = c("Condition"), idvar="Subject_ID", conf.interval = 0.95) #variances invalid, compute manually
CI_1 <- describe(data2use$P3_difference_onset[data2use$Condition == "Successful Nogo"])$se*1.96
CI_2 <- describe(data2use$P3_difference_onset[data2use$Condition == "Failed Nogo"])$se*1.96
CI <- rbind(CI_1, CI_2)
P3_onset_plot_exp2 <- ggplot(data = data2use, aes(x = Condition, y = P3_difference_onset)) +
  geom_jitter(alpha = 0.3, width = 0.2, size = 2, aes(color = Condition)) + #individual points
  scale_colour_manual(values=c("black","red")) +
  geom_errorbar(data = summary4plot, aes(ymin = P3_difference_onset-CI, ymax = P3_difference_onset+CI), colour = "black", width=.1, size = 0.75, position = position_dodge(.5)) + #confidence interval
  geom_point(data = summary4plot, aes(x = Condition, y = P3_difference_onset), size = 2) + #grand-averaged points
  geom_point(data = summary4plot, colour = "white",size = 1, alpha = 0.5) +
  geom_abline(intercept = 204.5605, slope = 0, col = "green", linetype = 'dashed', size = 1, alpha = 0.2) +
  geom_abline(intercept = 333.7678, slope = 0, col = "red", linetype = 'dashed', size = 1, alpha = 0.2) +
  ggtitle("") +
  xlab("") +
  ylab("P300 Onset Latency (ms)")+
  theme_classic() +
  theme(legend.position = "none", text = element_text(size=12, family="Calibri"), plot.title = element_text(hjust = 0.5))
P3_onset_plot_exp2

###################################
### P3 Onset vs. Movement Onset ###
###################################

# Exp. 1
data2use <- aggregate(P3_difference_onset ~ Subject_ID, data = subset(all_data_exp1, Condition %in% c("Successful Stop")), mean, na.rm = TRUE)
data2use$P3_difference_onset <- data2use$P3_difference_onset - 825
get_rt <- aggregate(RT_movement_onset ~ Subject_ID, data = subset(all_data_exp1, Condition == "Valid Go"), mean, na.rm = TRUE)
get_rt$RT_movement_onset <- get_rt$RT_movement_onset + 175
data2use <- merge(data2use, get_rt, by = c("Subject_ID"), na.omit = TRUE)
data2use<- melt(data = data2use, id.vars = "Subject_ID", measure.vars = c("P3_difference_onset", "RT_movement_onset"), value.name = "Latency", variable.name = "Measure")
P3_rt_onset_model_exp1 <- lmer(Latency ~ Measure + (1|Subject_ID), data = data2use)
emmeans(P3_rt_onset_model_exp1, list(pairwise ~ Measure), adjust = "tukey")
describe(data2use$Latency[data2use$Measure == "P3_difference_onset"])$sd
describe(data2use$Latency[data2use$Measure == "RT_movement_onset"])$sd
describe(data2use$Latency[data2use$Measure == "P3_difference_onset"])$mean
describe(data2use$Latency[data2use$Measure == "RT_movement_onset"])$mean

# Exp. 2
data2use <- aggregate(P3_difference_onset ~ Subject_ID, data = subset(all_data_exp2, Condition %in% c("Successful Nogo")), mean, na.rm = TRUE)
get_rt <- aggregate(RT_movement_onset ~ Subject_ID, data = subset(all_data_exp2, Condition == "Valid Go"), mean, na.rm = TRUE)
data2use <- merge(data2use, get_rt, by = c("Subject_ID"), na.omit = TRUE)
data2use<- melt(data = data2use, id.vars = "Subject_ID", measure.vars = c("P3_difference_onset", "RT_movement_onset"), value.name = "Latency", variable.name = "Measure")
P3_rt_onset_model_exp2 <- lmer(Latency ~ Measure + (1|Subject_ID), data = data2use)
emmeans(P3_rt_onset_model_exp2, list(pairwise ~ Measure), adjust = "tukey")
describe(data2use$Latency[data2use$Measure == "P3_difference_onset"])$sd
describe(data2use$Latency[data2use$Measure == "RT_movement_onset"])$sd
describe(data2use$Latency[data2use$Measure == "P3_difference_onset"])$mean
describe(data2use$Latency[data2use$Measure == "RT_movement_onset"])$mean

###############################################################
### P3 Onset vs. Peak Force Latency (on Failed Inhibitions) ###
###############################################################

# Exp. 1
data2use <- aggregate(P3_difference_onset ~ Subject_ID, data = subset(all_data_exp1, Condition %in% c("Failed Stop")), mean, na.rm = TRUE)
data2use$P3_difference_onset <- data2use$P3_difference_onset - 825
get_rt <- aggregate(Peak_force_latency ~ Subject_ID, data = subset(all_data_exp1, Condition == "Failed Stop"), mean, na.rm = TRUE)
get_rt$Peak_force_latency <- get_rt$Peak_force_latency + 175
data2use <- merge(data2use, get_rt, by = c("Subject_ID"), na.omit = TRUE)
data2use<- melt(data = data2use, id.vars = "Subject_ID", measure.vars = c("P3_difference_onset", "Peak_force_latency"), value.name = "Latency", variable.name = "Measure")
P3_onset_peak_force_latency_model_exp1 <- lmer(Latency ~ Measure + (1|Subject_ID), data = data2use)
emmeans(P3_onset_peak_force_latency_model_exp1, list(pairwise ~ Measure), adjust = "tukey")
describe(data2use$Latency[data2use$Measure == "P3_difference_onset"])$sd
describe(data2use$Latency[data2use$Measure == "Peak_force_latency"])$sd
describe(data2use$Latency[data2use$Measure == "P3_difference_onset"])$mean
describe(data2use$Latency[data2use$Measure == "Peak_force_latency"])$mean

# Exp. 2
data2use <- aggregate(P3_difference_onset ~ Subject_ID, data = subset(all_data_exp2, Condition %in% c("Failed Nogo")), mean, na.rm = TRUE)
get_rt <- aggregate(Peak_force_latency ~ Subject_ID, data = subset(all_data_exp2, Condition == "Failed Nogo"), mean, na.rm = TRUE)
data2use <- merge(data2use, get_rt, by = c("Subject_ID"), na.omit = TRUE)
data2use<- melt(data = data2use, id.vars = "Subject_ID", measure.vars = c("P3_difference_onset", "Peak_force_latency"), value.name = "Latency", variable.name = "Measure")
P3_onset_peak_force_latency_model_exp2 <- lmer(Latency ~ Measure + (1|Subject_ID), data = data2use)
emmeans(P3_onset_peak_force_latency_model_exp2, list(pairwise ~ Measure), adjust = "tukey")
describe(data2use$Latency[data2use$Measure == "P3_difference_onset"])$sd
describe(data2use$Latency[data2use$Measure == "Peak_force_latency"])$sd
describe(data2use$Latency[data2use$Measure == "P3_difference_onset"])$mean
describe(data2use$Latency[data2use$Measure == "Peak_force_latency"])$mean

###########################################################################
### Predicting Force with P300 on Failed Inhibitions at the Trial-level ###
###########################################################################

# Exp. 1
data2use <- as.data.table(subset(all_data_exp1, Condition %in% c("Failed Stop")))
data2use[,sPeak_force := scale(Peak_force), by = Subject_ID]
P3_force_model_exp1 <- lmerTest::lmer(P3_amplitude ~ sPeak_force + (1|Subject_ID), data = data2use)
anova(P3_force_model_exp1)
summ(P3_force_model_exp1)
# r2beta(P3_force_C3_model_exp1, method = "kr")

# Exp. 2
data2use <- as.data.table(subset(all_data_exp2, Condition %in% c("Failed Nogo")))
data2use[,sPeak_force := scale(Peak_force), by = Subject_ID]
P3_force_model_exp2 <- lmerTest::lmer(P3_amplitude ~ sPeak_force+ (1|Subject_ID), data = data2use)
anova(P3_force_model_exp2)
summ(P3_force_model_exp2)
r2beta(P3_force_model_exp2, method = "kr")

#############################################################
### Modelling P3 with Force and Binary Inhibition Success ###
#############################################################
# Exp. 1

# Full model
data2use <- as.data.table(subset(all_data_exp1, Condition %in% c("Successful Stop", "Failed Stop")))
data2use$Condition <- factor(data2use$Condition, levels = c("Successful Stop", "Failed Stop"))
data2use[,sPeak_force := scale(Peak_force), by = Subject_ID]
P3_force_binary_model_exp1 <- lmerTest::lmer(P3_amplitude ~ sPeak_force + Condition + (1|Subject_ID), data = data2use)
anova(P3_force_binary_model_exp1)
summ(P3_force_binary_model_exp1)
# r2beta(P3_force_binary_model_exp1, method = "kr")

# Force only
P3_force_only_model_exp1 <- lmerTest::lmer(P3_amplitude ~ sPeak_force + (1|Subject_ID), data = data2use)
anova(P3_force_only_model_exp1)
summ(P3_force_only_model_exp1)
# r2beta(P3_force_only_model_exp1, method = "kr")

# Condition only
P3_binary_only_model_exp1 <- lmerTest::lmer(P3_amplitude ~ Condition + (1|Subject_ID), data = data2use)
anova(P3_binary_only_model_exp1)
summ(P3_binary_only_model_exp1)
# r2beta(P3_binary_only_model_exp1, method = "kr")

P3_force_binary_model_exp1 <- lme4::lmer(P3_amplitude ~ sPeak_force + Condition + (1|Subject_ID), data = data2use)
P3_force_only_model_exp1 <- lme4::lmer(P3_amplitude ~ sPeak_force + (1|Subject_ID), data = data2use)
P3_binary_only_model_exp1 <- lme4::lmer(P3_amplitude ~ Condition + (1|Subject_ID), data = data2use)
P3_model_exp1_plot <- plot_summs(P3_force_binary_model_exp1, P3_force_only_model_exp1, P3_binary_only_model_exp1, model.names = c("Force + Inhibition Success", "Force", "Inhibition Success"), coefs = c("Standardised Force" = "sPeak_force", "Inhibition Success" = "Condition"), 
                                 ci_level = 0.99, inner_ci_level = 0.95, scale = TRUE, plot.distributions = FALSE, legend.title = "", colors = c("black", "black", "black")) +
  xlab("Scaled Regression Estimates") +
  ylab("") +
  theme_classic()
P3_model_exp1_plot

# Exp. 2

# Full Model
data2use <- as.data.table(subset(all_data_exp2, Condition %in% c("Successful Nogo", "Failed Nogo")))
data2use$Condition <- factor(data2use$Condition, levels = c("Successful Nogo", "Failed Nogo"))
data2use[,sPeak_force := scale(Peak_force), by = Subject_ID]
P3_force_binary_model_exp2 <- lmerTest::lmer(P3_amplitude ~ sPeak_force + Condition + (1|Subject_ID), data = data2use)
anova(P3_force_binary_model_exp2)
summ(P3_force_binary_model_exp2)
# r2beta(P3_force_binary_model_exp2, method = "kr")

# Force only
P3_force_only_model_exp2 <- lmerTest::lmer(P3_amplitude ~ sPeak_force + (1|Subject_ID), data = data2use)
anova(P3_force_only_model_exp2)
summ(P3_force_only_model_exp2)
# r2beta(P3_force_only_model_exp2, method = "kr")

# Condition only
P3_binary_only_model_exp2 <- lmerTest::lmer(P3_amplitude ~ Condition + (1|Subject_ID), data = data2use)
anova(P3_binary_only_model_exp2)
summ(P3_binary_only_model_exp2)
# r2beta(P3_binary_only_model_exp2, method = "kr")

P3_force_binary_model_exp2 <- lme4::lmer(P3_amplitude ~ sPeak_force + Condition + (1|Subject_ID), data = data2use)
P3_force_only_model_exp2 <- lme4::lmer(P3_amplitude ~ sPeak_force + (1|Subject_ID), data = data2use)
P3_binary_only_model_exp2 <- lme4::lmer(P3_amplitude ~ Condition + (1|Subject_ID), data = data2use)
P3_model_exp2_plot <- plot_summs(P3_force_binary_model_exp2, P3_force_only_model_exp2, P3_binary_only_model_exp2, model.names = c("Force + Inhibition Success", "Force", "Inhibition Success"), coefs = c("Standardised Force" = "sPeak_force", "Inhibition Success" = "Condition"), 
                                 ci_level = 0.99, inner_ci_level = 0.95, scale = TRUE, plot.distributions = FALSE, legend.title = "", colors = c("black", "black", "black")) +
  xlab("Scaled Regression Estimates") +
  ylab("") +
  theme_classic()
P3_model_exp2_plot
grid.arrange(P3_model_exp1_plot, P3_model_exp2_plot, nrow = 2, ncol = 1)

##############################
### Supplementary Analysis ###
##############################

# # Exp. 1 - P3 ~ Force + Condition
data2use <- as.data.table(subset(all_data_exp1, Condition %in% c("Valid Go", "Failed Stop")))
data2use[,sPeak_force := scale(Peak_force), by = Subject_ID]
P3_force_model_exp1 <- lmerTest::lmer(P3_amplitude ~ sPeak_force*Condition + (1|Subject_ID), data = data2use)
anova(P3_force_model_exp1)
summ(P3_force_model_exp1)
# r2beta(P3_force_model_exp1, method = "kr")

#interaction follow-up 1 (go trials)
data2use <- as.data.table(subset(all_data_exp1, Condition %in% c("Valid Go", "Failed Stop")))
data2use[,sPeak_force := scale(Peak_force), by = Subject_ID]
data2use <- subset(data2use, Condition %in% c("Valid Go"))
P3_force_model_exp1 <- lmerTest::lmer(P3_amplitude ~ sPeak_force + (1|Subject_ID), data = data2use)
anova(P3_force_model_exp1)
summ(P3_force_model_exp1)
# r2beta(P3_force_model_exp2, method = "kr")

#interaction follow-up 2 (nogo trials)
data2use <- as.data.table(subset(all_data_exp1, Condition %in% c("Valid Go", "Failed Stop")))
data2use[,sPeak_force := scale(Peak_force), by = Subject_ID]
data2use <- subset(data2use, Condition %in% c("Failed Stop"))
P3_force_model_exp1 <- lmerTest::lmer(P3_amplitude ~ sPeak_force + (1|Subject_ID), data = data2use)
anova(P3_force_model_exp1)
summ(P3_force_model_exp1)
# r2beta(P3_force_model_exp2, method = "kr")

# Exp. 2 - P3 ~ Force + Condition
data2use <- as.data.table(subset(all_data_exp2, Condition %in% c("Valid Go", "Failed Nogo")))
data2use[,sPeak_force := scale(Peak_force), by = Subject_ID]
P3_force_model_exp2 <- lmerTest::lmer(P3_amplitude ~ sPeak_force*Condition + (1|Subject_ID), data = data2use)
anova(P3_force_model_exp2)
summ(P3_force_model_exp2)
# r2beta(P3_force_model_exp2, method = "kr")

#interaction follow-up 1 (go trials)
data2use <- as.data.table(subset(all_data_exp2, Condition %in% c("Valid Go", "Failed Nogo")))
data2use[,sPeak_force := scale(Peak_force), by = Subject_ID]
data2use <- subset(data2use, Condition %in% c("Valid Go"))
P3_force_model_exp2 <- lmerTest::lmer(P3_amplitude ~ sPeak_force + (1|Subject_ID), data = data2use)
anova(P3_force_model_exp2)
summ(P3_force_model_exp2)
# r2beta(P3_force_model_exp2, method = "kr")

#interaction follow-up 2 (nogo trials)
data2use <- as.data.table(subset(all_data_exp2, Condition %in% c("Valid Go", "Failed Nogo")))
data2use[,sPeak_force := scale(Peak_force), by = Subject_ID]
data2use <- subset(data2use, Condition %in% c("Failed Nogo"))
P3_force_model_exp2 <- lmerTest::lmer(P3_amplitude ~ sPeak_force + (1|Subject_ID), data = data2use)
anova(P3_force_model_exp2)
summ(P3_force_model_exp2)
# r2beta(P3_force_model_exp2, method = "kr")

#############################################
### ACCOUNTING FOR MOTOR ACTIVATION AT C3 ###
#############################################
# The above analysis showed that the P300-force relationship was observed on both Go and Stop/Nogo trials
# This suggests that the P300-force effect may reflect general motor processes, instead of inhibitory control
# To examine whether general motor motor processes completely accounts for the P300-force relationship,
# we measured and include motor-activation at C3 during the time fo the stop-signal into our model
# The results show that motor-activation explains part but not all of the P300-force relationship.

rm(list=ls()) #clears environment
data_folder <- dirname(rstudioapi::getSourceEditorContext()$path)
all_data_exp1 <- data.table(read.xlsx(paste(data_folder, "all_data_exp1_sup.xlsx", sep = "/")))
all_data_exp2 <- data.table(read.xlsx(paste(data_folder, "all_data_exp2_sup.xlsx", sep = "/")))
# set condition variables as factors
all_data_exp1$Condition <- factor(all_data_exp1$Condition)
all_data_exp2$Condition <- factor(all_data_exp2$Condition)
all_data_exp1$Condition2 <- factor(all_data_exp1$Condition2)
all_data_exp2$Condition2 <- factor(all_data_exp2$Condition2)

# Exp. 1
data2use <- as.data.table(subset(all_data_exp1, Condition %in% c("Valid Go", "Failed Stop")))
data2use[,sPeak_force := scale(Peak_force), by = Subject_ID]
P3_force_C3_model_exp1 <- lmerTest::lmer(P3_amplitude ~ sPeak_force*Condition*C3_amplitude + (1|Subject_ID), data = data2use)
anova(P3_force_C3_model_exp1)
summ(P3_force_C3_model_exp1)
# emmeans(P3_force_C3_model_exp2, pairwise ~ Condition, adjust = "Tukey")

# Exp. 2
data2use <- as.data.table(subset(all_data_exp2, Condition %in% c("Valid Go", "Failed Nogo")))
data2use[,sPeak_force := scale(Peak_force), by = Subject_ID]
P3_force_C3_model_exp2 <- lmerTest::lmer(P3_amplitude ~ sPeak_force*Condition*C3_amplitude + (1|Subject_ID), data = data2use)
anova(P3_force_C3_model_exp2)
summ(P3_force_C3_model_exp2)
# emmeans(P3_force_C3_model_exp2, pairwise ~ Condition, adjust = "Tukey")

#follow-up 1
data2use <- as.data.table(subset(all_data_exp2, Condition %in% c("Valid Go")))
data2use[,sPeak_force := scale(Peak_force), by = Subject_ID]
P3_force_C3_model_exp2 <- lmerTest::lmer(P3_amplitude ~ sPeak_force*C3_amplitude + (1|Subject_ID), data = data2use)
anova(P3_force_C3_model_exp2)
summ(P3_force_C3_model_exp2)
# emmeans(P3_force_C3_model_exp2, pairwise ~ Condition, adjust = "Tukey")

#follow-up 2
data2use <- as.data.table(subset(all_data_exp2, Condition %in% c("Failed Nogo")))
data2use[,sPeak_force := scale(Peak_force), by = Subject_ID]
P3_force_C3_model_exp2 <- lmerTest::lmer(P3_amplitude ~ sPeak_force*C3_amplitude + (1|Subject_ID), data = data2use)
anova(P3_force_C3_model_exp2)
summ(P3_force_C3_model_exp2)
# emmeans(P3_force_C3_model_exp2, pairwise ~ Condition, adjust = "Tukey")

