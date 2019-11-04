#Survival Analysis HW 1
#Author: Jackson Perry
library(sqldf)
library(survival)
library(survminer)
library(dplyr)
library(lattice)
library(lawstat)

#######################################
#Read in the SAS file
#######################################
df <- read_sas("/Users/jacksonperry/Desktop/Fall3/Homework1_SA/hurricane.sas7bdat")

#reason:
#0 – no failure
#1 – flood failure
#2 – motor failure
#3 – surge failure
#4 – jammed failure

#hour: tenure variable

#survive: survival variable

#######################################
#Summary Statistics
#######################################
print("Percentage of pumps that survived the hurricane:")
print(count(df, vars=survive)[2,2]/(count(df, vars=survive)[1,2] + count(df, vars=survive)[2,2]))

print("Percentage of pumps in each type of failure:")
for (i in 2:5){
  print(paste((i-1), ": ", (count(df,vars=reason)[i,2])/(sum(count(df, vars=reason)[,2]))))
}

print("Average Failure Time By Type")
avgtime <- sqldf('SELECT reason, avg(hour) as AvgFailTime, count(*) as count FROM df GROUP BY reason')
print(avgtime)

#######################################
#ANOVA and t-tests for reason 
#######################################

#generate the factor variable needed for plotting
df$group <- factor(df$reason)

#run ANOVA and post-hoc t-tests
my_anova <- aov(hour~group,data=df)
summary(my_anova)
TukeyHSD(my_anova)

#check assumptions
hist(my_anova$residuals, col='black', density=20, prob=TRUE) 
curve(dnorm(x, mean=mean(my_anova$residuals), sd=sd(my_anova$residuals)), 
      col="red", lwd=2, add=TRUE, yaxt="n") #not quite normal distribution
levene.test(df$hour, df$group) #not equal variance

#######################################
#Survival probability graphs 
#######################################
simple_km <- survfit(Surv(time = hour, event = (survive == 0)) ~ 1,
                     data = df)
summary(simple_km)
ggsurvplot(simple_km, data = df, conf.int = FALSE, palette = "blue",
           xlab = "Hour", ylab = "Survival Probability", title="Survival Function", legend = "none",
           break.y.by = 0.1)

#grouped by reason
dfail <- df[which(df$reason %in% c("1","2","3","4")),]
pump_surv <- Surv(time = dfail$hour, event = dfail$survive == 0)
survdiff(pump_surv ~ reason, rho = 0, data = dfail) # logrank test, rho=1 is wilcoxon
pump_strat <- survfit(pump_surv ~ reason, data = dfail)
summary(pump_strat)
ggsurvplot(pump_strat, data=dfail, conf.int = FALSE,
           palette = c("purple", "red", "blue", "green"),
           xlab = "Hour", ylab = "Survival Probability", break.y.by = 0.1, title="Survival Probability By Failure Reason",
           legend.title = "Failure Reason", legend.labs = c("Flood","Motor","Surge","Jammed"),
            legend="bottom")

#######################################
#Conditional failure probability graphs
#######################################
#calculate hazard probability
simple_km$hp <- simple_km$n.event/simple_km$n.risk
print(simple_km$hp)

simple_haz <- merge(data.frame(time = seq(1,48,1)),
                    data.frame(time = simple_km$time, hp = simple_km$hp),
                    by = "time", all = TRUE)
simple_haz[is.na(simple_haz) == TRUE] <- 0
print(simple_haz)

plot(y = simple_haz$hp, x = simple_haz$time,
     main = "Hazard Probability Function", xlab = "Hour",
     ylab = "Hazard Probability", type = 'l', col="blue",lwd=3)

#grouped by reason
#calculate hazard probability
pump_strat$hp <- pump_strat$n.event/pump_strat$n.risk

#create the grouping factor variable
gp1 <- matrix(rep(1,45))
gp2 <- matrix(rep(2,24))
gp3 <- matrix(rep(3,28))
gp4 <- matrix(rep(4,21))
pump_strat$gp <- rbind(gp1,gp2,gp3,gp4)
pump_haz <- merge(data.frame(time = seq(1,48,1)),
                    data.frame(time = pump_strat$time, hp = pump_strat$hp, gp=pump_strat$gp),
                    by = "time", all = TRUE)
pump_haz[is.na(pump_haz) == TRUE] <- 0
print(pump_haz)
pump_haz$gp <- factor(pump_haz$gp)

ggplot(data=pump_haz, aes(x=time, y=hp, group=gp, color=gp)) +
        theme_classic() +
        theme(title = element_text(size=13), axis.text = element_text(size=12, color='black'), legend.text = element_text(size=10), legend.title = element_text(size=10)) +
        theme(legend.position="bottom") +
        scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0)) + 
         xlab("Hour") +
         ylab("Hazard Probability") +
         ggtitle("Hazard Probability By Failure Reason") +
         scale_color_manual(name="Failure Reason", labels = c("Flood","Motor","Surge", "Jammed"), values=c("purple", "red", "blue", "green")) +
         geom_line(size=1)


###################################
#flood/surge VS motor/jammed comparison
###################################

dfail$theirs <- dfail$reason %% 2
#0 corresponds to motor/jammed, 1 to flood/surge
dfail$ours <- ceiling(sqrt(dfail$reason) %% 1)
#0 corresponds to flood/jammed, 1 to motor/surge

#their categories statistical difference test
survdiff(pump_surv ~ theirs, rho = 0, data = dfail)

#our categories statistical difference test
survdiff(pump_surv ~ ours, rho = 0, data = dfail)






