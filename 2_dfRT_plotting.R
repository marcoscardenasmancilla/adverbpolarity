set.seed(12345)
library(readr)
library(dplyr)
library(tidyr)
library(rio)
library(gtools)
library(reshape)
library(ggplot2)
library(Rmisc)

#Data loading and var setting
df.spr <- read.delim("SPR_long.txt", sep=";", dec=",")

df.spr[1:3] <- lapply(df.spr[1:3], function(x) as.factor(as.character(x)))
df.spr[6:10] <- lapply(df.spr[6:10], function(x) as.factor(as.character(x)))
df.spr$WDNUM <- factor(df.spr$wd_number,
                        levels = c("1", "2", "3", "4", "5", "6",
                                   "7", "8", "9", "10", "11", "12",
                                   "13", "14", "15", "16", "17")
                              )

df.spr[11] <- lapply(df.spr[11], function(x) as.numeric(as.factor(x)))
df.spr$RT <- (df.spr$value)*1000

#Z-score for outlier identification
df.spr$z_RT <- (df.spr$RT - mean(df.spr$RT, na.rm = TRUE)) / sd(df.spr$RT, na.rm = TRUE)
df.spr$rt <- ifelse(df.spr$z_RT >= abs(2.5), NA, df.spr$RT)
plyr::count(is.na(df.spr$rt))[[2,2]]/plyr::count(is.na(df.spr$rt))[[1,2]]
#[1] 0.02300885

filenew = "df.spr.txt"
write.table(df.spr, file=filenew, sep=";", dec=",", na="", row.names=FALSE)

#Histograms after outlier removal
h1 <- ggplot(subset(df.spr, region == "N5CRI" | region == "preN5CRI"), aes(x=rt)) +
  geom_histogram(binwidth=50, colour='cyan3', fill='#FF9999', alpha = 0.9) +
  labs(title="Histograma de tiempo de respuesta en palabra crítica (N5CRI) y palabra precrítica 1 (preN5CRI)") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text=element_text(size=12),
    axis.title=element_text(size=14,face="bold")) +
  theme_bw() + facet_wrap(~region)
h1
dev.print(jpeg, "1_1_REGIONS_16_17.jpeg", res=900, height=8, width=16, units="in")

h2 <- ggplot(subset(df.spr, region == "N5CRI" | region == "POL"), aes(x=rt)) +
  geom_histogram(binwidth=50, colour='cyan3', fill="#FF9999", alpha = 0.9) +
  labs(title="Histograma de tiempo de respuesta en palabra crítica (N5CRI) y palabra precrítica 2 (POL)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme_bw(base_size = 12) +
  facet_wrap(~region)
h2
dev.print(jpeg, "1_2_REGIONS_12_17.jpeg", res=900, height=8, width=16, units="in")

h3 <- ggplot(subset(df.spr, region == "N5CRI" | region == "IC"), aes(x=rt)) +
  geom_histogram(binwidth=50, colour='cyan3', fill="#FF9999", alpha = 0.9) +
  labs(title="Histograma de tiempo de respuesta en palabra crítica (N5CRI) y palabra precrítica 3 (IC)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme_bw(base_size = 12) +
  facet_wrap(~region)
h3
dev.print(jpeg, "1_3_REGIONS_6_17.jpeg", res=900, height=8, width=16, units="in")

h4 <- ggplot(df.spr, aes(x=rt, y=..density.., fill=factor(PRED)), na.rm = TRUE) +
  theme_bw() + 
  geom_histogram(binwidth=1, alpha=0.9, position = 'dodge') +
  geom_density(alpha=0.25) +
  scale_fill_discrete(name = "Predictibilidad", breaks = c("hi", "lo"), labels = c("alta", "baja"))
h4 + labs(title="Histograma de distribución de predictibilidad ALTA vs BAJA") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) + 
  facet_wrap(~PRED)
dev.print(jpeg, "1_4_PRED_hi_lo.jpeg", res=900, height=8, width=16, units="in")

#Bar plots
plot.cond <- summarySEwithin(df.spr, measurevar = "rt", withinvars = c("COND", "PRED"), na.rm = T)
p1 <- ggplot(plot.cond, aes(x=COND, y=rt, fill=PRED)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=rt-ci, ymax=rt+ci),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Tiempo de respuesta promedio por condición. Barras de error representan intervalos de confianza.") +
  ylab("Tiempo de respuesta en milisegundos (ms)") +
  scale_fill_hue(name="Predictibilidad", # Legend label, use darker colors
                 breaks=c("hi", "lo"),
                 labels=c("alta", "baja")) +
  scale_y_continuous(breaks=seq(0, 1000, by = 50)) +
  theme_bw()
p1
dev.print(jpeg, "2_1_boxplot.COND_PRED.jpeg", res=900, height=8, width=16, units="in")

pd <- position_dodge(0.2) # move them .05 to the left and right
p2 <- ggplot(plot.cond, aes(x=PRED, y=rt, linetype=COND, shape=COND, group=COND)) +
  theme_bw()+
  geom_errorbar(aes(ymin=rt-sd, ymax=rt+sd), size=1.2, colour="black", width=.2, position=position_dodge(1)) + 
  geom_line(aes(color=COND), position=pd, size=1.2, stat = "identity") +
  geom_point(aes(color=COND), position=pd, size=5, fill="white") +
  xlab("Predictibilidad") +
  ylab("Tiempo de respuesta promedio en milisegundos") +
  scale_y_continuous(limits = c(350,500), breaks=c(seq(350,600,25)))+
  scale_x_discrete(breaks=c("hi", "lo"),
                   labels=c("alta", "baja"))
p2
dev.print(jpeg, "2_2_interactionsCON_PRED.jpeg", res=900, height=8, width=16, units="in")

plot.region <- summarySEwithin(df.spr, measurevar = "rt", withinvars = c("region", "PRED"), na.rm = T)
p3 <- ggplot(data=subset(plot.region, region == "N5CRI" | region == "preN5CRI"), aes(y=rt, x=PRED, fill=PRED)) +
  geom_bar(stat="identity", position=position_dodge(1), colour="black")+
  geom_errorbar(aes(ymin=rt-se, ymax=rt+se), position=position_dodge(1), width = .2)+
  scale_y_continuous(breaks = c(seq(0,600,10)))+
  coord_cartesian(ylim = c(200,600))+
  theme_bw(base_size = 12)+
  xlab("Palabra crítica vs palabra precrítica 1. Barras de error representan SE.")+
  ylab("Tiempo de respuesta promedio en milisegundos") +
  scale_x_discrete(breaks=c("hi", "lo"),
                   labels=c("alta", "baja")) + facet_wrap(~region)
p3 + scale_fill_hue(breaks=c("hi", "lo"),
                    labels=c("alta", "baja"))
dev.print(jpeg, "2_3_barplotCRIvsPRECRI1.jpeg", res=900, height=8, width=16, units="in")

plot.pol <- summarySEwithin(df.spr, measurevar = "rt", withinvars = c("POL", "PRED"), na.rm = T)
p4 <- ggplot(data=subset(plot.region, region == "N5CRI" | region == "POL"), aes(y=rt, x=PRED, fill=PRED)) +
  geom_bar(stat="identity", position=position_dodge(1), colour="black")+
  geom_errorbar(aes(ymin=rt-se, ymax=rt+se), position=position_dodge(1), width = .2)+
  scale_y_continuous(breaks = c(seq(0,600,10)))+
  coord_cartesian(ylim = c(200,600))+
  theme_bw(base_size = 12)+
  xlab("Palabra crítica vs polaridad adverbial. Barras de error representan SE.")+
  ylab("Tiempo de respuesta promedio en milisegundos") + 
  scale_x_discrete(breaks=c("hi", "lo"),
                   labels=c("alta", "baja")) + facet_wrap(~region)
p4 + scale_fill_hue(breaks=c("hi", "lo"),
                    labels=c("alta", "baja"))
dev.print(jpeg, "2_4_barplotCRIvsPOL.jpeg", res=900, height=8, width=16, units="in")

plot.IC <- summarySEwithin(df.spr, measurevar = "rt", withinvars = c("IC", "PRED"), na.rm = T)
p5 <- ggplot(data=subset(plot.region, region == "N5CRI" | region == "IC"), aes(y=rt, x=PRED, fill=PRED)) +
  geom_bar(stat="identity", position=position_dodge(1), colour="black")+
  geom_errorbar(aes(ymin=rt-se, ymax=rt+se), position=position_dodge(1), width = .2)+
  scale_y_continuous(breaks = c(seq(0,600,10)))+
  coord_cartesian(ylim = c(200,600))+
  theme_bw(base_size = 12)+
  xlab("Palabra crítica vs causalidad implícita (IC). Barras de error representan SE.")+
  ylab("Tiempo de respuesta promedio en milisegundos") + 
  scale_x_discrete(breaks=c("hi", "lo"),
                   labels=c("alta", "baja")) + facet_wrap(~region)
p5 + scale_fill_hue(breaks=c("hi", "lo"),
                    labels=c("alta", "baja"))
dev.print(jpeg, "2_5_barplotCRIvsIC.jpeg", res=900, height=8, width=16, units="in")

## Descriptive per text region, condition and predictability
# aggregate by subject, condition, region and predictability
df.des0 <- aggregate(rt ~ SUBJ + COND + region + PRED, mean, data=df.spr, na.rm=TRUE)
filenew = "df.descriptive.txt"
write.table(df.des0, file=filenew, sep=";", dec=",", na="", row.names=FALSE)

# Descriptive RTs
df.des1 <- ddply(df.des0, .(COND,region,PRED), plyr::summarize, N = length(rt), min = min(rt), max = max(rt), mean = mean(rt), sd = sd(rt))
View(df.des1)
filenew = "df.descriptiveRT.txt"
write.table(df.des1, file=filenew, sep=";", dec=",", na="", row.names=FALSE)

sum.data <- summarySEwithin(df.spr, measurevar = "rt", withinvars = c("IC", "POL","PRED"), na.rm = T)
