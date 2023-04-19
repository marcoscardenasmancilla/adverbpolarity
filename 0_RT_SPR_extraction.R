set.seed(369)
library(readr)
library(dplyr)
library(tidyr)
library(rio)
library(gtools)
library(reshape)

#data_master <- read_csv("C:/Users/marco/Desktop/Manuscrito_tesis/SPR/predRT/master_csv.csv")

my_data <- import_list(dir("C:/Users/marco/Desktop/Manuscrito_tesis/SPR/csv_files", pattern = ".csv"), rbind = TRUE)
df <- my_data %>% select(key_resp_s1.rt, key_resp_s10.rt, key_resp_s19.rt, key_resp_s28.rt,
                         key_resp_s5.rt, key_resp_s14.rt, key_resp_s23.rt, key_resp_s32.rt,
                         key_resp_s9.rt, key_resp_s18.rt, key_resp_s27.rt, key_resp_s4.rt,
                         key_resp_s13.rt, key_resp_s22.rt, key_resp_s31.rt, key_resp_s8.rt,
                         key_resp_s17.rt, key_resp_s26.rt, key_resp_s3.rt, key_resp_s12.rt,
                         key_resp_s21.rt, key_resp_s30.rt, key_resp_s7.rt, key_resp_s16.rt,
                         key_resp_s25.rt, key_resp_s2.rt, key_resp_s11.rt, key_resp_s20.rt,
                         key_resp_s29.rt, key_resp_s6.rt, key_resp_s15.rt, key_resp_s24.rt
                         )

s1 = subset(df[complete.cases(df$key_resp_s1.rt), ], select = c(1))
s10 = subset(df[complete.cases(df$key_resp_s10.rt), ], select = c(2))
s19 = subset(df[complete.cases(df$key_resp_s19.rt), ], select = c(3))
s28 = subset(df[complete.cases(df$key_resp_s28.rt), ], select = c(4))
s5 = subset(df[complete.cases(df$key_resp_s5.rt), ], select = c(5))
s14 = subset(df[complete.cases(df$key_resp_s14.rt), ], select = c(6))
s23 = subset(df[complete.cases(df$key_resp_s23.rt), ], select = c(7))
s32 = subset(df[complete.cases(df$key_resp_s32.rt), ], select = c(8))
s9 = subset(df[complete.cases(df$key_resp_s9.rt), ], select = c(9))
s18 = subset(df[complete.cases(df$key_resp_s18.rt), ], select = c(10))
s27 = subset(df[complete.cases(df$key_resp_s27.rt), ], select = c(11))
s4 = subset(df[complete.cases(df$key_resp_s4.rt), ], select = c(12))
s13 = subset(df[complete.cases(df$key_resp_s13.rt), ], select = c(13))
s22 = subset(df[complete.cases(df$key_resp_s22.rt), ], select = c(14))
s31 = subset(df[complete.cases(df$key_resp_s31.rt), ], select = c(15))
s8 = subset(df[complete.cases(df$key_resp_s8.rt), ], select = c(16))
s17 = subset(df[complete.cases(df$key_resp_s17.rt), ], select = c(17))
s26 = subset(df[complete.cases(df$key_resp_s26.rt), ], select = c(18))
s3 = subset(df[complete.cases(df$key_resp_s3.rt), ], select = c(19))
s12 = subset(df[complete.cases(df$key_resp_s12.rt), ], select = c(20))
s21 = subset(df[complete.cases(df$key_resp_s21.rt), ], select = c(21))
s30 = subset(df[complete.cases(df$key_resp_s30.rt), ], select = c(22))
s7 = subset(df[complete.cases(df$key_resp_s7.rt), ], select = c(23))
s16 = subset(df[complete.cases(df$key_resp_s16.rt), ], select = c(24))
s25 = subset(df[complete.cases(df$key_resp_s25.rt), ], select = c(25))
s2 = subset(df[complete.cases(df$key_resp_s2.rt), ], select = c(26))
s11 = subset(df[complete.cases(df$key_resp_s11.rt), ], select = c(27))
s20 = subset(df[complete.cases(df$key_resp_s20.rt), ], select = c(28))
s29 = subset(df[complete.cases(df$key_resp_s29.rt), ], select = c(29))
s6 = subset(df[complete.cases(df$key_resp_s6.rt), ], select = c(30))
s15 = subset(df[complete.cases(df$key_resp_s15.rt), ], select = c(31))
s24 = subset(df[complete.cases(df$key_resp_s24.rt), ], select = c(32))

df.wide <- cbind(s1, s10, s19, s28, s5, s14, s23, s32, 
               s9, s18, s27, s4, s13, s22, s31, s8, 
               s17, s26, s3, s12, s21, s30, s7, s16, 
               s25, s2, s11, s20, s29, s6, s15, s24,
               deparse.level = 0)
names(df.wide)
colnames(df.wide) <- c("s1", "s10", "s19", "s28", "s5", "s14", "s23", "s32", 
                       "s9", "s18", "s27", "s4", "s13", "s22", "s31", "s8", 
                       "s17", "s26", "s3", "s12", "s21", "s30", "s7", "s16", 
                       "s25", "s2", "s11", "s20", "s29", "s6", "s15", "s24"
)
write.csv2(df.wide, file = "dfRT_wide.csv")
filenew = "df.wide.txt"
write.table(df.wide, file=filenew, sep=";", dec=",", na="", row.names=FALSE)

df.wide$wd_number <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
df.wide$wd_number <- factor(df.wide$wd_number)

df.long <- melt(df.wide,
                  # ID variables - all the variables to keep but not split apart on
                  id.vars=c("wd_number"),
                  # The source columns
                  measure.vars=c("s1", "s10", "s19", "s28", "s5", "s14", "s23", "s32", 
                                 "s9", "s18", "s27", "s4", "s13", "s22", "s31", "s8", 
                                 "s17", "s26", "s3", "s12", "s21", "s30", "s7", "s16", 
                                 "s25", "s2", "s11", "s20", "s29", "s6", "s15", "s24"
                                 ),
                  # Name of the destination column that will identify the original
                  # column that the measurement came from
                  variable.name=c("item"),
                  value.name=c("rt")
)
df.long
write.csv2(df.long, file = "SPR.csv")

filenew = "SPR.txt"
write.table(df.long, file=filenew, sep=";", dec=",", na="", row.names=FALSE)
