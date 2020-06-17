raw <- read.csv2("statisztika35.csv")
## TATÁRJUH
tttatj <- raw[c(5:25,27), c(3,7,15)]
tttatj <- tttatj[-5,] # mi az az tavhiány
tttatj[,2] <- as.numeric(as.character(tttatj[,2]))
tttatj[,3] <- as.numeric(as.character(tttatj[,3]))
tttatj <- cbind(tttatj, "TATÁRJUH")
names(tttatj) <- c("Hely","Old","New","Group")
full.df <- tttatj
## MEZEIJUH
ttmezj <- raw[33:44, c(3,7,15)]
ttmezj[,2] <- as.numeric(as.character(ttmezj[,2]))
ttmezj[,3] <- as.numeric(as.character(ttmezj[,3]))
ttmezj <- cbind(ttmezj, "MEZEIJUH")
names(ttmezj) <- c("Hely","Old","New","Group")
full.df <- rbind(full.df, ttmezj)
## CS-Tdomb
ttcsdomb <- raw[49:73, c(3,7,15)]
ttcsdomb[,2] <- as.numeric(as.character(ttcsdomb[,2]))
ttcsdomb[,3] <- as.numeric(as.character(ttcsdomb[,3]))
ttcsdomb <- cbind(ttcsdomb, "CS-Tdomb")
names(ttcsdomb) <- c("Hely","Old","New","Group")
full.df <- rbind(full.df, ttcsdomb)
## CS-Thegy
ttcshegy <- raw[78:114, c(3,7,15)]
ttcshegy[,2] <- as.numeric(as.character(ttcshegy[,2]))
ttcshegy[,3] <- as.numeric(as.character(ttcshegy[,3]))
ttcshegy <- cbind(ttcshegy, "CS-Thegy")
names(ttcshegy) <- c("Hely","Old","New","Group")
full.df <- rbind(full.df, ttcshegy)
## GY-Tdomb
ttgydomb <- raw[119:162, c(3,7,15)]
ttgydomb <- ttgydomb[-(20:22), ]
ttgydomb[,2] <- as.numeric(as.character(ttgydomb[,2]))
ttgydomb[,3] <- as.numeric(as.character(ttgydomb[,3]))
ttgydomb <- cbind(ttgydomb, "GY-Tdomb")
names(ttgydomb) <- c("Hely","Old","New","Group")
full.df <- rbind(full.df, ttgydomb)
## GY-Thegyi
ttgyhegy <- raw[167:189, c(3,7,15)]
ttgyhegy[,2] <- as.numeric(as.character(ttgyhegy[,2]))
ttgyhegy[,3] <- as.numeric(as.character(ttgyhegy[,3]))
ttgyhegy <- cbind(ttgyhegy, "GY-Thegyi")
names(ttgyhegy) <- c("Hely","Old","New","Group")
full.df <- rbind(full.df, ttgyhegy)


full.df$Diff <- full.df$Old - full.df$New

full.pos <- full.df[full.df$Diff >= 0, 4:5]

sum.df <- as.data.frame(tapply(full.pos[,2], full.pos[,1], mean))
sum.df <- cbind(sum.df, as.data.frame(summary(full.pos[,1])))
names(sum.df) <- c("Mean", "Count")

