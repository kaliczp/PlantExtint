par(las = 2)
boxplot(Diff ~ Kat, full.df, xlab = "") # Boxplot

full.pos <- full.df[full.df$Diff <= 0, c("Kat", "Diff")] # Hol csökkent?

sum.df <- as.data.frame(tapply(full.pos[,2], full.pos[,1], mean)) # Átlagos csökkenés
sum.df <- cbind(sum.df, as.data.frame(summary(full.pos[,1]))) # Hely szám
names(sum.df) <- c("Mean", "Count")

full.nov <- full.df[full.df$Diff > 0, c("Kat", "Diff")] # Hol nőtt?
sumnov.df <- as.data.frame(tapply(full.nov[,2], full.nov[,1], mean)) # Átlagos növekedés
sumnov.df <- cbind(sumnov.df, as.data.frame(summary(full.nov[,1]))) # Hely szám
names(sumnov.df) <- c("Mean", "Count")

sumall.df <- rbind(sum.df, sumnov.df)
