par(las = 2)
boxplot(Diff ~ Kat, full.df, xlab = "") # Boxplot

full.pos <- full.df[full.df$Diff <= 0, c("Kat", "Diff")] # Hol csökkent?

sum.df <- as.data.frame(tapply(full.pos[,2], full.pos[,1], mean)) # Átlagos csökkenés
sum.df <- cbind(sum.df, as.data.frame(summary(full.pos[,1]))) # Hely szám
names(sum.df) <- c("Mean", "Count")
sum.df$Kat <- row.names(sum.df)

full.nov <- full.df[full.df$Diff > 0, c("Kat", "Diff")] # Hol nőtt?
sumnov.df <- as.data.frame(tapply(full.nov[,2], full.nov[,1], mean)) # Átlagos növekedés
sumnov.df <- cbind(sumnov.df, as.data.frame(summary(full.nov[,1]))) # Hely szám
names(sumnov.df) <- c("Mean", "Count")

sumall.df <- rbind(sum.df, sumnov.df)

## Variable column width
## https://www.r-graph-gallery.com/81-barplot-with-variable-width.html

library(ggplot2)

# Calculate the future positions on the x axis of each bar (left border, central position, right border)
sum.df$right <- cumsum(sum.df$Count) + 5*c(0:(nrow(sum.df)-1))
sum.df$left <- sum.df$right - sum.df$Count

# Plot
ggplot(sum.df, aes(ymin = 0)) +
    geom_rect(aes(xmin = left, xmax = right, ymax = Mean, colour = Kat, fill = Kat)) +
    xlab("number of obs") +
    ylab("Mean") +
    theme(legend.position="none")
