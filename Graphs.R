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

## Variable column width
## https://www.r-graph-gallery.com/81-barplot-with-variable-width.html

library(ggplot2)

# make data
data <- data.frame(
  group=c("A ","B ","C ","D ") ,
  value=c(33,62,56,67) ,
  number_of_obs=c(100,500,459,342)
)

# Calculate the future positions on the x axis of each bar (left border, central position, right border)
data$right <- cumsum(data$number_of_obs) + 30*c(0:(nrow(data)-1))
data$left <- data$right - data$number_of_obs

# Plot
ggplot(data, aes(ymin = 0)) +
    geom_rect(aes(xmin = left, xmax = right, ymax = value, colour = group, fill = group)) +
    xlab("number of obs") +
    ylab("value") +
    theme(legend.position="none")
