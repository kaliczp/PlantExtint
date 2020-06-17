raw <- read.csv2("statisztika35.csv")
raw <- raw[,-(9:11)] # Egy dátum kiesik a 25. sorban az NA-kal
raw <- raw[rowSums(raw == "") != ncol(raw),] # Az üres sorok kiejtése
## Forrás https://stackoverflow.com/questions/6437164/removing-empty-rows-of-a-data-file-in-r
ttcore <- raw[-1, c(3,7,12)] # Csak a fontos oszlopok kiszed, első sor töröl
ttcore <- ttcore[-grep("Nyirád", ttcore[,1]),] # Üres NyirádX kiszedése
tthelyek <- which(rowSums(ttcore[,2:3] == "") == 2) # Hol van csak kategória
tthelyek.nam <- as.character(ttcore[tthelyek, 1]) # Kategória nevek
hany.hely <- diff(c(tthelyek, nrow(ttcore)+1)) # Elem a kategóriában
kat.fac <- factor(rep(tthelyek.nam, hany.hely), levels = tthelyek.nam)
full.df <- cbind(kat.fac, ttcore) # Oszlop kategóriákkal
full.df <- full.df[-tthelyek, ] # Csak nev sorok törlése
names(full.df) <- c("Kat", "Hely", "Old", "New") # Oszlop nevek

full.df$Diff <- as.numeric(as.character(full.df$New)) -
    as.numeric(as.character(full.df$Old)) # Különbség képzés
full.df <- full.df[!is.na(full.df$Diff),] # A ki nem számolt különbségek törlése

full.pos <- full.df[full.df$Diff <= 0, c("Kat", "Diff")] # Hol csökkent?

sum.df <- as.data.frame(tapply(full.pos[,2], full.pos[,1], mean)) # Átlagos csökkenés
sum.df <- cbind(sum.df, as.data.frame(summary(full.pos[,1])))
names(sum.df) <- c("Mean", "Count")

