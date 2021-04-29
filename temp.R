df <- read.table('http://ls.home.amu.edu.pl/data_sets/hamulce.txt', stringsAsFactors = FALSE, dec = ',')
str(df$V1)
breaks_hist <- hist(df$V1, plot = FALSE)$breaks

liczebnosc <- table( cut(df$V1, breaks_hist) )
procenty <- prop.table(liczebnosc)

df_2 <- cbind(liczebnosc, procenty)

hist(df$V1, plot = TRUE)

hist(df$V1, plot = TRUE, probability = TRUE)

boxplot(df$V1)

mean(df$V1)

median(df$V1)

quantile(df$V1)

var(df$V1)

sd(df$V1)

sd(df$V1)/mean(df$V1)*100
