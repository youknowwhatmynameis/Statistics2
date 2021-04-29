Centrala
print(Centrala)
liczebnosc <- table(Centrala)
liczebnosc
procent <- prop.table(table(Centrala))
procent
tabela <- cbind(liczebnosc,procent)
tabela
barplot(liczebnosc, 
        xlab = "Liczba zgłoszeń", 
        ylab = "Liczebność",
        main = "Rozkład empiryczny",
        col = 1:6)
barplot(procent, 
        xlab = "Liczba zgłoszeń", 
        ylab = "Prawdopodobieństwo",
        main = "Rozkład empiryczny",
        col = 1:6)
mean(Centrala$Liczba)
median(Centrala$Liczba)
sd(Centrala$Liczba)
sd(Centrala$Liczba)/mean(Centrala$Liczba)*100

