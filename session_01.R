if(!("prob" %in% installed.packages())){
  install.packages("prob")
}
library(prob)

if(!("gtools" %in% installed.packages())){
  install.packages("gtools")
}
library(gtools)

if(!("gmp" %in% installed.packages())){
  install.packages("gmp")
}
library(gmp)

#############################################################################  2 Elementy kombinatoryki


###
# Zadanie 2. Ile jest liczb czterocyfrowych, w których nie powtarza się żadna cyfra?
## [1] 4536

FourDigitNoRepet <- 9*9*8*7  

###
# Zadanie 3. Grupa studentów idzie na górską wycieczkę gęsiego. W grupie jest dwanaście pań i jedenastu panów. Iloma sposobami mogą się ustawić, jeżeli panie nie mogą sąsiadować z paniami, a panowie z panami?
## [1] 1.912021e+16
## [1] "19120211066880000"
# Korzystajac z definicji permutacji bez powtorzen oraz zasady mnozenia.
factorial(12)*factorial(11)

###
#Zadanie 4. Ile różnych wyrazów można utworzyć ze słowa MATEMATYKA (muszą zawierać wszystkie litery)?
## [1] 151200
# Korzystajac z definicji permutacji z powtorzeniami.
factorial(10)/(factorial(2)*factorial(3)*factorial(2)*factorial(1)*factorial(1)*factorial(1))

###
# Zadanie 5. Iloma sposobami można położyć 12 książek na trzech półkach, tak aby na pierwszej półce było 6 książek, na drugiej 4, a na trzeciej reszta?
## [1] 13860
choose(12, 6)*choose(6, 4)*choose(2, 2)

###
# Zadanie 6. Kości do gry w domino są oznaczone dwiema liczbami. Ile różnych kości można utworzyć z liczb 0, 1, 2, …, n?
##     n liczba_kosci
## 1   1            3
## 2   2            6
## 3   3           10
## 4   4           15
## 5   5           21
## 6   6           28
## 7   7           36
## 8   8           45
## 9   9           55
## 10 10           66
for(n in 1:10){
  cat(sprintf("\"%i\" \"%i\"\n", n, choose(n+1+2-1, 2)))
}

###
# Zadanie 7. Przy grze w preferansa każdy z trzech graczy otrzymuje 10 kart a dwie karty zostają do tzw. kupna (w banku). Iloma sposobami można rozdać karty graczom siedzącym na ustalonych miejscach?
## [1] 2.753294e+15
## [1] "2753294408504640"
choose(32, 10)*choose(22, 10)*choose(12, 10)*choose(2, 2)

###
# Zadanie 8. Urna zawiera dziesięć kul ponumerowanych liczbami 1, 2, …, 10.Z urny losujemy
# ze zwracaniem
# bez zwracania
# kolejno trzy kule. Wygeneruj wszystkie trzyelementowe ciągi liczb, które można w ten sposób otrzymać. Ile ich jest? Ile spośród tych ciągów ma sumę elementów podzielną przez 13?
urn <- 1:10
urn.result.Rep <- permutations(n = length(urn), r = 3, v = urn, repeats.allowed = TRUE)
print(nrow(urn.result.Rep))
sum(rowSums(urn.result.Rep)%%13==0)
urn.result.NoRep <- permutations(n = length(urn), r = 3, v = urn, repeats.allowed = FALSE)
print(nrow(urn.result.NoRep))
sum(rowSums(urn.result.NoRep)%%13==0)

###
#Zadanie 9. Wykonaj zadanie 8 nie uwzględniając kolejności.
urn.result.Rep <- combinations(n = length(urn), r = 3, v = urn, repeats.allowed = TRUE)
print(nrow(urn.result.Rep))
sum(rowSums(urn.result.Rep)%%13==0)
urn.result.NoRep <- combinations(n = length(urn), r = 3, v = urn, repeats.allowed = FALSE)
print(nrow(urn.result.NoRep))
sum(rowSums(urn.result.NoRep)%%13==0)

###############################################################################  3 Klasyczna definicja prawdopodobieństwa


###
# Zadanie 1. Dokonujemy trzech rzutów monetą. Oblicz prawdopodobieństwo, że orzeł pojawi się co najmniej raz.
space.coin <- tosscoin(3, makespace = TRUE)
Prob(space.coin, toss1=='H'|toss2=='H'|toss3=='H')

###
# Zadanie 2. Z talii 52 kart, losujemy jedną. Oblicz prawdopodobieństwo, że wylosowana karta jest damą lub kierem lub karo.
space.cards <- cards(jokers = FALSE, makespace = TRUE)
Prob(space.cards, rank=='Q'|suit=='Spade'|suit=='Heart')

###
# Zadanie 3. Rzucamy dwiema kostkami do gry. Oblicz prawdopodobieństwo, że na drugiej kostce wypadła nieparzysta liczba oczek, a suma liczb wyrzuconych na obu kostkach jest także nieparzysta.
space.die <- rolldie(2, 6, makespace = TRUE)
Prob(space.die, X2%%2!=0 & (X1+X2)%%2!=0)

###
# Zadanie 4. Urna zawiera dwie kule białe i trzy kule czarne. Wybieramy losowo dwie kule. Oblicz prawdopodobieństwo, że są to kule tego samego koloru. Rozwiąż zadanie dwoma sposobami: nie uwzględniając kolejności i uwzględniając kolejność.
urn <- c(rep('B', 2), rep('C', 3))
space.urn.NoOrd <- probspace(urnsamples(urn, 2))
Prob(space.urn.NoOrd, X1==X2)

space.urn.Ord <- probspace(urnsamples(urn, 2, ordered = TRUE))
Prob(space.urn.Ord, X1==X2)

###
# Zadanie 5. Cyfry 1, 2, 3, 4, 5 są napisane na pięciu kartkach, tak że każdej cyfrze odpowiada jedna kartka. Pobieramy losowo jednocześnie trzy kartki. Jakie jest prawdopodobieństwo, że suma otrzymanych cyfr będzie liczbą parzystą?
urn <- 1:5
space.urn <- probspace(urnsamples(urn, 3))
Prob(space.urn, (X1+X2+X3)%%2==0)

###
# Zadanie 6. Spośród wszystkich liczb czterocyfrowych wybieramy jedną. Oblicz prawdopodobieństwo, że w wylosowanej liczbie na pierwszym miejscu i na ostatnim występuje ta sama cyfra.
urn <- 1000:9999
space.urn <- probspace(urnsamples(urn, 1))
Prob(space.urn, substr(space.urn$out, 1, 1)==substr(space.urn$out, 4, 4))

urn <- 0:9
space.urn <- probspace(urnsamples(urn, 4, replace = TRUE, ordered = TRUE))
Prob(space.urn, X1==X4, given = X1!=0)

###
# Zadanie 7. Spośród wszystkich liczb czterocyfrowych o niepowtarzających się cyfrach wybieramy jedną. Oblicz prawdopodobieństwo, że wylosowana liczba jest parzysta.
urn <- 0:9
space.urn <- probspace(urnsamples(urn, 4, replace = FALSE, ordered = TRUE))
Prob(space.urn, (X4==2|X4==4|X4==6|X4==8|X4==0), given = X1!=0)

###
# Zadanie 8. Oblicz prawdopodobieństwo, że wśród 10 wybranych osób żadne dwie nie obchodzą urodzin w tym samym dniu. Przyjmijmy, że rok ma 365 dni.
# licznik to wariacje bez powtorzen -> n!/(n-k)! ; mianownik to wariacje z powtorzeniami -> n^k
factorialZ(365)/(factorialZ(355)*(365^10))
(365*364*363*362*361*360*359*358*357*356)/(365^10)

###
# Zadanie 9. Oblicz prawdopodobieństwo, że w 5-osobowej delegacji z klasy, w której jest 15 dziewcząt i 16 chłopców, znajdzie się 3 chłopców.
urn <- c(rep(0, 15), rep(1, 16))
space.urn <- probspace(urnsamples(urn, 5))
Prob(space.urn, X1+X2+X3+X4+X5==3)

###############################################################################  5 Prawdopodobieństwo warunkowe i niezależność zdarzeń


###
# Zadanie 1. Wśród rodzin z dwojgiem dzieci losujemy jedną rodzinę. Jakie jest prawdopodobieństwo zdarzenia, że wybierzemy rodzinę z dwiema dziewczynkami, jeżeli wiadomo, że w tej rodzinie:
# 1. starsze dziecko jest dziewczynką,
# 2. jest przynajmniej jedna dziewczynka.
urn <- c('d', 'c')
space.urn <- probspace(permutations(n = length(urn), r = 2, v = urn, repeats.allowed = TRUE))

Prob(space.urn, X1=='d' & X2=='d', given = X2 =='d')
Prob(space.urn, X1=='d' & X2=='d', given = X1 =='d' | X2 == 'd')

###
# Zadanie 2. Rzucamy dwiema kostkami do gry. Obliczyć prawdopodobieństwo zdarzenia polegającego na tym, że suma oczek otrzymanych na obu kostkach będzie nie większa 
# od czterech, jeśli wiadomo, że co najmniej na jednej kostce otrzymano dwa oczka.
urn <- 1:6
space.urn <- probspace(permutations(n = length(urn), r = 2, v = urn, repeats.allowed = TRUE))

Prob(space.urn, X1+X2 <= 4, given = X1==2 | X2==2)

###
# Zadanie 3. W pudełku są ołówki: 10 czerwono – niebieskich, 2 niebieskie, 7 zielonych, 1 zielono – czerwony. 
# Losujemy jeden ołówek. Jakie jest prawdopodobieństwo kreślenia otrzymanym ołówkiem w kolorze czerwonym, jeśli wiadomo, że ołówek ten rysuje na zielono.
urn <- c(rep('CN', 10), rep('N', 2), rep('Z', 7), rep('ZC', 1))
space.urn <- probspace(urnsamples(urn, 1))

Prob(space.urn, out=='C', given = out=='Z')










###
# Zadanie 4. Student zna odpowiedź na pytanie egzaminacyjne z prawdopodobieństwem p. Jeżeli nie zna odpowiedzi, to zgaduje jedną z k możliwych odpowiedzi z prawdopodobieństwem  
# 1 k. Jeżeli odpowiedział prawidłowo, to jakie jest prawdopodobieństwo, że znał odpowiedź?
# Przykładowe wartości prawdopodobieństwa (aby przedstawić je w poniższej postaci użyj funkcji outer(), as.data.frame(), rownames(), colnames(), paste()):






###
# Zadanie 5. W pierwszej skrzynce jest 15 jabłek zdrowych i 5 zepsutych. W drugiej skrzynce 14 jabłek zdrowych i 6 zepsutych. 
# Wybieramy losowo jedną ze skrzyń i wyciągamy z niej jedno jabłko. Jakie jest prawdopodobieństwo, że wybraliśmy drugą skrzynię, 
# jeśli wiemy, że wybrane jabłko okazało się zdrowe? Szukane prawdopodobieństwo wyznacz dokładnie i oszacuj symulacyjnie.


#########################################

space <- probspace(urnsamples(c("B", "B", "C", "C", "C"), 2))

View(space)

Prob(space, X1==X2)

space.2 <- probspace(urnsamples(c("B", "B", "C", "C", "C"), 2, ordered = TRUE))

View(space.2)

Prob(space.2, X1==X2)

##############################################################################################

urn <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
# urn <- 1:10

urn.result.rep <- gtools::permutations(n = 10, r = 3, v = urn, repeats.allowed = TRUE)

nrow(urn.result.rep)

sum(rowSums(x = urn.result.rep)%%13 == 0)

urn.result.no.rep <- gtools::permutations(n = 10, r = 3, v = urn, repeats.allowed = FALSE)

nrow(urn.result.no.rep)

sum(rowSums(x = urn.result.no.rep)%%13 == 0)

##############################################################################################

list <- 0:9


space.3 <- probspace(urnsamples(list, 4, ordered = TRUE, replace = TRUE))
Prob(space.3, X1==X4, given = X1!=0)


space.4 <- probspace(urnsamples(list, 4, ordered = TRUE, replace = FALSE))
Prob(space.4, X4==0 | X4==2 | X4==4 | X4==6 | X4==8, given = X1!=0)


space.5 <- probspace(urnsamples(rep(c(0,1), c(15,16)), 5))
Prob(space.5, X1+X2+X3+X4+X5==3)

##############################################################################################

space.7 <- probspace(rolldie(2))
Prob(space.7, X1+X2<=4, given = X2==2|X1==2)
Prob(space.7, X1+X2==2, given = X1+X2>=1)

rolldie(2)