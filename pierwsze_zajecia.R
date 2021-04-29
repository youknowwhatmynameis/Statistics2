library(gtools)

urn <- c('a', 'b', 'c')

#wariacje bez powtorzen
war_bez_powt <- permutations(n = length(urn), r = 1, v = urn, repeats.allowed = FALSE)

war_bez_powt
