
library(readxl)
cdcData <-read_excel('bmi_cdc.xls')
wgsrData <-read.csv('wgsrData.csv')


bmiz_to_bmi <- function(l, m, s, bmiz) {
 x = m * (exp ((log(bmiz*l*s + 1))/l))
 return(x)
}

bmi_lookup_cdc <- function(sex, age, bmiz) {
  
## Adjust age in months based on CDC recommendation for age-based secondPart

age <- ifelse(age >= 0 & age < 0.5, 0, as.integer(age + 0.5) - 0.5)

## Lookup reference values and calculate z-score

lkpIndexSex <- cdcData[cdcData$Sex == sex, ]
L <- approx(lkpIndexSex$Agemos, lkpIndexSex$L,
            xout = age, ties = "ordered")$y
M <- approx(lkpIndexSex$Agemos, lkpIndexSex$M,
            xout = age, ties = "ordered")$y
S <- approx(lkpIndexSex$Agemos, lkpIndexSex$S,
            xout = age, ties = "ordered")$y

bmiz_to_bmi(l = L, m = M, s = S, bmiz = bmiz)

}

bmi_lookup_cdc(1, 78, 0.5)


bmi_lookup_wgsr <- function(sex, age, bmiz) {
  
  ## Adjust age in months based on CDC recommendation for age-based secondPart
 
  
  ## Lookup reference values and calculate z-score
  
  lkpIndexSex <- wgsrData[cdcData$Sex == sex, ]
  L <- approx(lkpIndexSex$given, lkpIndexSex$L,
              xout = age, ties = "ordered")$y
  M <- approx(lkpIndexSex$given, lkpIndexSex$M,
              xout = age, ties = "ordered")$y
  S <- approx(lkpIndexSex$given, lkpIndexSex$S,
              xout = age, ties = "ordered")$y
  
  bmiz_to_bmi(l = L, m = M, s = S, bmiz = bmiz)
  
}

bmi_lookup_wgsr(1, 2375.5, 0.5)

