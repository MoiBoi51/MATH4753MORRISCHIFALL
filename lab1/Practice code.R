 z <- (y - mean(y))/ sd(y)

 Z transformation

 ddt <- read.csv("DDT.csv")

 L <- ddt$LENGTH

 z <- (L - mean(L))/sd(L)



 ```{R}
 ddt <- read.csv("DDT.csv")

 L <- ddt$LENGTH

 L

 W <- ddt$WEIGHT

 z <- (W - mean(W))/sd(W)

 z

 ddt[z > 3, ]
 ddt[abs(z) > 3, ]
 W[ abs(z) > 2 & abs(z) <= 3]
 ```

 C <- ddt$SPECIES


 W <- ddt$WEIGHT

z <- scale(W)[,1]

W[abs(z) > 3]

Always follow the format of (OBJECT NAME) followed by ($)  dollar sign then followed
 by name of variable from data object (LENGTH) for example

 So, it looks like object name "ddt", $, variable name "LENGTH"
 "ddt$LENGTH"








 library(dplyr)

 Q1 <- quantile(ddt$LENGTH, 0.25)
 Q3 <- quantile(ddt$LENGTH, 0.75)
 IQR <- Q3 - Q1

 lower_bound <- Q1 - 1.5 * IQR
 upper_bound <- Q3 + 1.5 * IQR

 ddt %>%
   filter(LENGTH < lower_bound | LENGTH > upper_bound) %>%
   summarise(outlier_count = n())



 library(dplyr)

 ccatfish_outliers <- ddt %>%
   filter(SPECIES == "CCATFISH") %>%            # Keep only CCATFISH
   mutate(z_ddt = (DDT - mean(DDT)) / sd(DDT)) %>%  # Compute z-score for DDT
   filter(abs(z_ddt) > 3) %>%                   # Keep only outliers
   tally()                                      # Count them

 ccatfish_outliers


 library(dplyr)

 ccatfish_outliers <- ddt %>%
   filter(SPECIES == "CCATFISH") %>%         # Keep only CCATFISH
   mutate(z_ddt = (DDT - mean(DDT)) / sd(DDT)) %>%  # Compute z-score for DDT
   filter(abs(z_ddt) > 3) %>%                # Keep only outliers
   tally()                                   # Count how many rows

 ccatfish_outliers


 ddtc <- ddt[ddt$SPECIES == "CCATFISH",]
  L <- ddtc$LENGTH
  z <- scale(L)[,1]
  ddtc[abs(z) >= 2 & abs(z) <= 3, ]

  library(Intro2R)


  ddt$LENGTH < 1500












  ```{R}
  library(dplyr)

  ccatfish_outliers <- ddt %>%
    filter(SPECIES == "CCATFISH") %>%            # Keep only CCATFISH
    mutate(z_ddt = (DDT - mean(DDT)) / sd(DDT)) %>%  # Compute z-score for DDT
    filter(abs(z_ddt) > 3) %>%                   # Keep only outliers
    tally()                                      # Count them

  ccatfish_outliers




  ```
   ddt %>% mutate(z = scale(LENGTH)[,1], outlier = ifelse(abs(z) > 3,"yes", "no")) %>% group_by(outlier) %>% summarize(n = n())


  ```{R}
  ddt %>%
    filter(SPECIES == "LMBASS", RIVER == "TRM", DDT > 2) %>%
    summarise(count = n())


  ```
  ```{R}
  ddt %>% mutate(z = scale(LENGTH)[,1], outlier = ifelse(abs(z) > 3,"yes", "no")) %>% group_by(outlier) %>% summarize(n = n())


  ```








  library(dplyr)

  # 1. Fish with WEIGHT larger than 1600
  ddt %>%
    filter(WEIGHT > 1600) %>%
    summarise(count = n())

  # 2. LMBASS with WEIGHT strictly between 1000 and 1600
  ddt %>%
    filter(SPECIES == "LMBASS", WEIGHT > 1000, WEIGHT < 1600) %>%
    summarise(count = n())

