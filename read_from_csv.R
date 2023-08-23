# Import data
options(stringsAsFactors = FALSE)
avian <- read.csv('avianHabitat.csv')
avian$Observer <- as.factor(avian$Observer)

avian_new <- read.csv(sep = ';',
                      skip = 5,
                      header = T,
                      comment.char = '%',
                      quote = '',
                      na.strings = "Don't remember",
                      file = 'avianHabitat2.csv')
# Check data
str(avian)
head(avian)
summary(avian)

any(!complete.cases(avian)) # проверка на пропуски
any(avian$PDB < 0)
any(avian$PDB > 100) 

check_percant_range <- function(x){
  any(x < 0 | x > 100)
}
check_percant_range(avian$PW)
check_percant_range(avian$PDB)

library(stringr)
coverage_variables <- names(avian)[str_detect(names(avian), "^P")]
# Поиск всех столбцов, начинающихся на P
sapply(coverage_variables, function(name) check_percant_range(avian[[name]]))
# Проверка, что все процнты в диапазоне 0-100

# Transform variables
names(avian) # вывод переменных
#coverage_variables <- names(avian)[-(1:4)][c(T, F)] # переменные, содержащие в себе проценты
avian$total_coverage <- rowSums(avian[, coverage_variables])
summary(avian$total_coverage)

avian$site_name <- factor(str_replace(avian$Site, "[:digit:]+", ""))
tapply(avian$DBHt, avian$site_name, mean) 

tapply(avian$DB, avian$Observer, max)
tapply(avian$W, avian$Observer, max)
tapply(avian$E, avian$Observer, max)
tapply(avian$A, avian$Observer, max)
tapply(avian$H, avian$Observer, max)
tapply(avian$L, avian$Observer, max)

avian_new$Observer <- c(1:18)
for (i in 1:18) avian_new$Observer[i] <- 'KL'

avian0 <- rbind(avian, avian_new)
avian0$total_coverage <- rowSums(avian0[, coverage_variables])
summary(avian0$total_coverage)


#diplyr
library(stringr)
library(dplyr)
options(stringAsFactors = FALSE)

# First approach
avian <- read.csv('avianHabitat.csv')

avian <- subset(avian, PDB > 0 & DBHt >0, c("Site", "Observer", "PDB", "DBHt"))
avian$Site <- factor(str_replace(avian$Site, "[:digit:]+", ""))
subset(
  aggregate(avian$DBHt, list(Site = avian$Site, Observer = avian$Observer), max),
  x >= 5
)

# Second approach (using pipes)
avian <- read.csv('avianHabitat.csv')

avian <-
  avian %>%
  subset(PDB >0 & DBHt >0, c("Site", "Observer", "PDB", "DBHt")) %>%
  transform(Site = factor(str_replace(.$Site, "[:digit:]+", "")))
aggregate(avian$DBHt, list(Site = avian$Site, Observer = avian$Observer), max) %>%
  subset(x >= 5)

# Third approach
avian <- read.csv('avianHabitat.csv')

avian %>%
  filter(PDB > 0, DBHt >0) %>%
  select(Site, Observer, contains("DB")) %>%
  mutate(Site = factor(str_replace(Site, "[:digit:]+", ""))) %>%
  group_by(Site, Observer) %>%
  summarise(MaxHt = max(DBHt)) %>%
  filter(MaxHt >= 5)



warp <- warpbreaks
warp %>%
  group_by(wool, tension)
  summarise(avg = mean(breaks), max = max(breaks)) %>%
  filter(avg > 25 | max > 42)

  
  
  
  
avian <- read.csv('avianHabitat.csv')
  
avian %>%
  filter(PDB > 0, DBHt >0) %>%
  select(Site, Observer, contains("DB")) %>%
  mutate(Site = factor(str_replace(Site, "[:digit:]+", ""))) %>%
  group_by(Site, Observer) %>%
  summarise(MaxHt = max(DBHt)) %>%
  filter(MaxHt >= 5)