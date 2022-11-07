myName <- "Jingyao Wang"

library(tidyverse)
library(magrittr)
library(readxl)

## Start by reading the data
strawb <- read_xlsx("strawberries-2022oct30-a.xlsx", col_names = T)

cnames <- colnames(strawb)
x <- 1:dim(strawb)[2]

unique(strawb[1])
unique(strawb[2])
unique(strawb[3])

T <- NULL
for(i in x){T <- c(T, dim(unique(strawb[i]))[1])}
drop_cols <- cnames[which(T == 1)]
strawb %<>% select(!all_of(drop_cols))
strawb %<>% arrange(Year, State)


## Look at the strawb data frame again. You can see that the 
## columns need work. The State ANSI column contains a unique
## code for each state. If you need to access US Census data for
## the states, this code will come in handy.

colnames(strawb)

temp1 <- strawb %>% select(`Data Item`) %>% 
         distinct()

strawb2 <- strawb %>% separate(col=`Data Item`,
                into = c("Strawberries", "items", "units"),
                sep = ",",
                fill = "right")

strawb3 <- strawb %>% separate(col=`Data Item`,
            into = c("Strawberries", "type", "items", "units"),
                               sep = ",",
                               fill = "right")

## That worked. Clean up the dat.

rm(strawb2, strawb3)

strawb %<>% separate(col=`Data Item`,
                    into = c("Strawberries", "type", "items", "units"),
                    sep = ",",
                    fill = "right")


## 2
organ.ca.2016 <- filter(strawb, State == 'CALIFORNIA' 
                             & Year == 2016 
                             & Domain == 'ORGANIC STATUS')
231304956*1.96*0.137
231304956+62110007
231304956-62110007

## 3
library(gmodels)
library(Rmisc)
nonorgan.ca.2016 <- filter(strawb, State == 'CALIFORNIA' 
                                & Year == 2016 
                                & Domain != 'ORGANIC STATUS')
new.non <- filter(nonorgan.ca.2016, Value != "(NA)" & Value != "(D)" 
                  & Domain != "TOTAL")

CI(as.numeric(new.non$Value))

## 4
unique(strawb[10])
chem <- filter(strawb, Domain != 'ORGANIC STATUS' 
               & Domain != 'TOTAL')
grep("TOTAL", chem$`Domain Category`, ignore.case = T)
unique(chem[11])
175 - 36

## 5
chem.fl <- filter(strawb, State == 'FLORIDA' 
                  & Domain != 'ORGANIC STATUS' 
                  & Domain != 'TOTAL')
chem.ca <- filter(strawb, State == 'CALIFORNIA' 
                  & Domain != 'ORGANIC STATUS' 
                  & Domain != 'TOTAL')
unique(chem.fl[11])
unique(chem.ca[11])
142 - 119

## now explore the new columns

## we know that "THIRAM" is a chemical in the data, so
## test for it to check out the way code
r_thiram <- grep("THIRAM", strawb$`Domain Category`)
r_thiram_1 <- grep("Thiram", strawb$`Domain Category`, ignore.case = T)

## Chemicals mentioned in 
## the "Shoppers Guide to Pesticides in Produce"
## Carbendazim, Bifenthrin, methyl bromide, 1,3-dichloropropene,
## chloropicrin, Telone

df_carbendazim <- grep("carbendazim", 
                       strawb$`Domain Category`, ignore.case = T)

## Bifenthrin found 27
df_Bifenthrin <- grep("Bifenthrin", 
                       strawb$`Domain Category`, ignore.case = T)

## methyl bromide found 3
df_methyl_bromide <- grep("methyl bromide", 
                      strawb$`Domain Category`, ignore.case = T)

## 1,3-dichloropropene empty
df_1_3_dichloropropene <- grep("1,3-dichloropropene", 
                          strawb$`Domain Category`, 
                          ignore.case = T)

## chloropicrin found 18
df_chloropicrin <- grep("chloropicrin", 
                               strawb$`Domain Category`, 
                               ignore.case = T)

## Telone empty
df_Telone <- grep("Telone", 
                        strawb$`Domain Category`, 
                        ignore.case = T)







