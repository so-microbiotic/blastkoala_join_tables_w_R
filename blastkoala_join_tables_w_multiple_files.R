#library("tidyverse")

#read in files to make a single data frame for easier data cleaning and parsing

files <- list.files(pattern = ".txt")
kegg <- files %>% map(read.delim2)

#give column names to keep track of data frames within list "kegg"
dfnames <- c("marbor", "mbov", "mcuv", "mcut", "mgott",  "mmill", "moll", "moralis", "mrum" , "msmithii", "mtha", "mwoi", "mwol",  "mfili") 
names(kegg) <- dfnames

#preliminary clearning, selecting rows of interest, dropping na-s, and ensuring future colnames are unique
kegg_for_join <- kegg  %>% 
  map(~ select(.x, ,2:3)) %>% 
  map(~ drop_na(.x)) %>% 
  map(~ distinct(.x)) %>% 
  map(~ filter(.x,""))
#assign column names
colnames <- c("Query", "KO")

#create single data frame with all of the ko information called test
test <- kegg_for_join %>% 
  map(~ set_names(.x, nm=colnames)) %>% 
  map(~ modify_at(.x, c("KO", "Definition"),as.character)) %>%
  map(~ filter(.x, KO != "" & Definition != ""))

#create a second data frame with count values for merging tables
test2 <- test %>%
  map(~ group_by(.x, , KO)) %>%
  map(~ count(.x, , KO)) %>%
  map(~ arrange(.x, KO))

# assign indvidual taxa tables to variables in R environment

marbor <- data.frame(test$marbor)
mbov <- data.frame(test$mbov)
mcut <- data.frame(test$mcut)
mcuv <- data.frame(test$mcuv)
mfili <- data.frame(test$mfili)
mgott <- data.frame(test$mgott)
mmill <- data.frame(test$mmill)
moll <- data.frame(test$moll)
moralis <- data.frame(test$moralis)
mrum <- data.frame(test$mrum)
msmithii <- data.frame(test$msmithii)
mtha <- data.frame(test$mtha)
mwoi <- data.frame(test$mwoi)
mwol <- data.frame(test$mwol)

# combine aforementioned variables with counts
marbor <- marbor %>% full_join(test2$marbor)
mbov <- mbov %>% full_join(test2$mbov)
mcut <- mcut %>% full_join(test2$mcut)
mcuv <- mcuv %>% full_join(test2$mcuv)
mfili <- mfili %>% full_join(test2$mfili)
mgott <- mgott %>% full_join(test2$mgott)
mmill <- mmill %>% full_join(test2$mmill)
moll <- moll %>% full_join(test2$moll)
moralis <- moralis %>% full_join(test2$moralis)
mrum <- mrum %>% full_join(test2$mrum)
msmithii <- msmithii %>% full_join(test2$msmithii)
mtha <- mtha %>% full_join(test2$mtha)
mwoi <- mwoi %>% full_join(test2$mwoi)
mwol <- mwol %>% full_join(test2$mwol)

# full join for all taxa maintaining their KO#s, Definitions, and counts (n)

ko_full_join <- marbor %>% full_join(mbov, by = c("KO", "Definition")) %>%
  full_join(mcuv, by = c("KO", "Definition")) %>%
  full_join(mcut, by = c("KO", "Definition")) %>% 
  full_join(mgott, by = c("KO", "Definition")) %>% 
  full_join(mmill, by = c("KO", "Definition")) %>% 
  full_join(moll, by = c("KO", "Definition")) %>% 
  full_join(moralis, by = c("KO", "Definition")) %>% 
  full_join(mrum, by = c("KO", "Definition")) %>% 
  full_join(msmithii, by = c("KO", "Definition")) %>% 
  full_join(mtha, by = c("KO", "Definition")) %>% 
  full_join(mwoi, by = c("KO", "Definition")) %>% 
  full_join(mwol, by = c("KO", "Definition")) %>% 
  full_join(mfili, by = c("KO", "Definition")) %>%
  arrange(KO)

#reassign names in one line of code
names(ko_full_join)[3:16] <- names

#create a column with total counts across all rows and arrange in descending order
ko_full_join <- ko_full_join %>% 
  mutate(count_sum = rowSums(ko_full_join[,3:14], na.rm = TRUE)) %>% 
  arrange(desc(count_sum)) 






                                       





