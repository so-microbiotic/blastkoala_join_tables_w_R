# library("tidyverse")

moralis <- read.delim("Galaxy230-[m_oralis_definition.txt].txt")
msmithii <- read.delim("Galaxy232-[m_smithii_definition.txt].txt")

column_names <- c("Query", "KO", "Definition", "Score1", "Seco")nd_Best", "Score2
colnames(moralis) <- column_names
colnames(msmithii) <- column_names

moralis$KO <- as.character(moralis$KO)
msmithii$KO <- as.character(msmithii$KO)

moralis$Definition <- as.character(moralis$Definition)
msmithii$Definition <- as.character(msmithii$Definition)

msmithii_ko <- msmithii[, 2:3]
moralis_ko <- moralis[, 2:3]

msmithii_ko_to_join <- msmithii_ko %>% filter(str_detect(msmithii_ko[,1], "^K"))
moralis_ko_to_join <- moralis_ko %>% filter(str_detect(moralis_ko[,1], "^K"))

msmithii_ko_to_join2 <- msmithii_ko %>% group_by(KO) %>% count()
moralis_ko_to_join2 <- moralis_ko %>%  group_by(KO) %>% count()

ko_full_join <- msmithii_ko_to_join %>%
  full_join(moralis_ko_to_join, by = "KO", suffix=c("_msmithii", "_moralis")) %>%
  left_join(msmithii_ko_to_join2, by = "KO", suffix=c("_msmithii", "_moralis")) %>%
  left_join(moralis_ko_to_join2, by = "KO", suffix=c("_msmithii", "_moralis")) 

ko_full_join2 <- ko_full_join %>%
  mutate(count_totals = rowSums[, 4:5])


  

write.csv(ko_full_join, "ko_counts_and_defintions.csv")


  