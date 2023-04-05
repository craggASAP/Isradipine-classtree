# Load library
library(tidyverse)
library(rpart)
library(rpart.plot)

# Load data
df<-read.csv(file.choose(), header=TRUE)

# csv file is 5 columns with headers: sex; drug; genotype; region; L_effect. 
# each row is experimental replicate. 
#L_effect is [DA] in isradipine normalised to control

# tidy data to allow conversion to factors
df$sex <- tolower(df$sex)
df$sex <- str_remove(df$sex, " ")
df$drug <- tolower(df$drug)
df$region <- tolower(df$region)
df$genotype <- tolower(df$genotype)

# convert to factors
df <- df %>%
  mutate_at(c(1:4), as.factor)
# convert NA from "missing" to factors NB this might make more sense to change NA to none
df$drug <- addNA(df$drug)

df <- df %>%
  rename(norm.L.effect = L_effect)

str(df) # check data structure
levels(df$drug) # check the expected drugs are there 


# calculate mean and sd of CPU, Male, WT, control experiments to caluclate cut offs for TRUE L_effect
male_Cpu <- df %>%
  filter(sex == "male" & region == "cpu" & genotype == "wt" & drug != "cocaine")
summarise(male_Cpu, mean = mean(male_Cpu$norm.L.effect), sd = sd(male_Cpu$norm.L.effect))

# calculate mean and sd of NAc, Male, WT, control experiments to caluclate cut offs for FALSE L_effect (check doesn't differ from TRUE cut offs)
male_nac <- df %>%
  filter(sex == "male" & region == "nac" & genotype == "wt" & drug != "l741")
summarise(male_nac, mean = mean(male_nac$norm.L.effect), sd = sd(male_nac$norm.L.effect))

# TRUE cut off is mean of CPU, Male, WT, control +2 sd
# NB 0.9 is 2 sd from "male Cpu wt" i.e initial case of "Leffect)   and 0.85 is 2sd away from "male nac wt " (i,e initial case of no effect)
df <- df %>%
  mutate(L_effect = as.logical(df$norm.L.effect <= 0.90))

# remove numerical norm.L.effect
df1 <- df %>%
  select(sex, drug, genotype, region, L_effect)

# Plot classification tree
tree <- rpart(L_effect ~., data = df1, method = "class")
summary(tree)
rpart.plot(tree, extra = 106, type = 3) # https://cran.r-project.org/web/packages/rpart.plot/rpart.plot.pdf style guide to rpart.plot

# NB CPu is equivalent to DLS. Change in nomenclature in MS
