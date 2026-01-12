library(dplyr)
library(tidyr)
#### Loading all the data sets separately ####
fifteen <- read.csv("C:/Users/khoul/OneDrive/Documents/WorldHappiness/2015.CSV")
sixteen <- read.csv("C:/Users/khoul/OneDrive/Documents/WorldHappiness/2016.CSV")
seventeen <- read.csv("C:/Users/khoul/OneDrive/Documents/WorldHappiness/2017.CSV")
eighteen <- read.csv("C:/Users/khoul/OneDrive/Documents/WorldHappiness/2018.CSV")
nineteen <- read.csv("C:/Users/khoul/OneDrive/Documents/WorldHappiness/2019.CSV")
twenty <- read.csv("C:/Users/khoul/OneDrive/Documents/WorldHappiness/2020.CSV")
twentyone <- read.csv("C:/Users/khoul/OneDrive/Documents/WorldHappiness/2021.CSV")
twentytwo <- read.csv("C:/Users/khoul/OneDrive/Documents/WorldHappiness/2022.CSV")

# Display column names of each data set
names(fifteen)
names(sixteen)
names(seventeen)
names(eighteen)
names(nineteen)
names(twenty)
names(twentyone)
names(twentytwo)

# Quick check for data sets structures
str(fifteen)
str(sixteen)
str(seventeen)
str(eighteen)
str(nineteen)
str(twenty)
str(twentyone)
str(twentytwo)

#### Unique column names verification ####
all_column_names <- unique(unlist(lapply(list(fifteen, sixteen, seventeen,
                                              eighteen, nineteen, twenty,
                                              twentyone, twentytwo), colnames)))
unique_columns_per_dataset <- list()
for (dataset_name in c("fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty", "twentyone", "twentytwo")) {
  dataset <- get(dataset_name)
  unique_columns_per_dataset[[dataset_name]] <- setdiff(colnames(dataset), all_column_names)
}
for (dataset_name in names(unique_columns_per_dataset)) {
  unique_columns <- unique_columns_per_dataset[[dataset_name]]
  if (length(unique_columns) > 0) {
    cat("Unique columns in", dataset_name, ":", paste(unique_columns, collapse = ", "), "\n")
  } else {
    cat("No unique columns in", dataset_name, "\n")
  }
}

#### Adjust column names, create columns, Homogenize the data sets
#### Renaming ####
# 1 Country column
twenty <- twenty %>%
  rename(Country = `Country.name`)

twentyone <- twentyone %>%
  rename(Country = `Country.name`)

eighteen <- eighteen %>%
  rename(Country = `Country.or.region`)

nineteen <- nineteen %>%
  rename(Country = `Country.or.region`)

# 2 Happiness Score column
fifteen <- fifteen %>%
  rename(Happiness.score = Happiness.Score)

sixteen <- sixteen %>%
  rename(Happiness.score = Happiness.Score)

seventeen <- seventeen %>%
  rename(Happiness.score = Happiness.Score)

eighteen <- eighteen %>%
  rename(Happiness.score = Score)

nineteen <- nineteen %>%
  rename(Happiness.score = Score)

twenty <- twenty %>%
  rename(Happiness.score = `Ladder.score`)

twentyone <- twentyone %>%
  rename(Happiness.score = `Ladder.score`)

twentytwo <- twentytwo %>%
  rename(Happiness.score = `Happiness.score`)

# 3 Happiness Rank column
fifteen <- fifteen %>%
  rename(Happiness.rank = Happiness.Rank)

sixteen <- sixteen %>%
  rename(Happiness.rank = Happiness.Rank)

seventeen <- seventeen %>%
  rename(Happiness.rank = Happiness.Rank)

twentytwo <- twentytwo %>%
  rename(Happiness.rank = RANK)

eighteen <- eighteen %>%
  rename(Happiness.rank = `Overall.rank`)

nineteen <- nineteen %>%
  rename(Happiness.rank = `Overall.rank`)

#### Add Rank column to data sets twenty and twenty one ####
# Step 1: Order data sets twenty and twentyone in descending order based on Happiness.score
twenty <- twenty %>% arrange(desc(Happiness.score))
twentyone <- twentyone %>% arrange(desc(Happiness.score))

# Step 2: Create a new column for ranking based on happiness scores named Happiness.rank
twenty <- twenty %>%
  mutate(Happiness.rank = row_number())

twentyone <- twentyone %>%
  mutate(Happiness.rank = row_number())

#### Add Region column in data sets 18, 19 and 22 ####
# Step 1: Merge data sets with Country and Region columns with data sets 18, 19, 22
merged_df_eighteen <- merge(eighteen, fifteen[, c("Country", "Region")], by = "Country", all.x = TRUE) # Data set 18 is on the left
merged_df_nineteen <- merge(nineteen, fifteen[, c("Country", "Region")], by = "Country", all.x = TRUE)
merged_df_twentytwo <- merge(twentytwo, fifteen[, c("Country", "Region")], by = "Country", all.x = TRUE)
merged_df_seventeen <- merge(seventeen, fifteen[, c("Country", "Region")], by = "Country", all.x = TRUE)

# Step 2: Fill in Region column in data sets 18, 19 and 22
eighteen <- merge(eighteen, merged_df_eighteen[, c("Country", "Region")], by = "Country", all.x = TRUE)
nineteen <- merge(nineteen, merged_df_nineteen[, c("Country", "Region")], by = "Country", all.x = TRUE)
twentytwo <- merge(twentytwo, merged_df_twentytwo[, c("Country", "Region")], by = "Country", all.x = TRUE)
seventeen <- merge(seventeen, merged_df_seventeen[, c("Country", "Region")], by = "Country", all.x = TRUE)

# Rename Region.y column to Region
names(eighteen)[names(eighteen) == "Region.y"] <- "Region"
names(nineteen)[names(nineteen) == "Region.y"] <- "Region"
names(twentytwo)[names(twentytwo) == "Region.y"] <- "Region"
names(seventeen)[names(seventeen) == "Region.y"] <- "Region"

# Remove the intermediate merged data frames
rm(merged_df_eighteen, merged_df_nineteen, merged_df_twentytwo, merged_df_seventeen)

# Rename Regional.indicator as Region in datasets
twenty <- twenty %>%
  rename(Region = Regional.indicator)

twentyone <- twentyone %>%
  rename(Region = Regional.indicator)


#### Renaming Freedom, Social support, GDP PC, dystopia, health, corruption,...####
# 4 All at once
fifteen <- fifteen %>%
  rename(
    Social.support = Family,
    GDP.pc = Economy..GDP.per.Capita.,
    Healthy.life.expectancy = Health..Life.Expectancy.,
    Freedom = Freedom,
    Generosity = Generosity,
    Perceptions.of.corruption = Trust..Government.Corruption.,
    Dystopia.Residual = Dystopia.Residual
  )

sixteen <- sixteen %>%
  rename(
    Social.support = Family,
    GDP.pc = Economy..GDP.per.Capita.,
    Healthy.life.expectancy = Health..Life.Expectancy.,
    Freedom = Freedom,
    Generosity = Generosity,
    Perceptions.of.corruption = Trust..Government.Corruption.,
    Dystopia.Residual = Dystopia.Residual
  )

seventeen <- seventeen %>%
  rename(
    Social.support = Family,
    GDP.pc = Economy..GDP.per.Capita.,
    Healthy.life.expectancy = Health..Life.Expectancy.,
    Freedom = Freedom,
    Generosity = Generosity,
    Perceptions.of.corruption = Trust..Government.Corruption.,
    Dystopia.Residual = Dystopia.Residual
  )

eighteen <- eighteen %>%
  rename(
    Social.support = Social.support,
    GDP.pc = GDP.per.capita,
    Healthy.life.expectancy = Healthy.life.expectancy,
    Freedom = Freedom.to.make.life.choices,
    Generosity = Generosity,
    Perceptions.of.corruption = Perceptions.of.corruption
  )

nineteen <- nineteen %>%
  rename(
    Social.support = Social.support,
    GDP.pc = GDP.per.capita,
    Healthy.life.expectancy = Healthy.life.expectancy,
    Freedom = Freedom.to.make.life.choices,
    Generosity = Generosity,
    Perceptions.of.corruption = Perceptions.of.corruption
  )

twenty <- twenty %>%
  rename(
    Social.support = Social.support,
    GDP.pc = Logged.GDP.per.capita,
    Healthy.life.expectancy = Healthy.life.expectancy,
    Freedom = Freedom.to.make.life.choices,
    Generosity = Generosity,
    Perceptions.of.corruption = Perceptions.of.corruption,
    Dystopia.Residual = Dystopia...residual
  )

twentyone <- twentyone %>%
  rename(
    Social.support = Social.support,
    GDP.pc = Logged.GDP.per.capita,
    Healthy.life.expectancy = Healthy.life.expectancy,
    Freedom = Freedom.to.make.life.choices,
    Generosity = Generosity,
    Perceptions.of.corruption = Perceptions.of.corruption,
    Dystopia.Residual = Dystopia...residual
  )

twentytwo <- twentytwo %>%
  rename(
    Social.support = Explained.by..Social.support,
    GDP.pc = Explained.by..GDP.per.capita,
    Healthy.life.expectancy = Explained.by..Healthy.life.expectancy,
    Freedom = Explained.by..Freedom.to.make.life.choices,
    Generosity = Explained.by..Generosity,
    Perceptions.of.corruption = Explained.by..Perceptions.of.corruption,
    Dystopia.Residual = Dystopia..1.83....residual
  )












#### Explained By variables, adaptation, creation and computing ####
# they somehow only exist in data sets 20 and 21
# the log thing is a scam
twenty <- twenty %>%
  rename(EB.GDP.pc = "Explained.by..Log.GDP.per.capita",
         EB.Social.support = "Explained.by..Social.support",
         EB.Healthy.life.expectancy = "Explained.by..Healthy.life.expectancy",
         EB.Freedom = "Explained.by..Freedom.to.make.life.choices",
         EB.Generosity = "Explained.by..Generosity",
         EB.Perceptions.of.corruption = "Explained.by..Perceptions.of.corruption")

twentyone <- twentyone %>%
  rename(EB.GDP.pc = "Explained.by..Log.GDP.per.capita",
         EB.Social.support = "Explained.by..Social.support",
         EB.Healthy.life.expectancy = "Explained.by..Healthy.life.expectancy",
         EB.Freedom = "Explained.by..Freedom.to.make.life.choices",
         EB.Generosity = "Explained.by..Generosity",
         EB.Perceptions.of.corruption = "Explained.by..Perceptions.of.corruption")


# test for data set 15
# this is why we can't use log(GDP.pc) to calculate our coefficients
# any(is.na(fifteen$GDP.pc))
# any(fifteen$GDP.pc <= 0)
# fifteen$GDP.pc <- ifelse(fifteen$GDP.pc <= 0, 0.0001, fifteen$GDP.pc)
# applying the line right before this one will result in NAs when running model15

# 15
model15 <- lm(Happiness.score ~ GDP.pc + Social.support + Healthy.life.expectancy + Freedom + Generosity + Perceptions.of.corruption, data = fifteen)

# Extract coefficients
coefficients <- coef(model15)

# Compute "explained by" variables
fifteen <- fifteen %>%
  mutate(
    EB.GDP.pc = coefficients["GDP.pc"] * GDP.pc,
    EB.Social.support = coefficients["Social.support"] * Social.support,
    EB.Healthy.life.expectancy = coefficients["Healthy.life.expectancy"] * Healthy.life.expectancy,
    EB.Freedom = coefficients["Freedom"] * Freedom,
    EB.Generosity = coefficients["Generosity"] * Generosity,
    EB.Perceptions.of.corruption = coefficients["Perceptions.of.corruption"] * Perceptions.of.corruption
  )

# Application on the rest
# 16
model16 <- lm(Happiness.score ~ GDP.pc + Social.support + Healthy.life.expectancy + Freedom + Generosity + Perceptions.of.corruption, data = sixteen)

# Extract coefficients
coefficients <- coef(model16)

# Compute "explained by" variables
sixteen <- sixteen %>%
  mutate(
    EB.GDP.pc = coefficients["GDP.pc"] * GDP.pc,
    EB.Social.support = coefficients["Social.support"] * Social.support,
    EB.Healthy.life.expectancy = coefficients["Healthy.life.expectancy"] * Healthy.life.expectancy,
    EB.Freedom = coefficients["Freedom"] * Freedom,
    EB.Generosity = coefficients["Generosity"] * Generosity,
    EB.Perceptions.of.corruption = coefficients["Perceptions.of.corruption"] * Perceptions.of.corruption
  )

# 17
model17 <- lm(Happiness.score ~ GDP.pc + Social.support + Healthy.life.expectancy + Freedom + Generosity + Perceptions.of.corruption, data = seventeen)

# Extract coefficients
coefficients <- coef(model17)

# Compute "explained by" variables
seventeen <- seventeen %>%
  mutate(
    EB.GDP.pc = coefficients["GDP.pc"] * GDP.pc,
    EB.Social.support = coefficients["Social.support"] * Social.support,
    EB.Healthy.life.expectancy = coefficients["Healthy.life.expectancy"] * Healthy.life.expectancy,
    EB.Freedom = coefficients["Freedom"] * Freedom,
    EB.Generosity = coefficients["Generosity"] * Generosity,
    EB.Perceptions.of.corruption = coefficients["Perceptions.of.corruption"] * Perceptions.of.corruption
  )

# 18
# Extra step before model18, is to convert perceptions of corruption to numerical values
eighteen$Perceptions.of.corruption <- as.numeric(gsub(",", ".", eighteen$Perceptions.of.corruption))

model18 <- lm(Happiness.score ~ GDP.pc + Social.support + Healthy.life.expectancy + Freedom + Generosity + Perceptions.of.corruption, data = eighteen)

# Extract coefficients
coefficients <- coef(model18)

# Compute "explained by" variables
eighteen <- eighteen %>%
  mutate(
    EB.GDP.pc = coefficients["GDP.pc"] * GDP.pc,
    EB.Social.support = coefficients["Social.support"] * Social.support,
    EB.Healthy.life.expectancy = coefficients["Healthy.life.expectancy"] * Healthy.life.expectancy,
    EB.Freedom = coefficients["Freedom"] * Freedom,
    EB.Generosity = coefficients["Generosity"] * Generosity,
    EB.Perceptions.of.corruption = coefficients["Perceptions.of.corruption"] * Perceptions.of.corruption
  )

# 19
model19 <- lm(Happiness.score ~ GDP.pc + Social.support + Healthy.life.expectancy + Freedom + Generosity + Perceptions.of.corruption, data = nineteen)

# Extract coefficients
coefficients <- coef(model19)

# Compute "explained by" variables
nineteen <- nineteen %>%
  mutate(
    EB.GDP.pc = coefficients["GDP.pc"] * GDP.pc,
    EB.Social.support = coefficients["Social.support"] * Social.support,
    EB.Healthy.life.expectancy = coefficients["Healthy.life.expectancy"] * Healthy.life.expectancy,
    EB.Freedom = coefficients["Freedom"] * Freedom,
    EB.Generosity = coefficients["Generosity"] * Generosity,
    EB.Perceptions.of.corruption = coefficients["Perceptions.of.corruption"] * Perceptions.of.corruption
  )

# 22
# Extra step before model22, is to convert all to numerical values
twentytwo$Happiness.score <- as.numeric(gsub(",", ".", twentytwo$Happiness.score))
twentytwo$Whisker.high <- as.numeric(gsub(",", ".", twentytwo$Whisker.high))
twentytwo$Whisker.low <- as.numeric(gsub(",", ".", twentytwo$Whisker.low))
twentytwo$Dystopia.Residual <- as.numeric(gsub(",", ".", twentytwo$Dystopia.Residual))
twentytwo$GDP.pc <- as.numeric(gsub(",", ".", twentytwo$GDP.pc))
twentytwo$Social.support <- as.numeric(gsub(",", ".", twentytwo$Social.support))
twentytwo$Healthy.life.expectancy <- as.numeric(gsub(",", ".", twentytwo$Healthy.life.expectancy))
twentytwo$Freedom <- as.numeric(gsub(",", ".", twentytwo$Freedom))
twentytwo$Generosity <- as.numeric(gsub(",", ".", twentytwo$Generosity))
twentytwo$Perceptions.of.corruption <- as.numeric(gsub(",", ".", twentytwo$Perceptions.of.corruption))

# Now we model
model22 <- lm(Happiness.score ~ GDP.pc + Social.support + Healthy.life.expectancy + Freedom + Generosity + Perceptions.of.corruption, data = twentytwo)

# Extract coefficients
coefficients <- coef(model22)

# Compute "explained by" variables
twentytwo <- twentytwo %>%
  mutate(
    EB.GDP.pc = coefficients["GDP.pc"] * GDP.pc,
    EB.Social.support = coefficients["Social.support"] * Social.support,
    EB.Healthy.life.expectancy = coefficients["Healthy.life.expectancy"] * Healthy.life.expectancy,
    EB.Freedom = coefficients["Freedom"] * Freedom,
    EB.Generosity = coefficients["Generosity"] * Generosity,
    EB.Perceptions.of.corruption = coefficients["Perceptions.of.corruption"] * Perceptions.of.corruption
  )

#### Isolating the few variables left that are not present everywhere ####

########### 15
standard_error15 <- data.frame(
  Country = fifteen$Country,
  Standard.Error = fifteen$Standard.Error,
  Dystopia.Residual = fifteen$Dystopia.Residual
)
fifteen <- subset(fifteen, select = -c(Standard.Error, Dystopia.Residual))

########### 16 
remnants16 <- data.frame(
  Country = sixteen$Country,
  Lower.Confidence.Interval = sixteen$Lower.Confidence.Interval,
  Upper.Confidence.Interval = sixteen$Upper.Confidence.Interval,
  Dystopia.Residual = sixteen$Dystopia.Residual
)
sixteen <- subset(sixteen, select = -c(Lower.Confidence.Interval,
                                       Upper.Confidence.Interval,
                                       Dystopia.Residual))

############# 17 

remnants17 <- data.frame(
  Country = seventeen$Country,
  Whisker.high = seventeen$Whisker.high,
  Whisker.low = seventeen$Whisker.low,
  Dystopia.Residual = seventeen$Dystopia.Residual
)
seventeen <- subset(seventeen, select = -c(Whisker.high, Whisker.low, Dystopia.Residual))

############# 20

remnants20 <- data.frame(
  Country = twenty$Country,
  upperwhisker = twenty$upperwhisker,
  lowerwhisker = twenty$lowerwhisker,
  Standard.error.of.ladder.score = twenty$Standard.error.of.ladder.score,
  Ladder.score.in.Dystopia = twenty$Ladder.score.in.Dystopia,
  Dystopia.Residual = twenty$Dystopia.Residual
)
twenty <- subset(twenty, select = -c(upperwhisker, lowerwhisker,
                                     Standard.error.of.ladder.score,
                                     Ladder.score.in.Dystopia, Dystopia.Residual))

########### 21

remnants21 <- data.frame(
  Country = twentyone$Country,
  upperwhisker = twentyone$upperwhisker,
  lowerwhisker = twentyone$lowerwhisker,
  Standard.error.of.ladder.score = twentyone$Standard.error.of.ladder.score,
  Ladder.score.in.Dystopia = twentyone$Ladder.score.in.Dystopia,
  Dystopia.Residual = twentyone$Dystopia.Residual
)
twentyone <- subset(twentyone, select = -c(upperwhisker, lowerwhisker,
                                           Standard.error.of.ladder.score,
                                           Ladder.score.in.Dystopia, Dystopia.Residual))

########### 22

remnants22 <- data.frame(
  Country = twentytwo$Country,
  Whisker.high = twentytwo$Whisker.high,
  Whisker.low = twentytwo$Whisker.low, Dystopia.Residual = twentytwo$Dystopia.Residual
)
twentytwo <- subset(twentytwo, select = -c(Whisker.high, Whisker.low,
                                           Dystopia.Residual))




#### final column names verification ####
# List of variable names for each dataset
var_names_fifteen <- names(fifteen)
var_names_sixteen <- names(sixteen)
var_names_seventeen <- names(seventeen)
var_names_eighteen <- names(eighteen)
var_names_nineteen <- names(nineteen)
var_names_twenty <- names(twenty)
var_names_twentyone <- names(twentyone)
var_names_twentytwo <- names(twentytwo)

# Find the set differences between variable names
setdiff_all <- list(
  fifteen_sixteen = setdiff(var_names_fifteen, var_names_sixteen),
  sixteen_seventeen = setdiff(var_names_sixteen, var_names_seventeen),
  seventeen_eighteen = setdiff(var_names_seventeen, var_names_eighteen),
  eighteen_nineteen = setdiff(var_names_eighteen, var_names_nineteen),
  nineteen_twenty = setdiff(var_names_nineteen, var_names_twenty),
  twenty_twentyone = setdiff(var_names_twenty, var_names_twentyone),
  twentyone_twentytwo = setdiff(var_names_twentyone, var_names_twentytwo)
)

# Print set differences
print(setdiff_all)







#### Reordering data sets (columns order) and dernieres retouches ####
sixteen <- sixteen[, names(fifteen)]
seventeen <- seventeen[, names(fifteen)]
eighteen <- eighteen[, names(fifteen)]
nineteen <- nineteen[, names(fifteen)]
twenty <- twenty[, names(fifteen)]
twentyone <- twentyone[, names(fifteen)]
twentytwo <- twentytwo[, names(fifteen)]

# Add a Year column to each dataset with the corresponding year value
fifteen <- fifteen %>% mutate(Year = 2015)
sixteen <- sixteen %>% mutate(Year = 2016)
seventeen <- seventeen %>% mutate(Year = 2017)
eighteen <- eighteen %>% mutate(Year = 2018)
nineteen <- nineteen %>% mutate(Year = 2019)
twenty <- twenty %>% mutate(Year = 2020)
twentyone <- twentyone %>% mutate(Year = 2021)
twentytwo <- twentytwo %>% mutate(Year = 2022)

# Reorder the columns of each dataset
fifteen <- fifteen %>% select(Year, everything())
sixteen <- sixteen %>% select(Year, everything())
seventeen <- seventeen %>% select(Year, everything())
eighteen <- eighteen %>% select(Year, everything())
nineteen <- nineteen %>% select(Year, everything())
twenty <- twenty %>% select(Year, everything())
twentyone <- twentyone %>% select(Year, everything())
twentytwo <- twentytwo %>% select(Year, everything())

# Quick check: null values and NAs

# NAs
any(is.na(fifteen))
any(is.na(sixteen))
any(is.na(seventeen))
any(is.na(eighteen))
any(is.na(nineteen))
any(is.na(twenty))
any(is.na(twentyone))
any(is.na(twentytwo))

# Nulls
any(fifteen <= 0)
any(sixteen <= 0)
any(seventeen <= 0)
any(eighteen <= 0)
any(nineteen <= 0)
any(twenty <= 0)
any(twentyone <= 0)
any(twentytwo <= 0)

# reordering the messy data sets (line order) based on happiness rank/score
seventeen <- seventeen[order(seventeen$Happiness.rank), ]
eighteen <- eighteen[order(eighteen$Happiness.rank), ]
nineteen <- nineteen[order(nineteen$Happiness.rank), ]
twentytwo <- twentytwo[order(twentytwo$Happiness.rank), ]

#### Creation of the master data set ####
Master <- rbind(fifteen, sixteen, seventeen, eighteen, nineteen, twenty, twentyone, twentytwo)
str(Master)

Master <- Master %>%
  mutate(GDP.pc = ifelse(Year %in% c(2020, 2021), GDP.pc * 0.1, GDP.pc),
         Healthy.life.expectancy = ifelse(Year %in% c(2020, 2021), Healthy.life.expectancy * 0.1, Healthy.life.expectancy))

Master <- Master %>%
  mutate(Healthy.life.expectancy = ifelse(Year %in% c(2020, 2021), Healthy.life.expectancy * 0.1, Healthy.life.expectancy))

write.csv(Master, "master.0.csv", row.names = FALSE)
