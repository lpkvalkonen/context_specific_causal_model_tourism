#-------------------------------------------------------------------------------
### Load packages
#-------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(xtable)

#-------------------------------------------------------------------------------
### Load data
#-------------------------------------------------------------------------------

# Data downloaded on 11.8.2025
# https://www.avoindata.fi/data/dataset/visit-finland-matkailijamittari
data <- read.csv(
  "matkailijamittari_data_final.csv",
  header = TRUE,
  sep = ",",
  fill = NA
)

#-------------------------------------------------------------------------------
### Data preparations and selections
#-------------------------------------------------------------------------------

# Paste m before PkKunta variable for factor coding purposes
data$PkKunta <- paste("m", data$PkKunta, sep = "")
data$Mk1Kunta <- paste("m", data$Mk1Kunta, sep = "")
data$Mk2Kunta <- paste("m", data$Mk2Kunta, sep = "")
data$Mk3Kunta <- paste("m", data$Mk3Kunta, sep = "")

# Mutate variable types
data <- mutate(
  data,
  Kuukausi = as.factor(Kuukausi),
  Neljannes = as.factor(Neljannes),
  VkonPv = as.factor(VkonPv),
  RajaPiste = as.factor(RajaPiste),
  Hsatama = as.factor(Hsatama),
  AsMaa = as.factor(AsMaa),
  Alue = as.factor(Alue),
  Kansal = as.factor(Kansal),
  Sukup =  as.factor(Sukup),
  Ikaryhma = as.factor(Ikaryhma),
  Kulkuvaline = as.factor(Kulkuvaline),
  MatTyyp = as.factor(MatTyyp),
  MatTark = as.factor(MatTark),
  MuutMaat = as.factor(MuutMaat),
  PkKunta = as.factor(PkKunta),
  PkMaakunta = as.factor(PkMaakunta),
  PkSuuralue = as.factor(PkSuuralue),
  PkRyhma = as.factor(PkRyhma),
  Mk1Kunta = as.factor(Mk1Kunta),
  Mk1Maakunta = as.factor(Mk1Maakunta),
  Mk1Suuralue = as.factor(Mk1Suuralue),
  Mk1Ryhma = as.factor(Mk1Ryhma),
  Mk2Kunta = as.factor(Mk2Kunta),
  Mk2Maakunta = as.factor(Mk2Maakunta),
  Mk2Suuralue = as.factor(Mk2Suuralue),
  Mk2Ryhma = as.factor(Mk2Ryhma),
  Mk3Kunta = as.factor(Mk3Kunta),
  Mk3Maakunta = as.factor(Mk3Maakunta),
  Mk3Suuralue = as.factor(Mk3Suuralue),
  Mk3Ryhma = as.factor(Mk3Ryhma),
  Majoitus = as.factor(Majoitus),
  Pake = as.factor(Pake),
  PakeA = as.factor(PakeA),
  PakePL = as.factor(PakePL),
  PakeMPS = as.factor(PakeMPS),
  PakePMM = as.factor(PakePMM),
  SegLuonto = as.factor(SegLuonto),
  SegUrhei = as.factor(SegUrhei),
  SegHyvin = as.factor(SegHyvin),
  SegKult = as.factor(SegKult),
  SegKaupu = as.factor(SegKaupu),
  SegTapah = as.factor(SegTapah),
  SegOstos = as.factor(SegOstos),
  SegKierto = as.factor(SegKierto),
  SegEiMit = as.factor(SegEiMit),
  Yli50kmSuom = as.factor(Yli50kmSuom),
  Yli50Auto = as.factor(Yli50Auto),
  Yli50Juna = as.factor(Yli50Juna),
  Yli50Lento = as.factor(Yli50Lento),
  Yli50Linja = as.factor(Yli50Linja),
  Yli50Laiva = as.factor(Yli50Laiva),
  Yli50Taksi = as.factor(Yli50Taksi),
  Seura = as.factor(Seura),
  SeuraAlle7v = as.factor(SeuraAlle7v),
  Seura8_15v = as.factor(Seura8_15v)
)

# Select variables for further processing
data <- cbind(
  data %>% dplyr::select(
    EURYht,
    Neljannes,
    Sukup,
    Ikaryhma,
    KestoYo,
    Mk1Yot,
    Mk2Yot,
    Mk3Yot,
    Kulkuvaline,
    MatTyyp,
    MatTark,
    PkKunta,
    Majoitus,
    Kulkuvaline,
    SegLuonto,
    SegUrhei,
    SegHyvin,
    SegKult,
    SegKaupu,
    SegTapah,
    SegOstos,
    SegKierto,
    Yli50kmSuom,
    EnsiVarausKk,
    AsMaa,
    Seura,
    PkYot,
    Vuosi,
    Kuukausi,
    PkSuuralue,
    Mk1Suuralue,
    Mk2Suuralue,
    Mk3Suuralue,
    Mk1Kunta,
    Mk2Kunta,
    Mk3Kunta,
    PainoKk, 
    PainoQ
  )
)

str(data)

#-------------------------------------------------------------------------------
### Missing data handling
#-------------------------------------------------------------------------------

# Fill missing seasons from month information
data <- data %>%
  mutate(Neljannes = ifelse(Kuukausi == 3, 1, Neljannes))
data$Neljannes <- as.factor(data$Neljannes)

# Code missing data as NA
data$PkKunta[data$PkKunta == "m"] <- NA
data$PkKunta[data$PkKunta == "mXX"] <- NA
data$PkKunta <- droplevels(data$PkKunta)

#-------------------------------------------------------------------------------
# Indicator variables
#-------------------------------------------------------------------------------

# Code missing data as NA
data$Mk1Kunta[data$Mk1Kunta == "m"] <- NA
data$Mk1Kunta[data$Mk1Kunta == "mXX"] <- NA
data$Mk2Kunta[data$Mk2Kunta == "m"] <- NA
data$Mk2Kunta[data$Mk2Kunta == "mXX"] <- NA
data$Mk3Kunta[data$Mk3Kunta == "m"] <- NA
data$Mk3Kunta[data$Mk3Kunta == "mXX"] <- NA

# If no other visits -> MkYot = 0
data$Mk1Yot <- ifelse(is.na(data$Mk1Kunta), 0, data$Mk1Yot)
data$Mk2Yot <- ifelse(is.na(data$Mk2Kunta), 0, data$Mk2Yot)
data$Mk3Yot <- ifelse(is.na(data$Mk3Kunta), 0, data$Mk3Yot)

#-------------------------------------------------------------------------------
### Filter data
#-------------------------------------------------------------------------------

# Consider trips from 1 to 15 overnight stays
data <- data %>% filter(KestoYo > 0 & KestoYo <= 15)

# Consider trips where EURYht > 0
data <- data %>% filter(EURYht > 0)

# Total overnight stays in another destinations
data$overnights_secondary <- data$Mk1Yot + data$Mk2Yot + data$Mk3Yot

#-------------------------------------------------------------------------------
### Leave unnecessary variables out
#-------------------------------------------------------------------------------

data <- data %>% dplyr::select(-c("Mk1Yot","Mk2Yot","Mk3Yot","PkYot",
                                  "MatTyyp","Kuukausi","Vuosi","Mk1Suuralue",
                                  "Mk2Suuralue","Mk3Suuralue","Mk1Kunta","Mk2Kunta",
                                  "Mk3Kunta"))

#-------------------------------------------------------------------------------
### Change more describing names for variables and factor levels
#-------------------------------------------------------------------------------

levels(data$PkSuuralue) <- c(
  "Helsinki metropolitan area",
  "Coast and archipelago",
  "Lakeland",
  "Lapland"
)

# Add another level into PkSuuralue: Helsinki
data$PkSuuralue <- factor(
  data$PkSuuralue,
  levels = c("Helsinki", levels(data$PkSuuralue))
)

# Add Helsinki level
data$PkSuuralue[data$PkKunta=="m091"] <- "Helsinki"

# Remove PkKunta
data <- data %>% select(-PkKunta)

# Rename
data <- data %>%
  rename(Euros_total = EURYht ,
         Euros = EURYht,
         Quarter = Neljannes,
         Gender = Sukup,
         Age_group = Ikaryhma,
         Length_of_stay = KestoYo,
         Mode_of_transportation = Kulkuvaline,
         Purpose_of_the_trip = MatTark,
         Main_destination = PkSuuralue,
         Accommodation = Majoitus,
         Experienced_nature = SegLuonto,
         Experienced_sports = SegUrhei,
         Experienced_wellbeing = SegHyvin,
         Experienced_culture = SegKult,
         Experienced_city_life = SegKaupu,
         Experienced_events = SegTapah,
         Experienced_shopping = SegOstos,
         Experienced_road_trip = SegKierto,
         Over_50km_trips = Yli50kmSuom,
         First_reservation = EnsiVarausKk,
         Country_of_residence = AsMaa,
         Travel_group = Seura,
         Overnight_stays_in_secondary_destinations = overnights_secondary
         )

data$Gender <- factor(
  data$Gender,
  levels = c("1", "2", "3", "4"),
  labels = c(
    "Male",
    "Female",
    "Other",
    "Dont want to say"
  )
)

# Only two levels
data$Gender <- factor(
  ifelse(data$Gender == "Male", "Male", "Other than male"),
  levels = c("Male", "Other than male")
)

data$Age_group <- factor(
  data$Age_group,
  levels = c("1", "2", "3", "4"),
  labels = c(
    "15-24 years",
    "25-44 years",
    "45-64 years",
    "Minimum 65 years"
  )
)

data$Mode_of_transportation <- factor(
  data$Mode_of_transportation,
  levels = c("1", "2"),
  labels = c(
    "Ferry",
    "Airplane"
  )
)

data$Purpose_of_the_trip <- factor(
  data$Purpose_of_the_trip,
  levels = c("1", "2", "3", "4","5","6","7","9"),
  labels = c(
    "Vacation, leisure, or recreation",
    "Meeting friends or relatives",
    "Study",
    "Some other purpose?",
    "Meeting or work trip in the service of a non-Finnish employer",
    "Conference or congress or fair",
    "Work performed in Finland for a Finnish employer",
    "Some other work-related reason"
  )
)

data$Accommodation <- factor(
  data$Accommodation,
  levels = c("1", "2", "3", "4","5","6","7","8","9"),
  labels = c(
    "Hotel or hostel",
    "Campsite (for a fee, e g  tent, mobile home, caravan, cabin)",
    "Rental cottage or apartment (rented privately or from an intermediary, e g  Airbnb or booking com)",
    "With friends or relatives (incl  couchsurfing)",
    "Own apartment, cottage or holiday timeshare",
    "Housing provided by an employer",
    "Other accommodation",
    "Didnt stay overnight in Finland",
    "I can´t say"
  )
)

data$Experienced_nature <- factor(
  data$Experienced_nature,
  levels = c("0","1"),
  labels = c(
    "No",
    "Yes"
  )
)

data$Experienced_wellbeing <- factor(
  data$Experienced_wellbeing,
  levels = c("0","1"),
  labels = c(
    "No",
    "Yes"
  )
)

data$Experienced_culture <- factor(
  data$Experienced_culture,
  levels = c("0","1"),
  labels = c(
    "No",
    "Yes"
  )
)

data$Experienced_city_life <- factor(
  data$Experienced_city_life,
  levels = c("0","1"),
  labels = c(
    "No",
    "Yes"
  )
)

data$Experienced_events <- factor(
  data$Experienced_events,
  levels = c("0","1"),
  labels = c(
    "No",
    "Yes"
  )
)

data$Experienced_road_trip <- factor(
  data$Experienced_road_trip,
  levels = c("0","1"),
  labels = c(
    "No",
    "Yes"
  )
)

data$Experienced_sports <- factor(
  data$Experienced_sports,
  levels = c("0","1"),
  labels = c(
    "No",
    "Yes"
  )
)

data$Experienced_shopping <- factor(
  data$Experienced_shopping,
  levels = c("0","1"),
  labels = c(
    "No",
    "Yes"
  )
)

data$Over_50km_trips <- factor(
  data$Over_50km_trips,
  levels = c("1","2"),
  labels = c(
    "Yes",
    "No"
  )
)

data$Travel_group <- factor(
  data$Travel_group,
  levels = c("1","2","3","4"),
  labels = c(
    "I travel alone",
    "I travel with my spouse/significant other",
    "I travel with my family, relatives or friends",
    "other"
  )
)

#-------------------------------------------------------------------------------
# Separate datasets
#-------------------------------------------------------------------------------

### M=0 (personal trips)
data_m0 <- data %>% dplyr::filter(Purpose_of_the_trip %in% c("Vacation, leisure, or recreation",
                                                             "Some other purpose?"))
# Remove extra factor levels for all variables
data_m0[sapply(data_m0, is.factor)] <- lapply(data_m0[sapply(data_m0, is.factor)], droplevels)

### M=1 (work-related trips)
data_m1 <- data %>% 
  dplyr::filter(Purpose_of_the_trip %in% 
                  c("Study","Meeting or work trip in the service of a non-Finnish employer",
                    "Conference or congress or fair",
                    "Work performed in Finland for a Finnish employer",
                    "Some other work-related reason"))
# Remove extra factor levels
data_m1[sapply(data_m1, is.factor)] <- lapply(data_m1[sapply(data_m1, is.factor)], droplevels)

#-------------------------------------------------------------------------------
# Modify variables
#-------------------------------------------------------------------------------

# Accommodation
levels(data_m0$Accommodation)
data_m0$Accommodation <- factor(data_m0$Accommodation,
                   levels = c(
                     "Hotel or hostel",
                     "Campsite (for a fee, e g  tent, mobile home, caravan, cabin)",
                     "Rental cottage or apartment (rented privately or from an intermediary, e g  Airbnb or booking com)",
                     "With friends or relatives (incl  couchsurfing)",
                     "Own apartment, cottage or holiday timeshare",
                     "Housing provided by an employer",
                     "Other accommodation",
                     "Didnt stay overnight in Finland",
                     "I can´t say"
                   ),
                   labels = c("Hotel or hostel", 
                              "Other", 
                              "Rental cottage or apartment (rented privately or from an intermediary, e g  Airbnb or booking com)", 
                              "With friends or relatives (incl  couchsurfing)",
                              "Other","Other","Other","Other","Other")
)
data_m0$Accommodation <- factor(data_m0$Accommodation,
                                levels = c(
                                  "Hotel or hostel",
                                  "Rental cottage or apartment (rented privately or from an intermediary, e g  Airbnb or booking com)",
                                  "With friends or relatives (incl  couchsurfing)",
                                  "Other"
                                )) 


levels(data_m1$Accommodation)
data_m1$Accommodation <- factor(data_m1$Accommodation,
                                levels = c(
                                  "Hotel or hostel",
                                  "Campsite (for a fee, e g  tent, mobile home, caravan, cabin)",
                                  "Rental cottage or apartment (rented privately or from an intermediary, e g  Airbnb or booking com)",
                                  "With friends or relatives (incl  couchsurfing)",
                                  "Own apartment, cottage or holiday timeshare",
                                  "Housing provided by an employer",
                                  "Other accommodation",
                                  "Didnt stay overnight in Finland",
                                  "I can´t say"
                                ),
                                labels = c("Hotel or hostel", 
                                           "Other", 
                                           "Rental cottage or apartment (rented privately or from an intermediary, e g  Airbnb or booking com)", 
                                           "Other",
                                           "Other",
                                           "Housing provided by an employer",
                                           "Other","Other","Other")
)

data_m1$Accommodation <- factor(data_m1$Accommodation,
                                levels = c(
                                  "Hotel or hostel",
                                  "Rental cottage or apartment (rented privately or from an intermediary, e g  Airbnb or booking com)",
                                  "Housing provided by an employer",
                                  "Other"
                                )) 

#-------------------------------------------------------------------------------
### Save the cleaned datas
#-------------------------------------------------------------------------------

save(data_m0, data_m1, file = "data.RData")
