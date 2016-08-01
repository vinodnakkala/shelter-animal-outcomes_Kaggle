## Would like to Thank Megan Risdal and tamlin For sharing the initial scripts 
## which helped me to develop on top of that
## Including the packages Required
library(dplyr) # Data manipulation for data frames
library(dummies) #Useful for Handling categorical variables
library(ggplot2) # visualization using plots
library(ggthemes) # visualization
library(dplyr) # data manipulation
library(lubridate) # extracting info from dates
library(rpart) # rpart for imputation
library(randomForest) # classification algorithm 
library(stringr)

#detach("package:plyr", unload=TRUE)

#Set working dorectory to the location where CSVs are located
setwd("C:/Kaggle/AnimalChallenge/DataSet")

# Read the data into data frames
train <- read.csv("train.csv")
test <- read.csv("test.csv")
explore <- read.csv('train.csv', stringsAsFactors = F) # Reading the training data as strings for visualization

# Explore the data and see how it looks like in a snapshot
View(train)
dim(train)
#nrow(train)
#ncol(train)
#count(train,OutcomeType)
table(train$OutcomeType)
#summary(train$AgeuponOutcome)
summary(train$AnimalType)
head(train,n=3)
#head(train,n=-26725)
#tail(train,n=4)
names(train)
str(train)  # Summary of all variables and their types
levels(train$OutcomeType)  # Levels of Target Variable
summary(train$OutcomeType) # Distribution of Target Variable

## Visualize the how AnimalType is impacting outcomeType
outcomes <- train %>% group_by(AnimalType, OutcomeType) %>% summarise (num_animals = n())
View(outcomes)
ggplot(outcomes, aes(x = AnimalType, y = num_animals, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  labs(y = 'Proportion of Animals',x = 'Animal',title = 'Outcomes: Cats & Dogs') +
  theme_few() + coord_flip()
## Visualize the how SexuponOutcome is impacting outcomeType
sexofanimal <- train %>% group_by(SexuponOutcome, OutcomeType) %>% summarise (num_animals = n())
View(sexofanimal)
ggplot(sexofanimal, aes(x = SexuponOutcome, y = num_animals, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  labs(y = 'Proportion of Animals',x = 'Sex',title = 'Outcomes: Sex of Animal') +
  theme_few() + coord_flip()

#temp <- as.factor(c("a","b","c"))
#train$fss <- ifelse((train$Breed %in% fss), 1, 0)
#breed_kind <- ddply(train, .(OutcomeType), nrow)
#names(breed_kind)[2] <- "OutcomeType_Count"
#View(breed_kind)
#train <- merge(train, breed_kind, by="OutcomeType")
#View(train)


# Remove the variables which may not be useful in predicting the outcome
train$AnimalID <- NULL
train$OutcomeSubtype <- NULL
test_ID <- test$ID
test$ID <- NULL

# See the testd data and dimensions of datasets, to make sure all the changes are done in both the datasets
View(test)
dim(train)
dim(test)

# Change DateTime to POSIXct to extract features
train$DateTime <- as.POSIXct(train$DateTime)
test$DateTime <- as.POSIXct(test$DateTime)

# Add some date/time-related variables
train$year <- year(train$DateTime)
train$month <- month(train$DateTime)
train$wday <- wday(train$DateTime)
train$hour <- hour(train$DateTime)
train$date <- date(train$DateTime)
train$mins <- minute(train$DateTime)
train$hour <- train$hour + (train$mins/60)
train$mday <- days_in_month(train$DateTime)
# Removed week and quarter as they were not improving for prediction
#train$week <- week(train$DateTime)  
#train$quarter <- quarter(train$DateTime)

test$year <- year(test$DateTime)
test$month <- month(test$DateTime)
test$wday <- wday(test$DateTime)
test$hour <- hour(test$DateTime)
test$date <- date(test$DateTime)
test$mins <- minute(test$DateTime)
test$hour <- test$hour + (test$mins/60)
test$mday <- days_in_month(test$DateTime)
# Removed week and quarter as they were not improving for prediction
#test$week <- week(test$DateTime)
#test$quarter <- quarter(test$DateTime)

# Indicate if its weekend or not as Adoption is more on weekends
train$IsWeekend <- ifelse((train$wday == 1 | train$wday == 7), 1, 0)
test$IsWeekend <- ifelse((test$wday == 1 | test$wday == 7), 1, 0)

# Divide time into bins, to improve accuracy
train$timing <- ifelse((train$hour > 8 & train$hour <= 11), 1, ifelse((train$hour > 12 & train$hour <= 14)
                ,2,ifelse((train$hour > 14 & train$hour <= 18),3,ifelse((train$hour > 18),4,5))))
train$timing <- as.factor(train$timing)
test$timing <- ifelse((test$hour > 8 & test$hour <= 11), 1, ifelse((test$hour > 12 & test$hour <= 14)
                 ,2,ifelse((test$hour > 14 & test$hour <= 18),3,ifelse((test$hour > 18),4,5))))
test$timing <- as.factor(test$timing)

train$DateTime <- as.numeric(train$DateTime)
test$DateTime <- as.numeric(test$DateTime)

## Visualize effect of timing bins using the plots
timings <- train %>% group_by(AnimalType, timing, OutcomeType) %>%summarise(num_animals = n())
ggplot(timings, aes(x = timing, y = num_animals, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  facet_wrap(~AnimalType) +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Animal',
       title = 'Outcomes by Time of Day: Cats & Dogs') +
       theme_few()
## Visualize effect of week of the day using the plots
Day <- train %>% group_by(AnimalType, wday, OutcomeType) %>%summarise(num_animals = n())
ggplot(Day, aes(x = wday, y = num_animals, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  facet_wrap(~AnimalType) +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Animal',
       title = 'Outcomes by week of Day: Cats & Dogs') +
  theme_few()

#Neutered, Spayed are grouped
intact <- train %>% group_by(AnimalType, SexuponOutcome, OutcomeType) %>%
  summarise(num_animals = n())

## Visualize effect of SexuponOutcome using the plots
ggplot(intact, aes(x = SexuponOutcome, y = num_animals, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  facet_wrap(~AnimalType) +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Animal',
       title = 'Outcomes by Neutered, Spayed and Intactness: Cats & Dogs') +
  theme_few()

# Write a function to convert age outcome to numeric age in days
convert <- function(age_outcome){
  split <- strsplit(as.character(age_outcome), split=" ")
  period <- split[[1]][2]
  if (grepl("year", period)){
    per_mod <- 365
  } else if (grepl("month", period)){ 
    per_mod <- 30
  } else if (grepl("week", period)){
    per_mod <- 7
  } else
    per_mod <- 1
  age <- as.numeric(split[[1]][1]) * per_mod
  return(age)
}

#Apply the function to convert age into days
train$AgeuponOutcome <- sapply(train$AgeuponOutcome, FUN=convert)
test$AgeuponOutcome <- sapply(test$AgeuponOutcome, FUN=convert)
train[is.na(train)] <- 0  # Fill NA with 0
test[is.na(test)] <- 0    # Fill NA with 0
class(train$AgeuponOutcome)

# Remove row with missing sex label and drop the level
dim(train)
train <- train[-which(train$SexuponOutcome == ""),]
train$SexuponOutcome <- droplevels(train$SexuponOutcome)
dim(train)
# Add varable for length of the name
train$name_len <- sapply(as.character(train$Name),nchar)
test$name_len <- sapply(as.character(test$Name),nchar)

#Remove the name, as it may not have any impact on model
train$Name <- NULL
test$Name <- NULL

#Sex of the Animal which may help to predict better

#train$Sex <- ifelse(grepl('Female', train$SexuponOutcome), 1, ifelse(grepl('Male', train$SexuponOutcome), 2,3))
#test$Sex <- ifelse(grepl('Female', test$SexuponOutcome), 1, ifelse(grepl('Male', test$SexuponOutcome), 2,3))
#train$Kind <- ifelse(grepl('Intact', train$SexuponOutcome), 1,ifelse(grepl('Spayed',train$SexuponOutcome), 2,
#                                              ifelse(grepl('Neutered', train$SexuponOutcome), 2,3)))
#test$Kind <- ifelse(grepl('Intact', test$SexuponOutcome), 1,ifelse(grepl('Spayed',test$SexuponOutcome), 2,
#                                              ifelse(grepl('Neutered', test$SexuponOutcome), 2,3)))
#train$Sex <- as.factor(train$Sex)
#test$Sex <- as.factor(test$Sex)
#train$Kind <- as.factor(train$Kind)
#test$Kind <- as.factor(test$Kind)

# View the data by separating with Cat and Dog as types
#cat <- subset(train, grepl("Cat", train$AnimalType))
#dog <- subset(train, grepl("Dog", train$AnimalType))
#View(cat)
#View(dog)
#dim(cat)
#dim(dog)


# Create indicator variables for breeds and mix
train_breeds <- as.character(train$Breed)
test_breeds <- as.character(test$Breed)
all_breeds <- unique(c(train_breeds,test_breeds))
all_breeds
breed_words <- unique(unlist(strsplit(all_breeds, c("/| Mi"))))  
# Create Dummy Variables for each breed
breed_words
for (breed in breed_words){
  train[breed] <- as.numeric(grepl(breed, train_breeds))
  test[breed] <- as.numeric(grepl(breed, test_breeds))
}
#View(train)

dim(train)
train["crosses"] <- str_count(train$Breed, pattern="/")
test["crosses"] <- str_count(test$Breed, pattern="/")
dim(train)
dim(test)
train$Breed <- sapply(train$Breed,gsub,pattern="Mix",replacement="")
test$Breed <- sapply(test$Breed,gsub,pattern="Mix",replacement="")
train$Breed <- sapply(train$Breed,gsub,pattern="/.*",replacement="")
test$Breed <- sapply(test$Breed,gsub,pattern="/.*",replacement="")
##############################################################################################################
#View(train$hour)
#fss <- as.factor(c("AmericanHairlessTerrier","AmericanLeopardHound","AppenzellerSennenhunde","Azawakh","Barbet","BassetFauvedeBretagne","BelgianLaekenois","BiewerTerrier","Bolognese","BraccoItaliano","BraqueduBourbonnais","Broholmer","CatahoulaLeopardDog","CaucasianOvcharka","CentralAsianShepherdDog","CzechoslovakianVlcak","Danish-SwedishFarmdog","DeutscherWachtelhund","DogoArgentino","DrentschePatrijshond","Drever","DutchShepherd","EstrelaMountainDog","Eurasier","FrenchSpaniel","GermanLonghairedPointer","GermanSpitz","GrandBassetGriffonVendeen","Hamiltonstovare","Hovawart","Jagdterrier","Jindo","KaiKen","KarelianBearDog","KishuKen","Kromfohrlander","LancashireHeeler","Mudi","NederlandseKooikerhondje","Norrbottenspets","PerrodePresaCanario","PeruvianIncaOrchid","PortuguesePodengo","PortuguesePointer","PortugueseSheepdog","Pumi","PyreneanMastiff","RafeirodoAlentejo","RussianToy","RussianTsvetnayaBolonka","Schapendoes","Shikoku","SlovenskyCuvac","SlovenskyKopov","Sloughi","SmallMunsterlanderPointer","SpanishMastiff","Stabyhoun","SwedishLapphund","ThaiRidgeback","Tornjak","Tosa","TransylvanianHound","TreeingTennesseeBrindle","WorkingKelpie","AustralianCattleDog","AustralianShepherd","BeardedCollie","Beauceron","BelgianMalinois","BelgianSheepdog","BelgianTervuren","Bergamasco","BergerPicard","BorderCollie","BouvierdesFlandres","Briard","CanaanDog","CardiganWelshCorgi","Collie","EntlebucherMountainDog","FinnishLapphund","GermanShepherdDog","IcelandicSheepdog","MiniatureAmericanShepherd","NorwegianBuhund","OldEnglishSheepdog","PembrokeWelshCorgi","PolishLowlandSheepdog","Puli","PyreneanShepherd","ShetlandSheepdog","SpanishWaterDog","SwedishVallhund","AfghanHound","AmericanEnglishCoonhound","AmericanFoxhound","Basenji","BassetHound","Beagle","BlackandTanCoonhound","Bloodhound","BluetickCoonhound","Borzoi","Cirnecodeletna","Dachshund","EnglishFoxhound","Greyhound","Harrier","IbizanHound","IrishWolfhound","NorwegianElkhound","Otterhound","PetitBassetGriffonVendeen","PharaohHound","Plott","PortuguesePodengoPequeno","RedboneCoonhound","RhodesianRidgeback","Saluki","ScottishDeerhound","TreeingWalkerCoonhound","Whippet","AmericanEskimoDog","BichonFrise","BostonTerrier","Bulldog","ChineseSharPei","ChowChow","CotondeTulear","Dalmatian","FinnishSpitz","FrenchBulldog","Keeshond","LhasaApso","Lowchen","NorwegianLundehund","Poodle","Schipperke","ShibaInu","TibetanSpaniel","TibetanTerrier","Xoloitzcuintli","AmericanWaterSpaniel","BoykinSpaniel","Brittany","ChesapeakeBayRetriever","ClumberSpaniel","CockerSpaniel","Curly-CoatedRetriever","EnglishCockerSpaniel","EnglishSetter","EnglishSpringerSpaniel","FieldSpaniel","Flat-CoatedRetriever","GermanShorthairedPointer","GermanWirehairedPointer","GoldenRetriever","GordonSetter","IrishRedandWhiteSetter","IrishSetter","IrishWaterSpaniel","LabradorRetriever","LagottoRomagnolo","NovaScotiaDuckTollingRetriever","Pointer","SpinoneItaliano","SussexSpaniel","Vizsla","Weimaraner","WelshSpringerSpaniel","WirehairedPointingGriffon","WirehairedVizsla","AiredaleTerrier","AmericanStaffordshireTerrier","AustralianTerrier","BedlingtonTerrier","BorderTerrier","BullTerrier","CairnTerrier","CeskyTerrier","DandieDinmontTerrier","GlenofImaalTerrier","IrishTerrier","KerryBlueTerrier","LakelandTerrier","ManchesterTerrier","MiniatureBullTerrier","MiniatureSchnauzer","NorfolkTerrier","NorwichTerrier","ParsonRussellTerrier","RatTerrier","RussellTerrier","ScottishTerrier","SealyhamTerrier","SkyeTerrier","SmoothFoxTerrier","SoftCoatedWheatenTerrier","StaffordshireBullTerrier","WelshTerrier","WestHighlandWhiteTerrier","WireFoxTerrier","Affenpinscher","BrusselsGriffon","CavalierKingCharlesSpaniel","Chihuahua","ChineseCrested","EnglishToySpaniel","Havanese","ItalianGreyhound","JapaneseChin","Maltese","MiniaturePinscher","Papillon","Pekingese","Pomeranian","Pug","ShihTzu","SilkyTerrier","ToyFoxTerrier","YorkshireTerrier","Akita","AlaskanMalamute","AnatolianShepherdDog","BerneseMountainDog","BlackRussianTerrier","Boerboel","Boxer","Bullmastiff","CaneCorso","Chinook","DobermanPinscher","DoguedeBordeaux"))
# The Foundation Stock Service (FSS) Program is a breed registry of the American Kennel Club
fss1 <- as.factor(c("American Hairless Terrier","American Leopard Hound","Appenzeller Sennenhunde","Azawakh","Barbet","Basset Fauve de Bretagne","Belgian Laekenois","Biewer Terrier","Bolognese","Bracco Italiano","Braque du Bourbonnais","Broholmer","Catahoula Leopard Dog","Caucasian Ovcharka","Central Asian Shepherd Dog","Czechoslovakian Vlcak","Danish-Swedish Farmdog","Deutscher Wachtelhund","Dogo Argentino","Drentsche Patrijshond","Drever","Dutch Shepherd","Estrela Mountain Dog","Eurasier","French Spaniel","German Longhaired Pointer","German Spitz","Grand Basset Griffon Vendeen","Hamiltonstovare","Hovawart","Jagdterrier","Jindo","Kai Ken","Karelian Bear Dog","Kishu Ken","Kromfohrlander","Lancashire Heeler","Mudi","Nederlandse Kooikerhondje","Norrbottenspets","Perro de Presa Canario","Peruvian Inca Orchid","Portuguese Podengo","Portuguese Pointer","Portuguese Sheepdog","Pumi","Pyrenean Mastiff","Rafeiro do Alentejo","Russian Toy","Russian Tsvetnaya Bolonka","Schapendoes","Shikoku","Slovensky Cuvac","Slovensky Kopov","Sloughi","Small Munsterlander Pointer","Spanish Mastiff","Stabyhoun","Swedish Lapphund","Thai Ridgeback","Tornjak","Tosa","Transylvanian Hound","Treeing Tennessee Brindle","Working Kelpie","Australian Cattle Dog","Australian Shepherd","Bearded Collie","Beauceron","Belgian Malinois","Belgian Sheepdog","Belgian Tervuren","Bergamasco","Berger Picard","Border Collie","Bouvier des Flandres","Briard","Canaan Dog","Cardigan Welsh Corgi","Collie","Entlebucher Mountain Dog","Finnish Lapphund","German Shepherd Dog","Icelandic Sheepdog","Miniature American Shepherd","Norwegian Buhund","Old English Sheepdog","Pembroke Welsh Corgi","Polish Lowland Sheepdog","Puli","Pyrenean Shepherd","Shetland Sheepdog","Spanish Water Dog","Swedish Vallhund","Afghan Hound","American English Coonhound","American Foxhound","Basenji","Basset Hound","Beagle","Black and Tan Coonhound","Bloodhound","Bluetick Coonhound","Borzoi","Cirneco de l etna","Dachshund","English Foxhound","Greyhound","Harrier","Ibizan Hound","Irish Wolfhound","Norwegian Elkhound","Otterhound","Petit Basset Griffon Vendeen","Pharaoh Hound","Plott","Portuguese Podengo Pequeno","Redbone Coonhound","Rhodesian Ridgeback","Saluki","Scottish Deerhound","Treeing Walker Coonhound","Whippet","American Eskimo Dog","Bichon Frise","Boston Terrier","Bulldog","Chinese Shar-Pei","Chow Chow","Coton de Tulear","Dalmatian","Finnish Spitz","French Bulldog","Keeshond","Lhasa Apso","Lowchen","Norwegian Lundehund","Poodle","Schipperke","Shiba Inu","Tibetan Spaniel","Tibetan Terrier","Xoloitzcuintli","American Water Spaniel","Boykin Spaniel","Brittany","Chesapeake Bay Retriever","Clumber Spaniel","Cocker Spaniel","Curly-Coated Retriever","English Cocker Spaniel","English Setter","English Springer Spaniel","Field Spaniel","Flat-Coated Retriever","German Shorthaired Pointer","German Wirehaired Pointer","Golden Retriever","Gordon Setter","Irish Red and White Setter","Irish Setter","Irish Water Spaniel","Labrador Retriever","Lagotto Romagnolo","Nova Scotia Duck Tolling Retriever","Pointer","Spinone Italiano","Sussex Spaniel","Vizsla","Weimaraner","Welsh Springer Spaniel","Wirehaired Pointing Griffon","Wirehaired Vizsla","Airedale Terrier","American Staffordshire Terrier","Australian Terrier","Bedlington Terrier","Border Terrier","Bull Terrier","Cairn Terrier","Cesky Terrier","Dandie Dinmont Terrier","Glen of Imaal Terrier","Irish Terrier","Kerry Blue Terrier","Lakeland Terrier","Manchester Terrier","Miniature Bull Terrier","Miniature Schnauzer","Norfolk Terrier","Norwich Terrier","Parson Russell Terrier","Rat Terrier","Russell Terrier","Scottish Terrier","Sealyham Terrier","Skye Terrier","Smooth Fox Terrier","Soft Coated Wheaten Terrier","Staffordshire Bull Terrier","Welsh Terrier","West Highland White Terrier","Wire Fox Terrier","Affenpinscher","Brussels Griffon","Cavalier King Charles Spaniel","Chihuahua","Chinese Crested","English Toy Spaniel","Havanese","Italian Greyhound","Japanese Chin","Maltese","Miniature Pinscher","Papillon"))
fss2 <- as.factor(c("Pekingese","Pomeranian","Pug","Shih Tzu","Silky Terrier","Toy Fox Terrier","Yorkshire Terrier","Akita","Alaskan Malamute","Anatolian Shepherd Dog","Bernese Mountain Dog","Black Russian Terrier","Boerboel","Boxer","Bullmastiff","Cane Corso","Chinook","Doberman Pinscher","Dogue de Bordeaux","German Pinscher","Giant Schnauzer","Great Dane","Great Pyrenees","Greater Swiss Mountain Dog","Komondor","Kuvasz","Leonberger","Mastiff","Neapolitan Mastiff","Newfoundland","Portuguese Water Dog","Rottweiler","Samoyed","Siberian Husky","Standard Schnauzer","Tibetan Mastiff","St. Bernard"))
train$fss <- ifelse(((train$Breed %in% fss1) | (train$Breed %in% fss2)),"1","0")
train$fss <- as.factor(train$fss)
summary(train$fss)
test$fss <- ifelse(((test$Breed %in% fss1) | (test$Breed %in% fss2)),"1","0")
test$fss <- as.factor(test$fss)
summary(test$fss)
#Apartment Breeds
apt <- as.factor(c("Bichon Frise","Bulldog","Cavalier King Charles Spaniel","Chinese Crested","French Bulldog","Greyhound","Havanese","Maltese","Pug","Shih Tzu"))
train$apt <- ifelse((train$Breed %in% apt),"1","0")
train$apt <- as.factor(train$apt)
summary(train$apt)
test$apt <- ifelse((test$Breed %in% apt),"1","0")
test$apt <- as.factor(test$apt)
summary(test$apt)
## Family dogs
familydogs <- as.factor(c("Beagle","Brussels Griffon","Bulldog","Collie","French Bulldog","Golden Retriever","Irish Setter","Labrador Retriever","Newfoundland","Pug"))
train$familydogs <- ifelse((train$Breed %in% familydogs),"1","0")
train$familydogs <- as.factor(train$familydogs)
summary(train$familydogs)
test$familydogs <- ifelse((test$Breed %in% familydogs),"1","0")
test$familydogs <- as.factor(test$familydogs)
summary(test$familydogs)
#Hairless dogs
hairless <- as.factor(c("Mexican Hairless", "American Hairless Terrier","Chinese Crested","Peruvian Inca Orchid","Xoloitzcuintli"))
train$hairless <- ifelse((train$Breed %in% hairless),"1","0")
train$hairless <- as.factor(train$hairless)
summary(train$hairless)
test$hairless <- ifelse((test$Breed %in% hairless),"1","0")
test$hairless <- as.factor(test$hairless)
summary(test$hairless)
### Hypo Dogs
hypo <- as.factor(c("Afghan Hound","American Hairless Terrier","Bedlington Terrier","Bichon Frise","Chinese Crested","Coton de Tulear","Giant Schnauzer","Irish Water Spaniel","Kerry Blue Terrier","Lagotto Romagnolo","Maltese","Peruvian Inca Orchid","Poodle","Portuguese Water Dog","Soft Coated Wheaten Terrier","Standard Schnauzer","Xoloitzcuintli"))
train$hypo <- ifelse((train$Breed %in% hypo),"1","0")
train$hypo <- as.factor(train$hypo)
summary(train$hypo)
test$hypo <- ifelse((test$Breed %in% hypo),"1","0")
test$hypo <- as.factor(test$hypo)
summary(test$hypo)
#Kids Dogs
kids <- as.factor(c("Beagle","Boxer","Bull Terrier","Bulldog","Golden Retriever","Labrador Retriever","Newfoundland","Soft Coated Wheaten Terrier","Weimaraner"))
train$kids <- ifelse((train$Breed %in% kids),"1","0")
train$kids <- as.factor(train$kids)
summary(train$kids)
test$kids <- ifelse((test$Breed %in% kids),"1","0")
test$kids <- as.factor(test$kids)
summary(test$kids)
### Smart Dogs
smart <- as.factor(c("Bloodhound","Border Collie","Doberman Pinscher","German Shepherd Dog","Golden Retriever","Labrador Retriever","Papillon","Poodle","Rottweiler","Shetland Sheepdog"))
train$smart <- ifelse((train$Breed %in% smart),"1","0")
train$smart <- as.factor(train$smart)
summary(train$smart)
test$smart <- ifelse((test$Breed %in% smart),"1","0")
test$smart <- as.factor(test$smart)
summary(test$smart)
## Dangerous Dogs
dangerous <- as.factor(c("Dalmatian","Great Dane","Presa Canario","Doberman Pinsch","Chow Chow","Alaskan Malamute","Wolf-Dog Hybrid","German Shepherd","Rottweiler","Pit Bull"))
train$dangerous <- ifelse((train$Breed %in% dangerous),"1","0")
train$dangerous <- as.factor(train$dangerous)
summary(train$dangerous)
test$dangerous <- ifelse((test$Breed %in% dangerous),"1","0")
test$dangerous <- as.factor(test$dangerous)
summary(test$dangerous)
## Type1 Dogs depending on the size
T1Large <- as.factor(c("Akita","Alaskan Malamute","Anatolian Shepherd Dog","Argentine Dogo","Beauceron","Bernese Mountain Dog","Black Russian Terrier","Bloodhound","Borzoi","Bouvier des Flandres","Briard","Bullmastiff","Cane Corso","Caucasian Ovcharka","Central Asian Shepherd Dog","Curly-Coated Retriever","Doberman Pinsch","Greyhound","Otterhounds"))
T1MediumL <- as.factor(c("Spinone Italiano","Afghan Hound","Airedale Terrier","American Foxhound","American Staffordshire Terrier","Appenzeller Sennenhunde","Belgian Sheepdog","Belgian Tervuren","Bergamasco","Black and Tan Coonhound","Bluetick Coonhound","Boxer","Bracco Italiano","Catahoula Leopard","Chesapeake Bay Retriever","Chinook","Clumber Spaniel","English Setter","Flat Coat Retriever","German Shorthaired Pointer","German Wirehaired Pointer","Golden Retrievers","Gordon Setters","Irish Red and White Setters","Irish Setters","Labrador Retriever","The Standard Poodle","Redbone Coonhound"))
T1Medium <- as.factor(c("Thai Ridgeback","Weimaraner","American English Coonhound","Australian Shepherd","Azawakh","Basset Hound","Bearded Collie","Belgian Laekenois","Belgian Malinois","Bull Terrier","Bulldog","Chinese Sharpei","Chow Chow","Collie","Dalmatian","English Springer Spaniel","Entlebucher Mountain Dog","Field Spaniel","Finnish Lapphund","Grand Basset Griffon Vendéen","Harrier","Ibizan Hounds","Irish Water Spaniels","Keeshond","Norwegian Elkhound","Nova Scotia Duck Tolling Retriever","The Pharaoh Hound","The Plott","The Pointer","Portuguese Podengo","The Saluki","Samoyed","Siberian Husky","Sloughis","Stabyhouns","Sussex Spaniel","Treeing Walker Coonhound"))
T1MediumS <- as.factor(c("Vizslas","Wirehaired Pointing Griffon","Basenji","Beagle","Cardigan Welsh Corgi","Cocker Spaniel","Dandie Dinmont Terrier","English Cocker Spaniel","Finnish Spitz","German Pinscher","Kai Kens","Kerry Blue Terriers","Miniature Bull Terrier","Norwegian Buhunds","Pembroke Welsh Corgi","Petit Bassett Griffon Vendeen","Polish Lowland Sheepdog","Portuguese Podengo (Medio)","Portuguese Water Dog","Pyrenean Shepherd","Sealyham Terrier","Shetland Sheepdog","Skye Terrier","Soft Coated Wheaten Terrier","Spanish Water Dogs","Staffordshire Bull Terrier","Standard Schnauzer","Tibetan Terrier","Treeing Tennessee Brindle","Welsh Springer Spaniel","West Highland White Terrier","Whippet","Wire Fox Terrier"))
T1Small <- as.factor(c("Affenpinscher","American Eskimo","Miniature American Eskimo","Toy American Eskimo","Australian Terrier","Bichon Frise","Bolognese","Border Terrier","Boston Terrier","Brussels Griffon","Cairn Terrier","Cavalier King Charles Spaniel","Cesky Terrier","Chihuahua","Chinese Crested","Coton de Tulear","Dachshund","English Toy Spaniel","French Bulldog","Havanese","Italian Greyhound","Japanese Chin","Lancashire Heeler","Lhasa Apso","Lowchen","Maltese","Manchester Terrier","Standard Manchester Terrier","Toy Manchester Terrier","Miniature Pinscher","Miniature Schnauzer","Norfolk Terrier","Norwich Terrier","Papillon","Parson Russell Terrier","Pekingese","Pomeranian","Poodle","Miniature Poodle","Toy Poodle","Portuguese Podengo (Pequeno)","Pug","Rat Terrier","Schipperke","Scottish Terrier","Shiba Inu","Shih Tzu","Silky Terrier","Smooth Fox Terrier","Tibetan Spaniel","Toy Fox Terrier","Yorkshire Terrier","Xoloitzcuintli"))
train$Type1<-ifelse(train$Breed %in% T1Large,"Large",ifelse(train$Breed %in% T1MediumL,"Medium Large",
            ifelse(train$Breed %in% T1Medium,"Medium",ifelse(train$Breed %in% T1MediumS,"Medium Small"
            ,ifelse(train$Breed %in% T1Small,"Small","NONE")))))
train$Type1 <- as.factor(train$Type1)
summary(train$Type1)
test$Type1<-ifelse(test$Breed %in% T1Large,"Large",ifelse(test$Breed %in% T1MediumL,"Medium Large",
            ifelse(test$Breed %in% T1Medium,"Medium",ifelse(test$Breed %in% T1MediumS,"Medium Small"
            ,ifelse(test$Breed %in% T1Small,"Small","NONE")))))
test$Type1 <- as.factor(test$Type1)
summary(test$Type1)
## Type2 Dogs depending on the size
T2Large <- as.factor(c("Great Dane","Mastiff","Neapolitan Mastiff","Bullmastiff","Newfoundland","Dogue de Bordeaux","Cane Corso","Great Pyrenees","Bernese Mountain Dog","Tibetan Mastiff","Black Russian Terrier","Leonberger","Irish Wolfhound","Scottish Deerhound"))
T2Medium <- as.factor(c("Brittany","Bulldog","Cocker Spaniel","English Springer Spaniel","Pembroke Welsh Corgi","Shetland Sheepdog","Whippet"))
T2Small <- as.factor(c("Brussels Griffon","Cavalier King Charles Spaniel","Chihuahua","Chinese Crested","Dachshund","English Toy Spaniel","Havanese","Italian Greyhound","Japanese Chin","Maltese","Miniature Pinscher","Norfolk Terrier","Norwich Terrier","Papillon","Pekingese","Pomeranian","Pug","Schipperke","Shih Tzu","Silky Terrier","Toy Fox Terrier","Manchester Terrier","Poodle","Yorkshire Terrier"))
train$Type2<-ifelse(train$Breed %in% T2Large,"Large",ifelse(train$Breed %in% T2Medium,"Medium",ifelse(train$Breed %in% T2Small,"small","NONE")))
train$Type2 <- as.factor(train$Type2)
summary(train$Type2)
test$Type2<-ifelse(test$Breed %in% T2Large,"Large",ifelse(test$Breed %in% T2Medium,"Medium",ifelse(test$Breed %in% T2Small,"small","NONE")))
test$Type2 <- as.factor(test$Type2)
summary(test$Type2)
#####################################################################################################################
train$Breed <- NULL
test$Breed <- NULL

# Create indicator vars for color
train_colors <- as.character(train$Color)
test_colors <- as.character(test$Color)
all_colors <- unique(c(train_colors,test_colors))
color_words <- unique(unlist(strsplit(all_colors, c("/")))) 
color_words
for (color in color_words){
  train[color] <- as.numeric(grepl(color, train_colors))
  test[color] <- as.numeric(grepl(color, test_colors))
}

train["color_count"] <- str_count(train$Color, pattern="/")+1
test["color_count"] <- str_count(test$Color, pattern="/")+1

train$Color <- NULL
test$Color <- NULL
#View(train$color_count)
targets <- train$OutcomeType
train$OutcomeType <- NULL
#targets
#install.packages("xgboost")
library(xgboost)
ncol(train)
###############################################################################

train[is.na(train)] <- "0"  # Fill NA with 0
test[is.na(test)] <- "0"
train$DateTime <- NULL
test$DateTime <- NULL

#train_sex <- as.character(train$SexuponOutcome)
#test_sex <- as.character(test$SexuponOutcome)
#all_sex <- unique(c(train_sex,test_sex))
#all_sex
#sex_words <- unique(unlist(all_sex))  
# Create Dummy Variables for each breed
#sex_words
#for (sex in sex_words){
#  train[sex] <- as.numeric(grepl(sex, train_sex))
#  test[sex] <- as.numeric(grepl(sex, test_sex))
#}

dim(train)
dim(test)

###############################################################################
# Submission code
set.seed(121)
full_train_matrix <- matrix(as.numeric(data.matrix(train)),ncol=319)
test_matrix <- matrix(as.numeric(data.matrix(test)),ncol=319)

full_targets_train <- as.numeric(targets)-1
#full_targets_train
#Setting parameters for xgboost

# Run xgb on full train set
xgb_model_test = xgboost(data=full_train_matrix, 
                         label=full_targets_train, 
                         nrounds=200, 
                         verbose=2, 
                         eta=0.1, 
                         max_depth=7, 
                         subsample=0.75, 
                         colsample_bytree=0.85,
                         objective="multi:softprob", 
                         eval_metric="mlogloss",
                         num_class=5)


test_preds <- predict(xgb_model_test, test_matrix)
test_preds_frame <- data.frame(matrix(test_preds, ncol = 5, byrow=TRUE))
colnames(test_preds_frame) <- levels(targets)
#View(test_preds_frame)
submission <- cbind(data.frame(ID=test_ID), test_preds_frame)

write.csv(submission , "submission.csv", row.names=FALSE)
