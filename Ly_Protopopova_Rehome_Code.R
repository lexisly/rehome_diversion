library(tidyverse) 
library(readxl)
library(table1)
library(zoo)
library(lubridate) 
library(psych)
library(usdata)
library(janitor)
library(stringr) 
library(ggpubr)
library(reshape2)
library(ggstance)
library(RColorBrewer)
library(broom)
library(vcd)
library(tidytext)


setwd("C:/Users/lexis/Documents/Masters/Adoptapet/Data")
#Load data
relinq_full<-read.csv("Ly_Protopopova_Rehome_Data1.csv", stringsAsFactors = FALSE) #Data1: Owners who posted their animals on rehome
pets_full<-read.csv("Ly_Protopopova_Rehome_Data2.csv", stringsAsFactors = FALSE)#Data2: Pets posted on rehome
rehome_reasons<-read.csv("Ly_Protopopova_Rehome_Reasons.csv", stringsAsFactors = FALSE)#Categorized rehome reasons
rehome_test<-read.csv("Ly_Protopopova_Rehome_Test.csv", stringsAsFactors = FALSE) #Profiles that were uploaded by Rehome to test bugs on the website

#Change names of columns
names(pets_full)<-str_replace_all(names(pets_full), c(" " = "." , "," = "" ))
names(relinq_full)<-str_replace_all(names(relinq_full), c(" " = "." , "," = "" ,"_" = "." ))

#Combine Pets and Relinquish data
relinq_full<-relinq_full %>%
  select(rehome.pet.id, relinquisher.id, state, city, postal.code)
pets_relinq_full<-left_join(pets_full, relinq_full, by = 'rehome.pet.id')

#Combine pets_relinq with the new rehome reasons
pets_relinq_full<-full_join(pets_relinq_full, rehome_reasons, by = 'rehome.reason')

#Replace NA with None Listed - Rehome Reason
pets_relinq_full$rehome.reason[is.na(pets_relinq_full$rehome.reason)] <- "None listed"
pets_relinq_full$rehome.reason.2[is.na(pets_relinq_full$rehome.reason.2)] <- "None listed"
pets_relinq_full$rehome.reason.3[is.na(pets_relinq_full$rehome.reason.3)] <- "None listed"

#Combine data with test pet IDS - Test pets are uploaded to identify issues with the software
rehome_test <- rehome_test %>%
  dplyr::select(rehome.pet.id, test)%>%
  drop_na(rehome.pet.id)
pets_relinq_full<-left_join(pets_relinq_full,rehome_test, by = 'rehome.pet.id')
pets_relinq_full$test[is.na(pets_relinq_full$test)] <- 0

#Remove test pets from data
pets_test_removed<-pets_relinq_full %>%
  filter(test == 0)

#Remove profiles that do not have pet types (e.g., dog or cat)
pets_cat_dog <- pets_test_removed%>%
  filter(pet.type =="cat"|pet.type == "dog")

#Rename variables with long names
pets_rename<-pets_cat_dog%>%
  dplyr::rename(rehome.deadline = rehome.deadline..in.weeks.)%>%
  dplyr::rename(profile.views = X...of.pet.profile.views..note.pets.can.have.0.if.a..they.have.no.profile.views.or.b..they.did.not.appear.online..e.g...the.pet.was.uploaded.but.went.into.kept.pet.status.right.away...)%>%
  dplyr::rename(dog.size = dog.size.weight..1.small..2.medium..3.large..4.X.large.)

#Remove commas from the numeric columns (e.g., 2,335 --> 2335)
pets_rename$profile.views<-as.numeric(gsub(",","",pets_rename$profile.views))
pets_rename$number.of.applications<-as.numeric(gsub(",","",pets_rename$number.of.applications))
pets_cat_dog<-pets_rename

#Add AKC breed group (defined by AP) to the data set
breed_group<-read.csv("dog_breedSP2.csv", stringsAsFactors = FALSE)
breed_group<-breed_group%>%
  dplyr::select(breed.name, akc.new)
pets_cat_dog<-left_join(pets_cat_dog, breed_group, by='breed.name')

#Combine American Pit Bull Terrier and American Staffordshire Terrier 
pets_cat_dog$breed.name[(pets_cat_dog$breed.name == "American Staffordshire Terrier")]<-"American Pit Bull Terrier"

#Replace age NA with unknown for Age
pets_cat_dog$age[(pets_cat_dog$age == "")]<-"Unknown"

#Remove Unknown Age
pets_cat_dog<-pets_cat_dog%>%
  filter(age !="Unknown")

#Make puppy and kitten the same level and then change to ordinal
pets_cat_dog$age[(pets_cat_dog$age == "kitten")]<-"Kitten/Puppy"
pets_cat_dog$age[(pets_cat_dog$age == "puppy")]<-"Kitten/Puppy"
pets_cat_dog$age<-factor(pets_cat_dog$age, order = TRUE, levels = c("Kitten/Puppy", "young", "adult", "senior"))

#Replace those with unknown spay/neuter status as FALSE
pets_cat_dog$spayed.neutered[(pets_cat_dog$spayed.neutered == "")]<-"f"

##Change empty gender to NA
pets_cat_dog$gender[pets_cat_dog$gender==""] <- NA
pets_cat_dog$gender[pets_cat_dog$gender==" "] <- NA

####Calculate "Length of stay" (LOS) by subtracting the date adopted/updated from the date created
los<-pets_cat_dog%>%
  separate(created.at, into = c("date.created","time.created"), sep = " ")%>%
  separate(adopted.at, into = c("date.adopted","time.adopted"), sep = " ") %>%
  separate(updated.at, into = c("date.updated","time.updated"), sep = " ")

los$date.created<-parse_date_time(los$date.created,
                                  orders = c("Y-d-m", "d/m/Y"),
                                  locale = "eng")

los$date.adopted<-parse_date_time(los$date.adopted,
                                  orders = c("Y-d-m", "d/m/Y"),
                                  locale = "eng")

los$date.updated<-parse_date_time(los$date.updated,
                                  orders = c("Y-d-m", "d/m/Y"),
                                  locale = "eng")

los$adopt.diff <- difftime(los$date.adopted ,los$date.created , units = c("days"))
los$update.diff <- difftime(los$date.updated ,los$date.created , units = c("days"))

#### Change states from abbreviated to normal (for maps)
los$state.name<-abbr2state(los$state)

##Replace NA with 0 for questions, answers, applications, views (numerical)
los$number.of.answers.to.profile[is.na(los$number.of.answers.to.profile)] <- 0
los$number.of.questions.to.profile[is.na(los$number.of.questions.to.profile)] <- 0
los$number.of.applications[is.na(los$number.of.applications)] <- 0
los$profile.views[is.na(los$profile.views)] <- 0
los$photo.count[is.na(los$photo.count)] <- 0

##Replace blanks with NA (for categorical binary)
los$good.with.cats[los$good.with.cats==""] <- NA
los$good.with.dogs[los$good.with.dogs==""] <- NA
los$good.with.kids[los$good.with.kids==""] <- NA
los$housetrained[los$housetrained==""] <- NA
los$purebred[los$purebred==""] <- NA
los$microchipped[los$microchipped==""] <- NA
los$needs.experienced.adopter[los$needs.experienced.adopter==""] <- NA
los$special.needs[los$special.needs==""] <- NA

##Replace NA with f (categorical binary)
los$good.with.cats[is.na(los$good.with.cats)] <- "f"
los$good.with.dogs[is.na(los$good.with.dogs)] <- "f"
los$good.with.kids[is.na(los$good.with.kids)] <- "f"
los$housetrained[is.na(los$housetrained)] <- "f"
los$purebred[is.na(los$purebred)] <- "f"
los$microchipped[is.na(los$microchipped)] <- "f"
los$needs.experienced.adopter[is.na(los$needs.experienced.adopter)] <- "f"
los$special.needs[is.na(los$special.needs)] <- "f"

#pets_complete: All raw datasets combined
pets_complete<-los

#Look for those with and or & in their name (indicates bonded pairs)
bonded<-pets_complete%>%
  filter((grepl(' and', pet.name)|grepl('&', pet.name)|grepl('bonded', pet.name) | 
            grepl('sisters', pet.name ) | grepl('must be', pet.name )|grepl('brothers', pet.name )|grepl('siblings', pet.name )))

#Remove pairs with bonded animals
pets_complete_single<-anti_join(pets_complete, bonded, by = c("rehome.pet.id"))

#What about groups of kittens?
litters<-pets_complete_single%>%
  filter((grepl('litter', pet.name)|grepl('kittens', pet.name)|grepl('puppies', pet.name)))

#Remove litters
pets_complete_single_2<-anti_join(pets_complete_single, litters, by = c("rehome.pet.id"))

##Check those who have posted more than one animal (same relinquisher id)
multiple_id<-pets_complete_single_2%>% 
  group_by(relinquisher.id) %>% 
  dplyr::summarize(duplicated = n() > 1)%>%
  filter(duplicated == TRUE)
multiple_pet<-left_join(multiple_id, pets_complete_single_2, by = c("relinquisher.id"))
length(unique(multiple_pet[["rehome.pet.id"]])) #yes, each one is a unique animal
length(unique(multiple_pet[["relinquisher.id"]])) #yes, numnber of relinquisher ids matches the previous df

#Find duplicated pets that were created on the same day
duplicate_pet_same_day<-multiple_pet %>% 
  get_dupes(relinquisher.id, pet.name, date.created)%>%
  dplyr::select(relinquisher.id, rehome.pet.id, pet.name, pet.type, age, date.created, date.updated, update.diff, status, story)
length(unique(duplicate_pet_same_day$relinquisher.id)) #1027 unique relinquishers

##Remove animals that are duplicated and posted on the same day
pets_complete_single_3<-anti_join(pets_complete_single_2, duplicate_pet_same_day, by = c("rehome.pet.id"))

##for just those with 0 LOS
duplicate_pet_same_day %>%
  filter(update.diff == "0")%>%
  group_by(status)%>%
  dplyr::summarize(n=n())%>%
  dplyr::mutate(frequency = n / sum(n))%>%
  mutate(percent = frequency*100)

##Remove animals with 0 LOS
pets_complete_zero_remove<-anti_join(pets_complete_single_3, zero_los, by = c("rehome.pet.id"))

##Remove animals with non-final outcomes
pets_non_final<-pets_complete_zero_remove %>%
  filter(status == 'other' | status == 'pending-adoption' | status == 'high-demand')
pets_complete_single_final<-anti_join(pets_complete_zero_remove, pets_non_final, by = c("rehome.pet.id"))

## Merge statuses
pets_complete_single_final$status[pets_complete_single_final$status=="adopted-p2p"] <- "adopted"
pets_complete_single_final$status[pets_complete_single_final$status=="adopted-friend"] <- "adopted"
pets_complete_single_final$status[pets_complete_single_final$status=="adopted-other"] <- "adopted"
pets_complete_single_final$status[pets_complete_single_final$status=="kept-pet"] <- "keep"
pets_complete_single_final$status[pets_complete_single_final$status=="keep-pet"] <- "keep"
pets_complete_single_final$status[pets_complete_single_final$status=="relinquished-rescue"] <- "relinquished"
pets_complete_single_final$status[pets_complete_single_final$status=="relinquished-shelter"] <- "relinquished"

#Regions
pets_complete_single_final$region[pets_complete_single_final$state=="WA"|
                                    pets_complete_single_final$state=="OR"|
                                    pets_complete_single_final$state=="CA"|
                                    pets_complete_single_final$state=="NV"|
                                    pets_complete_single_final$state=="AZ"|
                                    pets_complete_single_final$state=="ID"|
                                    pets_complete_single_final$state=="MT"|
                                    pets_complete_single_final$state=="WY"|
                                    pets_complete_single_final$state=="CO"|
                                    pets_complete_single_final$state=="NM"|
                                    pets_complete_single_final$state=="UT"] <- "West"

pets_complete_single_final$region[pets_complete_single_final$state=="TX"|
                                    pets_complete_single_final$state=="OK"|
                                    pets_complete_single_final$state=="AR"|
                                    pets_complete_single_final$state=="LA"|
                                    pets_complete_single_final$state=="MS"|
                                    pets_complete_single_final$state=="AL"|
                                    pets_complete_single_final$state=="TN"|
                                    pets_complete_single_final$state=="KY"|
                                    pets_complete_single_final$state=="GA"|
                                    pets_complete_single_final$state=="FL"|
                                    pets_complete_single_final$state=="SC"|
                                    pets_complete_single_final$state=="NC"|
                                    pets_complete_single_final$state=="VA"|
                                    pets_complete_single_final$state=="WV"] <- "South"

pets_complete_single_final$region[pets_complete_single_final$state=="KS"|
                                    pets_complete_single_final$state=="NE"|
                                    pets_complete_single_final$state=="SD"|
                                    pets_complete_single_final$state=="ND"|
                                    pets_complete_single_final$state=="MN"|
                                    pets_complete_single_final$state=="MO"|
                                    pets_complete_single_final$state=="IA"|
                                    pets_complete_single_final$state=="IL"|
                                    pets_complete_single_final$state=="IN"|
                                    pets_complete_single_final$state=="MI"|
                                    pets_complete_single_final$state=="WI"|
                                    pets_complete_single_final$state=="OH"] <- "Midwest"

pets_complete_single_final$region[pets_complete_single_final$state=="ME"|
                                    pets_complete_single_final$state=="NH"|
                                    pets_complete_single_final$state=="NY"|
                                    pets_complete_single_final$state=="MA"|
                                    pets_complete_single_final$state=="RI"|
                                    pets_complete_single_final$state=="VT"|
                                    pets_complete_single_final$state=="PA"|
                                    pets_complete_single_final$state=="NJ"|
                                    pets_complete_single_final$state=="CT"|
                                    pets_complete_single_final$state=="DE"|
                                    pets_complete_single_final$state=="MD"|
                                    pets_complete_single_final$state=="DC"] <- "Northeast"

### Keep all columns for descriptive
pets_model<-pets_complete_single_final

###Convert all categorical to factors
pets_model[sapply(pets_model, is.character)] <- lapply(pets_model[sapply(pets_model, is.character)], 
                                                       as.factor)
col_names <- sapply(pets_model, function(col) length(unique(col)) < 6)
pets_model[ , col_names] <- lapply(pets_model[ , col_names] , factor)

#######Combine adoption through other means with adoption through Rehome
pets_adopted<-pets_model%>%
  filter((grepl('adopted', status))) #n=83,591

adopted_continuous<-pets_adopted%>%
  group_by(status)%>%
  get_summary_stats()

pets_adopted$adopted[pets_adopted$status=="adopted-rehome"] <- 1
pets_adopted$adopted_rehome[pets_adopted$status=="adopted-other"] <- 0

##Combine extra large and large
pets_model$dog.size[pets_model$dog.size=="4"] <- 3

##Combine Abandoned, Found, or none for CATS and DOGS
pets_model$rehome.reason.3<-as.character(pets_model$rehome.reason.3)
pets_model$rehome.reason.3[pets_model$rehome.reason.3=="Fostered, abandoned, found"] <- NA
pets_model$rehome.reason.3[pets_model$rehome.reason.3=="None listed"] <- NA
pets_model$rehome.reason.3[is.na(pets_model$rehome.reason.3)] <- "Neither"
pets_model$rehome.reason.3<-as.factor(pets_model$rehome.reason.3)
levels(pets_model$rehome.reason.3)

##Order Rehome deadline
pets_model$rehome.deadline<-ordered(pets_model$rehome.deadline, levels = c("Less than 1", "1", "2", "3", "4", "8", "none"))

## Create 'short, medium, long, none' Rehome deadlines
pets_model$rehome.deadline.2[pets_model$rehome.deadline=="Less than 1"] <- 'Short'
pets_model$rehome.deadline.2[pets_model$rehome.deadline=="1"] <- 'Short'
pets_model$rehome.deadline.2[pets_model$rehome.deadline=="2"] <- 'Medium'
pets_model$rehome.deadline.2[pets_model$rehome.deadline=="3"] <- 'Medium'
pets_model$rehome.deadline.2[pets_model$rehome.deadline=="4"] <- 'Medium'
pets_model$rehome.deadline.2[pets_model$rehome.deadline=="8"] <- 'Long'
pets_model$rehome.deadline.2[pets_model$rehome.deadline=="none"] <- 'None'
pets_model$rehome.deadline.2 <- factor(pets_model$rehome.deadline.2, ordered = TRUE, 
                                       levels = c("Short", "Medium", "Long", "None"))

pets_model$rehome.deadline<-ordered(pets_model$rehome.deadline, levels = c("Less than 1", "1", "2", "3", "4", "8", "none"))

###Selecting data for models
pets_model_real<-pets_model%>%
  dplyr::select(rehome.pet.id, status, pet.type, age, breed.name, 
                color.name, gender, good.with.cats, good.with.dogs, 
                good.with.kids, housetrained, purebred, microchipped, 
                needs.experienced.adopter, special.needs, photo.count,
                dog.size, spayed.neutered, rehome.reason, rehome.deadline, deleted.by.owner.reason,
                number.of.answers.to.profile, number.of.questions.to.profile, number.of.applications,
                profile.views, state, region, city, postal.code,
                akc.new, adopt.diff, update.diff, rehome.reason.2, rehome.reason.3, rehome.deadline.2)

##Subset model data into dog and cat
pets_model_real_dog<-pets_model_real %>%
  filter(pet.dtype=="dog")

pets_model_real_cat<-pets_model_real %>%
  filter(pet.type=="cat")

#Shuffle data so it is random
shuffle_pets <- sample(1:nrow(pets_model_real_dog))
pets_model_real_dog <- pets_model_real_dog[shuffle_pets, ]
head(pets_model_real_dog)

#Drop variables
clean_dog <- pets_model_real_dog %>%
  dplyr::select(-c(pet.type, breed.name, color.name, deleted.by.owner.reason,  adopt.diff, update.diff, rehome.reason)) %>% 
  filter(age != "Unknown")%>%
  filter(rehome.deadline != "Other")%>%
  na.omit()

#Shuffle data so it is random
shuffle_pets <- sample(1:nrow(pets_model_real_dog))
pets_model_real_dog <- pets_model_real_dog[shuffle_pets, ]
head(pets_model_real_dog)

#Drop variables
clean_dog <- pets_model_real_dog %>%
  dplyr::select(-c(pet.type, breed.name, color.name, deleted.by.owner.reason,  adopt.diff, update.diff, rehome.reason)) %>% 
  filter(age != "Unknown")%>%
  filter(rehome.deadline != "Other")%>%
  na.omit()

clean_cat <- pets_model_real_cat %>%
  dplyr::select(-c(pet.type, breed.name, dog.size, color.name, akc.new, deleted.by.owner.reason,  adopt.diff, update.diff, rehome.reason)) %>% 
  filter(age != "Unknown")%>%
  filter(rehome.deadline != "Other")%>%
  na.omit()

#Drop unused level
levels(clean_dog$dog.size)

clean_dog$dog.size<-droplevels(clean_dog$dog.size)
levels(clean_dog$dog.size)

####MODEL DATA
summary(clean_dog)
summary(clean_cat)

###Descriptive data by outcome
table1(~age + gender + good.with.cats + good.with.dogs + good.with.kids + housetrained +
         purebred + microchipped + needs.experienced.adopter + special.needs + photo.count + dog.size +
         spayed.neutered + rehome.reason.2 + rehome.deadline |status , data = clean_dog, export = "file_name")

##### Correlations between variables
##Binary-Binary: Phi correlation
phi(table(clean_cat$special.needs, clean_cat$spayed.neutered), digits = 3)#phi correlation coefficient

##Binary-Ordinal / Ordinal-Ordinal: Spearman Rank correlation
cor(as.numeric(clean_cat$photo.count), as.numeric(clean_cat$rehome.deadline.2), method = c("spearman"))

##Binary-Nominal / Ordinal-Nominal / Nominal-Nominal: Cramer's V (Chi-Squared)
#NOTE: Cramer's V does NOT show direction, only strength of the relationship
test_table<-table(clean_dog$akc.new, clean_dog$dog.size)
sqrt(chisq.test(test_table)$statistic / (sum(test_table) * min(dim(test_table) - 1 )))

##Packages for model
library(ISLR)
library(tree)
library(caret)
library(e1071)
library(Fdplyr::selector)
library(rpart)
library(rpart.plot)
library(data.tree)
library(caTools)
library(rattle)
library(partykit)
library(vip)
library(rcompanion)
library(visreg)
library(margins) 
library(chisq.posthoc.test)
library(MASS)
library(ROCR)
library(emmeans)
library(parameters)
library(ggeffects)
library(effects)
library(olsrr)
library(car)

####################################### DIVERTED/NOT DIVERTED DOGS ####################################
#Create diverted (1) vs relinquished (0)
clean_cat$divert[(clean_cat$status != "relinquished")]<-1
clean_cat$divert[(clean_cat$status == "relinquished")]<-0
clean_dog$divert[(clean_dog$status != "relinquished")]<-1
clean_dog$divert[(clean_dog$status == "relinquished")]<-0
clean_dog$divert<-as.factor(clean_dog$divert)
clean_cat$divert<-as.factor(clean_cat$divert)

table(clean_cat$status)
table(clean_dog$status)

clean_cat %>%
  group_by(status)%>%
  dplyr::summarize(n=n())%>%
  dplyr::mutate(frequency = n / sum(n))%>%
  mutate(percent = frequency*100)
clean_dog %>%
  group_by(status)%>%
  dplyr::summarize(n=n())%>%
  dplyr::mutate(frequency = n / sum(n))%>%
  mutate(percent = frequency*100)

#make sure the training data has approximately equal proportion of classes
clean_cat %>%
  group_by(divert)%>%
  dplyr::summarize(n=n())%>%
  dplyr::mutate(frequency = n / sum(n))%>%
  mutate(percent = frequency*100)
clean_dog %>%
  group_by( rehome.reason.2, divert)%>%
  dplyr::summarize(n=n())%>%
  dplyr::mutate(frequency = n / sum(n))%>%
  mutate(percent = frequency*100)

#Both have unequal classes - will need to downsample 

# Prep Training and Test data.
set.seed(123)
trainDataIndex <- createDataPartition(clean_dog$divert, p=0.8, list = F)  
train_dog_divert <- clean_dog[trainDataIndex, ]
test_dog_divert <- clean_dog[-trainDataIndex, ]
# Down Sample
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)
set.seed(123)
down_train_dog_divert <- downSample(x = train_dog_divert[, colnames(train_dog_divert) %ni% "divert"],
                                  y = train_dog_divert$divert)
table(down_train_dog_divert$Class)

#turn all ordinal to factors
down_train_dog_divert$dog.size<-factor(down_train_dog_divert$dog.size, ordered = FALSE )
down_train_dog_divert$age<-factor(down_train_dog_divert$age, ordered = FALSE )
down_train_dog_divert$rehome.deadline.2<-factor(down_train_dog_divert$rehome.deadline.2, ordered = FALSE )
down_train_dog_divert$photo.count<-factor(down_train_dog_divert$photo.count, ordered = FALSE )

#FULL MODEL
dog_divert_full <- glm(Class~age + gender + good.with.cats + good.with.dogs + good.with.kids +
                        housetrained + purebred + microchipped + needs.experienced.adopter+
                        special.needs + photo.count + dog.size + spayed.neutered +
                        rehome.deadline.2 + relevel(akc.new, ref = "Terrier")  + rehome.reason.2 , data = down_train_dog_divert, family = "binomial")

summary(dog_divert_full)
car::vif(dog_divert_full)
dog_divert_full_summary<-tidy(dog_divert_full, exponentiate = TRUE, conf.level = 0.95)
OR_dog_divert_full<-exp(cbind(coef(dog_divert_full), confint(dog_divert_full)))
write.csv(dog_divert_full_summary, 'dog_divert_full_summary.csv')
write.csv(OR_dog_divert_full, 'OR_dog_divert_full.csv')

##Marginal effects 
effects_dog_divert_full = margins(dog_divert_full)
summary(effects_dog_divert_full)
plot(effects_dog_divert_full)

##Marginal means
ggpredict(dog_divert_full) #gives you the predicted probabilities for divertion (adjusting for all other variables in the model)

# Pseudo R_squared values and Likelyhood ratio test
nagelkerke(dog_divert_full)

####Model evaluation on the test dataset
# predict the test dataset
pred <- predict(dog_divert_full, test_dog_divert, type="response") 
predicted <- round(pred) # round of the value; >0.5 will convert to 
# 1 else 0
# Add predicted to dataset
test_dog_divert$pred <- predict(dog_divert_full, test_dog_divert, type="response") 
test_dog_divert$predicted <- round(pred) # round of the value; >0.5 will convert to 
# Creating a contigency table
tab <- table(predicted, test_dog_divert$divert)
confusionMatrix(tab, positive = '1')
?confusionMatrix

#FINAL MODEL (STEP 2) - Photo Count, Gender removed
down_train_dog_divert$akc.new<-relevel(down_train_dog_divert$akc.new, "Terrier")
dog_divert_back2 <- glm(Class~age +good.with.cats + good.with.dogs + good.with.kids +
                          housetrained + purebred + microchipped + needs.experienced.adopter+
                          special.needs + dog.size + spayed.neutered +
                          rehome.deadline.2 + akc.new + rehome.reason.2 , data = down_train_dog_divert, family = "binomial")
summary(dog_divert_back2)
car::vif(dog_divert_back2)
dog_divert_back2_summary<-tidy(dog_divert_back2, exponentiate = TRUE, conf.level = 0.95)
OR_dog_divert_back2<-exp(cbind(coef(dog_divert_back2), confint(dog_divert_back2)))

##Marginal effects
effects_dog_divert_back2 = margins(dog_divert_back2)
summary(effects_dog_divert_back2)
plot(effects_dog_divert_back2)

##Marginal means
ggpredict(dog_divert_back2) #gives you the predicted probabilities for diversion (adjusting for all other variables in the model)
ggeffect(dog_divert_back2)

# Pseudo R_squared values and Likelyhood ratio test
nagelkerke(dog_divert_back2)

####Model evaluation on the test dataset
# predict the test dataset
pred <- predict(dog_divert_back2, test_dog_divert, type="response") 
predicted <- round(pred) # round of the value; >0.5 will convert to 
# 1 else 0
# Add predicted to dataset
test_dog_divert$pred <- predict(dog_divert_back2, test_dog_divert, type="response") 
test_dog_divert$predicted <- round(pred) # round of the value; >0.5 will convert to 
# Creating a contigency table
tab <- table(predicted, test_dog_divert$divert)
confusionMatrix(tab, positive = '1')
?confusionMatrix

###Proportion of diverted animals
clean_dog %>%
  group_by(needs.experienced.adopter, divert)%>%
  dplyr::summarise(n = n()) %>%
  mutate(frequency = n / sum(n))

###Create Odds Ratio Plot - Divert Dog
OR_divert_dog<-read.csv("OR_divert_dog.csv", stringsAsFactors = FALSE)
OR_divert_dog<-OR_divert_dog %>%
  filter(OR != "Reference")%>%
  drop_na()
OR_divert_dog$Level1[(OR_divert_dog$Level1 == "FALSE")]<-"False"
OR_divert_dog$Level1[(OR_divert_dog$Level1 == "TRUE")]<-"True"
  
OR_divert_dog<-OR_divert_dog %>%
  mutate(Level1= fct_relevel(Level1, 'Young', 'Adult', 'Senior', 'Medium', 'Large',
                                       'Working', 'Toy ', 'Sporting', 'Hound', 'Herding', 'Purebred', 'True',
                                       'Medium', 'Long', 'None', 'Fostered, abandoned, found',
                                       'Housing issue', 'Human health issue', 'Personal issue',
                                       'Cost issue', 'None')) 


colnames(OR_divert_dog)[1] <- gsub('^...','',colnames(OR_divert_dog)[1])
OR_divert_dog$Category<- OR_divert_dog$x
OR_divert_dog$OR<-as.numeric(OR_divert_dog$OR)

OR_divert_dog$Category <- factor(OR_divert_dog$Category, levels = c('Age (ref = Puppy)',
                                                                    'Dog size (ref = Small)', 'Breed group (ref = Terrier)',
                                                                    'Purebred (ref = Mixed breed)', 'Good with cats (ref = False)',
                                                                    'Good with dogs (ref = False)', 'Good with kids (ref = False)',
                                                                    'Housetrained (ref = False)', 'Microchipped (ref = False)',
                                                                    'Spayed/neutered (ref = False)', 'Needs experienced adopter (ref = False)',
                                                                    'Special needs (ref = False)', 'Rehome deadline (ref = Short)',
                                                                    'Rehome reason (ref = Animal behaviour issue)'))



divert_dog_plot<-ggplot(OR_divert_dog, aes(x = OR, y = Level1, group = Category)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = CI_97.5, xmin = CI_2.5), size = .5, height = 
                   .2, color = "gray50") +
  scale_x_continuous(limits = c(0, 3.5), expand = c(0,0) )+
  geom_point(size = 3.5, color = "royalblue4") +
  theme_bw()+
  ylab("") +
  xlab("Odds Ratio") +
  facet_grid(Category~., scales = "free", switch = "y", space = "free_y", labeller = label_wrap_gen(50))+
  theme(strip.background = element_blank(),  strip.placement = "outside", 
        panel.spacing = unit(0, "mm"),  axis.ticks.x = element_blank())+
  theme(strip.text.y.left = element_text(angle = 0), strip.text.y = element_text(
    size = 8, face = "bold"),axis.text.y = element_text(
      size = 10))+
  scale_y_discrete(limits=rev)

tiff('divert_dog_plotJULY27.tiff', units="in", width=11, height=7, res=300, compression = 'lzw')
divert_dog_plot
dev.off()


OR_divert_dog$Level1 <- factor(OR_divert_dog$Level1, ordered = TRUE, levels = c('Young', 'Adult', 'Senior', 'Medium', 'Large',
                                                                                'Working', 'Toy ', 'Sporting', 'Hound', 'Herding', 'Purebred', 'True',
                                                                                'Medium', 'Long', 'None', 'Fostered, abandoned, found',
                                                                                'Housing issue', 'Human health issue', 'Personal issue',
                                                                                'Cost issue', 'None listed'))

OR_divert_dog$Level1 <- factor(OR_divert_dog$Level1, ordered = TRUE, levels=unique(OR_divert_dog$Level1))

###PUREBRED DOGS ONLY -- DIVERTED/RELINQUISHED
clean_dog_pure<-clean_dog%>%
  filter(purebred == 't')

#Create diverted (1) vs relinquished (0)
clean_dog_pure$divert[(clean_dog_pure$status != "relinquished")]<-1
clean_dog_pure$divert[(clean_dog_pure$status == "relinquished")]<-0
clean_dog_pure$divert<-as.factor(clean_dog_pure$divert)
clean_dog_pure$divert<-as.factor(clean_dog_pure$divert)

table(clean_dog_pure$status)

clean_dog_pure %>%
  group_by(status)%>%
  dplyr::summarize(n=n())%>%
  dplyr::mutate(frequency = n / sum(n))%>%
  mutate(percent = frequency*100)

#make sure the training data has approximately equal proportion of classes
clean_dog_pure %>%
  group_by(divert)%>%
  dplyr::summarize(n=n())%>%
  dplyr::mutate(frequency = n / sum(n))%>%
  mutate(percent = frequency*100)

# Prep Training and Test data.
set.seed(123)
trainDataIndex <- createDataPartition(clean_dog_pure$divert, p=0.8, list = F)  
train_dog_pure_divert <- clean_dog_pure[trainDataIndex, ]
test_dog_pure_divert <- clean_dog_pure[-trainDataIndex, ]
# Down Sample
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)
set.seed(123)
down_train_dog_pure_divert <- downSample(x = train_dog_pure_divert[, colnames(train_dog_pure_divert) %ni% "divert"],
                                    y = train_dog_pure_divert$divert)
table(down_train_dog_pure_divert$Class)

#turn all ordinal to factors
down_train_dog_pure_divert$dog.size<-factor(down_train_dog_pure_divert$dog.size, ordered = FALSE )
down_train_dog_pure_divert$age<-factor(down_train_dog_pure_divert$age, ordered = FALSE )
down_train_dog_pure_divert$rehome.deadline.2<-factor(down_train_dog_pure_divert$rehome.deadline.2, ordered = FALSE )
down_train_dog_pure_divert$photo.count<-factor(down_train_dog_pure_divert$photo.count, ordered = FALSE )

#FULL MODEL
dog_pure_divert_full <- glm(Class~age + gender + good.with.cats + good.with.dogs + good.with.kids +
                         housetrained + microchipped + needs.experienced.adopter+
                         special.needs + photo.count + dog.size + spayed.neutered +
                         rehome.deadline.2 + relevel(akc.new, ref = "Terrier")  + rehome.reason.2 , data = down_train_dog_pure_divert, family = "binomial")

summary(dog_pure_divert_full)
car::vif(dog_pure_divert_full)
dog_pure_divert_full_summary<-tidy(dog_pure_divert_full, exponentiate = TRUE, conf.level = 0.95)
OR_dog_pure_divert_full<-exp(cbind(coef(dog_pure_divert_full), confint(dog_pure_divert_full)))

##Marginal effects
effects_dog_pure_divert_full = margins(dog_pure_divert_full)
summary(effects_dog_pure_divert_full)
plot(effects_dog_pure_divert_full)

##Marginal means
ggpredict(dog_pure_divert_full) 

# Pseudo R_squared values and Likelyhood ratio test
nagelkerke(dog_pure_divert_full)

####Model evaluation on the test dataset
# predict the test dataset
pred <- predict(dog_pure_divert_full, test_dog_pure_divert, type="response") 
predicted <- round(pred) # round of the value; >0.5 will convert to 
# 1 else 0
# Add predicted to dataset
test_dog_pure_divert$pred <- predict(dog_pure_divert_full, test_dog_pure_divert, type="response") 
test_dog_pure_divert$predicted <- round(pred) # round of the value; >0.5 will convert to 
# Creating a contigency table
tab <- table(predicted, test_dog_pure_divert$divert)
confusionMatrix(tab, positive = '1')
?confusionMatrix

#FINAL MODEL (STEP 5) - Photo count, Dog size, Microchipped, Gender, Good with cats
dog_pure_divert_back5 <- glm(Class~age + good.with.dogs + good.with.kids +
                          housetrained + needs.experienced.adopter+
                          special.needs + spayed.neutered +
                          rehome.deadline.2 + relevel(akc.new, ref = "Terrier") + rehome.reason.2 , data = down_train_dog_pure_divert, family = "binomial")
summary(dog_pure_divert_back5)
dog_pure_divert_back5_summary<-tidy(dog_pure_divert_back5, exponentiate = TRUE, conf.level = 0.95)
OR_dog_pure_divert_back5<-exp(cbind(coef(dog_pure_divert_back5), confint(dog_pure_divert_back5)))

##Marginal effects
effects_dog_pure_divert_back5 = margins(dog_pure_divert_back5)
summary(effects_dog_pure_divert_back5)
plot(effects_dog_pure_divert_back5)

##Marginal means
ggpredict(dog_pure_divert_back5) #gives you the predicted probabilities for divertion (adjusting for all other variables in the model)

# Pseudo R_squared values and Likelyhood ratio test
nagelkerke(dog_pure_divert_back5)

####Model evaluation on the test dataset
# predict the test dataset
pred <- predict(dog_pure_divert_back5, test_dog_pure_divert, type="response") 
predicted <- round(pred) # round of the value; >0.5 will convert to 
# 1 else 0
# Add predicted to dataset
test_dog_pure_divert$pred <- predict(dog_pure_divert_back5, test_dog_pure_divert, type="response") 
test_dog_pure_divert$predicted <- round(pred) # round of the value; >0.5 will convert to 
# Creating a contigency table
tab <- table(predicted, test_dog_pure_divert$divert)
confusionMatrix(tab, positive = '1')
?confusionMatrix

####################################### DIVERTED/NOT DIVERTED CATS ####################################
#Create diverted (1) vs relinquished (0)
clean_cat$divert[(clean_cat$status != "relinquished")]<-1
clean_cat$divert[(clean_cat$status == "relinquished")]<-0
clean_cat$divert<-as.factor(clean_cat$divert)

#make sure the training data has approximately equal proportion of classes
clean_cat %>%
  group_by(divert)%>%
  dplyr::summarize(n=n())%>%
  dplyr::mutate(frequency = n / sum(n))%>%
  mutate(percent = frequency*100)

# Prep Training and Test data.
set.seed(123)
trainDataIndex <- createDataPartition(clean_cat$divert, p=0.8, list = F)  
train_cat_divert <- clean_cat[trainDataIndex, ]
test_cat_divert <- clean_cat[-trainDataIndex, ]
# Down Sample
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)
set.seed(123)
down_train_cat_divert <- downSample(x = train_cat_divert[, colnames(train_cat_divert) %ni% "divert"],
                                    y = train_cat_divert$divert)
table(down_train_cat_divert$Class)

#turn all ordinal to factors
down_train_cat_divert$age<-factor(down_train_cat_divert$age, ordered = FALSE )
down_train_cat_divert$rehome.deadline.2<-factor(down_train_cat_divert$rehome.deadline.2, ordered = FALSE )
down_train_cat_divert$photo.count<-factor(down_train_cat_divert$photo.count, ordered = FALSE )

#FULL MODEL
cat_divert_full <- glm(Class~age + gender + good.with.cats + good.with.dogs + good.with.kids +
                         housetrained + purebred + microchipped + needs.experienced.adopter+
                         special.needs + photo.count  + spayed.neutered +
                         rehome.deadline.2  + rehome.reason.2 , data = down_train_cat_divert, family = "binomial")

summary(cat_divert_full)
cat_divert_full_summary<-tidy(cat_divert_full, exponentiate = TRUE, conf.level = 0.95)
OR_cat_divert_full<-exp(cbind(coef(cat_divert_full), confint(cat_divert_full)))

##Marginal effects 
effects_cat_divert_full = margins(cat_divert_full)
summary(effects_cat_divert_full)
plot(effects_cat_divert_full)

##Marginal means
ggpredict(cat_divert_full) #gives you the predicted probabilities for divertion (adjusting for all other variables in the model)

# Pseudo R_squared values and Likelyhood ratio test
nagelkerke(cat_divert_full)

####Model evaluation on the test dataset
# predict the test dataset
pred <- predict(cat_divert_full, test_cat_divert, type="response") 
predicted <- round(pred) # round of the value; >0.5 will convert to 
# 1 else 0
# Add predicted to dataset
test_cat_divert$pred <- predict(cat_divert_full, test_cat_divert, type="response") 
test_cat_divert$predicted <- round(pred) # round of the value; >0.5 will convert to 
# Creating a contigency table
tab <- table(predicted, test_cat_divert$divert)
confusionMatrix(tab, positive = '1')

#FINAL MODELSTEP 3 - Gender, Spayed/Neutered, Good with Cats, Housetrained, Special needs removed
cat_divert_back5 <- glm(Class~age + good.with.dogs + good.with.kids +
                          purebred + microchipped + needs.experienced.adopter+
                           photo.count  + rehome.deadline.2  + rehome.reason.2 , data = down_train_cat_divert, family = "binomial")

summary(cat_divert_back5)
cat_divert_back5_summary<-tidy(cat_divert_back5, exponentiate = TRUE, conf.level = 0.95)
OR_cat_divert_back5<-exp(cbind(coef(cat_divert_back5), confint(cat_divert_back5)))

##Marginal effects 
effects_cat_divert_back5 = margins(cat_divert_back5)
summary(effects_cat_divert_back5)
plot(effects_cat_divert_back5)

##Marginal means
ggpredict(cat_divert_back5) #give you the predicted probabilities for divertion (adjusting for all other variables in the model)
ggeffect(cat_divert_back5)

# Pseudo R_squared values and Likelyhood ratio test
nagelkerke(cat_divert_back5)

####Model evaluation on the test dataset
# predict the test dataset
pred <- predict(cat_divert_back5, test_cat_divert, type="response") 
predicted <- round(pred) # round of the value; >0.5 will convert to 
# 1 else 0
# Add predicted to dataset
test_cat_divert$pred <- predict(cat_divert_back5, test_cat_divert, type="response") 
test_cat_divert$predicted <- round(pred) # round of the value; >0.5 will convert to 
# Creating a contigency table
tab <- table(predicted, test_cat_divert$divert)
confusionMatrix(tab, positive = '1')

###Proportion of diverted animals
clean_cat %>%
  group_by(rehome.reason.2, divert)%>%
  dplyr::summarise(n = n()) %>%
  mutate(frequency = n / sum(n))

###Create Odds Ratio Plot - Divert Cat
OR_divert_cat<-read.csv("OR_divert_cat.csv", stringsAsFactors = FALSE)
OR_divert_cat<-OR_divert_cat %>%
  filter(OR != "Reference")%>%
  drop_na()
OR_divert_cat$Level1[(OR_divert_cat$Level1 == "FALSE")]<-"False"
OR_divert_cat$Level1[(OR_divert_cat$Level1 == "TRUE")]<-"True"

OR_divert_cat<-OR_divert_cat %>%
  mutate(Level1= fct_relevel(Level1, 'Young', 'Adult', 'Senior', 'Purebred', 
                             'True','True','True','True',
                             '1', '2', '3', '4',
                             'Medium', 'Long', 'None', 'Fostered, abandoned, found',
                             'Housing issue', 'Human health issue', 'Personal issue',
                             'Cost issue', 'None')) 


colnames(OR_divert_cat)[1] <- gsub('^...','',colnames(OR_divert_cat)[1])
OR_divert_cat$OR<-as.numeric(OR_divert_cat$OR)

OR_divert_cat$Category <- factor(OR_divert_cat$Category, levels = c('Age (ref = Kitten)',
                                                                    'Purebred (ref = Mixed breed)', 
                                                                    'Good with dogs (ref = False)', 'Good with kids (ref = False)',
                                                                    'Microchipped (ref = False)','Needs experienced adopter (ref = False)',
                                                                    'Photos on profile (ref = 0)', 'Rehome deadline (ref = Short)',
                                                                    'Rehome reason (ref = Animal behaviour issue)'))


OR_divert_dog$Level1 <- factor(OR_divert_dog$Level1, ordered = TRUE, levels=unique(OR_divert_dog$Level1))


divert_cat_plot<-ggplot(OR_divert_cat, aes(x = OR, y = Level1, group = Category)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = CI_97.5, xmin = CI_2.5), size = .5, height = 
                   .2, color = "gray50") +
  scale_x_continuous(limits = c(0, 3.5), expand = c(0,0) )+
  geom_point(size = 3.5, color = "royalblue4") +
  theme_bw()+
  ylab("") +
  xlab("Odds Ratio") +
  facet_grid(Category~., scales = "free", switch = "y", space = "free_y", labeller = label_wrap_gen(50))+
  theme(strip.background = element_blank(),  strip.placement = "outside", 
        panel.spacing = unit(0, "mm"),  axis.ticks.x = element_blank())+
  theme(strip.text.y.left = element_text(angle = 0), strip.text.y = element_text(
    size = 8, face = "bold"),axis.text.y = element_text(
      size = 10))+
  scale_y_discrete(limits=rev)

tiff('divert_cat_plot.tiff', units="in", width=11, height=7, res=300, compression = 'lzw')
divert_cat_plot
dev.off()

OR_divert_cat$Level1 <- factor(OR_divert_cat$Level1, ordered = TRUE, levels = c('Young', 'Adult', 'Senior', 'Purebred', 'True',
                                                                                '1', '2', '3', '4',
                                                                                'Medium', 'Long', 'None', 'Fostered, abandoned, found',
                                                                                'Housing issue', 'Human health issue', 'Personal issue',
                                                                                'Cost issue', 'None'))


OR_divert_cat$Level1 <- factor(OR_divert_cat$Level1, ordered = TRUE, levels=unique(OR_divert_cat$Level1))

####################################### KEPT/ADOPT DOGS ####################################
#Create diverted (1) vs relinquished (0)
clean_dog_kept_adopt<-clean_dog%>%
  filter(status!= 'relinquished')
clean_dog_kept_adopt$kept_adopt[(clean_dog_kept_adopt$status == "keep")]<-1
clean_dog_kept_adopt$kept_adopt[(clean_dog_kept_adopt$status == "adopted")]<-0
clean_dog_kept_adopt$kept_adopt<-as.factor(clean_dog_kept_adopt$kept_adopt)
summary(clean_dog_kept_adopt$kept_adopt)

#mkae sure the training data has approximately equal proportion of classes
clean_dog_kept_adopt %>%
  group_by(kept_adopt)%>%
  dplyr::summarize(n=n())%>%
  dplyr::mutate(frequency = n / sum(n))%>%
  mutate(percent = frequency*100)
# Prep Training and Test data.
set.seed(123)
trainDataIndex <- createDataPartition(clean_dog_kept_adopt$kept_adopt, p=0.8, list = F)  
train_dog_kept_adopt <- clean_dog_kept_adopt[trainDataIndex, ]
test_dog_kept_adopt <- clean_dog_kept_adopt[-trainDataIndex, ]
# Down Sample
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)
set.seed(123)
down_train_dog_kept_adopt <- downSample(x = train_dog_kept_adopt[, colnames(train_dog_kept_adopt) %ni% "kept_adopt"],
                                    y = train_dog_kept_adopt$kept_adopt)
table(down_train_dog_kept_adopt$Class)

#turn all ordinal to factors
down_train_dog_kept_adopt$dog.size<-factor(down_train_dog_kept_adopt$dog.size, ordered = FALSE )
down_train_dog_kept_adopt$age<-factor(down_train_dog_kept_adopt$age, ordered = FALSE )
down_train_dog_kept_adopt$rehome.deadline.2<-factor(down_train_dog_kept_adopt$rehome.deadline.2, ordered = FALSE )
down_train_dog_kept_adopt$photo.count<-factor(down_train_dog_kept_adopt$photo.count, ordered = FALSE )
down_train_dog_kept_adopt$akc.new<-relevel(down_train_dog_kept_adopt$akc.new, "Terrier")

#FULL MODEL
dog_ka_full <- glm(Class~age + gender + good.with.cats + good.with.dogs + good.with.kids +
                         housetrained + purebred + microchipped + needs.experienced.adopter+
                         special.needs + photo.count + dog.size + spayed.neutered +
                         rehome.deadline.2 + akc.new + rehome.reason.2 , data = down_train_dog_kept_adopt, family = "binomial")

summary(dog_ka_full)
dog_ka_full_summary<-tidy(dog_ka_full, exponentiate = TRUE, conf.level = 0.95)
OR_dog_ka_full<-exp(cbind(coef(dog_ka_full), confint(dog_ka_full)))

##Marginal effects
effects_dog_ka_full = margins(dog_ka_full)
summary(effects_dog_ka_full)
plot(effects_dog_ka_full)

##Marginal means
ggpredict(dog_ka_full) #gives you the predicted probabilities for kaion (adjusting for all other variables in the model)

# Pseudo R_squared values and Likelyhood ratio test
nagelkerke(dog_ka_full)

####Model evaluation on the test dataset
# predict the test dataset
pred <- predict(dog_ka_full, test_dog_kept_adopt, type="response") 
predicted <- round(pred) # round of the value; >0.5 will convert to 
# 1 else 0
# Add predicted to dataset
test_dog_kept_adopt$pred <- predict(dog_ka_full, test_dog_kept_adopt, type="response") 
test_dog_kept_adopt$predicted <- round(pred) # round of the value; >0.5 will convert to 
# Creating a contigency table
tab <- table(predicted, test_dog_kept_adopt$kept_adopt)
confusionMatrix(tab, positive = '1')


# FINAL MODEL STEP 3 - GENDER, HOUSETRAINED, PUREBRED REMOVED
dog_ka_back3 <- glm(Class~age + good.with.cats + good.with.dogs + good.with.kids +
                      microchipped + needs.experienced.adopter+
                     special.needs + photo.count + dog.size + spayed.neutered +
                     rehome.deadline.2 + akc.new + rehome.reason.2 , data = down_train_dog_kept_adopt, family = "binomial")

summary(dog_ka_back3)
dog_ka_back3_summary<-tidy(dog_ka_back3, exponentiate = TRUE, conf.level = 0.95)
OR_dog_ka_back3<-exp(cbind(coef(dog_ka_back3), confint(dog_ka_back3)))

##Marginal effects
effects_dog_ka_back3 = margins(dog_ka_back3)
summary(effects_dog_ka_back3)
plot(effects_dog_ka_back3)

##Marginal means
ggpredict(dog_ka_back3) #gives you the predicted probabilities for kaion (adjusting for all other variables in the model)

# Pseudo R_squared values and Likelyhood ratio test
nagelkerke(dog_ka_back3)

####Model evaluation on the test dataset
# predict the test dataset
pred <- predict(dog_ka_back3, test_dog_kept_adopt, type="response") 
predicted <- round(pred) # round of the value; >0.5 will convert to 
# 1 else 0
# Add predicted to dataset
test_dog_kept_adopt$pred <- predict(dog_ka_back3, test_dog_kept_adopt, type="response") 
test_dog_kept_adopt$predicted <- round(pred) # round of the value; >0.5 will convert to 
# Creating a contigency table
tab <- table(predicted, test_dog_kept_adopt$kept_adopt)
confusionMatrix(tab, positive = '1')

###Odds Ratio Plot - Kept Dog
OR_kept_dog<-read.csv("OR_kept_dog.csv", stringsAsFactors = FALSE)
OR_kept_dog<-OR_kept_dog %>%
  filter(OR != "Reference")%>%
  drop_na()
OR_kept_dog$Level1[(OR_kept_dog$Level1 == "FALSE")]<-"False"
OR_kept_dog$Level1[(OR_kept_dog$Level1 == "TRUE")]<-"True"

OR_kept_dog<-OR_kept_dog %>%
  mutate(Level1= fct_relevel(Level1, 'Young', 'Adult', 'Senior','Medium', 'Large',
                             'Terrier', 'Herding', 'Sporting', 'Toy', 'Working',
                             'True','True','True','True','True', 'True',
                             '1', '2', '3', '4',
                             'Medium', 'Long', 'None', 'Abandoned or found',
                             'Housing issue', 'Human health issue', 'Personal issue',
                             'Cost issue', 'None')) 



OR_kept_dog$OR<-as.numeric(OR_kept_dog$OR)

OR_kept_dog$Category <- factor(OR_kept_dog$Category, levels = c('Age (ref = Puppy)','Dog size (ref = Small)', 'Breed group (ref = Terrier)',
                                                                    'Purebred (ref = Mixed breed)', 'Good with cats (ref = False)',
                                                                    'Good with dogs (ref = False)', 'Good with kids (ref = False)',
                                                                  'Microchipped (ref = False)','Spayed/neutered (ref = False)',
                                                                  'Needs experienced adopter (ref = False)', 'Special needs (ref = False)',
                                                                    'Photos on profile (ref = 0)', 'Rehome deadline (ref = Short)',
                                                                    'Rehome reason (ref = Animal behaviour issue)'))


OR_kept_dog$Level1 <- factor(OR_kept_dog$Level1, ordered = TRUE, levels=unique(OR_kept_dog$Level1))


kept_dog_plot<-ggplot(OR_kept_dog, aes(x = OR, y = Level1, group = Category)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = CI_97.5, xmin = CI_2.5), size = .5, height = 
                   .2, color = "gray50") +
  scale_x_continuous(limits = c(0, 3.5), expand = c(0,0) )+
  geom_point(size = 3.5, color = "royalblue4") +
  theme_bw()+
  ylab("") +
  xlab("Odds Ratio") +
  facet_grid(Category~., scales = "free", switch = "y", space = "free_y", labeller = label_wrap_gen(50))+
  theme(strip.background = element_blank(),  strip.placement = "outside", 
        panel.spacing = unit(0, "mm"),  axis.ticks.x = element_blank())+
  theme(strip.text.y.left = element_text(angle = 0), strip.text.y = element_text(
    size = 8, face = "bold"),axis.text.y = element_text(
      size = 10))+
  scale_y_discrete(limits=rev)

tiff('kept_dog_plotJULY27.tiff', units="in", width=11, height=7, res=300, compression = 'lzw')
kept_dog_plot
dev.off()

OR_kept_dog$Level1 <- factor(OR_kept_dog$Level1, ordered = TRUE, levels = c('Young', 'Adult', 'Senior', 'Purebred', 'True',
                                                                                '1', '2', '3', '4',
                                                                                'Medium', 'Long', 'None', 'Fostered, abandoned, found',
                                                                                'Housing issue', 'Human health issue', 'Personal issue',
                                                                                'Cost issue', 'None'))


OR_kept_dog$Level1 <- factor(OR_kept_dog$Level1, ordered = TRUE, levels=unique(OR_kept_dog$Level1))

####################################### KEPT/ADOPTED CATS####################################
#Create diverted (1) vs relinquished (0)
clean_cat_kept_adopt<-clean_cat%>%
  filter(status!= 'relinquished')
clean_cat_kept_adopt$kept_adopt[(clean_cat_kept_adopt$status == "keep")]<-1
clean_cat_kept_adopt$kept_adopt[(clean_cat_kept_adopt$status == "adopted")]<-0
clean_cat_kept_adopt$kept_adopt<-as.factor(clean_cat_kept_adopt$kept_adopt)

#mkae sure the training data has approximately equal proportion of classes
clean_cat_kept_adopt %>%
  group_by(kept_adopt)%>%
  dplyr::summarize(n=n())%>%
  dplyr::mutate(frequency = n / sum(n))%>%
  mutate(percent = frequency*100)
# Prep Training and Test data.
set.seed(123)
trainDataIndex <- createDataPartition(clean_cat_kept_adopt$kept_adopt, p=0.8, list = F)  
train_cat_kept_adopt <- clean_cat_kept_adopt[trainDataIndex, ]
test_cat_kept_adopt <- clean_cat_kept_adopt[-trainDataIndex, ]
# Down Sample
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)
set.seed(123)
down_train_cat_kept_adopt <- downSample(x = train_cat_kept_adopt[, colnames(train_cat_kept_adopt) %ni% "kept_adopt"],
                                        y = train_cat_kept_adopt$kept_adopt)
table(down_train_cat_kept_adopt$Class)

#turn all ordinal to factors
down_train_cat_kept_adopt$age<-factor(down_train_cat_kept_adopt$age, ordered = FALSE )
down_train_cat_kept_adopt$rehome.deadline.2<-factor(down_train_cat_kept_adopt$rehome.deadline.2, ordered = FALSE )
down_train_cat_kept_adopt$photo.count<-factor(down_train_cat_kept_adopt$photo.count, ordered = FALSE )


#FULL MODEL
cat_ka_full <- glm(Class~age + gender + good.with.cats + good.with.dogs + good.with.kids +
                     housetrained + purebred + microchipped + needs.experienced.adopter+
                     special.needs + photo.count + spayed.neutered +
                     rehome.deadline.2 +  rehome.reason.2 , data = down_train_cat_kept_adopt, family = "binomial")

summary(cat_ka_full)
cat_ka_full_summary<-tidy(cat_ka_full, exponentiate = TRUE, conf.level = 0.95)
OR_cat_ka_full<-exp(cbind(coef(cat_ka_full), confint(cat_ka_full)))

##Marginal effects 
effects_cat_ka_full = margins(cat_ka_full)
summary(effects_cat_ka_full)
plot(effects_cat_ka_full)

##Marginal means
ggpredict(cat_ka_full) #gives you the predicted probabilities for kaion (adjusting for all other variables in the model)

# Pseudo R_squared values and Likelyhood ratio test
nagelkerke(cat_ka_full)

####Model evaluation on the test dataset
# predict the test dataset
pred <- predict(cat_ka_full, test_cat_kept_adopt, type="response") 
predicted <- round(pred) # round of the value; >0.5 will convert to 
# 1 else 0
# Add predicted to dataset
test_cat_kept_adopt$pred <- predict(cat_ka_full, test_cat_kept_adopt, type="response") 
test_cat_kept_adopt$predicted <- round(pred) # round of the value; >0.5 will convert to 
# Creating a contigency table
tab <- table(predicted, test_cat_kept_adopt$kept_adopt)
confusionMatrix(tab, positive = '1')

#FINAL MODEL STEP 2 - MICROCHIPPED and good with cats, Good with dogs removed
cat_ka_back2 <- glm(Class~age + gender + good.with.kids + housetrained +
                      purebred + needs.experienced.adopter+ special.needs + photo.count + spayed.neutered +
                      rehome.deadline.2 + rehome.reason.2 , data = down_train_cat_kept_adopt, family = "binomial")

summary(cat_ka_back2)
cat_ka_back2_summary<-tidy(cat_ka_back2, exponentiate = TRUE, conf.level = 0.95)
OR_cat_ka_back2<-exp(cbind(coef(cat_ka_back2), confint(cat_ka_back2)))

##Marginal effects 
effects_cat_ka_back2 = margins(cat_ka_back2)
summary(effects_cat_ka_back2)
plot(effects_cat_ka_back2)

##Marginal means
ggpredict(cat_ka_back2) #gives you the predicted probabilities for kaion (adjusting for all other variables in the model)
ggeffect(cat_ka_back2)
# Pseudo R_squared values and Likelyhood ratio test
nagelkerke(cat_ka_back2)

####Model evaluation on the test dataset
# predict the test dataset
pred <- predict(cat_ka_back2, test_cat_kept_adopt, type="response") 
predicted <- round(pred) # round of the value; >0.5 will convert to 
# 1 else 0
# Add predicted to dataset
test_cat_kept_adopt$pred <- predict(cat_ka_back2, test_cat_kept_adopt, type="response") 
test_cat_kept_adopt$predicted <- round(pred) # round of the value; >0.5 will convert to 
# Creating a contigency table
tab <- table(predicted, test_cat_kept_adopt$kept_adopt)
confusionMatrix(tab, positive = '1')
?confusionMatrix

###Proportion of diverted animals
clean_cat_kept_adopt %>%
  group_by(rehome.reason.2, kept_adopt)%>%
  dplyr::summarise(n = n()) %>%
  mutate(frequency = n / sum(n))

###Create Odds Ratio Plot - Kept cat
OR_kept_cat<-read.csv("OR_kept_cat.csv", stringsAsFactors = FALSE)
OR_kept_cat<-OR_kept_cat %>%
  filter(OR != "Reference")
OR_kept_cat$Level1[(OR_kept_cat$Level1 == "FALSE")]<-"False"
OR_kept_cat$Level1[(OR_kept_cat$Level1 == "TRUE")]<-"True"


OR_kept_cat$OR<-as.numeric(OR_kept_cat$OR)

OR_kept_cat$Category <- factor(OR_kept_cat$Category, levels = c('Age (ref = Kitten)','Sex (ref = Female)',
                                                                  'Purebred (ref = Mixed breed)', 
                                                                  'Good with dogs (ref = False)', 'Good with kids (ref = False)',
                                                                  'Housetrained (ref = False)','Spayed/neutered (ref = False)',
                                                                  'Needs experienced adopter (ref = False)', 'Special needs (ref = False)',
                                                                  'Photos on profile (ref = 0)', 'Rehome deadline (ref = Short)',
                                                                  'Rehome reason (ref = Animal behaviour issue)'))


OR_kept_cat$Level1 <- factor(OR_kept_cat$Level1, ordered = TRUE, levels=unique(OR_kept_cat$Level1))


kept_cat_plot<-ggplot(OR_kept_cat, aes(x = OR, y = Level1, group = Category)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = CI_97.5, xmin = CI_2.5), size = .5, height = 
                   .2, color = "gray50") +
  scale_x_continuous(limits = c(0, 3.5), expand = c(0,0) )+
  geom_point(size = 3.5, color = "royalblue4") +
  theme_bw()+
  ylab("") +
  xlab("Odds Ratio") +
  facet_grid(Category~., scales = "free", switch = "y", space = "free_y", labeller = label_wrap_gen(50))+
  theme(strip.background = element_blank(),  strip.placement = "outside", 
        panel.spacing = unit(0, "mm"),  axis.ticks.x = element_blank())+
  theme(strip.text.y.left = element_text(angle = 0), strip.text.y = element_text(
    size = 8, face = "bold"),axis.text.y = element_text(
      size = 10))+
  scale_y_discrete(limits=rev)

tiff('kept_cat_plot.tiff', units="in", width=11, height=7, res=300, compression = 'lzw')
kept_cat_plot
dev.off()

