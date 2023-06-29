

---
  title: "Galls are Awesome- exploratory"
author: "Sarah Melissa Witiak, Amira Burns, and Wilmer"
date: "June 28, 2023"
output:
  html_document:
  keep_md: yes
toc: yes
toc_float: yes

---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, dev="png")
```





library(dplyr)
library(tidyr)


#Import the data
LPIGall<-read.csv("Gall_LPI_2023_1.csv", stringsAsFactors=T)
str(LPIGall)
#add treatment columns
LPIGall <- dplyr::mutate(LPIGall,
                           Fire = ifelse(PastureID == "1B" | PastureID == "2B" | PastureID == "EX-2B" | PastureID == "EX-1B",
                                         "Burn", "NoBurn"),
                           Graze = ifelse(PastureID == "1B" | PastureID == "2A", "Spring",
                                          ifelse(PastureID == "1A" | PastureID == "2B", "Fall", "NoGraze")))
LPIGall <- dplyr::relocate(LPIGall, c(Fire, Graze), .after = PastureID)
LPIGall$TopLayerCount<-1
str(LPIGall)
#summarize it by treatment

LPIGall<-LPIGall%>%dplyr::select(Transect, PastureID, Fire, Graze, TopLayer,TopLayerCount)
  
  GallSum<-LPIGall%>%
  group_by(Transect,Fire, Graze)%>%
  pivot_wider(names_from=TopLayer, 
              values_from=TopLayerCount,
              values_fn=sum,
              values_fill=0)
  write.csv(GallSum, "summarytophitcover.csv")
  
              

 GallSum<-GallSum%>%group_by(Transect,Fire,Graze)%>%
   mutate(ShrubTotal=(PUTR2+ARTR4+TECA2+CHVI+CHVI8))%>%
   mutate(TotalCover=(ShrubTotal+AF+PG+AG+PF))%>%
   mutate(PercentShrubCover=ShrubTotal/TotalCover*100)%>%
   mutate(PercentAFCover=AF/TotalCover*100, 
          PercentPFCover=PF/TotalCover*100,
          PercentPGCover=PG/TotalCover*100,
          PercentAGCover=AG/TotalCover*100,
          PercentARTR4Cover=ARTR4/TotalCover*100) %>%
   mutate(PercentShrubCover+PercentPFCover+PercentPGCover+PercentAGCover)
write.csv(GallSum, "functionalGroupCover.csv")

GallSumLong<-GallSum%>%group_by(Transect,Fire,Graze)%>%pivot_longer(cols=PercentShrubCover:PercentAGCover,
                                                           names_to="CoverType",
                                                           values_to="CoverPercent")
GallSumLong
library(ggplot2)
library(RColorBrewer)
ggplot(GallSumLong, aes(x = Graze, y = CoverPercent, fill = CoverType)) + 
  geom_bar(stat="identity") + facet_grid(cols = vars(Fire))+
                                      # Apply scale_colour_brewer function
  scale_fill_brewer(palette = "PiYG")

ggsave("CoverFunctionalGroup.png")

GallSum$TotalCover

colnames(GallSumLong)

ARTR4Coverdf<-GallSumLong%>%select(Transect, PastureID, PercentARTR4Cover)
## Gall Data Analysis


#########
## -- Set Up
#########

# load libraries
library(tidyverse) # data formatting and plotting
library(kableExtra) # pretty tables
library(ggsci) # colors
library(emmeans) # treatment contrasts
library(rstatix) # dplyr-friendly stat  calculations

# set directory if working outside RProj
#setwd("path/to/gall/data/folder")

# load data
gall_data <- readxl::read_xlsx("./Gall Data '23.xlsx", sheet = "Gall Data")

# overview df
str(gall_data)

# remove special characters from column names
colnames(gall_data) <- gsub(" ", "", colnames(gall_data))
colnames(gall_data) <- gsub("\\(", "_", colnames(gall_data))
colnames(gall_data) <- gsub("\\)", "", colnames(gall_data))

## - format variables
# turn pasture id, transect side, host species into factors
gall_data <- dplyr::mutate(gall_data,
                           PastureID = as.factor(PastureID),
                           Transectside = as.factor(Transectside),
                           HostSpecies = as.factor(HostSpecies))

# add columns for each treatment type
gall_data <- dplyr::mutate(gall_data,
                           Fire = factor(ifelse(PastureID == "1B" | PastureID == "2B" | PastureID == "EX-2B" | PastureID == "EX-1B",
                                                "Burn", "NoBurn"), levels = c("NoBurn", "Burn")),
                           Graze = factor(ifelse(PastureID == "1B" | PastureID == "2A", "Spring",
                                                 ifelse(PastureID == "1A" | PastureID == "2B", "Fall", "NoGraze")), 
                                          levels = c("NoGraze", "Spring", "Fall")),
                           Treatment = factor(paste0(Graze, "_", Fire), 
                                              levels = c("NoGraze_NoBurn", "Spring_NoBurn", "Fall_NoBurn",
                                                         "NoGraze_Burn", "Spring_Burn", "Fall_Burn")))
gall_data <- dplyr::relocate(gall_data, c(Fire, Graze, Treatment), .after = PastureID)

# replace NA values
gall_data <- dplyr::mutate_at(gall_data, vars(DaisyGall:Greenthorn), ~replace_na(., 0))

# add col for volume of plant
gall_data <- dplyr::mutate(gall_data, PlantVol_cm3 = Width_cm * Height_cm * Cross_cm * pi/6) 
gall_data <- dplyr::relocate(gall_data, PlantVol_cm3, .after = Height_cm)

# add gall totals
galls <- dplyr::select(gall_data, c(DaisyGall:Greenthorn))
gall_data$GallTotal <- rowSums(galls)

# add galls per plant volume
gall_data <- gall_data %>%
  dplyr::filter(PlantVol_cm3 != 0) %>% # remove two rows with vol=0
  dplyr::mutate(GallperVol = GallTotal/PlantVol_cm3) # calculate galls by plant vol

# add plant density
density_df <- readxl::read_xlsx("./Gall Data '23.xlsx", sheet = "Summary gall data")
density_df <- density_df %>%
  dplyr::select(c(PastureID, Transect, PlantTotal, TransectArea_m2)) %>%
  dplyr::mutate(Plants_m2 = PlantTotal / TransectArea_m2)

gall_data$Transect<-gall_data$`Transect#`
gall_data$`Transect#`=NULL
gall_data <- gall_data %>%
  dplyr::left_join(density_df, by = c("PastureID", "Transect"))

# check the data again
str(gall_data)

# create pivot df for plotting
gall_long_df <- gall_data %>%
  pivot_longer(cols = c(DaisyGall:Greenthorn),
               names_to = "GallType",
               values_to = "GallCount") %>%
  mutate(GallCountperVol = GallCount / PlantVol_cm3,
         GallPercent = GallCount / GallTotal,
         GallPercentperVol = GallPercent * PlantVol_cm3,
         GallCount_m2 = GallCount * Plants_m2) %>%
  mutate(across(GallPercent:GallPercentperVol, ~ replace(., is.nan(.), 0)))


########
## -- Treatment Effects on Gall Abundance
########

## How many plants have galls?

# create a binary factor for presence/absence of galls on a plant
gall_binary <- gall_data %>%
  dplyr::mutate(GallsPresent = factor(ifelse(GallTotal == 0, "No", "Yes")))

# summarise presence/absence per individual treatment, and across combination treatment
gall_binary %>%
  dplyr::group_by(Fire, GallsPresent) %>%
  dplyr::summarize(PlantCount = n()) %>%
  dplyr::mutate(Prop = PlantCount / sum(PlantCount))

gall_binary %>%
  dplyr::group_by(Graze, GallsPresent) %>%
  dplyr::summarize(PlantCount = n()) %>%
  dplyr::mutate(Prop = PlantCount / sum(PlantCount))

gall_presence <- gall_binary %>%
  dplyr::group_by(Treatment, GallsPresent) %>%
  dplyr::summarize(PlantCount = n()) %>%
  dplyr::mutate(Prop = round(PlantCount / sum(PlantCount), 3)) %>%
  dplyr::mutate(GallsPresent = ifelse(GallsPresent ==0, "No", "Yes"))

# Example of making a pretty table. 
# See kableExtra user guide for more options: https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html
gall_presence %>%
  kbl(caption = "Gall Presence by Treatment") %>% # create table and give it a title
  kable_classic_2(full_width = F) %>%  # formatting
  save_kable("./viz/gall_presence_table.png")  # save it as a .png file

# make plots of presence/absence
ggplot(gall_binary, aes(x = GallsPresent, fill = Fire)) + 
  geom_bar(position = "dodge") + 
  theme_bw() + scale_fill_startrek() + 
  facet_wrap(vars(Graze)) + 
  labs(x = "Galls Present", y = "Plant Count", title = "Gall Presence by Treatment")
ggplot(gall_binary, aes(x = GallsPresent, fill = Graze)) + 
  geom_bar(position = "dodge") + 
  theme_bw() + scale_fill_startrek() + 
  facet_wrap(vars(Fire)) + 
  labs(x = "Galls Present", y = "Plant Count", title = "Gall Presence by Treatment")
ggplot(gall_binary, aes(x = GallsPresent, fill = as.factor(Transect))) + 
  geom_bar(position = "dodge") + 
  theme_bw() + scale_fill_startrek() + 
  facet_wrap(vars(Treatment)) + 
  labs(x = "Galls Present", y = "Plant Count", title = "Gall Presence by Treatment, Transect")


#---#

## Does total number of galls per plant vary by treatment?
gall_totals <- gall_data %>% 
  dplyr::group_by(Fire, Graze, Treatment) %>% 
  dplyr::summarize(GallTotal = sum(GallTotal), PlantTotal = n(), 
                   TotalPlantVol = sum(PlantVol_cm3), MeanPlantVol = mean(PlantVol_cm3), sdPlantVol = sd(PlantVol_cm3),
                   MeanPlantDensity = mean(GallperVol), sdPlantDensity = sd(GallperVol)) %>%
  dplyr::mutate(MeanGallsperPlant = GallTotal / PlantTotal) # calculate galls per plant to account for dif sample sizes

# make table of gall totals
gall_totals %>%
  select(!Treatment) %>%
  relocate(MeanGallsperPlant, .after = PlantTotal) %>%
  kbl(caption = "Gall Summary by Treatment") %>%
  kable_minimal(full_width = F) %>%
  save_kable("./viz/gall_summary_table.png")

# perform Kruskal-Wallis rank sum test for significant differences in gall totals by treatment
# Note: Kruskal-Wallis test is a non-parametric test comparable to ANOVA, but does not make any underlying assumptions (ie linearity, normality) about the data
kruskal.test(GallTotal ~ Fire, data = gall_totals)
kruskal.test(GallTotal ~ Graze, data = gall_totals)
kruskal.test(GallTotal ~ Treatment, data = gall_totals)
kruskal.test(MeanGallsperPlant ~ Fire, data = gall_totals)
kruskal.test(MeanGallsperPlant ~ Graze, data = gall_totals)
kruskal.test(MeanGallsperPlant ~ Treatment, data = gall_totals)
# no significant differences identified

# frequency plot of gall totals by treatment   
ggplot(gall_data, aes(x = GallTotal, fill = Treatment)) + 
  geom_histogram(binwidth = 10, alpha = 0.5) + 
  theme_bw() + 
  labs(x = "Gall Total", y = "Plant Count", title = "Gall Total per Plant, by Treatment")

# density plot of gall totals by treatment (to account for different sample sizes)   
ggplot(gall_data, aes(x = GallTotal, after_stat(density), color = Treatment)) + 
  geom_freqpoly(binwidth = 50, lwd = 1.2, alpha = 0.5) + 
  theme_bw() + 
  labs(x = "Gall Total", y = "Plant Count Density", title = "Gall Total per Plant")

#---#

## Do gall totals vary between transects within treatment?
galltotals_transect <- gall_data %>%
  dplyr::select(c(Fire, Graze, Treatment, Transect, PlantVol_cm3, GallTotal, GallperVol, Plants_m2)) %>%
  group_by(Treatment, Transect, Fire, Graze) %>%  # you could add Transectside as a grouping variable to check if transect side is important
  dplyr::summarise(meanPlantVol = mean(PlantVol_cm3), 
                   meanGalls = mean(GallTotal), 
                   meanGallperVol = mean(GallperVol),
                   sumGallperTransect=sum(GallTotal),
                   ARTR4Density=mean(Plants_m2)) %>%
  dplyr::arrange(Treatment, Transect)

# Test for differences of gall totals within treatment between transects
gall_data %>% group_by(Treatment) %>% rstatix::kruskal_test(GallTotal ~ Transect) # Note: p-value of true significance is .05 / 6 = .0083 (6 levels)
gall_data %>% group_by(Fire) %>% rstatix::kruskal_test(GallTotal ~ Transect) # Note: p-value of true significance is .05 / 2 = .025 (2 levels)
gall_data %>% group_by(Graze) %>% rstatix::kruskal_test(GallTotal ~ Transect) # Note: p-value of true significance is .05 / 3 = .017 (3 levels)

# Test for differences of gall per plant volumne within treatment between transects
# Note: p-value of true significance is .05 / 6 = .0083 (6 treatments)
gall_data %>% group_by(Treatment) %>% rstatix::kruskal_test(GallperVol ~ Transect)
gall_data %>% group_by(Fire) %>% rstatix::kruskal_test(GallperVol ~ Transect)
gall_data %>% group_by(Graze) %>% rstatix::kruskal_test(GallperVol ~ Transect)

# patterns look similar between measures of total galls and galls per volume

# visualize gall totals within treatment by transect
ggplot(gall_data, aes(x = Transect, y = GallTotal)) + 
  geom_col() + 
  facet_wrap(vars(Treatment)) +
  scale_fill_aaas() + 
  theme_bw() + 
  ggtitle("Gall Total By Treatment, Transect")
# most galls come from east side of transects

#---#

# get number of gall types
n_galls <- length(unique(gall_long_df$GallType))

# average galls per plant by gall type, treatment
gall_type_counts <- gall_long_df %>%
  dplyr::group_by(Fire, Graze, GallType) %>%
  dplyr::summarize(TotalCount = sum(GallCount), MeanCount = mean(GallCount), sdCount = sd(GallCount),
                   TotalDensity = sum(GallCount)/sum(PlantVol_cm3), MeanDensity = mean(GallCountperVol), sdDensity = sd(GallCountperVol),
                   MeanPercent = mean(GallPercent), sdPercent = sd(GallPercent), 
                   MeanPercentperVol = mean(GallPercentperVol), sdPercentperVol = sd(GallPercentperVol))

# playing with colors and themes in plots
# gall counts by treatment
ggplot(gall_long_df, aes(x = Graze, y = GallCount, fill = GallType)) + 
  geom_col() + 
  facet_grid(cols = vars(Fire), scales = "free_x") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_fill_ucscgb() +
  ggtitle("Total Galls by Treatment and Gall Type")

ggplot(gall_long_df, aes(x = Graze, y = GallPercentperVol, fill = GallType)) + 
  geom_col() + 
  facet_grid(cols = vars(Fire), scales = "free_x") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_fill_ucscgb() +
  labs(x = "Treament", y = "Gall Count per cm^3")+ 
  ggtitle("Gall per Plant Density by Treatment and Gall Type")


ggplot(gall_long_df, aes(x = Treatment, y = GallCount, fill = GallType)) + 
  geom_col() + 
  facet_grid(cols = vars(Graze), scales = "free_x") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_fill_ucscgb() +
  ggtitle("Total Galls by Treatment and Gall Type")

# call counts by fire treatment
ggplot(gall_long_df, aes(x = Fire, y = GallCount, fill = GallType)) + 
  geom_col() + 
  theme_minimal() +
  scale_fill_igv(palette = "default") +
  ggtitle("Total Galls per Fire Treatment, by Gall Type")

# gall counts by graze treatment
ggplot(gall_long_df, aes(x = Graze, y = GallCount, fill = GallType)) + 
  geom_col() + 
  theme_light() +
  scale_fill_d3(palette = "category20b") +
  ggtitle("Total Galls per Graze Treatment, by Gall Type")

# gall counts by treatment combo
ggplot(gall_long_df, aes(x = Treatment, y = GallCount, fill = GallType)) + 
  geom_col() + 
  theme_light() +
  scale_fill_d3(palette = "category20b") +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
  ggtitle("Total Galls per Graze:Fire Treatment, by Gall Type")




########
## -- Effects of Available Host Plant Material on Gall Abundance
########

# Are there more galls on larger plants?
# look at correlation between galltotal and plant volume
cor(gall_data$PlantVol_cm3, gall_data$GallTotal)
cor(gall_data$PlantVol_cm3, gall_data$GallTotal, method = "kendall")
cor(gall_data$PlantVol_cm3, gall_data$GallTotal, method = "spearman")
# seems like there is a moderately positive correlation

# look at average gall per plant volume by treatments
gall_data %>%
  group_by(Fire, Graze) %>%
  filter(PlantVol_cm3 != 0) %>%
  summarize(avg.gall = mean(GallperVol))

ggplot(gall_data, (aes(x = Fire, y = GallperVol, fill = Graze))) + 
  geom_violin(draw_quantiles = TRUE) + 
  ggtitle("Total Galls per Plant Volume by Treatment")

ggplot(gall_long_df, aes(x = Graze, y = GallCountperVol, fill = GallType)) + 
  geom_col() + facet_grid(cols = vars(Fire)) + 
  ggtitle("Total Galls per Plant Volume by Fire, Graze Treatments and Gall Type")

ggplot(gall_long_df, aes(x = Graze, y = GallCount, fill = GallType)) + 
  geom_col() + facet_grid(cols = vars(Fire)) + 
  ggtitle("Total Gall Counts by Fire, Graze Treatments and Gall Type")

ggplot(gall_long_df, aes(x = PlantVol_cm3, y = GallCount)) + 
  geom_point(aes(col = Treatment)) + 
  facet_wrap(vars(GallType))

#join the cover data


 Gdf<-gall_long_df %>%
  dplyr::left_join(ARTR4Coverdf, by = c("PastureID", "Transect"))%>%
   select(-ends_with(".x")) %>% rename(Fire=Fire.y, Graze=Graze.y) 

 
 #average percent artr4 cover per transect determined by LPI
 
 ARTR4CoverT<-Gdf%>%group_by(PastureID,Transect, Fire, Graze, Treatment)%>%
   summarise(ARTR4CoverT=mean(PercentARTR4Cover))%>%
   select(PastureID, Treatment,
          Transect, Fire, Graze, ARTR4CoverT, ARTR4Count)

   #need to count total number of plants
 
 galltotals_transect<-galltotals_transect %>%
   left_join(ARTR4CoverT, by=c("Treatment", "Transect"))
 


galltotals_transect <- dplyr::mutate_at(galltotals_transect, vars(ARTR4CoverT),
                                        ~replace_na(., 0))
###

#PLot Gall per transect x ARTR4 density
 ggplot(galltotals_transect, aes(x=ARTR4Density, y=sumGallperTransect))+
   geom_point()+
   geom_smooth(method=lm)+
   facet_wrap(~Treatment)

 #Plot Cover of ARTR4 by mean Galls
 ggplot(galltotals_transect, aes(x=ARTR4CoverT, y=meanGalls))+
   geom_point()+
   geom_smooth(method=lm)+
   facet_wrap(~Treatment)
 #does plant volume matter
 ggplot(galltotals_transect, aes(x=meanPlantVol, y=meanGalls))+
   geom_point()+
   geom_smooth(method=lm)+
   facet_wrap(~Treatment)
 
 
 ggplot(galltotals_transect, aes(x=Fire.x, y=meanGalls))+
   geom_point()+
   geom_col()+
   geom_violin()+
   geom_smooth(method=lm)
 
 #What we learned is: we don't see much at the transect level and we want to work with
 #the plant level. That's this dataframe: gall_data. We also learned that we 
 #didn't see a relationship between cover of artr4 and gall otucomes
 #we also learned we want to take young galls out or sep. to see patterns
 #we want to see gall totals and gall sp/community data by plant.
 
 
 ggplot(gall_data, aes(x=Treatment, y=GallTotal))+
   geom_point()+
   geom_col()+
   geom_violin()+
   geom_smooth(method=lm)
 
 #identify mature galls
 #DaisyGall
 #MedusaGall
 #TinyBudGall_Vase
 #BunnyEarsGall
 #FuzzyBallGall+MultiocularBallGall
 #EchidnaGall
 #Greenthorn+ThornGall
 #StemGall
 #SmoothBallGall
 #BulbousStem
 #FlatDaisy
 #BudGall
 #Too young to id: TeenyTinyLeafGall+TeenyTinyBabyFuzzyBallGall+BabyBinnyEarsGall
 
 #give the plants a unique ID
gall_data<- dplyr::mutate(gall_data, PlantID = row_number())

 #Mutate to create new columns

  #lump some gall species together
 gall_data2<- gall_data%>%
  mutate(Fuzzy=FuzzyBallGall+MultiocularBallGall,
         Thorn= Greenthorn+ThornGall, 
        Baby=TeenyTinyLeafGall+TeenyTinyBabyFuzzyBallGall+BabyBunnyEarsGall, .keep="unused")
 
 #create mature gall column
 gall_data2<-gall_data2%>%
   mutate(MatureGallTotal=GallTotal-Baby)
#move the columns around
 gall_data2 <- dplyr::relocate(gall_data2, c(GallperVol, Transect, PlantTotal, TransectArea_m2, Plants_m2, GallTotal), .after = HostSpecies)

 #add a plant ID column
 gall_data2 <- dplyr::relocate(gall_data2, (PlantID), .after = Date)

 
 ggplot(gall_data2, aes(x=Fire, y=Baby, color=Graze))+
geom_jitter()+
   geom_boxplot()
   geom_smooth(method=lm) 
 
 ggplot(gall_data2, aes(x=Graze, y=Thorn, color=Fire))+
   geom_jitter()+
   facet_grid(~Fire)
   geom_smooth(method=lm)

   ggplot(gall_data2, aes(x=Graze, y=DaisyGall, color=Fire))+
   
     geom_jitter()+
     geom_boxplot()+
     facet_grid(~Fire)
   geom_smooth(method=lm)
   
   
   ggplot(gall_data2, aes(x=Graze, y=MedusaGall, color=Fire))+
     geom_jitter()+
     facet_grid(~Fire)
   geom_smooth(method=lm)
   
   ggplot(gall_data2, aes(x=Graze, y=TinyBudGall_Vase, color=Fire))+
     geom_jitter()+
     facet_grid(~Fire)
   geom_smooth(method=lm)
   
   ggplot(gall_data2, aes(x=PlantVol_cm3, y=MatureGallTotal, color=Fire))+
     geom_jitter()+
     facet_grid(Fire~Graze)+
   geom_smooth()
   
   
   #remove rows with 0 total galls
   
 gall3<-gall_data2%>%
   filter(MatureGallTotal!=0)
 
 
 ggplot(gall3, aes(x=PlantVol_cm3, y=MatureGallTotal, color=Fire))+
   geom_jitter()+
   facet_grid(Fire~Graze)+
   geom_smooth()
  gall3$MatureGallTotal
  
  gall3<-gall3%>% rowwise() %>% 
  filter(sum(c(DaisyGall:Thorn)) != 0)
  
 gallSpecies<-gall3%>%select(DaisyGall:Thorn)%>%
 rowwise() %>% 
   filter(sum(c(DaisyGall:Thorn)) != 0)
 gallFunctional<-gall3%>%
   mutate(LeafGalls=SmoothBallGall+BunnyEarsGall+DaisyGall+Fuzzy+Thorn,
          StemGalls=BulbousStem+StemGall,
          BudGalls=EchidnaGall+FlatDaisy+BudGall+TinyBudGall_Vase+MedusaGall, .keep="unused")
   

 library(vegan)  
gallOrd<-metaMDS(gallSpecies, k=3)
#ef<-envfit(gallOrd, gallFunctional, display="sites")
ef<-envfit(gallOrd ~LeafGalls+BudGalls+PlantVol_cm3, gallFunctional, display="sites")
(ef)

plot.new()
scl<-3
colvec <- c("purple", "green4", "mediumblue")
colvec2<-c("red", "black", "yellow")

plot(gallOrd, type="n")
with(gallFunctional, points(gallOrd, display = "sites", col = colvec[Graze],
                      pch = 15, bg = colvec[Graze]))

#text(gallOrd, display="species", col=colvec[gall3$Graze],choices=c(1,2))

ordihull(gallOrd, gall3$Graze, display="sites",
            draw=c("lines"),
             col=colvec2[((gallFunctional$Graze))], alpha=60, label=TRUE,choices=c(1,2))
plot(ef, col="grey")
with(gallFunctional, legend("topright", legend = levels(Graze), bty = "n",
                      col = colvec, pch = 15, pt.bg = colvec))

summary(gallOrd)

 #points(gallOrd,display="species",col="red",air=0.01)



adonis2(gallSpecies ~ Fire+Graze+Fire*Graze+PlantVol_cm3, gall3)
library(RVAideMemoire)
pairwise.factorfit(gallOrd, gall3$Treatment, perm=499)

simper(gallSpecies, group=gall3$Treatment)


 
gallFunctional<-gall3%>%
  mutate(LeafGalls=SmoothBallGall+BunnyEarsGall+DaisyGall+Fuzzy+Thorn,
         StemGalls=BulbousStem+StemGall,
         BudGalls=EchidnaGall+FlatDaisy+BudGall+TinyBudGall_Vase+MedusaGall, .keep="unused")
gallFunctional$StemGalls

#gallFunctional<-select(gallFunctional, -StemGalls)

gallFunctionalSp<-gallSpecies%>%  mutate(LeafGalls=SmoothBallGall+BunnyEarsGall+DaisyGall+Fuzzy+Thorn,
                                         StemGalls=BulbousStem+StemGall,
                                         BudGalls=EchidnaGall+FlatDaisy+BudGall+TinyBudGall_Vase+MedusaGall, .keep="unused")
#gallFunctionalSp<-select(gallFunctionalSp, -StemGalls)

ggplot(gallFunctional, aes(x=Graze, y=LeafGalls, color=Fire))+
  geom_jitter()+
  geom_violin()+
  stat_summary(fun = "mean",
               geom = "point",
               color = "red")+
  facet_grid(.~Fire)

ggplot(gallFunctional, aes(x=Treatment, y=BudGalls, color=Graze))+
  geom_point()+
  geom_violin()
geom_smooth()+
  theme_classic()

ggplot(gallFunctional, aes(x=LeafGalls, y=GallperVol, color=Graze))+
  geom_point()+
  geom_smooth()+
  theme_classic()

gallFunctional<-gallFunctional%>%
  dplyr::mutate(BudtoLeaf=(BudGalls/LeafGalls))

ggplot(gallFunctional, aes(x=Treatment, y=BudtoLeaf, color=Graze))+
  geom_point()+
  geom_smooth()
  theme_classic()
stat_summary(fun = "mean",
               geom = "point",
               color = "red")
  facet_grid(~Fire)
  

