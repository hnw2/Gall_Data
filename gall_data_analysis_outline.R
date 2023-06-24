## Gall Data Analysis


#########
## -- Set Up
#########

# load libraries
library(tidyverse) # data formatting and plotting
library(kableExtra) # pretty tables
library(ggsci) # colors
library(emmeans) # treatment contrasts

# set directory 
setwd("path/to/gall/data/folder")

# load data
# gall_data <- readxl::readxlsx("~/Gall Data '23.xlsx", sheet = "Gall Data")
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
                           Fire = factor(ifelse(PastureID == "1B" | PastureID == "2B" | PastureID == "EX-2B",
                                         "Burn", "NoBurn")),
                           Graze = factor(ifelse(PastureID == "1B" | PastureID == "2A", "Spring",
                                          ifelse(PastureID == "1A" | PastureID == "2B", "Fall", "NoGraze"))),
                           Treatment = factor(paste0(Graze, "_", Fire)))
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
  dplyr::mutate(GallperVol = GallTotal/PlantVol_cm3) %>%
  dplyr::filter(PlantVol_cm3 != 0)

# check the data again
str(gall_data)

# create pivot df for plotting
gall_plot_df <- gall_data %>%
  pivot_longer(cols = c(DaisyGall:Greenthorn),
               names_to = "GallType",
               values_to = "GallCount") %>%
  mutate(GallCountperVol = GallCount / PlantVol_cm3)


########
## -- Treatment Effects on Gall Abundance
########

## How many plants have galls?

# create a binary factor for presence/absence of galls on a plant
gall_binary <- gall_data %>%
   dplyr::mutate(GallsPresent = factor(ifelse(GallTotal == 0, 0, 1)))

# summarise presence/absense per individual treatment, and across combination treatment
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
  dplyr::mutate(Prop = PlantCount / sum(PlantCount))


# Does total number of galls per plant vary by treatment?
galltotals_df <- gall_data %>% 
  dplyr::group_by(Graze, Fire) %>%
  count(GallTotal) %>%
  dplyr::rename(N_Plants = n)

# frequency plot of gall totals by treatment   
ggplot(gall_data, aes(x = GallTotal, color = Treatment)) + 
  geom_freqpoly(binwidth = 50, lwd = 1.2, alpha = 0.5) + 
  theme_bw() + 
  labs(x = "Gall Total", y = "Plant Count", title = "Gall Total per Plant")

# density plot of gall totals by treatment (to account for different sample sizes)   
ggplot(gall_data, aes(x = GallTotal, after_stat(density), color = Treatment)) + 
  geom_freqpoly(binwidth = 50, lwd = 1.2, alpha = 0.5) + 
  theme_bw() + 
  labs(x = "Gall Total", y = "Plant Count Density", title = "Gall Total per Plant")

# Are there variations in gall type across treatment? 
gall_type_counts <- gall_plot_df %>%
  dplyr::group_by(Treatment, GallType) %>%
  dplyr::count(GallCount)

# playing with colors and themes in plots
# gall counts by pasture
ggplot(gall_plot_df, aes(x = PastureID, y = GallCount, fill = GallType)) + 
  geom_col() + 
  facet_grid(cols = vars(Fire), scales = "free_x") + 
  theme_bw() +
  scale_fill_ucscgb() +
  ggtitle("Total Galls per Pasture, by Fire Treatment and Gall Type")

# call counts by fire treatment
ggplot(gall_plot_df, aes(x = Fire, y = GallCount, fill = GallType)) + 
  geom_col() + 
  theme_minimal() +
  scale_fill_igv(palette = "default") +
  ggtitle("Total Galls per Fire Treatment, by Gall Type")

# gall counts by graze treatment
ggplot(gall_plot_df, aes(x = Graze, y = GallCount, fill = GallType)) + 
  geom_col() + 
  theme_light() +
  scale_fill_d3(palette = "category20b") +
  ggtitle("Total Galls per Graze Treatment, by Gall Type")

# gall counts by treatment combo
ggplot(gall_plot_df, aes(x = Treatment, y = GallCount, fill = GallType)) + 
  geom_col() + 
  theme_light() +
  scale_fill_d3(palette = "category20b") +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
  ggtitle("Total Galls per Graze:Fire Treatment, by Gall Type")

# average gall count and galls per plant vol by gall type
gall_averages <- gall_plot_df %>%
  group_by(Fire, Graze, GallType) %>%
  summarize(avgpervol = mean(GallCountperVol),
            avgct = mean(GallCount)) %>%
  mutate(Treatment = paste(Fire, Graze, sep = "_"))




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

ggplot(daisies, aes(x = Fire, y = GallCountperVol, fill = Graze)) + 
  geom_violin(draw_quantiles = TRUE) + 
  ggtitle("Daisy Gall Count per Plant Volume by Treatment")

ggplot(gall_plot_df, aes(x = Graze, y = GallCountperVol, fill = GallType)) + 
  geom_col() + facet_grid(cols = vars(Fire)) + 
  ggtitle("Total Galls per Plant Volume by Fire, Graze Treatments and Gall Type")

ggplot(gall_plot_df, aes(x = Graze, y = GallCount, fill = GallType)) + 
  geom_col() + facet_grid(cols = vars(Fire)) + 
  ggtitle("Total Gall Counts by Fire, Graze Treatments and Gall Type")

