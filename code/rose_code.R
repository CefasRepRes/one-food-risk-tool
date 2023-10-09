#### Rose diagram ####

# Load packages
library(tidyverse)
library(viridis)
library(readxl)
library(here)
library(data.table)

# Import synthetic data
hazard_scores <- read_xlsx(here::here("data",
                                    "data_structure.xlsx")) %>% data.table()

# By sector --------------------------------------------------------------------

sector_cols <- c("#56B4E9",
                          "#009E73",
                          "#0072B2",
                          "#F0E442")

names(sector_cols) <- c("Aquaculture",
                        "Crops",
                        "Fisheries",
                        "Livestock")

## Category by sector ==========================================================

ggplot(data = hazard_scores) +
  geom_hline(yintercept = seq(0, 400, 100), colour = "darkgrey") +
  geom_bar(aes(x = Category, y = Score, fill = Sector), stat = "identity", alpha = 0.8) +
  coord_polar() +
  scale_fill_manual(values = sector_cols) +
  scale_y_continuous(limits = c(-10, 500), expand = c(0, 50)) +
  coord_polar() +
  theme_light() +
  ggtitle("Hazard categories by sector") +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 15))

## Hazards by sector ===========================================================

biological_hazards <- hazard_scores[Category == "Biological"]
ggplot(data = biological_hazards) +
  geom_hline(yintercept = seq(0, 90, 10), colour = "darkgrey") +
  geom_bar(aes(x = str_wrap(Hazard, 5), y = Score, fill = Sector), stat = "identity", alpha = 0.8) +
  coord_polar() +
  scale_fill_manual(values = sector_cols) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 10)) +
  coord_polar() +
  theme_light() +
  ggtitle("Biological hazards by sector") +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 15))


