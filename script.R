# install.packages(c("tidyverse","janitor", "ggtext", "ragg"))
# install.packages("twriTemplates", repos = c(
#   txwri = 'https://txwri.r-universe.dev',
#   CRAN = 'https://cloud.r-project.org'))

library(tidyverse)
library(twriTemplates)
library(readr)
library(janitor)
library(ggtext)
library(ragg)

# ************LIST OF UNIQUE PARAMETERS ******************************
#   00010 TEMPERATURE, WATER (DEGREES CENTIGRADE)
# 00061 FLOW  STREAM, INSTANTANEOUS (CUBIC FEET PER SEC)
# 00076 TURBIDITY,HACH TURBIDIMETER (FORMAZIN TURB UNIT)
# 00094 SPECIFIC CONDUCTANCE,FIELD (US/CM @ 25C)
# 00300 OXYGEN, DISSOLVED (MG/L)
# 00400 PH (STANDARD UNITS)
# 00530 RESIDUE, TOTAL NONFILTRABLE (MG/L)
# 00600 NITROGEN, TOTAL (MG/L AS N)
# 00610 NITROGEN, AMMONIA, TOTAL (MG/L AS N)
# 00620 NITRATE NITROGEN, TOTAL (MG/L AS N)
# 00625 NITROGEN, KJELDAHL, TOTAL (MG/L AS N)
# 00630 NITRITE PLUS NITRATE, TOTAL ONE LAB DETERMINED VALUE (MG/L AS N)
# 00665 PHOSPHORUS, TOTAL, WET METHOD (MG/L AS P)
# 00671 ORTHOPHOSPHATE PHOSPHORUS,DISS,MG/L,FLDFILT<15MIN
# 00680 CARBON, TOTAL ORGANIC, NPOC (TOC), MG/L
# 01351 FLOW SEVERITY:1=No Flow,2=Low,3=Normal,4=Flood,5=High,6=Dry
# 31616 FECAL COLIFORM,MEMBR FILTER,M-FC BROTH, #/100ML
# 31648 E. COLI, MTEC, MF, #/100 ML
# 31699 E. COLI, COLILERT, IDEXX METHOD, MPN/100ML
# 70507 ORTHOPHOSPHATE PHOSPHORUS,DISS,MG/L,FILTER >15MIN
# 72053 DAYS SINCE PRECIPITATION EVENT (DAYS)
# 74069 STREAM FLOW ESTIMATE (CFS)
# 82078 TURBIDITY,FIELD NEPHELOMETRIC TURBIDITY UNITS, N
# 82903 DEPTH OF BOTTOM OF WATER BODY AT SAMPLE SITE
# 89835 FLOW MTH 1=GAGE 2=ELEC 3=MECH 4=WEIR/FLU 5=DOPPLER

# Leon River ###############

## ecoli ###################
leon_df <- read_delim("Data/20230831_1221.txt", 
                      delim = "|", escape_double = FALSE, col_types = cols(`RFA/Tag ID` = col_skip(), 
                                                                           `End Date` = col_date(format = "%m/%d/%Y"), 
                                                                           `End Time` = col_skip(), `End Depth` = col_skip(), 
                                                                           `Start Date` = col_skip(), `Start Time` = col_skip(), 
                                                                           `Start Depth` = col_skip(), `Composite Category` = col_skip(), 
                                                                           `Composite Type` = col_skip(), Comment = col_skip(), 
                                                                           `Submitting Entity` = col_skip(), 
                                                                           `Collecting Entity` = col_skip(), 
                                                                           MDL = col_skip(), LOQ = col_skip(), 
                                                                           `Data Qualifier` = col_character(), 
                                                                           `Verify Flag` = col_skip(), `Validation Flag` = col_skip(), 
                                                                           `Exempt Status` = col_skip()), trim_ws = TRUE)
leon_df <- janitor::clean_names(leon_df)
p1 <- leon_df |> 
  mutate(AU = case_when(
    station_id == 11929 ~ "1221_04",
    station_id == 11930 ~ "1221_04",
    station_id == 18781 ~ "1221_05",
    station_id == 20905 ~ "1221_05",
    station_id == 17591 ~ "1221_06",
    station_id == 11934 ~ "1221_07",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("31648", "31699")) |> 
  ggplot() +
  geom_hline(yintercept = 126, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), method = "lm") +
  facet_wrap(~AU, ncol = 1) +
  scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "<i>E. coli</i> (MPN/100mL)",
       title = "Leon River 1121",
       subtitle = "<i>E. coli</i>") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())


ragg::agg_png(filename = "out/1221_leon_ecoli.png",
              width = 6.5,
              height = 6.5,
              units = "in",
              res = 300)
p1
dev.off()

## DO ######################

p1 <- leon_df |> 
  mutate(AU = case_when(
    station_id == 11929 ~ "1221_04",
    station_id == 11930 ~ "1221_04",
    station_id == 18781 ~ "1221_05",
    station_id == 20905 ~ "1221_05",
    station_id == 17591 ~ "1221_06",
    station_id == 11934 ~ "1221_07",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00300")) |> 
  ggplot() +
  geom_hline(yintercept = 5, linetype = 2, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value),
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "DO (mg/L)",
       title = "Leon River 1121",
       subtitle = "Dissolved Oxygen (Grab)") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())
ragg::agg_png(filename = "out/1221_leon_DO.png",
              width = 6.5,
              height = 6.5,
              units = "in",
              res = 300)
p1
dev.off()

## TP ######################

p1 <- leon_df |> 
  mutate(AU = case_when(
    station_id == 11929 ~ "1221_04",
    station_id == 11930 ~ "1221_04",
    station_id == 18781 ~ "1221_05",
    station_id == 20905 ~ "1221_05",
    station_id == 17591 ~ "1221_06",
    station_id == 11934 ~ "1221_07",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00665")) |> 
  ggplot() +
  geom_hline(yintercept = 0.69, linetype = 2, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value),
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "TP (mg/L)",
       title = "Leon River 1121",
       subtitle = "Total Phosphorus") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())
ragg::agg_png(filename = "out/1221_leon_TP.png",
              width = 6.5,
              height = 6.5,
              units = "in",
              res = 300)
p1
dev.off()

## Nitrate ################
p1 <- leon_df |> 
  mutate(AU = case_when(
    station_id == 11929 ~ "1221_04",
    station_id == 11930 ~ "1221_04",
    station_id == 18781 ~ "1221_05",
    station_id == 20905 ~ "1221_05",
    station_id == 17591 ~ "1221_06",
    station_id == 11934 ~ "1221_07",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00620")) |> 
  ggplot() +
  geom_hline(yintercept = 1.95, linetype = 2, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), 
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "NO~3~-N (mg/L)",
       title = "Leon River 1121",
       subtitle = "Nitrate") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())
ragg::agg_png(filename = "out/1221_leon_NO3.png",
              width = 6.5,
              height = 6.5,
              units = "in",
              res = 300)
p1
dev.off()


# Resley Creek ##############


## Ecoli #####################
resley_df <- read_delim("Data/20230831_1221A.txt", 
                      delim = "|", escape_double = FALSE, col_types = cols(`RFA/Tag ID` = col_skip(), 
                                                                           `End Date` = col_date(format = "%m/%d/%Y"), 
                                                                           `End Time` = col_skip(), `End Depth` = col_skip(), 
                                                                           `Start Date` = col_skip(), `Start Time` = col_skip(), 
                                                                           `Start Depth` = col_skip(), `Composite Category` = col_skip(), 
                                                                           `Composite Type` = col_skip(), Comment = col_skip(), 
                                                                           `Submitting Entity` = col_skip(), 
                                                                           `Collecting Entity` = col_skip(), 
                                                                           MDL = col_skip(), LOQ = col_skip(), 
                                                                           `Data Qualifier` = col_character(), 
                                                                           `Verify Flag` = col_skip(), `Validation Flag` = col_skip(), 
                                                                           `Exempt Status` = col_skip()), trim_ws = TRUE)

resley_df <- janitor::clean_names(resley_df)

p1 <- resley_df |> 
  mutate(AU = case_when(
    station_id == 11808 ~ "1221A_01",
    station_id == 17377 ~ "1221A_01",
    station_id == 17477 ~ "1221A_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("31648", "31699")) |> 
  ggplot() +
  geom_hline(yintercept = 126, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), method = "lm") +
  facet_wrap(~AU, ncol = 1) +
  scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "<i>E. coli</i> (MPN/100mL)",
       title = "Resley Creek 1121A",
       subtitle = "<i>E. coli</i>") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221A_resley_ecoli.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()

## DO #####################
p1 <- resley_df |> 
  mutate(AU = case_when(
    station_id == 11808 ~ "1221A_01",
    station_id == 17377 ~ "1221A_01",
    station_id == 17477 ~ "1221A_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00300")) |> 
  ggplot() +
  geom_hline(yintercept = 2, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), 
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "DO (mg/L)",
       title = "Resley Creek 1121A",
       subtitle = "Dissolved Oxygen (Grab)") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221A_resley_DO.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()

## TP ###################

p1 <- resley_df |> 
  mutate(AU = case_when(
    station_id == 11808 ~ "1221A_01",
    station_id == 17377 ~ "1221A_01",
    station_id == 17477 ~ "1221A_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00665")) |> 
  ggplot() +
  geom_hline(yintercept = 0.69, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value),
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "TP (mg/L)",
       title = "Resley Creek 1121A",
       subtitle = "Total Phosophorus") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221A_resley_TP.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()

## Nitrate #####################
p1 <- resley_df |> 
  mutate(AU = case_when(
    station_id == 11808 ~ "1221A_01",
    station_id == 17377 ~ "1221A_01",
    station_id == 17477 ~ "1221A_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00620")) |> 
  ggplot() +
  geom_hline(yintercept = 1.95, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), 
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "NO~3~-N (mg/L)",
       title = "Resley Creek 1121A",
       subtitle = "Nitrate") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221A_resley_NO3.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()

# South Leon ##############

sleon_df <- read_delim("Data/20230831_1221B.txt", 
                        delim = "|", escape_double = FALSE, col_types = cols(`RFA/Tag ID` = col_skip(), 
                                                                             `End Date` = col_date(format = "%m/%d/%Y"), 
                                                                             `End Time` = col_skip(), `End Depth` = col_skip(), 
                                                                             `Start Date` = col_skip(), `Start Time` = col_skip(), 
                                                                             `Start Depth` = col_skip(), `Composite Category` = col_skip(), 
                                                                             `Composite Type` = col_skip(), Comment = col_skip(), 
                                                                             `Submitting Entity` = col_skip(), 
                                                                             `Collecting Entity` = col_skip(), 
                                                                             MDL = col_skip(), LOQ = col_skip(), 
                                                                             `Data Qualifier` = col_character(), 
                                                                             `Verify Flag` = col_skip(), `Validation Flag` = col_skip(), 
                                                                             `Exempt Status` = col_skip()), trim_ws = TRUE)

sleon_df <- janitor::clean_names(sleon_df)

p1 <- sleon_df |> 
  mutate(AU = case_when(
    station_id == 11817 ~ "1221B_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("31648", "31699")) |> 
  ggplot() +
  geom_hline(yintercept = 126, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), method = "lm") +
  facet_wrap(~AU, ncol = 1) +
  scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "<i>E. coli</i> (MPN/100mL)",
       title = "South Leon 1121B",
       subtitle = "<i>E. coli</i>") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221B_south_leon_ecoli.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()

## DO ####################
p1 <- sleon_df |> 
  mutate(AU = case_when(
    station_id == 11817 ~ "1221B_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00300")) |> 
  ggplot() +
  geom_hline(yintercept = 5, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), 
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "DO (mg/L)",
       title = "South Leon 1121B",
       subtitle = "Dissolved Oxygen (Grab)") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221B_south_leon_DO.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()

## TP ###################
p1 <- sleon_df |> 
  mutate(AU = case_when(
    station_id == 11817 ~ "1221B_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00665")) |> 
  ggplot() +
  #geom_hline(yintercept = 5, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), 
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "TP (mg/L)",
       title = "South Leon 1121B",
       subtitle = "Total Phosphorus") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221B_south_leon_TP.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()

## Nitrate #################
p1 <- sleon_df |> 
  mutate(AU = case_when(
    station_id == 11817 ~ "1221B_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00620")) |> 
  ggplot() +
  geom_hline(yintercept = 1.95, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "NO~3~ (mg/L)",
       title = "South Leon 1121B",
       subtitle = "Nitrate") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221B_south_leon_NO3.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()

# Pecan ##################

## ecoli

pecan_df <- read_delim("Data/20230831_1221C.txt", 
                       delim = "|", escape_double = FALSE, col_types = cols(`RFA/Tag ID` = col_skip(), 
                                                                            `End Date` = col_date(format = "%m/%d/%Y"), 
                                                                            `End Time` = col_skip(), `End Depth` = col_skip(), 
                                                                            `Start Date` = col_skip(), `Start Time` = col_skip(), 
                                                                            `Start Depth` = col_skip(), `Composite Category` = col_skip(), 
                                                                            `Composite Type` = col_skip(), Comment = col_skip(), 
                                                                            `Submitting Entity` = col_skip(), 
                                                                            `Collecting Entity` = col_skip(), 
                                                                            MDL = col_skip(), LOQ = col_skip(), 
                                                                            `Data Qualifier` = col_character(), 
                                                                            `Verify Flag` = col_skip(), `Validation Flag` = col_skip(), 
                                                                            `Exempt Status` = col_skip()), trim_ws = TRUE)

pecan_df <- janitor::clean_names(pecan_df)

p1 <- pecan_df |> 
  mutate(AU = case_when(
    station_id == 17547 ~ "1221C_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("31648", "31699")) |> 
  ggplot() +
  geom_hline(yintercept = 126, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), method = "lm") +
  facet_wrap(~AU, ncol = 1) +
  scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "<i>E. coli</i> (MPN/100mL)",
       title = "Pecan Creek 1121C",
       subtitle = "<i>E. coli</i>") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221C_pecan_ecoli.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()

## DO

p1 <- pecan_df |> 
  mutate(AU = case_when(
    station_id == 17547 ~ "1221C_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00300")) |> 
  ggplot() +
  geom_hline(yintercept = 4, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "DO (mg/L)",
       title = "Pecan Creek 1121C",
       subtitle = "Dissolved Oxygen (Grab)") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221C_pecan_DO.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()

## TP
p1 <- pecan_df |> 
  mutate(AU = case_when(
    station_id == 17547 ~ "1221C_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00665")) |> 
  ggplot() +
  #geom_hline(yintercept = 4, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), 
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "TP (mg/L)",
       title = "Pecan Creek 1121C",
       subtitle = "Total Phosphorus") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221C_pecan_TP.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()


## Nitrate
p1 <- pecan_df |> 
  mutate(AU = case_when(
    station_id == 17547 ~ "1221C_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00620")) |> 
  ggplot() +
  geom_hline(yintercept = 1.95, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value),
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "NO~3~ (mg/L)",
       title = "Pecan Creek 1121C",
       subtitle = "Nitrate") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221C_pecan_NO3.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()



# Indian Creek ###############

## ecoli
indian_df <- read_delim("Data/20230831_1221D.txt", 
                       delim = "|", escape_double = FALSE, col_types = cols(`RFA/Tag ID` = col_skip(), 
                                                                            `End Date` = col_date(format = "%m/%d/%Y"), 
                                                                            `End Time` = col_skip(), `End Depth` = col_skip(), 
                                                                            `Start Date` = col_skip(), `Start Time` = col_skip(), 
                                                                            `Start Depth` = col_skip(), `Composite Category` = col_skip(), 
                                                                            `Composite Type` = col_skip(), Comment = col_skip(), 
                                                                            `Submitting Entity` = col_skip(), 
                                                                            `Collecting Entity` = col_skip(), 
                                                                            MDL = col_skip(), LOQ = col_skip(), 
                                                                            `Data Qualifier` = col_character(), 
                                                                            `Verify Flag` = col_skip(), `Validation Flag` = col_skip(), 
                                                                            `Exempt Status` = col_skip()), trim_ws = TRUE)

indian_df <- janitor::clean_names(indian_df)

p1 <- indian_df |> 
  mutate(AU = case_when(
    station_id == 11818 ~ "1221D_01",
    station_id == 17542 ~ "1221D_02",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("31648", "31699")) |> 
  ggplot() +
  geom_hline(yintercept = 126, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), method = "lm") +
  facet_wrap(~AU, ncol = 1) +
  scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "<i>E. coli</i> (MPN/100mL)",
       title = "Indian Creek 1121D",
       subtitle = "<i>E. coli</i>") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221D_indian_ecoli.png",
              width = 6.5,
              height = 6.5,
              units = "in",
              res = 300)
p1
dev.off()

## DO

p1 <- indian_df |> 
  mutate(AU = case_when(
    station_id == 11818 ~ "1221D_01",
    station_id == 17542 ~ "1221D_02",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00300")) |> 
  ggplot() +
  geom_hline(yintercept = 5, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value),
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "DO (mg/L)",
       title = "Indian Creek 1121D",
       subtitle = "Dissolved Oxygen (Grab)") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221D_indian_DO.png",
              width = 6.5,
              height = 6.5,
              units = "in",
              res = 300)
p1
dev.off()

## TP #########################

p1 <- indian_df |> 
  mutate(AU = case_when(
    station_id == 11818 ~ "1221D_01",
    station_id == 17542 ~ "1221D_02",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00665")) |> 
  ggplot() +
  geom_hline(yintercept = 0.69, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), 
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "TP (mg/L)",
       title = "Indian Creek 1121D",
       subtitle = "Total Phosphorus") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221D_indian_TP.png",
              width = 6.5,
              height = 6.5,
              units = "in",
              res = 300)
p1
dev.off()

## Nitrate #################
p1 <- indian_df |> 
  mutate(AU = case_when(
    station_id == 11818 ~ "1221D_01",
    station_id == 17542 ~ "1221D_02",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00620")) |> 
  ggplot() +
  geom_hline(yintercept = 1.95, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value),
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1,
             scales = "free_y") +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "NO~3~ (mg/L)",
       title = "Indian Creek 1121D",
       subtitle = "Nitrate") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221D_indian_NO3.png",
              width = 6.5,
              height = 6.5,
              units = "in",
              res = 300)
p1
dev.off()

# Plum Creek ###############

## ecoli ##################

plum_df <- read_delim("Data/20230831_1221E.txt", 
                        delim = "|", escape_double = FALSE, col_types = cols(`RFA/Tag ID` = col_skip(), 
                                                                             `End Date` = col_date(format = "%m/%d/%Y"), 
                                                                             `End Time` = col_skip(), `End Depth` = col_skip(), 
                                                                             `Start Date` = col_skip(), `Start Time` = col_skip(), 
                                                                             `Start Depth` = col_skip(), `Composite Category` = col_skip(), 
                                                                             `Composite Type` = col_skip(), Comment = col_skip(), 
                                                                             `Submitting Entity` = col_skip(), 
                                                                             `Collecting Entity` = col_skip(), 
                                                                             MDL = col_skip(), LOQ = col_skip(), 
                                                                             `Data Qualifier` = col_character(), 
                                                                             `Verify Flag` = col_skip(), `Validation Flag` = col_skip(), 
                                                                             `Exempt Status` = col_skip()), trim_ws = TRUE)

plum_df <- janitor::clean_names(plum_df)

p1 <- plum_df |> 
  mutate(AU = case_when(
    station_id == 18405 ~ "1221E_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("31648", "31699")) |> 
  ggplot() +
  geom_hline(yintercept = 126, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), method = "lm") +
  facet_wrap(~AU, ncol = 1) +
  scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "<i>E. coli</i> (MPN/100mL)",
       title = "Plum Creek 1121E",
       subtitle = "<i>E. coli</i>") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221E_plum_ecoli.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()

## DO #####################
p1 <- plum_df |> 
  mutate(AU = case_when(
    station_id == 18405 ~ "1221E_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00300")) |> 
  ggplot() +
  geom_hline(yintercept = 5, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "DO (mg/mL)",
       title = "Plum Creek 1121E",
       subtitle = "Dissolved Oxygen (Grab)") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221E_plum_DO.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()


## TP ######################
p1 <- plum_df |> 
  mutate(AU = case_when(
    station_id == 18405 ~ "1221E_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00665")) |> 
  ggplot() +
  geom_hline(yintercept = 0.69, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), 
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "TP (mg/mL)",
       title = "Plum Creek 1121E",
       subtitle = "Total Phosphorus") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221E_plum_TP.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()

## Nitrate #####################
p1 <- plum_df |> 
  mutate(AU = case_when(
    station_id == 18405 ~ "1221E_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00620")) |> 
  ggplot() +
  geom_hline(yintercept = 1.95, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value),
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "NO~3~-N (mg/mL)",
       title = "Plum Creek 1121E",
       subtitle = "Nitrate") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221E_plum_NO3.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()


# Walnut Creek ###############

## ecoli #####################

walnut_df <- read_delim("Data/20230831_1221F.txt", 
                      delim = "|", escape_double = FALSE, col_types = cols(`RFA/Tag ID` = col_skip(), 
                                                                           `End Date` = col_date(format = "%m/%d/%Y"), 
                                                                           `End Time` = col_skip(), `End Depth` = col_skip(), 
                                                                           `Start Date` = col_skip(), `Start Time` = col_skip(), 
                                                                           `Start Depth` = col_skip(), `Composite Category` = col_skip(), 
                                                                           `Composite Type` = col_skip(), Comment = col_skip(), 
                                                                           `Submitting Entity` = col_skip(), 
                                                                           `Collecting Entity` = col_skip(), 
                                                                           MDL = col_skip(), LOQ = col_skip(), 
                                                                           `Data Qualifier` = col_character(), 
                                                                           `Verify Flag` = col_skip(), `Validation Flag` = col_skip(), 
                                                                           `Exempt Status` = col_skip()), trim_ws = TRUE)

walnut_df <- janitor::clean_names(walnut_df)

p1 <- walnut_df |> 
  mutate(AU = case_when(
    station_id == 17379 ~ "1221F_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("31648", "31699")) |> 
  ggplot() +
  geom_hline(yintercept = 126, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), method = "lm") +
  facet_wrap(~AU, ncol = 1) +
  scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "<i>E. coli</i> (MPN/100mL)",
       title = "Walnut Creek 1121F",
       subtitle = "<i>E. coli</i>") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221F_walnut_ecoli.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()

## DO ############################

p1 <- walnut_df |> 
  mutate(AU = case_when(
    station_id == 17379 ~ "1221F_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00300")) |> 
  ggplot() +
  geom_hline(yintercept = 3, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), 
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "DO (mg/mL)",
       title = "Walnut Creek 1121F",
       subtitle = "Dissolved Oxygen (Grab)") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221F_walnut_DO.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()


## TP #################################

p1 <- walnut_df |> 
  mutate(AU = case_when(
    station_id == 17379 ~ "1221F_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00665")) |> 
  ggplot() +
  geom_hline(yintercept = 0.69, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value),
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "TP (mg/mL)",
       title = "Walnut Creek 1121F",
       subtitle = "Total Phosphorus") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221F_walnut_TP.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()

## Nitrate ########################

p1 <- walnut_df |> 
  mutate(AU = case_when(
    station_id == 17379 ~ "1221F_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00620")) |> 
  ggplot() +
  geom_hline(yintercept = 1.95, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value),
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "NO~3~-N (mg/mL)",
       title = "Walnut Creek 1121F",
       subtitle = "Nitrate") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221F_walnut_NO3.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()


# Coryell Creek ###############

## Ecoli #####################

coryell_df <- read_delim("Data/20230831_1221G.txt", 
                        delim = "|", escape_double = FALSE, col_types = cols(`RFA/Tag ID` = col_skip(), 
                                                                             `End Date` = col_date(format = "%m/%d/%Y"), 
                                                                             `End Time` = col_skip(), `End Depth` = col_skip(), 
                                                                             `Start Date` = col_skip(), `Start Time` = col_skip(), 
                                                                             `Start Depth` = col_skip(), `Composite Category` = col_skip(), 
                                                                             `Composite Type` = col_skip(), Comment = col_skip(), 
                                                                             `Submitting Entity` = col_skip(), 
                                                                             `Collecting Entity` = col_skip(), 
                                                                             MDL = col_skip(), LOQ = col_skip(), 
                                                                             `Data Qualifier` = col_character(), 
                                                                             `Verify Flag` = col_skip(), `Validation Flag` = col_skip(), 
                                                                             `Exempt Status` = col_skip()), trim_ws = TRUE)

coryell_df <- janitor::clean_names(coryell_df)

p1 <- coryell_df |> 
  mutate(AU = case_when(
    station_id == 11804 ~ "1221G_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("31648", "31699")) |> 
  ggplot() +
  geom_hline(yintercept = 126, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value), method = "lm") +
  facet_wrap(~AU, ncol = 1) +
  scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "<i>E. coli</i> (MPN/100mL)",
       title = "Coryell Creek 1121G",
       subtitle = "<i>E. coli</i>") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221G_coryell_ecoli.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()

## DO ###########

p1 <- coryell_df |> 
  mutate(AU = case_when(
    station_id == 11804 ~ "1221G_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00300")) |> 
  ggplot() +
  geom_hline(yintercept = 5, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value),
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "DO (mg/L)",
       title = "Coryell Creek 1121G",
       subtitle = "Dissolved Oxygen (Grab)") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221G_coryell_DO.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()

## TP ################

p1 <- coryell_df |> 
  mutate(AU = case_when(
    station_id == 11804 ~ "1221G_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00665")) |> 
  ggplot() +
  geom_hline(yintercept = 0.69, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value),
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "TP (mg/L)",
       title = "Coryell Creek 1121G",
       subtitle = "Total Phosphorus") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221G_coryell_TP.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()


## Nitrate #######################

p1 <- coryell_df |> 
  mutate(AU = case_when(
    station_id == 11804 ~ "1221G_01",
    .default = NA
  )) |> 
  filter(parameter_code %in% c("00620")) |> 
  ggplot() +
  geom_hline(yintercept = 1.95, linetype = 6, alpha = 0.5) +
  geom_point(aes(end_date, value), alpha = 0.5) +
  geom_smooth(aes(end_date, value),
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  facet_wrap(~AU, ncol = 1) +
  #scale_y_log10(labels = scales::comma) +
  theme_TWRI_print(base_family = "Roboto Condensed") +
  labs(x = "Date", y = "N0~3~-N (mg/L)",
       title = "Coryell Creek 1121G",
       subtitle = "Nitrate") +
  theme(plot.subtitle = element_markdown(),
        axis.title.y = element_markdown())

ragg::agg_png(filename = "out/1221G_coryell_NO3.png",
              width = 6.5,
              height = 3.5,
              units = "in",
              res = 300)
p1
dev.off()
