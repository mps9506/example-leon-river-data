# Example R Script for iteration

## uncomment if you need to install the packages:
# install.packages(c("tidyverse","janitor", "ggtext", "ragg"))
# install.packages("twriTemplates", repos = c(
#   txwri = 'https://txwri.r-universe.dev',
#   CRAN = 'https://cloud.r-project.org'))

## Load Libraries
library(tidyverse)
library(twriTemplates)
library(readr)
library(janitor)
library(ggtext)
library(ragg)

## Load Data and clean data ####################################################
leon_df <- read_delim("Data/20230831_1221.txt", 
                      delim = "|", escape_double = FALSE, 
                      col_types = cols(`RFA/Tag ID` = col_skip(), 
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
                                       `Exempt Status` = col_skip()), trim_ws = TRUE) |> 
  janitor::clean_names() |> 
  mutate(AU = case_when(
    station_id == 11929 ~ "1221_04",
    station_id == 11930 ~ "1221_04",
    station_id == 18781 ~ "1221_05",
    station_id == 20905 ~ "1221_05",
    station_id == 17591 ~ "1221_06",
    station_id == 11934 ~ "1221_07",
    .default = NA
  )) 



resley_df <- read_delim("Data/20230831_1221A.txt", 
                        delim = "|", escape_double = FALSE, 
                        col_types = cols(`RFA/Tag ID` = col_skip(), 
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
                                         `Exempt Status` = col_skip()), trim_ws = TRUE) |> 
  janitor::clean_names() |> 
  mutate(AU = case_when(
    station_id == 11808 ~ "1221A_01",
    station_id == 17377 ~ "1221A_01",
    station_id == 17477 ~ "1221A_01",
    .default = NA
  )) 



sleon_df <- read_delim("Data/20230831_1221B.txt", 
                       delim = "|", escape_double = FALSE, 
                       col_types = cols(`RFA/Tag ID` = col_skip(), 
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
                                        `Exempt Status` = col_skip()), trim_ws = TRUE) |> 
  janitor::clean_names() |> 
  mutate(AU = case_when(
    station_id == 11817 ~ "1221B_01",
    .default = NA
  ))




pecan_df <- read_delim("Data/20230831_1221C.txt", 
                       delim = "|", escape_double = FALSE, 
                       col_types = cols(`RFA/Tag ID` = col_skip(), 
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
                                        `Exempt Status` = col_skip()), trim_ws = TRUE) |> 
  janitor::clean_names() |> 
  mutate(AU = case_when(
    station_id == 17547 ~ "1221C_01",
    .default = NA
  ))


indian_df <- read_delim("Data/20230831_1221D.txt", 
                        delim = "|", escape_double = FALSE, 
                        col_types = cols(`RFA/Tag ID` = col_skip(), 
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
                                         `Exempt Status` = col_skip()), trim_ws = TRUE) |> 
  janitor::clean_names() |> 
  mutate(AU = case_when(
    station_id == 11818 ~ "1221D_01",
    station_id == 17542 ~ "1221D_02",
    .default = NA
  ))


plum_df <- read_delim("Data/20230831_1221E.txt", 
                      delim = "|", escape_double = FALSE, 
                      col_types = cols(`RFA/Tag ID` = col_skip(), 
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
                                       `Exempt Status` = col_skip()), trim_ws = TRUE) |> 
  janitor::clean_names() |> 
  mutate(AU = case_when(
    station_id == 18405 ~ "1221E_01",
    .default = NA
  ))


walnut_df <- read_delim("Data/20230831_1221F.txt", 
                        delim = "|", escape_double = FALSE, 
                        col_types = cols(`RFA/Tag ID` = col_skip(), 
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
                                         `Exempt Status` = col_skip()), trim_ws = TRUE) |> 
  janitor::clean_names() |> 
  mutate(AU = case_when(
    station_id == 17379 ~ "1221F_01",
    .default = NA
  )) 


coryell_df <- read_delim("Data/20230831_1221G.txt", 
                         delim = "|", escape_double = FALSE, 
                         col_types = cols(`RFA/Tag ID` = col_skip(), 
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
                                          `Exempt Status` = col_skip()), trim_ws = TRUE) |> 
  janitor::clean_names() |> 
  mutate(AU = case_when(
    station_id == 11804 ~ "1221G_01",
    .default = NA
  )) 

## create functions for plotting ##############################################

plot_data <- function(data,
                      parameter_codes,
                      limit,
                      trans = "log10",
                      title,
                      filename,
                      height,
                      width) {
  
  link <- "log"
  
  if(identical(parameter_codes, c("31648", "31699"))) {
    y <- "<i>E. coli</i> (MPN/100mL)"
    subtitle <- "<i>E. coli</i>"
    link <- "identity"
  }
  if(identical(parameter_codes, c("00300"))) {
    y <- "DO (mg/L)"
    subtitle <- "Dissolved Oxygen"
  }
  if(identical(parameter_codes, c("00665"))) {
    y <- "TP (mg/L)"
    subtitle <- "Total Phosphorus"
  }
  if(identical(parameter_codes, c("00620"))) {
    y <- "NO~3~-N (mg/L)"
    subtitle <- "Nitrate"
  }
  
  p1 <- data |> 
    ##subset data to parameter
    filter(parameter_code %in% parameter_codes) |> 
    ##start plot
    ggplot() +
    ##add limit line
    geom_hline(yintercept = limit, linetype = 6, alpha = 0.5) +
    ##scatter plot x = end date, y = value
    geom_point(aes(end_date, value), alpha = 0.5) +
    ##add regression line to data
    geom_smooth(aes(end_date, value),
                ## using a generalized linear model so we can bound
                ## the regression model to positive values only 
                ## by using the log link for DO, TP, and NO3
                ## use the identity link for e.coli
                method = "glm",
                method.args = list(family = gaussian(link = link))) +
    ##facet the plots by assessment unit
    facet_wrap(~AU, ncol = 1,
               scales = "free_y") +
    ##transform the y axis as desired (identity or trans)
    scale_y_continuous(labels = scales::comma,
                       trans = trans) +
    ##preset theme
    theme_TWRI_print(base_family = "Roboto Condensed") +
    ##label the plot
    labs(x = "Date", y = y,
         title = title,
         subtitle = subtitle) +
    ##other theme elements
    ## element_markdown allows the use of
    ## html and markdown syntax to format text
    theme(plot.subtitle = element_markdown(),
          axis.title.y = element_markdown(),
          axis.text.y = element_text(hjust = 1))
  
  ##save the figure
  ragg::agg_png(filename = filename,
                width = width,
                height = height,
                res = 300,
                unit = "in")
  print(p1)
  dev.off()
  
  ##returns the ggplot object
  return(p1)
}


## example plot using this function ###########################################
plot_data(coryell_df,
          parameter_codes = c("31648", "31699"),
          limit = 126,
          trans = "log",
          title = "Coryell Creek 1121G",
          filename = "ex_out/1121G_coryell_ecoli.png",
          width = 6.5,
          height = 3.5) 


## use purrr to iterate by creating a dataframe of arguments ###################

## create a dataframe of arguments
## used by the function we created above
## each column must match the argument names exactly
df <- tibble(
  data = list(coryell_df),
  parameter_codes = list(c("31648", "31699"),  #ecoli
             c("00300"), #DO
             c("00665"), #TP
             c("00620")), #Nitrate
  limit = c(126, #ecoli
            5, #DO
            0.69, #TP
            1.95), #NO3
  trans = c("log10",
            "identity",
            "identity",
            "identity"),
  title = rep("Coryell_Creek 1121G", 4),
  filename = paste0("ex_out/1121G_coryell_", c("ecoli", "DO", "TP", "NO3"), ".png"),
  width = rep(6.5, 4),
  height = rep(3.5, 4)
) 

## use the purrr package of functions to
## iterate through the dataframe.
## since the columns in the dataframe match
## the arguments in the function
## the pmap function will iterate through the rows
## rerunning the function for each row of arguments

purrr::pmap(df, plot_data)

## for walnut creek do it again but with a new dataframe of arguments
df <- tibble(
  data = list(walnut_df),
  parameter_codes = list(c("31648", "31699"),  #ecoli
                         c("00300"), #DO
                         c("00665"), #TP
                         c("00620")), #Nitrate
  limit = c(126, #ecoli
            3, #DO
            0.69, #TP
            1.95), #NO3
  trans = c("log10",
            "identity",
            "identity",
            "identity"),
  title = rep("Walnut Creek 1121F", 4),
  filename = paste0("ex_out/1121F_walnut_", c("ecoli", "DO", "TP", "NO3"), ".png"),
  width = rep(6.5, 4),
  height = rep(3.5, 4)
) 

purrr::pmap(df, plot_data)


## and for plum creek
## note that one zero value for E.coli causes issues
## for log transforming the y-axis. We could change that value to 1 
## without issue, but leaving that along for now.
df <- tibble(
  data = list(plum_df),
  parameter_codes = list(c("31648", "31699"),  #ecoli
                         c("00300"), #DO
                         c("00665"), #TP
                         c("00620")), #Nitrate
  limit = c(126, #ecoli
            5, #DO
            0.69, #TP
            1.95), #NO3
  trans = c("log10",
            "identity",
            "identity",
            "identity"),
  title = rep("Plum Creek 1121E", 4),
  filename = paste0("ex_out/1121E_plum_", c("ecoli", "DO", "TP", "NO3"), ".png"),
  width = rep(6.5, 4),
  height = rep(3.5, 4)
) 

purrr::pmap(df, plot_data)


## and for indian creek
df <- tibble(
  data = list(indian_df),
  parameter_codes = list(c("31648", "31699"),  #ecoli
                         c("00300"), #DO
                         c("00665"), #TP
                         c("00620")), #Nitrate
  limit = c(126, #ecoli
            4, #DO
            0.69, #TP
            1.95), #NO3
  trans = c("log10",
            "identity",
            "identity",
            "identity"),
  title = rep("Indian Creek 1121D", 4),
  filename = paste0("ex_out/1121D_indian_", c("ecoli", "DO", "TP", "NO3"), ".png"),
  width = rep(6.5, 4),
  height = rep(5, 4)
) 

purrr::pmap(df, plot_data) ## wow, 1121D_02 is obscenely high

## and for Pecan Creek
df <- tibble(
  data = list(pecan_df),
  parameter_codes = list(c("31648", "31699"),  #ecoli
                         c("00300"), #DO
                         c("00665"), #TP
                         c("00620")), #Nitrate
  limit = c(126, #ecoli
            4, #DO
            0.69, #TP
            1.95), #NO3
  trans = c("log10",
            "identity",
            "identity",
            "identity"),
  title = rep("Pecan Creek 1121C", 4),
  filename = paste0("ex_out/1121C_pecan_", c("ecoli", "DO", "TP", "NO3"), ".png"),
  width = rep(6.5, 4),
  height = rep(3.5, 4)
) 

purrr::pmap(df, plot_data)


## and south leon
df <- tibble(
  data = list(sleon_df),
  parameter_codes = list(c("31648", "31699"),  #ecoli
                         c("00300"), #DO
                         c("00665"), #TP
                         c("00620")), #Nitrate
  limit = c(126, #ecoli
            5, #DO
            0.69, #TP
            1.95), #NO3
  trans = c("log10",
            "identity",
            "identity",
            "identity"),
  title = rep("South Leon River 1121B", 4),
  filename = paste0("ex_out/1121B_s_leon_", c("ecoli", "DO", "TP", "NO3"), ".png"),
  width = rep(6.5, 4),
  height = rep(3.5, 4)
) 

purrr::pmap(df, plot_data)

## and resley
df <- tibble(
  data = list(resley_df),
  parameter_codes = list(c("31648", "31699"),  #ecoli
                         c("00300"), #DO
                         c("00665"), #TP
                         c("00620")), #Nitrate
  limit = c(126, #ecoli
            5, #DO
            0.69, #TP
            1.95), #NO3
  trans = c("log10",
            "identity",
            "identity",
            "identity"),
  title = rep("Resley Creek 1121A", 4),
  filename = paste0("ex_out/1121A_resley_", c("ecoli", "DO", "TP", "NO3"), ".png"),
  width = rep(6.5, 4),
  height = rep(3.5, 4)
) 

purrr::pmap(df, plot_data)


## and leon
df <- tibble(
  data = list(leon_df),
  parameter_codes = list(c("31648", "31699"),  #ecoli
                         c("00300"), #DO
                         c("00665"), #TP
                         c("00620")), #Nitrate
  limit = c(126, #ecoli
            5, #DO
            0.69, #TP
            1.95), #NO3
  trans = c("log10",
            "identity",
            "identity",
            "identity"),
  title = rep("Leon River 1121", 4),
  filename = paste0("ex_out/1121_Leon_", c("ecoli", "DO", "TP", "NO3"), ".png"),
  width = rep(6.5, 4),
  height = rep(6.5, 4)
) 

purrr::pmap(df, plot_data)
