# This file will be for manipulating teh data frames before they actually go into the app


data_w <- read_xlsx("wheat_results_2018-12-11.xlsx")
data_p <- read_csv("potato_starch_results_plus10_2018-11-27.csv")

data_p <- coded.data(data_p,cooltime~(x1 - 228)/90, heattime~(x2 - 222)/90, holdtime~(x3 - 150)/90, 
                     stirrpm~(x4-160)/40, starchg~(x5-2)/.1 )

data_w <- data_w %>% 
  select(-`Verified (Y)`)

data_w$Starch <- "Wheat"
data_p$Starch <- "Potato"

dat <- rbind(data_w, data_p)

th_left <- theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title.x = element_blank())