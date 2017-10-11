library(data.table)
library(dplyr)

boros <- c("All", "Manhattan", "Bronx", "Queens", "Brooklyn", "Staten Island")

csvpath = "./raw_data/2015_Street_Tree_Census_-_Tree_Data.csv"

# import data
dt <- fread(input = csvpath, sep = ",", header = TRUE)

dt$created_at = as.Date(dt$created_at, "%m/%d/%Y")

dt$diameter = ifelse(dt$tree_dbh > 0, dt$tree_dbh, dt$stump_diam)
  
sites <- c("All", unique(dt$curb_loc))

health_indicators <- c("perc_poor",
                       "perc_poor_fair",
                       "perc_root_stone",
                       "perc_root_grate",
                       "perc_root_other",
                       "perc_trunk_wire",
                       "perc_trnk_light",
                       "perc_trnk_other",
                       "perc_brch_light",
                       "perc_brch_shoe",
                       "perc_brch_other")

top_species <- c("All", 
                 dt %>%
                   filter(spc_common != "") %>%
                   group_by(spc_common) %>%
                   summarise(count = n()) %>%
                   arrange(desc(count)) %>%
                   pull(1)
                 )

# heatmap_data = dt %>% 
#   filter(status == "Alive" & health != "" & spc_common != "") %>%
#   group_by(spc_common) %>%
#   summarise(perc_poor = sum(ifelse(health == "Poor", 1, 0))/n(),
#             perc_poor_fair = sum(ifelse(health == "Poor"|health == "Fair", 1, 0))/n(),
#             perc_root_stone = sum(ifelse(root_stone == "Yes", 1, 0))/n(),
#             perc_root_grate = sum(ifelse(root_grate == "Yes", 1, 0))/n(),
#             perc_root_other = sum(ifelse(root_other == "Yes", 1, 0))/n(),
#             perc_trunk_wire = sum(ifelse(trunk_wire == "Yes", 1, 0))/n(),
#             perc_trnk_light = sum(ifelse(trnk_light == "Yes", 1, 0))/n(),
#             perc_trnk_other = sum(ifelse(trnk_other == "Yes", 1, 0))/n(),
#             perc_brch_light = sum(ifelse(brch_light == "Yes", 1, 0))/n(),
#             perc_brch_shoe = sum(ifelse(brch_shoe == "Yes", 1, 0))/n(),
#             perc_brch_other = sum(ifelse(brch_other == "Yes", 1, 0))/n()
#   ) %>% gather("health_indicator", "value", 2:12)
# 
# head(heatmap_data)
# 
# View(heatmap_data)
# 
# heatmap_data %>%
#   filter(spc_common %in% top_species[1:10]) %>%
#   ggplot(aes(x = health_indicator, y = spc_common)) + 
#   geom_tile(aes(fill = value))  + 
#   scale_fill_gradient(low = "green", high = "red") +
#   theme(axis.text.x = element_text(angle = 90))
# 
# 
# 
# 
# library(tidyr)

# gather(heatmap_data, "health_indicator", "value", 2:11)
# 
# View(heatmap_data)
  
# zip_data <- read.dbf("./raw_data/ZIP_CODE_040114/ZIP_CODE_040114.dbf")


# View(zip_data)




# library(maps)
# convert matrix to dataframe
# states <- data.frame(state.name, state.x77)
# remove row names
# rownames(state_stat) <- NULL
# create variable with colnames as choice
#choice <- colnames(state_stat)[-1]

# colStates <- map("state", fill = TRUE,
#                  plot = FALSE,
#                  region = c("florida", "louisiana", "mississippi",
#                             "alabama", "georgia", "tennesse"))
