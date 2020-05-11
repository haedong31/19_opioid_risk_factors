library(GGally)
library(tidyverse)
library(pals)


## load opioid data ----
opioid_data <- read_csv("./data_state/opioid_data.csv")
sub_opioid_data <- opioid_data %>% 
  select(-year, -state, -methad_death)

## Investigate high p-values of insur, poverty, poverty_rate ----
opioid_data %>% 
  filter(state == "Pennsylvania") %>% 
  ggplot() +
  geom_line(aes(x = seq_along(insur), y = log(insur)), color = "black") +
  geom_line(aes(x = seq_along(uninsur), y = log(uninsur)), color = "red")

opioid_data %>% 
  filter(state == "Pennsylvania") %>% 
  ggplot(aes(x = seq_along(insur_rate), y = insur_rate)) +
  geom_line()

opioid_data %>% 
  filter(state == "Pennsylvania") %>% 
  ggplot(aes(x = seq_along(insur_rate), y = log(insur_rate))) +
  geom_line()

opioid_data %>% 
  ggplot() +
  geom_point(aes(x = insur, y = poverty))

sub_opioid_data %>% 
  select(-all_death, -nat_death, -syn_death, -heroin_death) %>% 
  ggpairs(lower = list(continuous = wrap("smooth", alpha = 0.5, size = 0.1))) +
  theme(axis.title = element_blank(), axis.text = element_blank())

## Population distribution ----
deaths_df <- opioid_data %>% 
  group_by(year) %>% 
  summarise(`Total` = sum(all_death, na.rm = TRUE), 
            `Prescription Opioids` = sum(nat_death, na.rm = TRUE),
            `Fentanyl` = sum(syn_death, na.rm = TRUE),
            `Heroin` = sum(heroin_death, na.rm = TRUE))

data_container <- vector("list", nrow(deaths_df))
for (i in 1:nrow(deaths_df)) {
  gathered_row <- deaths_df %>% 
    slice(i) %>% 
    select(`Total`, `Prescription Opioids`, `Fentanyl`, `Heroin`) %>% 
    gather(key = `Cause of Death`, value = num_deaths) %>% 
    mutate(year = deaths_df$year[i])
  
  data_container[[i]] <- gathered_row
}
gathered_death_df <- bind_rows(data_container) %>% 
  arrange(`Cause of Death`)

# qplot(x = year, y = heroin_death, data = my_df)

p <- ggplot(gathered_death_df, aes(x = year, y = num_deaths, group = `Cause of Death`)) +
  geom_line(aes(linetype = `Cause of Death`, color = `Cause of Death`), size = 2) +
  geom_point(aes(color = `Cause of Death`)) +
  #  geom_vline(xintercept = 2010, color = "black") +
  #  geom_vline(xintercept = 2013, color = "black") +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(2006, 2017)) +
  ylab("number of deaths") +
  annotate("text", x = 2009, y = 25000, label = "All Opioids", size = 7) +
  annotate("text", x = 2009, y = 12500, label = "Natural Opioids", size = 7) +
  annotate("text", x = 2011, y = 7000, label = "Heroin", size = 7) +
  annotate("text", x = 2015.5, y = 4500, label = "Synthetic Opioids", size = 7) +
  theme(text = element_text(face = "bold", size = 17))
p
ggsave(filename = "./num_deaths.jpg", plot = p)

## Histogram ---- 
# select(sub_opioid_data, presc_rate) %>% 
#   ggplot(aes(presc_rate)) + 
#   geom_histogram(bins = 20) +
#   xlab("") + ylab("")

sub_opioid_data %>%
  select(insur, poverty, poverty_rate) %>%
  gather() %>% 
  replace_na(list(value = 0)) %>% 
  ggplot(aes(value)) +
  geom_histogram(bins = 13) +
  facet_wrap(~key, scales = "free_x") +
  theme(axis.title = element_blank())

# log transformation
sub_opioid_data$labor_force <- log(sub_opioid_data$labor_force)
sub_opioid_data$all_death <- log(sub_opioid_data$all_death)
sub_opioid_data$heroin_death <- log(sub_opioid_data$heroin_death)
sub_opioid_data$syn_death <- log(sub_opioid_data$syn_death)
sub_opioid_data$treat_fac <- log(sub_opioid_data$treat_fac)
sub_opioid_data$employed <- log(sub_opioid_data$employed)
sub_opioid_data$unemployed <- log(sub_opioid_data$unemployed)
sub_opioid_data$uninsur <- log(sub_opioid_data$uninsur)
sub_opioid_data$insur <- log(sub_opioid_data$insur)
sub_opioid_data$poverty <- log(sub_opioid_data$poverty)

## Scatter plot ----
ggplot(opioid_data, aes(x = treat_fac, y = all_death)) +
  geom_smooth() +
  geom_point() +
  xlab("predictor") + ylab("response")

# multiple scatter plot
cor_mx <- cor(select(opioid_data, -year, -state), method = "pearson", use = "complete.obs")
colnames(cor_mx)[cor_mx[1, ] >= 0.6]
colnames(cor_mx)[cor_mx[5, ] >= 0.6]
cor_mx[3, ]
multi_scat_df <- sub_opioid_data %>% 
  select(all_death, heroin_death, syn_death, treat_fac, uninsur_rate, unemployed_rate, poverty_rate, median_income) %>% 
  rename(income = median_income)
p <- ggpairs(multi_scat_df, lower = list(continuous = wrap("smooth", alpha = 0.5, size = 0.1)), 
             fontface = "bold", size = 16) +
  theme(axis.title = element_blank(), axis.text = element_blank(), 
        text = element_text(face = "bold", size = 16))
p
ggsave(filename = "./multiple_scatter.jpg", plot = p)

## PA map ----
# load overdose-deaths data
raw_data <- read_csv("./data/PCW_deaths.csv")

# split the data by year
years <- c("2017", "2018")
overdose_container <- vector(mode = "list", length = length(years))
for (i in seq_along(years)) {
  overdose_container[[i]] <- raw_data %>%
    filter(Year == years[i]) %>%
    filter(County != "Pennsylvania") %>%
    filter(`Type of Overdose Death` == "Opioid Overdose Deaths")
}
names(overdose_container) <- years

# data pre-processing
for (i in seq_along(overdose_container)) {
  # encode suppressed and missing values
  # overdose_container[[i]]$Count <- replace_na(overdose_container[[i]]$Count, -99)
  
  # change county names to lower cases
  overdose_container[[i]]$County <- str_to_lower(overdose_container[[i]]$County)
}

# split Latitude/Longitude
# for (i in seq_along(data_container)) {
#   tmp_overdose_df <- data_container[[i]]
#   lat_col <- vector(mode = "numeric", length = nrow(tmp_overdose_df))
#   long_col <- vector(mode = "numeric", length = nrow(tmp_overdose_df))
#   
#   for (j in 1:nrow(tmp_overdose_df)) {
#     lat_long <- tmp_overdose_df$`Latitude/Longitude`[j]
#     
#     # (latitude, longitude) / remove whitespace / remove parenthesis / split
#     lat_long <- str_remove_all(lat_long, "\\s")  # white space
#     lat_long <- str_remove_all(lat_long, "\\(|\\)")  # parenthesis
#     lat_long <- str_split(lat_long, ",") %>% unlist()
#     lat_col[j] <- as.numeric(lat_long[1])
#     long_col[j] <- as.numeric(lat_long[2])
#   }
#   
#   # append new columns to the existing data: latitude and logitude
#   tmp_overdose_df <- tmp_overdose_df %>% 
#     mutate(latitude = lat_col, longitude = long_col)
#   data_container[[i]] <- tmp_overdose_df
# }

# Pennsylvania map data with county boundaries
pa_map_whole <- map_data("state", region = "pennsylvania")
map_container <- vector(mode = "list", length = length(overdose_container))
for (i in seq_along(map_container)) {
  map_container[[i]] <- map_data("county", region = "pennsylvania")
}
names(map_container) <- years

# county information
for (i in seq_along(map_container)) {
  overdose_df <- overdose_container[[i]]
  pa_map <- map_container[[i]]
  
  # county information
  counties <- unique(pa_map$subregion)
  num_counties <- length(counties)
  
  county_container <- vector(mode = "list", length = num_counties)
  for (j in 1:num_counties) {
    # subset of the map for a county
    pa_map_county <- pa_map %>%
      filter(subregion == counties[j])
    
    # find number of deaths for a county from "overdose_df"
    num_deaths <- vector(mode = "numeric", length = nrow(pa_map_county))
    matching_idx <- which(overdose_df$County == counties[j])
    if (is_empty(matching_idx)) {
      num_deaths[] <- -99
    } else{
      num_deaths[] <- overdose_df$Count[matching_idx]
    }
    pa_map_county <- pa_map_county %>%
      mutate(deaths = num_deaths)
    county_container[[j]] <- pa_map_county
  }
  map_container[[i]] <- bind_rows(county_container)
}

# draw an Opioid-overdose-deaths-in-Pennsylvania map

pa_base_map <- ggplot(data = pa_map_whole, aes(x = long, y = lat)) +
  geom_polygon(color = "black", fill = "gray") +
  coord_fixed(1.3)
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

# {1: 2017, 2: 2018}
year_idx <- 2
p <- pa_base_map + 
  geom_polygon(data = map_container[[year_idx]], aes(group = group, fill = deaths), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  ditch_the_axes +
  scale_fill_gradientn(na.value = "gray", colours = jet(100)) +
  labs(title = str_c("Opioid Overdose Deaths in ", years[year_idx])) +
  theme(text = element_text(face = "bold", size = 17))
p
ggsave(filename = str_c("./figures/pa_map_", years[year_idx], ".jpg"), plot = p)

#--- Mallow's Cp
library(readxl)
cp <- read_csv('./model_sel.dat')
cp <- bind_cols(cp, idx = seq(1:nrow(cp)))
ggplot(data = cp) + 
  geom_line(aes(x = idx, y = BIC)) +
  geom_point(aes(x = idx, y = BIC)) +
  scale_x_continuous(breaks = seq(0, nrow(cp))) +
  labs(x = "Model Size", y = "Mallow's Cp") +
  theme(text = element_text(face = "bold", size = 17))

