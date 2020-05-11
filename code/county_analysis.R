library(tidyverse)
library(lme4)


## data load and simple pre-processing ----
county_df <- read_csv('./data_county/county_data_new.csv', col_types = cols(age_under18 = col_number()))
county_df$year <- county_df$year %>% as.integer()
county_df <- county_df %>% 
  filter(year %in% 2008:2017) %>% 
  arrange(year, state_code, county_code)
county_df$year <- county_df$year %>% as.character()


## basic statistics ----
XY <- select(county_df, -year, -state_code, -county_code, -county)
means <- colMeans(XY, na.rm = TRUE) 
format(means, digit = 2, nsmall = 2, scientific = FALSE)

SDs <- vector('numeric', ncol(XY))
names(SDs) <- colnames(XY)
for (i in seq_along(XY)) {
  SDs[i] <- sd(XY[[i]], na.rm = TRUE)
}
format(SDs, digit = 2, nsmall = 2, scientific = FALSE)

mins <- vector('numeric', ncol(XY))
names(mins) <- colnames(XY)
for (i in seq_along(XY)) {
  mins[i] <- min(XY[[i]], na.rm = TRUE)
}
format(mins, digit = 2, nsmall = 2, scientific = FALSE)

maxs <- vector('numeric', ncol(XY))
names(maxs) <- colnames(XY)
for (i in seq_along(XY)) {
  maxs[i] <- max(XY[[i]], na.rm = TRUE)
}
format(maxs, digit = 2, nsmall = 2, scientific = FALSE)

cvs <- vector('numeric', ncol(XY))
names(cvs) <- colnames(XY)
for (i in seq_along(XY)) {
  cvs[i] <- cv(XY[[i]], na.rm = TRUE) * 100
}
format(cvs, digit = 2, nsmall = 2, scientific = FALSE)

## number of NA's for each variable ----
num_nas <- county_df %>% map(is.na) %>% map(sum) %>% unlist()
(nrow(county_df) - num_nas) / nrow(county_df)


## check correlations between variables ----
X <- XY %>% 
  select(-death_all, -death_he, -death_fen)
cor_mx <- cor(X, use = 'pairwise.complete.obs')

high_cor <- vector('list', length = ncol(X))
names(high_cor) <- colnames(X)
for (i in seq_along(X)) {
  high_cor_idx <- which(cor_mx[i, ] > 0.6)
  high_cor[[i]] <- cor_mx[i, ][high_cor_idx]
}


## drop a few variables and standardize the data ----
county_df_new <- county_df %>% 
  select(-population, -age_under18, -age_over65, -under18_poverty_rate)
county_df_new[, 5:23] <- scale(county_df_new[, 5:23])

# check the standarization
# colMeans(county_df_new[, 5:26], na.rm = TRUE) # ok
# SDs <- vector('numeric', ncol(county_df_new[, 5:26])) # ok
# names(SDs) <- colnames(county_df_new[, 5:26])
# for (i in seq_along(county_df_new[, 5:26])) {
#   SDs[i] <- sd(county_df_new[, 5:26][[i]], na.rm = TRUE)
# }
# format(SDs, digit = 2, nsmall = 2, scientific = FALSE) 


## splite the data into the three by type of deaths ----
county_all_nona <- county_df_new %>% 
  select(-death_he, -death_fen) %>% 
  drop_na()
county_he_nona <- county_df_new %>% 
  select(-death_all, -death_fen) %>% 
  drop_na()
county_fen_nona <- county_df_new %>% 
  select(-death_all, -death_he) %>% 
  drop_na()


## modeling ----
indep_vars <- str_c(colnames(county_df_new[, 5:23]), collapse = ' + ')
all_formula <- formula(str_c('death_all ~ ', indep_vars, ' + (1|state_code) + (1|county_code)'))
he_formula <- formula(str_c('death_he ~ ', indep_vars, ' + (1|state_code) + (1|county_code)'))
fen_formula <- formula(str_c('death_fen ~ ', indep_vars, ' + (1|state_code) + (1|county_code)'))

all_lmer <- lmer(all_formula, data = county_all_nona)
he_lmer <- lmer(he_formula, data = county_he_nona, )
fen_lmer <- lmer(fen_formula, data = county_fen_nona)

confint.merMod(all_lmer)
confint.merMod(he_lmer)
confint.merMod(fen_lmer)


## visualize the coefficients ----
# extract coefficients from the models
all_coef <- fixef(all_lmer)
he_coef <- fixef(he_lmer)
fen_coef <- fixef(fen_lmer)

names(all_coef) <- c(seq(1, 11), 20, 12:19)
names(he_coef) <- c(seq(1, 11), 20, 12:19)
names(fen_coef) <- c(seq(1, 11), 20, 12:19)

# gather necessary information
coef_mx <- tibble(`All Opioids` = all_coef, Heroin = he_coef, Fentanyl = fen_coef, Features = names(all_coef))

# pivot the data for drawing a bar plot
coef_mx_pivot <- coef_mx %>%
  pivot_longer(c('All Opioids', 'Heroin', 'Fentanyl'), 
               names_to = 'Opioid Types', values_to = 'Coefficients')

coef_mx_pivot$Features <- factor(coef_mx_pivot$Features, levels = seq(1, 20))
# levels(coef_mx_pivot) <- c('(Intercept)', 'population_acs', 'male_rate', 'age_15to44', 'child_dependency_rate', 'old_dependecy_rate',
#                            'white', 'black', 'asian', 'other_races', 'income', 'labor_force_rate', 'unemployed_rate', 'all_poverty_rate',
#                            'less_highschool', 'above_bs', 'veteran_rate', 'female_household_rate', 'divorce_rate', 'uninsured_rate')

# plotting
#x = Features, y = Coefficients, 
#position = 'dodge'
coef_mx_pivot %>% ggplot(aes(Features, Coefficients)) +   
  geom_bar(aes(fill = `Opioid Types`), stat = "identity") +
  scale_fill_manual('legend', values = c('All Opioids' = 'black', 'Heroin' = 'blue', 'Fentanyl' = 'red')) +
  theme_bw(base_size = 28) + # theme_bw(base_size = 18) font size
  theme(legend.position = 'bottom') +
  theme(text = element_text(face = 'bold'))

# export {width: 1500 and height: 1000}
