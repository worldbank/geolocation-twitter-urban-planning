# Sendy Stats 

# Load data --------------------------------------------------------------------
sendy_df <- read.csv(file.path(sendy_dir, "data", "sendy_data.csv"),
                     stringsAsFactors = F)

# Proportion of Crashes Verified -----------------------------------------------

## Remove cases where driver couldn't reach crash
sendy_df <- sendy_df[!(sendy_df$why_no_crash %in% "Cannot Reach"),]

## Variable for verifying crash
sendy_df$verified_crash <- sendy_df$observe_crash %in% "Crash There" | sendy_df$why_no_crash %in% c("Crash Cleared", "Crash Nearby")

## Stats
nrow(sendy_df) # Total Observations
mean(sendy_df$verified_crash) # Proportion crash verified

table(sendy_df$observe_crash) / nrow(sendy_df)
table(sendy_df$why_no_crash) / nrow(sendy_df)

# Time Stats -------------------------------------------------------------------
## Tweet and survey started
difftime(sendy_df$datetime_tweet, sendy_df$datetime_survey_start, units = "mins") %>% median()

## Order places and survey start
difftime(sendy_df$datetime_order_placed, sendy_df$datetime_survey_start, units = "mins") %>% median(na.rm=T)


