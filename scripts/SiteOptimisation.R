#############################################################################
# Import R libraries
library(dplyr)
library(lubridate)
library(tibble)
library(FinCal)
library(FinancialMath)
library(styler)
#############################################################################
# Stage 1 - Select technologies and order
# Technologies are "led", ("solar" or "solarandbattery"), ("ashp" or "chp"), "batteryresidual", "batteryunconstrained", ""
technology1 <- "led"
technology2 <- "solar"
technology3 <- "chp"
technology4 <- "gridbattery"
###
# Create dataframe
technology_list <- c("technology1", "technology2", "technology3", "technology4")
technology_order <- c(technology1, technology2, technology3, technology4)
technology_df <- data.frame(technology_list, technology_order)
##############################################################################
# Step 2 - Define and consolidate key external dataframes
# read all data
elec_price <- read.csv("./inputs/Elec_Price.csv", fileEncoding = "UTF-8-BOM")
gas_price <- read.csv("./inputs/Gas_Price.csv", fileEncoding = "UTF-8-BOM")
elec_units <- read.csv("./inputs/Base_Elec_Units.csv", fileEncoding = "UTF-8-BOM")
gas_units <- read.csv("./inputs/Base_Gas_Units.csv", fileEncoding = "UTF-8-BOM")
solar_panel <- read.csv("./inputs/Solar_Panel.csv", fileEncoding = "UTF-8-BOM")

# Check for duplicates due to clock changes
nrow(unique(elec_price[, c("Date", "Time")])) == nrow(elec_price)
nrow(unique(gas_price[, c("Date", "Time")])) == nrow(gas_price)
nrow(unique(elec_units[, c("Date", "Time")])) == nrow(elec_units)
nrow(unique(gas_units[, c("Date", "Time")])) == nrow(gas_units)
nrow(unique(solar_panel[, c("Date", "Time")])) == nrow(solar_panel)

# Remove duplicates using average values
elec_price <- summarise(group_by(elec_price, Date, Time), ElecPriceMWh = mean(ElecPriceMWh, na.rm = TRUE))
gas_price <- summarise(group_by(gas_price, Date, Time), GasPriceMWh = mean(GasPriceMWh, na.rm = TRUE))
elec_units <- summarise(group_by(elec_units, Date, Time), BaseElecUnitskWh = mean(BaseElecUnitskWh, na.rm = TRUE))
gas_units <- summarise(group_by(gas_units, Date, Time), BaseGasUnitskWh = mean(BaseGasUnitskWh, na.rm = TRUE))
solar_panel <- summarise(group_by(solar_panel, Date, Time), SolarPanel1kWgenkWh = mean(SolarPanel1kWgenkWh, na.rm = TRUE))

# join key electricity and gas dataframes - solar_panel kept separate
site_opt <- left_join(elec_price, gas_price, by = c("Date", "Time"))
site_opt2 <- left_join(site_opt, elec_units, by = c("Date", "Time"))
site_opt3 <- left_join(site_opt2, gas_units, by = c("Date", "Time"))
site_opt4 <- site_opt3

# convert to date
site_opt4$Date <- as.Date(site_opt4$Date, format = "%d/%m/%Y")
site_opt4 <- site_opt4[order(site_opt4$Date, decreasing = FALSE), ]

##############################################################################
# Stage 3 - Define and classify inputs for variables - exception is battery size which is done later
# Financial assumptions
npv_discount <- 1.1

# CO2 Factors
elec_co2 <- 0.19338
gas_co2 <- 0.18254

# Building factors
site_roof_m2 <- 500
usable_solar_m2 <- 250
usable_roof_space <- usable_solar_m2 / site_roof_m2
internal_area_m2 <- 1000

# Gas & Heating
peak_gas_kwh <- max(gas_units$BaseGasUnitskWh, na.rm = TRUE)
heat_gas_use_pc <- 0.7
peak_gas_heat_kwh <- peak_gas_kwh * heat_gas_use_pc

# LED Lighting
av_m2_per_light <- 5
lights <- internal_area_m2 / av_m2_per_light
exist_power_rate_bulbs <- 14
site_open_time <- 5
site_close_time <- 20
hour_use_per_day <- site_close_time - site_open_time
hour_use_per_year <- 365 * hour_use_per_day
exist_light_power_use_kwh_annual <- lights * exist_power_rate_bulbs * hour_use_per_day * 365 / 1000
new_power_rate_bulbs <- 6
new_light_power_use_kwh_annual <- lights * new_power_rate_bulbs * hour_use_per_day * 365 / 1000
cost_per_cfl_gbp <- 3
cost_per_led_gbp <- 17.5
cost_per_light_per_fit_gpb <- 100 / 6
cost_bulbs <- cost_per_led_gbp * lights
cost_labour <- round(cost_per_light_per_fit_gpb * lights, 0)
cost_set_up <- cost_bulbs + cost_labour
hours_per_bulb_cfl <- 10000
lifetime_cfl_years <- hours_per_bulb_cfl / hour_use_per_year
hours_per_bulb_led <- 60000
lifetime_led_years <- round(hours_per_bulb_led / hour_use_per_year, 2)
bulb_replace_cost_save_annual <- round((10 / lifetime_cfl_years) * cost_per_cfl_gbp * (lights / 10), 2)

# solar panels
usable_panel_m2 <- usable_solar_m2
panel_size <- 1.7
panels <- floor(usable_panel_m2 / panel_size)
kw_per_panel <- 0.375
cost_per_panel <- 375
panels_cost <- cost_per_panel * panels
investor_cost_pc_panel <- 0.08
investor_other_capex <- investor_cost_pc_panel * panels_cost
install_cost_per_panel <- 100
install_cost_panels <- install_cost_per_panel * panels
total_setup_panels <- install_cost_panels + investor_other_capex + panels_cost
annual_mainten_panels_coef <- 0.1
annual_mainten_panels <- round(annual_mainten_panels_coef * total_setup_panels, 0)
annual_cost_solar <- annual_mainten_panels

# air source heat pump
heat_pump_effic <- 0.85
air_source_heat_pump_req <- round(peak_gas_heat_kwh / heat_pump_effic, 2)
air_source_heat_pump_req_round <- round(air_source_heat_pump_req, 0)
capex_cost_per_kw_ashp <- 1000
install_cost_per_kw_ashp <- 500
performance_coef_ashp <- 3.6
total_setup_cost_ashp <- (capex_cost_per_kw_ashp + install_cost_per_kw_ashp) * air_source_heat_pump_req_round
annual_mainten_ashp <- 0.1 * total_setup_cost_ashp

# CHP
heat_demand <- peak_gas_heat_kwh
max_elec_demand_round <- ceiling(heat_demand / 50) * 50
installed_cost_coef_chp <- 2500000 / 1.1
installed_cost_chp <- round((max_elec_demand_round / 1000) * installed_cost_coef_chp, 0)
annual_maint_cost_coef_chp <- 103200 / 1.1
annual_maint_cost_chp <- round((max_elec_demand_round / 1000) * annual_maint_cost_coef_chp, 0)
heat_recovery <- 0.4
annual_cost_chp <- annual_maint_cost_chp

# Initial battery assumptions
solar_battery_cost_per_kwh <- 600
grid_battery_cost_per_kwh <- 600

##############################################################################
# Stage 4 - Functions for forecast demand after technology investments and costs
led_function <- function(site_opt_df, technology_number) {
  site_opt_led <- site_opt_df
  site_opt_led$LightUseBasekwh <- round(lights * exist_power_rate_bulbs / 1000, 2)
  site_opt_led$LightUseBasekwh[1:5] <- 0
  site_opt_led$LightUseNewkwh <- round(lights * new_power_rate_bulbs / 1000, 2)
  site_opt_led$LightUseNewkwh[1:5] <- 0
  site_opt_led$SavingElecwhLED <- site_opt_led$LightUseBasekwh - site_opt_led$LightUseNewkwh
  site_opt_led$SavingElecwhLED[1:5] <- 0
  site_opt_led$LEDElecSaving <- round(site_opt_led$SavingElecwhLED * site_opt4$ElecPriceMWh / 1000, 2)
  site_opt_led$AdjElecUnitskwh <- round(pmax(site_opt_led$AdjElecUnitskwh - site_opt_led$SavingElecwhLED, 0), 2)
  site_opt_led[paste0(technology_number, "_savings_elec")] <- round(site_opt_led$LEDElecSaving, 2)
  site_opt_led[paste0(technology_number, "_savings_gas")] <- 0
  site_opt_led <- select(site_opt_led, -LightUseBasekwh, -LightUseNewkwh, -SavingElecwhLED, -LEDElecSaving)
  return(site_opt_led)
}

solar_function <- function(site_opt_df, solar_panel, technology_number) {
  site_opt_solar <- site_opt_df
  site_opt_solar$TotalGenSolarkwh <- round(solar_panel$SolarPanel1kWgenkWh * panels * kw_per_panel, 2)
  site_opt_solar$SavingElecSolar <- round(site_opt_solar$TotalGenSolarkwh * site_opt_solar$ElecPriceMWh / 1000, 2)
  site_opt_solar$AdjElecUnitskwh <- pmax(site_opt_solar$AdjElecUnitskwh - site_opt_solar$TotalGenSolarkwh, 0)
  site_opt_solar[paste0(technology_number, "_savings_elec")] <- round(site_opt_solar$SavingElecSolar, 2)
  site_opt_solar[paste0(technology_number, "_savings_gas")] <- 0
  site_opt_solar <- select(site_opt_solar, -TotalGenSolarkwh, -SavingElecSolar)
  return(site_opt_solar)
}

# Return list of battery demand profile, battery cost, battery size
solar_battery_function <- function(site_opt_df, solar_panel, technology_number) {
  # Stage 1 - calculate max solar for a day to generate battery size
  site_opt_solar_batt <- site_opt_df
  site_opt_solar_batt$TotalGenSolarkwh <- round(solar_panel$SolarPanel1kWgenkWh * panels * kw_per_panel, 2)
  site_opt_solar_batt$InitialElecUnitskwh <- site_opt_solar_batt$AdjElecUnitskwh
  site_opt_solar_batt$AdjElecUnitskwh <- pmax(site_opt_solar_batt$InitialElecUnitskwh - site_opt_solar_batt$TotalGenSolarkwh, 0)
  site_opt_solar_batt$ExcessGeneration <- ifelse(site_opt_solar_batt$AdjElecUnitskwh < 0, -(site_opt_solar_batt$AdjElecUnitskwh), 0)
  solar_battery_storage_per_day <- site_opt_solar_batt %>%
    group_by(Date) %>%
    summarise(sum = sum(ExcessGeneration))
  solar_battery_size_req <- (max(solar_battery_storage_per_day$sum))
  # Stage 2 - calculate cost based on 
  solar_battery_cost <- solar_battery_cost_per_kwh * solar_battery_size_req
  total_setup_solar_battery <- solar_battery_cost
  # Stage 3 - While loop to find battery storage within day and apply at highest
  # price point in evening
  all_days <- site_opt_solar_batt$Date[!duplicated(site_opt_solar_batt$Date)]
  yesterdays_excess_generation <- 0  ### Start excess generation at 0
  site_opt_solar_battery <- data.frame()
  for (day in all_days) {
    solar_batt_day_df <- site_opt_solar_batt[site_opt_solar_batt$Date == day, ]
    solar_batt_day_df <- solar_batt_day_df %>% mutate(UsageRank = row_number(max(solar_batt_day_df$ElecPriceMWh) - solar_batt_day_df$ElecPriceMWh))
    # Find if any excess generation from yesterday and add to today's
    excess_generation <- yesterdays_excess_generation + sum(solar_batt_day_df$ExcessGeneration)
    # If there was excess generation in the day - find the last available time it occurred
    if (excess_generation > 0) {
      last_excess_generation_row <- max(c(which(solar_batt_day_df$ExcessGeneration > 0)))
    } else {
      last_excess_generation_row <- 0}
    # Select dataframe of all hrs in day after last period of excess generation
    remaining_values <- solar_batt_day_df[(last_excess_generation_row + 1):nrow(solar_batt_day_df), ] %>%
      ungroup() %>%
      select(UsageRank, AdjElecUnitskwh)
    # Order remaining values by cost
    remaining_values <- remaining_values[order(remaining_values$UsageRank), ]
    remaining_values$UsageFromBattery <- 0
    # While loop to use excess generation at highest point in day
    for (i in (1:nrow(remaining_values_df))) {
      OutputElecComp <- remaining_values$AdjElecUnitskwh[i]
      remaining_values_df$UsageFromBattery[i] <- min(excess_generation, OutputElecComp)
      excess_generation <- ifelse(excess_generation - OutputElecComp > 0, excess_generation - OutputElecComp, max(0, excess_generation - OutputElecComp))
    }
    remaining_values <- remaining_values %>% select(UsageRank, UsageFromBattery)
    solar_batt_day_df <- merge(x = solar_batt_day_df, y = remaining_values, by = "UsageRank", all.x = TRUE)
    solar_batt_day_df[is.na(solar_batt_day_df)] <- 0
    solar_batt_day_df <- solar_batt_day_df[order(solar_batt_day_df$Time), ]
    solar_batt_day_df$AdjElecUnitskwh <- ifelse(solar_batt_day_df$AdjElecUnitskwh > 0, solar_batt_day_df$AdjElecUnitskwh - solar_batt_day_df$UsageFromBattery, 0)
    yesterdays_excess_generation <- excess_generation
    site_opt_solar_battery <- rbind(site_opt_solar_battery, day_df)
  }
  # Select savings and key columns
  site_opt_solar_battery[paste0(technology_number, "_savings_elec")] <- round((site_opt_solar_battery$InitialElecUnitskwh - site_opt_solar_battery$AdjElecUnitskwh) * site_opt_solar_battery$ElecPriceMWh / 1000, 2)
  site_opt_solar_battery[paste0(technology_number, "_savings_gas")] <- 0
  # Return list of battery demand profile, battery cost, battery size
  solar_battery_list <- list(site_opt_solar_battery, total_setup_solar_battery, solar_battery_size_req)
  return(solar_battery_list)
}

# Air source heat pump function
ashp_function <- function(site_opt_df, technology_number) {
  site_opt_df_ashp <- site_opt_df
  site_opt_df_ashp$GasHeatingUnits <- round(site_opt_df_ashp$BaseGasUnitskWh * heat_gas_use_pc, 2)
  site_opt_df_ashp$ReductionGasAir <- site_opt_df_ashp$GasHeatingUnits
  site_opt_df_ashp$IncreaseElecAir <- round(site_opt_df_ashp$ReductionGasAir / performance_coef_ashp, 2)
  site_opt_df_ashp$SavingGasAir <- round(site_opt_df_ashp$ReductionGasAir * site_opt_df_ashp$GasPriceMWh / 1000, 2)
  site_opt_df_ashp$AdjGasUnitskWh <- round(pmax(site_opt_df_ashp$AdjGasUnitskWh - site_opt_df_ashp$ReductionGasAir, 0), 2)
  site_opt_df_ashp$SavingElecAir <- -round(site_opt_df_ashp$IncreaseElecAir * site_opt_df_ashp$ElecPriceMWh / 1000, 2)
  site_opt_df_ashp$AdjElecUnitskwh <- round(site_opt_df_ashp$IncreaseElecAir + site_opt_df_ashp$AdjElecUnitskwh, 2)
  site_opt_df_ashp[paste0(technology_number, "_savings_elec")] <- round(site_opt_df_ashp$SavingElecAir, 2)
  site_opt_df_ashp[paste0(technology_number, "_savings_gas")] <- round(site_opt_df_ashp$SavingGasAir, 2)
  site_opt_df_ashp <- select(site_opt_df_ashp, -GasHeatingUnits, -ReductionGasAir, -IncreaseElecAir, -SavingGasAir, -SavingElecAir)
  return(site_opt_df_ashp)
}

# Control heat pump function
chp_function <- function(site_opt_df, technology_number) {
  site_opt_df_chp <- site_opt_df
  site_opt_df_chp$GasHeatUnitsComp <- round(site_opt_df_chp$AdjGasUnitskWh * heat_gas_use_pc, 2)
  site_opt_df_chp$SavingElecComp <- round(site_opt_df_chp$GasHeatUnitsComp * heat_recovery, 2)
  site_opt_df_chp$SavingElecCompGBP <- round(site_opt_df_chp$SavingElecComp * site_opt_df_chp$ElecPriceMWh / 1000, 2)
  site_opt_df_chp$AdjElecUnitskwh <- ifelse(round(site_opt_df_chp$AdjElecUnitskwh - site_opt_df_chp$SavingElecComp, 2) > 0, round(site_opt_df_chp$AdjElecUnitskwh - site_opt_df_chp$SavingElecComp, 2), 0)
  site_opt_df_chp[paste0(technology_number, "_savings_elec")] <- round(site_opt_df_chp$SavingElecComp, 2)
  site_opt_df_chp[paste0(technology_number, "_savings_gas")] <- 0
  site_opt_df_chp <- select(site_opt_df_chp, -GasHeatUnitsComp, -SavingElecComp, -SavingElecCompGBP)
  return(site_opt_df_chp)
}

# Residual battery function
# Return list of battery demand profile, battery cost, battery size and npv
grid_battery_residual <- function(site_opt_df, technology_number, battery_size_proportion, battery_lifetime_cycle, battery_lifetime) {
  # Stage 1 - Battery size requirements and cost
  site_opt_grid <- site_opt_df
  grid_battery_storage_per_day <- site_opt_grid %>%
    group_by(Date) %>% summarise(sum = sum(AdjElecUnitskwh))
  grid_battery_size_req <- max(grid_battery_storage_per_day$sum) * battery_size_proportion
  grid_battery_cost <- grid_battery_cost_per_kwh * grid_battery_size_req
  total_setup_grid_battery <- grid_battery_cost

  # Stage 2 - Initiate while loop to make daily optimization solution
  grid_battery_df <- data.frame()
  all_days <- site_opt_grid$Date[!duplicated(site_opt_grid$Date)]
  number_of_cycles <- 0
  for (day in all_days) {
    grid_battery_day <- site_opt_grid[site_opt_grid$Date == day, ]
    # Use initial electricity unites 
    grid_battery_day$InitialElecUnitskwh <- grid_battery_day$AdjElecUnitskwh
    grid_battery_day$when_charge <- 0
    min_row <- which.min(grid_battery_day[grid_battery_day$Time < "12:00:00", ]$ElecPriceMWh)
    grid_battery_day$when_charge[min_row] <- 1
    grid_battery_day <- grid_battery_day %>% mutate(usage_rank = row_number(max(grid_battery_day$ElecPriceMWh) - grid_battery_day$ElecPriceMWh))
    charge_amount <- min(grid_battery_size_req, with(grid_battery_day, min(sum(grid_battery_day$InitialElecUnitskwh[(min_row + 1):nrow(grid_battery_day)]))))
    grid_battery_day$ChargeAmount <- 0
    grid_battery_day$ChargeAmount[min_row] <- charge_amount
    remaining_values <- grid_battery_day[(min_row + 1):nrow(grid_battery_day), ] %>%
      ungroup() %>%
      select(usage_rank, InitialElecUnitskwh)
    remaining_values <- remaining_values[order(remaining_values$usage_rank), ]
    remaining_values$usage_from_grid_battery <- 0
    remaining_charge_amount <- charge_amount
    for (i in (1:nrow(remaining_values))) {
      InitialElecUnitskwh <- remaining_values$InitialElecUnitskwh[i]
      remaining_values$usage_from_grid_battery[i] <- min(remaining_charge_amount, InitialElecUnitskwh)
      remaining_charge_amount <- ifelse(remaining_charge_amount - InitialElecUnitskwh > 0, remaining_charge_amount - InitialElecUnitskwh, max(0, remaining_charge_amount - InitialElecUnitskwh))
    }
    remaining_values <- remaining_values %>%
      ungroup() %>%
      select(usage_rank, usage_from_grid_battery)
    remaining_values[is.na(remaining_values)] <- 0
    grid_battery_day <- merge(x = grid_battery_day, y = remaining_values, by = "usage_rank", all.x = TRUE)
    grid_battery_day[is.na(grid_battery_day)] <- 0
    grid_battery_day <- grid_battery_day[order(grid_battery_day$Time), ]
    grid_battery_day$AdjElecUnitskwh <- round(grid_battery_day$InitialElecUnitskwh + grid_battery_day$ChargeAmount - grid_battery_day$usage_from_grid_battery, 2)
    grid_battery_day[paste0(technology_number, "_savings_elec")] <- round(grid_battery_day$InitialElecUnitskwh - grid_battery_day$AdjElecUnitskwh, 2) * grid_battery_day$ElecPriceMWh / 1000
    grid_battery_day[paste0(technology_number, "_savings_gas")] <- 0
    grid_battery_df <- rbind(grid_battery_df, grid_battery_day)
    number_of_cycles <- number_of_cycles + 1
    print(number_of_cycles)
    if(number_of_cycles > battery_lifetime_cycle){
      break
    }
  }
  
  # Calculate NPV value over lifetime
  grid_battery_df$Savings <- round(grid_battery_df$InitialElecUnitskwh - grid_battery_df$AdjElecUnitskwh, 2) * grid_battery_df$ElecPriceMWh / 1000
  npv <- select(grid_battery_df, Date, Time, Savings)
  npv$Year <- 0
  min_date <- min(npv$Date, na.rm = TRUE)
  npv$Year[npv$Date >= min_date & npv$Date < (min_date + years(1))] <- 1
  npv$Year[npv$Date >= (min_date + years(1)) & npv$Date < (min_date + years(2))] <- 2
  npv$Year[npv$Date >= (min_date + years(2)) & npv$Date < (min_date + years(3))] <- 3
  npv$Year[npv$Date >= (min_date + years(3)) & npv$Date < (min_date + years(4))] <- 4
  npv$Year[npv$Date >= (min_date + years(4)) & npv$Date < (min_date + years(5))] <- 5
  npv$Year[npv$Date >= (min_date + years(5)) & npv$Date < (min_date + years(6))] <- 6
  npv$Year[npv$Date >= (min_date + years(6)) & npv$Date < (min_date + years(7))] <- 7
  npv$Year[npv$Date >= (min_date + years(7)) & npv$Date < (min_date + years(8))] <- 8
  npv$Year[npv$Date >= (min_date + years(8)) & npv$Date < (min_date + years(9))] <- 9
  npv$Year[npv$Date >= (min_date + years(9)) & npv$Date < (min_date + years(10))] <- 10
  
  # Calculate NPV for ten years
  year1_battery_disc_cash_flow_tot <- sum(npv$Savings[npv$Year == 1], na.rm = TRUE) / (npv_discount)
  year2_battery_disc_cash_flow_tot <- sum(npv$Savings[npv$Year == 2], na.rm = TRUE) / (npv_discount^2)
  year3_battery_disc_cash_flow_tot <- sum(npv$Savings[npv$Year == 3], na.rm = TRUE) / (npv_discount^3)
  year4_battery_disc_cash_flow_tot <- sum(npv$Savings[npv$Year == 4], na.rm = TRUE) / (npv_discount^4)
  year5_battery_disc_cash_flow_tot <- sum(npv$Savings[npv$Year == 5], na.rm = TRUE) / (npv_discount^5)
  year6_battery_disc_cash_flow_tot <- sum(npv$Savings[npv$Year == 6], na.rm = TRUE) / (npv_discount^6)
  year7_battery_disc_cash_flow_tot <- sum(npv$Savings[npv$Year == 7], na.rm = TRUE) / (npv_discount^7)
  year8_battery_disc_cash_flow_tot <- sum(npv$Savings[npv$Year == 8], na.rm = TRUE) / (npv_discount^8)
  year9_battery_disc_cash_flow_tot <- sum(npv$Savings[npv$Year == 9], na.rm = TRUE) / (npv_discount^9)
  year10_battery_disc_cash_flow_tot <- sum(npv$Savings[npv$Year == 10], na.rm = TRUE) / (npv_discount^10)
  
  total_npv <- sum(year1_battery_disc_cash_flow_tot, year2_battery_disc_cash_flow_tot, year3_battery_disc_cash_flow_tot, year4_battery_disc_cash_flow_tot,
                   year5_battery_disc_cash_flow_tot, year6_battery_disc_cash_flow_tot, year7_battery_disc_cash_flow_tot, year8_battery_disc_cash_flow_tot,
                   year9_battery_disc_cash_flow_tot, year10_battery_disc_cash_flow_tot)
  
  # Select only relevant columns for orginal grid battery dataframe
  grid_battery_df <- select(grid_battery_df, -InitialElecUnitskwh, when_charge, ChargeAmount, usage_from_grid_battery)
  # Return list of battery demand profile, battery cost, battery size and npv
  grid_battery_list <- list(grid_battery_df, total_setup_grid_battery, )
  return(grid_battery_df, total_setup_grid_battery, grid_battery_size_req, total_npv)
}

##############################################################################
# Stage 6 - Apply demand forecasting after investments
site_opt5 <- site_opt4
# Calculate base electricity spend
site_opt5$BaseElecSpendGBP <- round((site_opt5$ElecPriceMWh * site_opt5$BaseElecUnitskWh) / 1000, 2)
site_opt5$BaseGasSpendGBP <- round(site_opt5$BaseGasUnitskWh * site_opt5$GasPriceMWh / 1000, 2)
# AdjElecUnitskwh and  AdjGasUnitskWh is added below - these are the outputs that adjust as technologies are added
site_opt5$AdjElecUnitskwh <- site_opt5$BaseElecUnitskWh
site_opt5$AdjGasUnitskWh <- site_opt5$BaseGasUnitskWh
# Reorder
site_opt5 <- site_opt5 %>% select(Date, Time, ElecPriceMWh, BaseElecUnitskWh, BaseElecSpendGBP, AdjElecUnitskwh, GasPriceMWh, BaseGasUnitskWh, BaseGasSpendGBP, AdjGasUnitskWh)

# Process output function based on siteopt
for (i in 1:nrow(technology_df)) {
  technology_number <- technology_df[i, 1]
  technology_name <- technology_df[i, 2]
  if (technology_name == "led") {
    site_opt5 <- led_function(site_opt5, technology_number)
  } else if (technology_name == "solar") {
    site_opt5 <- solar_function(site_opt5, solar_panel, technology_number)
  } else if (technology_name == "solarandbattery") {
    # Needs to be run in this order
    site_opt5 <- solar_battery_function(site_opt5, solar_panel, technology_number)
    total_setup_solar_battery <- solar_battery_cost_function(site_opt5)
    site_opt5 <- select(site_opt5, -TotalGenSolarkwh, -InitialElecUnitskwh, -UsageRank, -ExcessGeneration, -UsageFromBattery)
  } else if (technology_name == "ashp") {
    site_opt5 <- ashp_function(site_opt5, technology_number)
  } else if (technology_name == "chp") {
    site_opt5 <- chp_function(site_opt5, technology_number)
  } else if (technology_name == "gridbattery") {
    # Needs to be run in this order
    total_setup_grid_battery <- grid_battery_cost_function(site_opt5)
    site_opt5 <- grid_battery_function(site_opt5, technology_number)
  } else {
    site_opt5[paste0(technology_number, "_savings_elec")] <- 0
    site_opt5[paste0(technology_number, "_savings_gas")] <- 0
  }
}

# Calculate
site_opt5$FinalElecUnitskWh <- site_opt5$AdjElecUnitskwh
site_opt5$FinalGasUnitskWh <- site_opt5$AdjGasUnitskWh
site_opt5$FinalElecSpendGBP <- round((site_opt5$ElecPriceMWh * site_opt5$FinalElecUnitskWh) / 1000, 2)
site_opt5$FinalGasSpendGBP <- round(site_opt5$AdjGasUnitskWh * site_opt5$GasPriceMWh / 1000, 2)

# create year col
site_opt5$Year <- 0
min_date <- min(site_opt5$Date, na.rm = TRUE)
site_opt5$Year[site_opt5$Date >= min_date & site_opt5$Date < (min_date + years(1))] <- 1
site_opt5$Year[site_opt5$Date >= (min_date + years(1)) & site_opt5$Date < (min_date + years(2))] <- 2
site_opt5$Year[site_opt5$Date >= (min_date + years(2)) & site_opt5$Date < (min_date + years(3))] <- 3
site_opt5$Year[site_opt5$Date >= (min_date + years(3)) & site_opt5$Date < (min_date + years(4))] <- 4
site_opt5$Year[site_opt5$Date >= (min_date + years(4)) & site_opt5$Date < (min_date + years(5))] <- 5
site_opt5$Year[site_opt5$Date >= (min_date + years(5)) & site_opt5$Date < (min_date + years(6))] <- 6
site_opt5$Year[site_opt5$Date >= (min_date + years(6)) & site_opt5$Date < (min_date + years(7))] <- 7
site_opt5$Year[site_opt5$Date >= (min_date + years(7)) & site_opt5$Date < (min_date + years(8))] <- 8
site_opt5$Year[site_opt5$Date >= (min_date + years(8)) & site_opt5$Date < (min_date + years(9))] <- 9
site_opt5$Year[site_opt5$Date >= (min_date + years(9)) & site_opt5$Date < (min_date + years(10))] <- 10

site_opt5 <- site_opt5[, c(1, 2, ncol(site_opt5), 3:(ncol(site_opt5) - 1))]

# save
site_opt_final <- site_opt5
save(site_opt_final, file = "./outputs/site_opt_final.RData")
write.csv(site_opt_final, file = "./outputs/site_opt_final.csv", row.names = FALSE)

##############################################################################
# Technology setup costs
for (i in 1:nrow(technology_df)) {
  technology_number <- technology_df[i, 1]
  technology_name <- technology_df[i, 2]
  if (technology_name == "led") {
    assign(paste0(technology_number, "_setup_costs"), cost_set_up)
  } else if (technology_name == "solar") {
    assign(paste0(technology_number, "_setup_costs"), total_setup_panels)
  } else if (technology_name == "solarandbattery") {
    assign(paste0(technology_number, "_setup_costs"), total_setup_panels + total_setup_solar_battery)
  } else if (technology_name == "ashp") {
    assign(paste0(technology_number, "_setup_costs"), total_setup_cost_ashp)
  } else if (technology_name == "chp") {
    assign(paste0(technology_number, "_setup_costs"), installed_cost_chp)
  } else if (technology_name == "gridbattery") {
    assign(paste0(technology_number, "_setup_costs"), total_setup_grid_battery)
  } else {
    assign(paste0(technology_number, "_setup_costs"), 0)
  }
}
set_up_list <- c(technology1_setup_costs, technology2_setup_costs, technology3_setup_costs, technology4_setup_costs)

# Technology annual costs
for (i in 1:nrow(technology_df)) {
  technology_number <- technology_df[i, 1]
  technology_name <- technology_df[i, 2]
  if (technology_name == "led") {
    assign(paste0(technology_number, "_annual_costs"), 0)
  } else if (technology_name == "solar") {
    assign(paste0(technology_number, "_annual_costs"), annual_mainten_panels)
  } else if (technology_name == "solarandbattery") {
    assign(paste0(technology_number, "_annual_costs"), annual_mainten_panels + 0)
  } else if (technology_name == "ashp") {
    assign(paste0(technology_number, "_annual_costs"), annual_mainten_ashp)
  } else if (technology_name == "chp") {
    assign(paste0(technology_number, "_annual_costs"), annual_maint_cost_chp)
  } else if (technology_name == "gridbattery") {
    assign(paste0(technology_number, "_annual_costs"), 0)
  } else {
    assign(paste0(technology_number, "_annual_costs"), 0)
  }
}
annual_cost_list <- c(technology1_annual_costs, technology2_annual_costs, technology3_annual_costs, technology4_annual_costs)

# Net increase cost
for (i in 1:nrow(technology_df)) {
  technology_number <- technology_df[i, 1]
  technology_name <- technology_df[i, 2]
  if (technology_name == "led") {
    assign(paste0(technology_number, "_increase_costs"), -round(bulb_replace_cost_save_annual, 0))
  } else if (technology_name == "solar") {
    assign(paste0(technology_number, "_increase_costs"), annual_cost_solar)
  } else if (technology_name == "solarandbattery") {
    assign(paste0(technology_number, "_increase_costs"), annual_cost_solar + 0)
  } else if (technology_name == "ashp") {
    assign(paste0(technology_number, "_increase_costs"), 0)
  } else if (technology_name == "chp") {
    assign(paste0(technology_number, "_increase_costs"), annual_cost_chp)
  } else if (technology_name == "gridbattery") {
    assign(paste0(technology_number, "_increase_costs"), 0)
  } else {
    assign(paste0(technology_number, "_increase_costs"), 0)
  }
}

increase_cost_list <- c(technology1_increase_costs, technology2_increase_costs, technology3_increase_costs, technology4_increase_costs)
tech_cost_df <- data.frame(technology_list, set_up_list, annual_cost_list, increase_cost_list)
names(tech_cost_df) <- c("Technologies", "SetUp", "GrossAnCost", "NetIncrCost")
net_incr_cost_tot <- sum(tech_cost_df$NetIncrCost, na.rm = TRUE)
gross_ann_cost <- sum(tech_cost_df$GrossAnCost, na.rm = TRUE)
setup_tot <- sum(tech_cost_df$SetUp, na.rm = TRUE)

########################################################################################################################
# Electricity
# Initial
year0_elec_base <- 0
year1_elec_base <- sum(site_opt5$BaseElecUnitskWh[site_opt5$Year == 1], na.rm = TRUE)
year2_elec_base <- sum(site_opt5$BaseElecUnitskWh[site_opt5$Year == 2], na.rm = TRUE)
year3_elec_base <- sum(site_opt5$BaseElecUnitskWh[site_opt5$Year == 3], na.rm = TRUE)
year4_elec_base <- sum(site_opt5$BaseElecUnitskWh[site_opt5$Year == 4], na.rm = TRUE)
year5_elec_base <- sum(site_opt5$BaseElecUnitskWh[site_opt5$Year == 5], na.rm = TRUE)
year6_elec_base <- sum(site_opt5$BaseElecUnitskWh[site_opt5$Year == 6], na.rm = TRUE)
year7_elec_base <- sum(site_opt5$BaseElecUnitskWh[site_opt5$Year == 7], na.rm = TRUE)
year8_elec_base <- sum(site_opt5$BaseElecUnitskWh[site_opt5$Year == 8], na.rm = TRUE)
year9_elec_base <- sum(site_opt5$BaseElecUnitskWh[site_opt5$Year == 9], na.rm = TRUE)
year10_elec_base <- sum(site_opt5$BaseElecUnitskWh[site_opt5$Year == 10], na.rm = TRUE)

# Final
year0_elec_new <- 0
year1_elec_new <- sum(site_opt5$FinalElecUnitskWh[site_opt5$Year == 1], na.rm = TRUE)
year2_elec_new <- sum(site_opt5$FinalElecUnitskWh[site_opt5$Year == 2], na.rm = TRUE)
year3_elec_new <- sum(site_opt5$FinalElecUnitskWh[site_opt5$Year == 3], na.rm = TRUE)
year4_elec_new <- sum(site_opt5$FinalElecUnitskWh[site_opt5$Year == 4], na.rm = TRUE)
year5_elec_new <- sum(site_opt5$FinalElecUnitskWh[site_opt5$Year == 5], na.rm = TRUE)
year6_elec_new <- sum(site_opt5$FinalElecUnitskWh[site_opt5$Year == 6], na.rm = TRUE)
year7_elec_new <- sum(site_opt5$FinalElecUnitskWh[site_opt5$Year == 7], na.rm = TRUE)
year8_elec_new <- sum(site_opt5$FinalElecUnitskWh[site_opt5$Year == 8], na.rm = TRUE)
year9_elec_new <- sum(site_opt5$FinalElecUnitskWh[site_opt5$Year == 9], na.rm = TRUE)
year10_elec_new <- sum(site_opt5$FinalElecUnitskWh[site_opt5$Year == 10], na.rm = TRUE)

# Change year 1 to 10
year0_elec_change <- 0
year1_elec_change <- year1_elec_new - year1_elec_base
year2_elec_change <- year2_elec_new - year2_elec_base
year3_elec_change <- year3_elec_new - year3_elec_base
year4_elec_change <- year4_elec_new - year4_elec_base
year5_elec_change <- year5_elec_new - year5_elec_base
year6_elec_change <- year6_elec_new - year6_elec_base
year7_elec_change <- year7_elec_new - year7_elec_base
year8_elec_change <- year8_elec_new - year8_elec_base
year9_elec_change <- year9_elec_new - year9_elec_base
year10_elec_change <- year10_elec_new - year10_elec_base

############################### Gas ############################
# base year 1 - 10
year0_gas_base <- 0
year1_gas_base <- sum(site_opt5$BaseGasUnitskWh[site_opt5$Year == 1], na.rm = TRUE)
year2_gas_base <- sum(site_opt5$BaseGasUnitskWh[site_opt5$Year == 2], na.rm = TRUE)
year3_gas_base <- sum(site_opt5$BaseGasUnitskWh[site_opt5$Year == 3], na.rm = TRUE)
year4_gas_base <- sum(site_opt5$BaseGasUnitskWh[site_opt5$Year == 4], na.rm = TRUE)
year5_gas_base <- sum(site_opt5$BaseGasUnitskWh[site_opt5$Year == 5], na.rm = TRUE)
year6_gas_base <- sum(site_opt5$BaseGasUnitskWh[site_opt5$Year == 6], na.rm = TRUE)
year7_gas_base <- sum(site_opt5$BaseGasUnitskWh[site_opt5$Year == 7], na.rm = TRUE)
year8_gas_base <- sum(site_opt5$BaseGasUnitskWh[site_opt5$Year == 8], na.rm = TRUE)
year9_gas_base <- sum(site_opt5$BaseGasUnitskWh[site_opt5$Year == 9], na.rm = TRUE)
year10_gas_base <- sum(site_opt5$BaseGasUnitskWh[site_opt5$Year == 10], na.rm = TRUE)

# new year 1 - 10
year0_gas_new <- 0
year1_gas_new <- sum(site_opt5$FinalGasUnitskWh[site_opt5$Year == 1], na.rm = TRUE)
year2_gas_new <- sum(site_opt5$FinalGasUnitskWh[site_opt5$Year == 2], na.rm = TRUE)
year3_gas_new <- sum(site_opt5$FinalGasUnitskWh[site_opt5$Year == 3], na.rm = TRUE)
year4_gas_new <- sum(site_opt5$FinalGasUnitskWh[site_opt5$Year == 4], na.rm = TRUE)
year5_gas_new <- sum(site_opt5$FinalGasUnitskWh[site_opt5$Year == 5], na.rm = TRUE)
year6_gas_new <- sum(site_opt5$FinalGasUnitskWh[site_opt5$Year == 6], na.rm = TRUE)
year7_gas_new <- sum(site_opt5$FinalGasUnitskWh[site_opt5$Year == 7], na.rm = TRUE)
year8_gas_new <- sum(site_opt5$FinalGasUnitskWh[site_opt5$Year == 8], na.rm = TRUE)
year9_gas_new <- sum(site_opt5$FinalGasUnitskWh[site_opt5$Year == 9], na.rm = TRUE)
year10_gas_new <- sum(site_opt5$FinalGasUnitskWh[site_opt5$Year == 10], na.rm = TRUE)

# Change year 1 - 10
year0_gas_change <- 0
year1_gas_change <- year1_gas_new - year1_gas_base
year2_gas_change <- year2_gas_new - year2_gas_base
year3_gas_change <- year3_gas_new - year3_gas_base
year4_gas_change <- year4_gas_new - year4_gas_base
year5_gas_change <- year5_gas_new - year5_gas_base
year6_gas_change <- year6_gas_new - year6_gas_base
year7_gas_change <- year7_gas_new - year7_gas_base
year8_gas_change <- year8_gas_new - year8_gas_base
year9_gas_change <- year9_gas_new - year9_gas_base
year10_gas_change <- year10_gas_new - year10_gas_base

############################### CO2 ############################
# base year 1 - 10
year0_co2_base <- 0
year1_co2_base <- elec_co2 * year1_elec_base + gas_co2 * year1_gas_base
year2_co2_base <- elec_co2 * year2_elec_base + gas_co2 * year2_gas_base
year3_co2_base <- elec_co2 * year3_elec_base + gas_co2 * year3_gas_base
year4_co2_base <- elec_co2 * year4_elec_base + gas_co2 * year4_gas_base
year5_co2_base <- elec_co2 * year5_elec_base + gas_co2 * year5_gas_base
year6_co2_base <- elec_co2 * year6_elec_base + gas_co2 * year6_gas_base
year7_co2_base <- elec_co2 * year7_elec_base + gas_co2 * year7_gas_base
year8_co2_base <- elec_co2 * year8_elec_base + gas_co2 * year8_gas_base
year9_co2_base <- elec_co2 * year9_elec_base + gas_co2 * year9_gas_base
year10_co2_base <- elec_co2 * year10_elec_base + gas_co2 * year10_gas_base

# new
year0_co2_new <- 0
year1_co2_new <- ifelse(is.na(elec_co2 * year1_elec_new + gas_co2 * year1_gas_new),
                        year1_co2_base,
                        elec_co2 * year1_elec_new + gas_co2 * year1_gas_new
)
year2_co2_new <- ifelse(is.na(elec_co2 * year2_elec_new + gas_co2 * year2_gas_new),
                        year2_co2_base,
                        elec_co2 * year2_elec_new + gas_co2 * year2_gas_new
)
year3_co2_new <- ifelse(is.na(elec_co2 * year3_elec_new + gas_co2 * year3_gas_new),
                        year3_co2_base,
                        elec_co2 * year3_elec_new + gas_co2 * year3_gas_new
)
year4_co2_new <- ifelse(is.na(elec_co2 * year4_elec_new + gas_co2 * year4_gas_new),
                        year4_co2_base,
                        elec_co2 * year4_elec_new + gas_co2 * year4_gas_new
)
year5_co2_new <- ifelse(is.na(elec_co2 * year5_elec_new + gas_co2 * year5_gas_new),
                        year5_co2_base,
                        elec_co2 * year5_elec_new + gas_co2 * year5_gas_new
)
year6_co2_new <- ifelse(is.na(elec_co2 * year6_elec_new + gas_co2 * year6_gas_new),
                        year6_co2_base,
                        elec_co2 * year6_elec_new + gas_co2 * year6_gas_new
)
year7_co2_new <- ifelse(is.na(elec_co2 * year7_elec_new + gas_co2 * year7_gas_new),
                        year7_co2_base,
                        elec_co2 * year7_elec_new + gas_co2 * year7_gas_new
)
year8_co2_new <- ifelse(is.na(elec_co2 * year8_elec_new + gas_co2 * year8_gas_new),
                        year8_co2_base,
                        elec_co2 * year8_elec_new + gas_co2 * year8_gas_new
)
year9_co2_new <- ifelse(is.na(elec_co2 * year9_elec_new + gas_co2 * year9_gas_new),
                        year9_co2_base,
                        elec_co2 * year9_elec_new + gas_co2 * year9_gas_new
)
year10_co2_new <- ifelse(is.na(elec_co2 * year10_elec_new + gas_co2 * year10_gas_new),
                         year10_co2_base,
                         elec_co2 * year10_elec_new + gas_co2 * year10_gas_new
)

# Change year 1 - 10
year0_co2_change <- 0
year1_co2_change <- ifelse(is.na(year1_co2_new - year1_co2_base), 0, year1_co2_new - year1_co2_base)
year2_co2_change <- ifelse(is.na(year2_co2_new - year2_co2_base), 0, year2_co2_new - year2_co2_base)
year3_co2_change <- ifelse(is.na(year3_co2_new - year3_co2_base), 0, year3_co2_new - year3_co2_base)
year4_co2_change <- ifelse(is.na(year4_co2_new - year4_co2_base), 0, year4_co2_new - year4_co2_base)
year5_co2_change <- ifelse(is.na(year5_co2_new - year5_co2_base), 0, year5_co2_new - year5_co2_base)
year6_co2_change <- ifelse(is.na(year6_co2_new - year6_co2_base), 0, year6_co2_new - year6_co2_base)
year7_co2_change <- ifelse(is.na(year7_co2_new - year7_co2_base), 0, year7_co2_new - year7_co2_base)
year8_co2_change <- ifelse(is.na(year8_co2_new - year8_co2_base), 0, year8_co2_new - year8_co2_base)
year9_co2_change <- ifelse(is.na(year9_co2_new - year9_co2_base), 0, year9_co2_new - year9_co2_base)
year10_co2_change <- ifelse(is.na(year10_co2_new - year10_co2_base), 0, year10_co2_new - year10_co2_base)

######################### Electricity Paid #######################################
# baseline all years 1 - 10
year0_elec_paid_base <- 0
year1_elec_paid_base <- sum(site_opt5$BaseElecSpendGBP[site_opt5$Year == 1], na.rm = TRUE)
year2_elec_paid_base <- sum(site_opt5$BaseElecSpendGBP[site_opt5$Year == 2], na.rm = TRUE)
year3_elec_paid_base <- sum(site_opt5$BaseElecSpendGBP[site_opt5$Year == 3], na.rm = TRUE)
year4_elec_paid_base <- sum(site_opt5$BaseElecSpendGBP[site_opt5$Year == 4], na.rm = TRUE)
year5_elec_paid_base <- sum(site_opt5$BaseElecSpendGBP[site_opt5$Year == 5], na.rm = TRUE)
year6_elec_paid_base <- sum(site_opt5$BaseElecSpendGBP[site_opt5$Year == 6], na.rm = TRUE)
year7_elec_paid_base <- sum(site_opt5$BaseElecSpendGBP[site_opt5$Year == 7], na.rm = TRUE)
year8_elec_paid_base <- sum(site_opt5$BaseElecSpendGBP[site_opt5$Year == 8], na.rm = TRUE)
year9_elec_paid_base <- sum(site_opt5$BaseElecSpendGBP[site_opt5$Year == 9], na.rm = TRUE)
year10_elec_paid_base <- sum(site_opt5$BaseElecSpendGBP[site_opt5$Year == 10], na.rm = TRUE)

# technology 1 all years 1 - 10
year0_tech_1_elec_paid <- 0
year1_tech_1_elec_paid <- -sum(site_opt5$technology1_savings_elec[site_opt5$Year == 1], na.rm = TRUE)
year2_tech_1_elec_paid <- -sum(site_opt5$technology1_savings_elec[site_opt5$Year == 2], na.rm = TRUE)
year3_tech_1_elec_paid <- -sum(site_opt5$technology1_savings_elec[site_opt5$Year == 3], na.rm = TRUE)
year4_tech_1_elec_paid <- -sum(site_opt5$technology1_savings_elec[site_opt5$Year == 4], na.rm = TRUE)
year5_tech_1_elec_paid <- -sum(site_opt5$technology1_savings_elec[site_opt5$Year == 5], na.rm = TRUE)
year6_tech_1_elec_paid <- -sum(site_opt5$technology1_savings_elec[site_opt5$Year == 6], na.rm = TRUE)
year7_tech_1_elec_paid <- -sum(site_opt5$technology1_savings_elec[site_opt5$Year == 7], na.rm = TRUE)
year8_tech_1_elec_paid <- -sum(site_opt5$technology1_savings_elec[site_opt5$Year == 8], na.rm = TRUE)
year9_tech_1_elec_paid <- -sum(site_opt5$technology1_savings_elec[site_opt5$Year == 9], na.rm = TRUE)
year10_tech_1_elec_paid <- -sum(site_opt5$technology1_savings_elec[site_opt5$Year == 10], na.rm = TRUE)

# technology 2 all years 1 - 10
year0_tech_2_elec_paid <- 0
year1_tech_2_elec_paid <- -sum(site_opt5$technology2_savings_elec[site_opt5$Year == 1], na.rm = TRUE)
year2_tech_2_elec_paid <- -sum(site_opt5$technology2_savings_elec[site_opt5$Year == 2], na.rm = TRUE)
year3_tech_2_elec_paid <- -sum(site_opt5$technology2_savings_elec[site_opt5$Year == 3], na.rm = TRUE)
year4_tech_2_elec_paid <- -sum(site_opt5$technology2_savings_elec[site_opt5$Year == 4], na.rm = TRUE)
year5_tech_2_elec_paid <- -sum(site_opt5$technology2_savings_elec[site_opt5$Year == 5], na.rm = TRUE)
year6_tech_2_elec_paid <- -sum(site_opt5$technology2_savings_elec[site_opt5$Year == 6], na.rm = TRUE)
year7_tech_2_elec_paid <- -sum(site_opt5$technology2_savings_elec[site_opt5$Year == 7], na.rm = TRUE)
year8_tech_2_elec_paid <- -sum(site_opt5$technology2_savings_elec[site_opt5$Year == 8], na.rm = TRUE)
year9_tech_2_elec_paid <- -sum(site_opt5$technology2_savings_elec[site_opt5$Year == 9], na.rm = TRUE)
year10_tech_2_elec_paid <- -sum(site_opt5$technology2_savings_elec[site_opt5$Year == 10], na.rm = TRUE)

# technology 3 all years 1 - 10
year0_tech_3_elec_paid <- 0
year1_tech_3_elec_paid <- -sum(site_opt5$technology3_savings_elec[site_opt5$Year == 1], na.rm = TRUE)
year2_tech_3_elec_paid <- -sum(site_opt5$technology3_savings_elec[site_opt5$Year == 2], na.rm = TRUE)
year3_tech_3_elec_paid <- -sum(site_opt5$technology3_savings_elec[site_opt5$Year == 3], na.rm = TRUE)
year4_tech_3_elec_paid <- -sum(site_opt5$technology3_savings_elec[site_opt5$Year == 4], na.rm = TRUE)
year5_tech_3_elec_paid <- -sum(site_opt5$technology3_savings_elec[site_opt5$Year == 5], na.rm = TRUE)
year6_tech_3_elec_paid <- -sum(site_opt5$technology3_savings_elec[site_opt5$Year == 6], na.rm = TRUE)
year7_tech_3_elec_paid <- -sum(site_opt5$technology3_savings_elec[site_opt5$Year == 7], na.rm = TRUE)
year8_tech_3_elec_paid <- -sum(site_opt5$technology3_savings_elec[site_opt5$Year == 8], na.rm = TRUE)
year9_tech_3_elec_paid <- -sum(site_opt5$technology3_savings_elec[site_opt5$Year == 9], na.rm = TRUE)
year10_tech_3_elec_paid <- -sum(site_opt5$technology3_savings_elec[site_opt5$Year == 10], na.rm = TRUE)

# technology 4 all years 1 - 10
year0_tech_4_elec_paid <- 0
year1_tech_4_elec_paid <- -sum(site_opt5$technology4_savings_elec[site_opt5$Year == 1], na.rm = TRUE)
year2_tech_4_elec_paid <- -sum(site_opt5$technology4_savings_elec[site_opt5$Year == 2], na.rm = TRUE)
year3_tech_4_elec_paid <- -sum(site_opt5$technology4_savings_elec[site_opt5$Year == 3], na.rm = TRUE)
year4_tech_4_elec_paid <- -sum(site_opt5$technology4_savings_elec[site_opt5$Year == 4], na.rm = TRUE)
year5_tech_4_elec_paid <- -sum(site_opt5$technology4_savings_elec[site_opt5$Year == 5], na.rm = TRUE)
year6_tech_4_elec_paid <- -sum(site_opt5$technology4_savings_elec[site_opt5$Year == 6], na.rm = TRUE)
year7_tech_4_elec_paid <- -sum(site_opt5$technology4_savings_elec[site_opt5$Year == 7], na.rm = TRUE)
year8_tech_4_elec_paid <- -sum(site_opt5$technology4_savings_elec[site_opt5$Year == 8], na.rm = TRUE)
year9_tech_4_elec_paid <- -sum(site_opt5$technology4_savings_elec[site_opt5$Year == 9], na.rm = TRUE)
year10_tech_4_elec_paid <- -sum(site_opt5$technology4_savings_elec[site_opt5$Year == 10], na.rm = TRUE)

# new all years 1 - 10
year0_elec_paid_new <- 0
year1_elec_paid_new <- year1_elec_paid_base + year1_tech_1_elec_paid + year1_tech_2_elec_paid + year1_tech_3_elec_paid + year1_tech_4_elec_paid
year2_elec_paid_new <- year2_elec_paid_base + year2_tech_1_elec_paid + year2_tech_2_elec_paid + year2_tech_3_elec_paid + year2_tech_4_elec_paid
year3_elec_paid_new <- year3_elec_paid_base + year3_tech_1_elec_paid + year3_tech_2_elec_paid + year3_tech_3_elec_paid + year3_tech_4_elec_paid
year4_elec_paid_new <- year4_elec_paid_base + year4_tech_1_elec_paid + year4_tech_2_elec_paid + year4_tech_3_elec_paid + year4_tech_4_elec_paid
year5_elec_paid_new <- year5_elec_paid_base + year5_tech_1_elec_paid + year5_tech_2_elec_paid + year5_tech_3_elec_paid + year5_tech_4_elec_paid
year6_elec_paid_new <- year6_elec_paid_base + year6_tech_1_elec_paid + year6_tech_2_elec_paid + year6_tech_3_elec_paid + year6_tech_4_elec_paid
year7_elec_paid_new <- year7_elec_paid_base + year7_tech_1_elec_paid + year7_tech_2_elec_paid + year7_tech_3_elec_paid + year7_tech_4_elec_paid
year8_elec_paid_new <- year8_elec_paid_base + year8_tech_1_elec_paid + year8_tech_2_elec_paid + year8_tech_3_elec_paid + year8_tech_4_elec_paid
year9_elec_paid_new <- year9_elec_paid_base + year9_tech_1_elec_paid + year9_tech_2_elec_paid + year9_tech_3_elec_paid + year9_tech_4_elec_paid
year10_elec_paid_new <- year10_elec_paid_base + year10_tech_1_elec_paid + year10_tech_2_elec_paid + year10_tech_3_elec_paid + year10_tech_4_elec_paid

############################ Gas Paid ############################################
# baseline
year0_gas_paid_base <- 0
year1_gas_paid_base <- sum(site_opt5$BaseGasSpendGBP[site_opt5$Year == 1], na.rm = TRUE)
year2_gas_paid_base <- sum(site_opt5$BaseGasSpendGBP[site_opt5$Year == 2], na.rm = TRUE)
year3_gas_paid_base <- sum(site_opt5$BaseGasSpendGBP[site_opt5$Year == 3], na.rm = TRUE)
year4_gas_paid_base <- sum(site_opt5$BaseGasSpendGBP[site_opt5$Year == 4], na.rm = TRUE)
year5_gas_paid_base <- sum(site_opt5$BaseGasSpendGBP[site_opt5$Year == 5], na.rm = TRUE)
year6_gas_paid_base <- sum(site_opt5$BaseGasSpendGBP[site_opt5$Year == 6], na.rm = TRUE)
year7_gas_paid_base <- sum(site_opt5$BaseGasSpendGBP[site_opt5$Year == 7], na.rm = TRUE)
year8_gas_paid_base <- sum(site_opt5$BaseGasSpendGBP[site_opt5$Year == 8], na.rm = TRUE)
year9_gas_paid_base <- sum(site_opt5$BaseGasSpendGBP[site_opt5$Year == 9], na.rm = TRUE)
year10_gas_paid_base <- sum(site_opt5$BaseGasSpendGBP[site_opt5$Year == 10], na.rm = TRUE)

# technology 1
year0_tech_1_gas_paid <- 0
year1_tech_1_gas_paid <- -sum(site_opt5$technology1_savings_gas[site_opt5$Year == 1], na.rm = TRUE)
year2_tech_1_gas_paid <- -sum(site_opt5$technology1_savings_gas[site_opt5$Year == 2], na.rm = TRUE)
year3_tech_1_gas_paid <- -sum(site_opt5$technology1_savings_gas[site_opt5$Year == 3], na.rm = TRUE)
year4_tech_1_gas_paid <- -sum(site_opt5$technology1_savings_gas[site_opt5$Year == 4], na.rm = TRUE)
year5_tech_1_gas_paid <- -sum(site_opt5$technology1_savings_gas[site_opt5$Year == 5], na.rm = TRUE)
year6_tech_1_gas_paid <- -sum(site_opt5$technology1_savings_gas[site_opt5$Year == 6], na.rm = TRUE)
year7_tech_1_gas_paid <- -sum(site_opt5$technology1_savings_gas[site_opt5$Year == 7], na.rm = TRUE)
year8_tech_1_gas_paid <- -sum(site_opt5$technology1_savings_gas[site_opt5$Year == 8], na.rm = TRUE)
year9_tech_1_gas_paid <- -sum(site_opt5$technology1_savings_gas[site_opt5$Year == 9], na.rm = TRUE)
year10_tech_1_gas_paid <- -sum(site_opt5$technology1_savings_gas[site_opt5$Year == 10], na.rm = TRUE)

# technology 2
year0_tech_2_gas_paid <- 0
year1_tech_2_gas_paid <- -sum(site_opt5$technology2_savings_gas[site_opt5$Year == 1], na.rm = TRUE)
year2_tech_2_gas_paid <- -sum(site_opt5$technology2_savings_gas[site_opt5$Year == 2], na.rm = TRUE)
year3_tech_2_gas_paid <- -sum(site_opt5$technology2_savings_gas[site_opt5$Year == 3], na.rm = TRUE)
year4_tech_2_gas_paid <- -sum(site_opt5$technology2_savings_gas[site_opt5$Year == 4], na.rm = TRUE)
year5_tech_2_gas_paid <- -sum(site_opt5$technology2_savings_gas[site_opt5$Year == 5], na.rm = TRUE)
year6_tech_2_gas_paid <- -sum(site_opt5$technology2_savings_gas[site_opt5$Year == 6], na.rm = TRUE)
year7_tech_2_gas_paid <- -sum(site_opt5$technology2_savings_gas[site_opt5$Year == 7], na.rm = TRUE)
year8_tech_2_gas_paid <- -sum(site_opt5$technology2_savings_gas[site_opt5$Year == 8], na.rm = TRUE)
year9_tech_2_gas_paid <- -sum(site_opt5$technology2_savings_gas[site_opt5$Year == 9], na.rm = TRUE)
year10_tech_2_gas_paid <- -sum(site_opt5$technology2_savings_gas[site_opt5$Year == 10], na.rm = TRUE)

# technology 3
year0_tech_3_gas_paid <- 0
year1_tech_3_gas_paid <- -sum(site_opt5$technology3_savings_gas[site_opt5$Year == 1], na.rm = TRUE)
year2_tech_3_gas_paid <- -sum(site_opt5$technology3_savings_gas[site_opt5$Year == 2], na.rm = TRUE)
year3_tech_3_gas_paid <- -sum(site_opt5$technology3_savings_gas[site_opt5$Year == 3], na.rm = TRUE)
year4_tech_3_gas_paid <- -sum(site_opt5$technology3_savings_gas[site_opt5$Year == 4], na.rm = TRUE)
year5_tech_3_gas_paid <- -sum(site_opt5$technology3_savings_gas[site_opt5$Year == 5], na.rm = TRUE)
year6_tech_3_gas_paid <- -sum(site_opt5$technology3_savings_gas[site_opt5$Year == 6], na.rm = TRUE)
year7_tech_3_gas_paid <- -sum(site_opt5$technology3_savings_gas[site_opt5$Year == 7], na.rm = TRUE)
year8_tech_3_gas_paid <- -sum(site_opt5$technology3_savings_gas[site_opt5$Year == 8], na.rm = TRUE)
year9_tech_3_gas_paid <- -sum(site_opt5$technology3_savings_gas[site_opt5$Year == 9], na.rm = TRUE)
year10_tech_3_gas_paid <- -sum(site_opt5$technology3_savings_gas[site_opt5$Year == 10], na.rm = TRUE)

# technology 4
year0_tech_4_gas_paid <- 0
year1_tech_4_gas_paid <- -sum(site_opt5$technology4_savings_gas[site_opt5$Year == 1], na.rm = TRUE)
year2_tech_4_gas_paid <- -sum(site_opt5$technology4_savings_gas[site_opt5$Year == 2], na.rm = TRUE)
year3_tech_4_gas_paid <- -sum(site_opt5$technology4_savings_gas[site_opt5$Year == 3], na.rm = TRUE)
year4_tech_4_gas_paid <- -sum(site_opt5$technology4_savings_gas[site_opt5$Year == 4], na.rm = TRUE)
year5_tech_4_gas_paid <- -sum(site_opt5$technology4_savings_gas[site_opt5$Year == 5], na.rm = TRUE)
year6_tech_4_gas_paid <- -sum(site_opt5$technology4_savings_gas[site_opt5$Year == 6], na.rm = TRUE)
year7_tech_4_gas_paid <- -sum(site_opt5$technology4_savings_gas[site_opt5$Year == 7], na.rm = TRUE)
year8_tech_4_gas_paid <- -sum(site_opt5$technology4_savings_gas[site_opt5$Year == 8], na.rm = TRUE)
year9_tech_4_gas_paid <- -sum(site_opt5$technology4_savings_gas[site_opt5$Year == 9], na.rm = TRUE)
year10_tech_4_gas_paid <- -sum(site_opt5$technology4_savings_gas[site_opt5$Year == 10], na.rm = TRUE)

# New
year0_gas_paid_new <- 0
year1_gas_paid_new <- year1_gas_paid_base + year1_tech_1_gas_paid + year1_tech_2_gas_paid + year1_tech_3_gas_paid + year1_tech_4_gas_paid
year2_gas_paid_new <- year2_gas_paid_base + year2_tech_1_gas_paid + year2_tech_2_gas_paid + year2_tech_3_gas_paid + year2_tech_4_gas_paid
year3_gas_paid_new <- year3_gas_paid_base + year3_tech_1_gas_paid + year3_tech_2_gas_paid + year3_tech_3_gas_paid + year3_tech_4_gas_paid
year4_gas_paid_new <- year4_gas_paid_base + year4_tech_1_gas_paid + year4_tech_2_gas_paid + year4_tech_3_gas_paid + year4_tech_4_gas_paid
year5_gas_paid_new <- year5_gas_paid_base + year5_tech_1_gas_paid + year5_tech_2_gas_paid + year5_tech_3_gas_paid + year5_tech_4_gas_paid
year6_gas_paid_new <- year6_gas_paid_base + year6_tech_1_gas_paid + year6_tech_2_gas_paid + year6_tech_3_gas_paid + year6_tech_4_gas_paid
year7_gas_paid_new <- year7_gas_paid_base + year7_tech_1_gas_paid + year7_tech_2_gas_paid + year7_tech_3_gas_paid + year7_tech_4_gas_paid
year8_gas_paid_new <- year8_gas_paid_base + year8_tech_1_gas_paid + year8_tech_2_gas_paid + year8_tech_3_gas_paid + year8_tech_4_gas_paid
year9_gas_paid_new <- year9_gas_paid_base + year9_tech_1_gas_paid + year9_tech_2_gas_paid + year9_tech_3_gas_paid + year9_tech_4_gas_paid
year10_gas_paid_new <- year10_gas_paid_base + year10_tech_1_gas_paid + year10_tech_2_gas_paid + year10_tech_3_gas_paid + year10_tech_4_gas_paid

######################### 10 year Cashflow ###################################
# benefits
year0_cash_flow_benefits <- 0
year1_cash_flow_benefits <- -((year1_elec_paid_new - year1_elec_paid_base) + (year1_gas_paid_new - year1_gas_paid_base))
year2_cash_flow_benefits <- -((year2_elec_paid_new - year2_elec_paid_base) + (year2_gas_paid_new - year2_gas_paid_base))
year3_cash_flow_benefits <- -((year3_elec_paid_new - year3_elec_paid_base) + (year3_gas_paid_new - year3_gas_paid_base))
year4_cash_flow_benefits <- -((year4_elec_paid_new - year4_elec_paid_base) + (year4_gas_paid_new - year4_gas_paid_base))
year5_cash_flow_benefits <- -((year5_elec_paid_new - year5_elec_paid_base) + (year5_gas_paid_new - year5_gas_paid_base))
year6_cash_flow_benefits <- -((year6_elec_paid_new - year6_elec_paid_base) + (year6_gas_paid_new - year6_gas_paid_base))
year7_cash_flow_benefits <- -((year7_elec_paid_new - year7_elec_paid_base) + (year7_gas_paid_new - year7_gas_paid_base))
year8_cash_flow_benefits <- -((year8_elec_paid_new - year8_elec_paid_base) + (year8_gas_paid_new - year8_gas_paid_base))
year9_cash_flow_benefits <- -((year9_elec_paid_new - year9_elec_paid_base) + (year9_gas_paid_new - year9_gas_paid_base))
year10_cash_flow_benefits <- -((year10_elec_paid_new - year10_elec_paid_base) + (year10_gas_paid_new - year10_gas_paid_base))

# costs
year0_cash_flow_costs <- 0
year1_cash_flow_costs <- -net_incr_cost_tot
year2_cash_flow_costs <- -net_incr_cost_tot
year3_cash_flow_costs <- -net_incr_cost_tot
year4_cash_flow_costs <- -net_incr_cost_tot
year5_cash_flow_costs <- -net_incr_cost_tot
year6_cash_flow_costs <- -net_incr_cost_tot
year7_cash_flow_costs <- -net_incr_cost_tot
year8_cash_flow_costs <- -net_incr_cost_tot
year9_cash_flow_costs <- -net_incr_cost_tot
year10_cash_flow_costs <- -net_incr_cost_tot

# set up
year0_cash_flow_setup <- -setup_tot
year1_cash_flow_setup <- 0
year2_cash_flow_setup <- 0
year3_cash_flow_setup <- 0
year4_cash_flow_setup <- 0
year5_cash_flow_setup <- 0
year6_cash_flow_setup <- 0
year7_cash_flow_setup <- 0
year8_cash_flow_setup <- 0
year9_cash_flow_setup <- 0
year10_cash_flow_setup <- 0

# total
year0_cash_flow_tot <- year0_cash_flow_benefits + year0_cash_flow_costs + year0_cash_flow_setup
year1_cash_flow_tot <- year1_cash_flow_benefits + year1_cash_flow_costs + year1_cash_flow_setup
year2_cash_flow_tot <- year2_cash_flow_benefits + year2_cash_flow_costs + year2_cash_flow_setup
year3_cash_flow_tot <- year3_cash_flow_benefits + year3_cash_flow_costs + year3_cash_flow_setup
year4_cash_flow_tot <- year4_cash_flow_benefits + year4_cash_flow_costs + year4_cash_flow_setup
year5_cash_flow_tot <- year5_cash_flow_benefits + year5_cash_flow_costs + year5_cash_flow_setup
year6_cash_flow_tot <- year6_cash_flow_benefits + year6_cash_flow_costs + year6_cash_flow_setup
year7_cash_flow_tot <- year7_cash_flow_benefits + year7_cash_flow_costs + year7_cash_flow_setup
year8_cash_flow_tot <- year8_cash_flow_benefits + year8_cash_flow_costs + year8_cash_flow_setup
year9_cash_flow_tot <- year9_cash_flow_benefits + year9_cash_flow_costs + year9_cash_flow_setup
year10_cash_flow_tot <- year10_cash_flow_benefits + year10_cash_flow_costs + year10_cash_flow_setup


# total - discounted at npv
year0_disc_cash_flow_tot <- (year0_cash_flow_benefits + year0_cash_flow_costs + year0_cash_flow_setup)
year1_disc_cash_flow_tot <- (year1_cash_flow_benefits + year1_cash_flow_costs + year1_cash_flow_setup) / (npv_discount)
year2_disc_cash_flow_tot <- (year2_cash_flow_benefits + year2_cash_flow_costs + year2_cash_flow_setup) / (npv_discount^2)
year3_disc_cash_flow_tot <- (year3_cash_flow_benefits + year3_cash_flow_costs + year3_cash_flow_setup) / (npv_discount^3)
year4_disc_cash_flow_tot <- (year4_cash_flow_benefits + year4_cash_flow_costs + year4_cash_flow_setup) / (npv_discount^4)
year5_disc_cash_flow_tot <- (year5_cash_flow_benefits + year5_cash_flow_costs + year5_cash_flow_setup) / (npv_discount^5)
year6_disc_cash_flow_tot <- (year6_cash_flow_benefits + year6_cash_flow_costs + year6_cash_flow_setup) / (npv_discount^6)
year7_disc_cash_flow_tot <- (year7_cash_flow_benefits + year7_cash_flow_costs + year7_cash_flow_setup) / (npv_discount^7)
year8_disc_cash_flow_tot <- (year8_cash_flow_benefits + year8_cash_flow_costs + year8_cash_flow_setup) / (npv_discount^8)
year9_disc_cash_flow_tot <- (year9_cash_flow_benefits + year9_cash_flow_costs + year9_cash_flow_setup) / (npv_discount^9)
year10_disc_cash_flow_tot <- (year10_cash_flow_benefits + year10_cash_flow_costs + year10_cash_flow_setup) / (npv_discount^10)

# create cols for year df
# create cols for year df
year0_col <- c(
  year0_elec_base, year0_elec_change, year0_elec_new,
  year0_gas_base, year0_gas_change, year0_gas_new,
  year0_co2_base, year0_co2_change, year0_co2_new,
  year0_elec_paid_base, year0_elec_paid_new,
  year0_gas_paid_base, year0_gas_paid_new,
  year0_cash_flow_benefits, year0_cash_flow_costs, year0_cash_flow_setup, year0_cash_flow_tot, year0_disc_cash_flow_tot
)

year1_col <- c(
  year1_elec_base, year1_elec_change, year1_elec_new,
  year1_gas_base, year1_gas_change, year1_gas_new,
  year1_co2_base, year1_co2_change, year1_co2_new,
  year1_elec_paid_base, year1_elec_paid_new,
  year1_gas_paid_base, year1_gas_paid_new,
  year1_cash_flow_benefits, year1_cash_flow_costs, year1_cash_flow_setup, year1_cash_flow_tot, year1_disc_cash_flow_tot
)

year2_col <- c(
  year2_elec_base, year2_elec_change, year2_elec_new,
  year2_gas_base, year2_gas_change, year2_gas_new,
  year2_co2_base, year2_co2_change, year2_co2_new,
  year2_elec_paid_base, year2_elec_paid_new,
  year2_gas_paid_base, year2_gas_paid_new,
  year2_cash_flow_benefits, year2_cash_flow_costs, year2_cash_flow_setup, year2_cash_flow_tot, year2_disc_cash_flow_tot
)

year3_col <- c(
  year3_elec_base, year3_elec_change, year3_elec_new,
  year3_gas_base, year3_gas_change, year3_gas_new,
  year3_co2_base, year3_co2_change, year3_co2_new,
  year3_elec_paid_base, year3_elec_paid_new,
  year3_gas_paid_base, year3_gas_paid_new,
  year3_cash_flow_benefits, year3_cash_flow_costs, year3_cash_flow_setup, year3_cash_flow_tot, year3_disc_cash_flow_tot
)

year4_col <- c(
  year4_elec_base, year4_elec_change, year4_elec_new,
  year4_gas_base, year4_gas_change, year4_gas_new,
  year4_co2_base, year4_co2_change, year4_co2_new,
  year4_elec_paid_base, year4_elec_paid_new,
  year4_gas_paid_base, year4_gas_paid_new,
  year4_cash_flow_benefits, year4_cash_flow_costs, year4_cash_flow_setup, year4_cash_flow_tot, year4_disc_cash_flow_tot
)

year5_col <- c(
  year5_elec_base, year5_elec_change, year5_elec_new,
  year5_gas_base, year5_gas_change, year5_gas_new,
  year5_co2_base, year5_co2_change, year5_co2_new,
  year5_elec_paid_base, year5_elec_paid_new,
  year5_gas_paid_base, year5_gas_paid_new,
  year5_cash_flow_benefits, year5_cash_flow_costs, year5_cash_flow_setup, year5_cash_flow_tot, year5_disc_cash_flow_tot
)

year6_col <- c(
  year6_elec_base, year6_elec_change, year6_elec_new,
  year6_gas_base, year6_gas_change, year6_gas_new,
  year6_co2_base, year6_co2_change, year6_co2_new,
  year6_elec_paid_base, year6_elec_paid_new,
  year6_gas_paid_base, year6_gas_paid_new,
  year6_cash_flow_benefits, year6_cash_flow_costs, year6_cash_flow_setup, year6_cash_flow_tot, year6_disc_cash_flow_tot
)

year7_col <- c(
  year7_elec_base, year7_elec_change, year7_elec_new,
  year7_gas_base, year7_gas_change, year7_gas_new,
  year7_co2_base, year7_co2_change, year7_co2_new,
  year7_elec_paid_base, year7_elec_paid_new,
  year7_gas_paid_base, year7_gas_paid_new,
  year7_cash_flow_benefits, year7_cash_flow_costs, year7_cash_flow_setup, year7_cash_flow_tot, year7_disc_cash_flow_tot
)

year8_col <- c(
  year8_elec_base, year8_elec_change, year8_elec_new,
  year8_gas_base, year8_gas_change, year8_gas_new,
  year8_co2_base, year8_co2_change, year8_co2_new,
  year8_elec_paid_base, year8_elec_paid_new,
  year8_gas_paid_base, year8_gas_paid_new,
  year8_cash_flow_benefits, year8_cash_flow_costs, year8_cash_flow_setup, year8_cash_flow_tot, year8_disc_cash_flow_tot
)

year9_col <- c(
  year9_elec_base, year9_elec_change, year9_elec_new,
  year9_gas_base, year9_gas_change, year9_gas_new,
  year9_co2_base, year9_co2_change, year9_co2_new,
  year9_elec_paid_base, year9_elec_paid_new,
  year9_gas_paid_base, year9_gas_paid_new,
  year9_cash_flow_benefits, year9_cash_flow_costs, year9_cash_flow_setup, year9_cash_flow_tot, year9_disc_cash_flow_tot
)

year10_col <- c(
  year10_elec_base, year10_elec_change, year10_elec_new,
  year10_gas_base, year10_gas_change, year10_gas_new,
  year10_co2_base, year10_co2_change, year10_co2_new,
  year10_elec_paid_base, year10_elec_paid_new,
  year10_gas_paid_base, year10_gas_paid_new,
  year10_cash_flow_benefits, year10_cash_flow_costs, year10_cash_flow_setup, year10_cash_flow_tot, year10_disc_cash_flow_tot
)

year_df <- data.frame(year0_col, year1_col, year2_col, year3_col, year4_col, year5_col, year6_col, year7_col, year8_col, year9_col, year10_col)
names(year_df) <- c("Year0", "Year1", "Year2", "Year3", "Year4", "Year5", "Year6", "Year7", "Year8", "Year9", "Year10")

# calculate total metrics for each row of year dataframe

# electricity
elec_base_tot <- sum(year_df[1, ], na.rm = TRUE)
elec_change_tot <- sum(year_df[2, ], na.rm = TRUE)
elec_new_tot <- ifelse(year1_elec_new != "nm", sum(year_df[3, ], na.rm = TRUE), elec_base_tot)

# Gas
gas_base_tot <- sum(year_df[4, ], na.rm = TRUE)
gas_change_tot <- sum(year_df[5, ], na.rm = TRUE)
gas_new_tot <- ifelse(year1_gas_new != "nm", sum(year_df[6, ], na.rm = TRUE), gas_base_tot)

# CO2
co2_base_tot <- sum(year_df[7, ], na.rm = TRUE)
co2_change_tot <- sum(year_df[8, ], na.rm = TRUE)
co2_new_tot <- sum(year_df[9, ], na.rm = TRUE)

# electricity paid
elec_paid_base_tot <- sum(year_df[10, ], na.rm = TRUE)
elec_paid_new_tot <- sum(year_df[11, ], na.rm = TRUE)

# gas paid
gas_paid_base_tot <- sum(year_df[12, ], na.rm = TRUE)
gas_paid_new_tot <- sum(year_df[13, ], na.rm = TRUE)

# 10 year cash flow
ten_year_cash_ben_tot <- sum(year_df[16, ], na.rm = TRUE)
ten_year_cash_costs_tot <- sum(year_df[17, ], na.rm = TRUE)
ten_year_cash_setup_tot <- sum(year_df[18, ], na.rm = TRUE)
ten_year_cash_total_tot <- sum(year_df[19, ], na.rm = TRUE)
ten_year_cash_total_disc_tot <- sum(year_df[20, ], na.rm = TRUE)


# create energy col names for clarity
energy_col <- c(
  "Electricity", "Electricity", "Electricity",
  "Gas", "Gas", "Gas",
  "CO2", "CO2", "CO2",
  "ElecPaid", "ElecPaid",
  "GasPaid", "GasPaid",
  "10YCash", "10YCash", "10YCash", "10YCash", "10YCash"
)

# variable col
V1 <- c(
  "Baseline", "Change", "New",
  "Baseline", "Change", "New",
  "Baseline", "Change", "New",
  "Baseline", "New",
  "Baseline", "New",
  "Benefits", "Costs", "Setup", "Total", "TotalDisc"
)

year_df2 <- add_column(year_df, .before = 1, EnergyType = energy_col)
year_df3 <- add_column(year_df2, .before = 2, Variable = V1)

year10_final <- year_df3
save(year10_final, file = "./outputs/year10_final.RData")
