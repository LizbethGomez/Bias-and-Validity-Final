########################################
### Inverse Odds for Transportability###
########################################

#Create a simple function that mimics the numerical example in [[Westreich et al., 2017]]

calculate_odds_and_weighted_risk_difference <- function(trial_population, target_population, 
                                                        trial_Z1_percent, target_Z1_percent, 
                                                        effect_Z1, effect_Z0) {
  # Calculate the number of individuals with Z = 1 and Z = 0 in each population
  trial_Z1 <- trial_population * trial_Z1_percent / 100
  trial_Z0 <- trial_population - trial_Z1
  target_Z1 <- target_population * target_Z1_percent / 100
  target_Z0 <- target_population - target_Z1
  
  # Calculate the combined population numbers
  combined_Z1 <- trial_Z1 + target_Z1
  combined_Z0 <- trial_Z0 + target_Z0
  
  # Calculate odds for Z = 1 and Z = 0
  odds_Z1 <- trial_Z1 / combined_Z1 / (1 - trial_Z1 / combined_Z1)
  odds_Z0 <- trial_Z0 / combined_Z0 / (1 - trial_Z0 / combined_Z0)
  
  # Calculate weighted pseudopopulations
  weighted_pseudo_Z1 <- trial_Z1 * odds_Z1
  weighted_pseudo_Z0 <- trial_Z0 * odds_Z0
  
  # Calculate weighted risk difference
  weighted_risk_difference <- (weighted_pseudo_Z1 * effect_Z1 + weighted_pseudo_Z0 * effect_Z0) / (weighted_pseudo_Z1 + weighted_pseudo_Z0)
  
  return(list("Odds Z=1" = odds_Z1,
              "Odds Z=0" = odds_Z0,
              "Weighted Pseudopopulation Z=1" = weighted_pseudo_Z1,
              "Weighted Pseudopopulation Z=0" = weighted_pseudo_Z0,
              "Weighted Risk Difference" = weighted_risk_difference))
}

# Example usage
result <- calculate_odds_and_weighted_risk_difference(295, 109, 74.9, 51.3, 14, 48)
print(result)
