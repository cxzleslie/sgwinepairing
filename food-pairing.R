# The wine should have a weight similar to that of the food
rule1 <- function(wines_df, food) { 
  return(wines_df[(wines_df$weight >= food$weight - 1) & (wines_df$weight <= food$weight + 1), ])
}

# The wine should have an acidity greater than or equal to that of the food
rule2 <- function(wines_df, food) {
  return(wines_df[wines_df$acid >= food$acid, ])
}

# The wine should be at least as sweet as the food 
rule3 <- function(wines_df, food) {
  return(wines_df[wines_df$sweet >= food$sweet, ])
}

# Bitter wines should not go with bitter foods
rule4 <- function(wines_df, food) {
  if (food$bitter > 3.5) {
    wines <- wines_df[wines_df$bitter <= 2.5, ]
  }
  return(wines_df)
}

# Bitter and salt should not be matched
rule5 <- function(wines_df, food) {
  if (food$bitter > 3.5) {
    wines_df <- wines_df[wines_df$salt <= 2.5, ]
  } 
  if (food$salt > 3.5) {
    wines_df <- wines_df[wines_df$bitter <= 2.5, ]
  }
  return(wines_df)
}

# Acid and bitter should not be matched
rule6 <- function(wines_df, food) {
  if (food$bitter > 3.5) {
    wines_df <- wines_df[wines_df$acid <= 2.5, ]
  } 
  if (food$acid > 3.5) {
    wines_df <- wines_df[wines_df$bitter <= 2.5, ]
  }
  return(wines_df)
}

# Acid and piquant should not be matched
rule7 <- function(wines_df, food) {
  if (food$piquant > 3.5) {
    wines_df <- wines_df[wines_df$acid <= 2.5, ]
  } 
  if (food$acid > 3.5) {
    wines_df <- wines_df[wines_df$piquant <= 2.5, ]
  }
  return(wines_df)
}

# We do not care about the weight now, we keep the other parameters
aromas_no_weight <- c("sweet", "bitter", "fat", "piquant", "salt",  "acid")

# Order wine suggestions
order_wine_suggestions <- function(wines_df, food, match_type) {
  
  if (match_type == "Contrasting taste") {
    # Find the aroma with the heighest value in the food
    max_aroma <- names(which.max(food[aromas_no_weight]))
    
    # Order the wines with respect to the selected aroma in ascending order
    wines_df <- wines_df[order(wines_df[, max_aroma]), ]
  } else if (match_type == "Similar taste") {
    
    wines_df <- wines_df %>% 
      mutate(difference = ((sweet - food$sweet) + (fat - food$fat) + (piquant - food$piquant) + (salt - food$salt) + (bitter - food$bitter) + (acid - food$acid))) %>%
      arrange(difference)
  }
  
  # Keep only the 4 wines that have the highest contrast with the food
  wines_df <- head(wines_df, 4)
  
  return(wines_df)
}

find_match <- function(wines_df, food, match_type) {
  
  # We will apply each rule only if doing so allows us to keep enough wines to suggest. If not enough matches are found for one particular rule, the rule is skipped
  rules <- c(rule1, rule2, rule3, rule4, rule5, rule6, rule7)
  
  #if(exists("food$bitter")) {
    for (rule in rules) {
    test_df <- rule(wines_df, food)
    if (nrow(test_df) > 5) {
      wines_df <- test_df
      }
    }
    
    # Order the suggestions
    wines_df <- order_wine_suggestions(wines_df, food, match_type)
 # }
  
  return(wines_df)
}