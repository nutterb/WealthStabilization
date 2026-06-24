#' @name recalculateStatus
#' @title Recalculate the Monthly Status Based on Updates to the Beneficiary
#'   Object
#'   
#' @description Recalculates the Monthly Status and returns the new 
#'   beneficiary object. 
#'   
#' @param beneficiary A `beneficiary` object.
#' @param tier_one_threshold `numeric(1)`. The threshold for receiving 
#'   Tier 1 distributions.
#' @param tier_two_threshold `numeric(1)`. The threshold for receiving 
#'   Tier 2 distributions.
#' @param tier_three_threshold `numeric(1)`. The threshold for receiving 
#'   Tier 3 distributions.
#' @param max_monthly_contribution `numeric(1)`. The maximum monthly 
#'   contribution permitted to a Roth IRA per IRS guidelines. The monthly
#'   distribution is based on this amount.
#' @param poverty_floor `numeric(1)`. The poverty threshold marking the minimum
#'   for annual Tier 3 distributions.
#' 
#' @export

recalculateStatus <- function(beneficiary, 
                              tier_one_threshold = 250000,
                              tier_two_threshold = 500000, 
                              tier_three_threshold = 1000000, 
                              max_monthly_contribution = 625, 
                              poverty_floor = 15000, 
                              income_multiplier_cap = 120000) { 
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertClass(x = beneficiary, 
                         classes = "beneficiary", 
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  Status <- beneficiary$MonthlyStatus
  starting_balance <- beneficiary$starting_balance
  
  # Pre Pension Activity --------------------------------------------
  
  PrePensionStatus <- Status[Status$date < beneficiary$date_of_pension, ]
  PrePensionStatus$matched_contribution <- PrePensionStatus$contribution

  for (i in seq_len(nrow(PrePensionStatus))) {
    if (i == 1) {
      PrePensionStatus$personal_interest[i] <- 0
      PrePensionStatus$personal_balance[i] <- 
        starting_balance + 
        PrePensionStatus$contribution[i] + 
        PrePensionStatus$personal_interest[i]
      
      PrePensionStatus$subaccount_interest[i] <- 0
      PrePensionStatus$subaccount_balance[i] <- 
        PrePensionStatus$matched_contribution[i]
    } else {
      if (PrePensionStatus$combined_balance[i-1] >= tier_three_threshold) {
        PrePensionStatus$matched_contribution[i] <- 0
      }
      
      PrePensionStatus$personal_interest[i] <- 
        PrePensionStatus$personal_balance[i - 1] * 
          PrePensionStatus$personal_rate[i]
      PrePensionStatus$personal_balance[i] <- 
        PrePensionStatus$personal_balance[i - 1] + 
        PrePensionStatus$personal_interest[i] + 
        PrePensionStatus$contribution[i]
      
      PrePensionStatus$subaccount_interest[i] <- 
        PrePensionStatus$subaccount_balance[i - 1] * 
        PrePensionStatus$subaccount_rate[i]
      PrePensionStatus$subaccount_balance[i] <- 
        PrePensionStatus$subaccount_balance[i - 1] + 
        PrePensionStatus$subaccount_interest[i] + 
        PrePensionStatus$matched_contribution[i]
      
      PrePensionStatus$combined_balance[i] <- 
        PrePensionStatus$personal_balance[i] + PrePensionStatus$subaccount_balance[i]
      
      
      # Determine Distributions and tier PrePensionStatus
      if (PrePensionStatus$combined_balance[i] < tier_one_threshold& 
          PrePensionStatus$date[i] < beneficiary$date_of_pension) {
        # Tier 0
        PrePensionStatus$distribution[i] <- 0
        PrePensionStatus$is_tier_one[i] <- FALSE
        PrePensionStatus$is_tier_two[i] <- FALSE
        PrePensionStatus$is_tier_three[i] <- FALSE
        PrePensionStatus$is_pension[i] <- FALSE
      } else if (PrePensionStatus$combined_balance[i] >= tier_one_threshold & 
                 PrePensionStatus$combined_balance[i] < tier_two_threshold& 
                 PrePensionStatus$date[i] < beneficiary$date_of_pension) {
        # Tier 1
        PrePensionStatus$distribution[i] <- max_monthly_contribution
        PrePensionStatus$is_tier_one[i] <- TRUE
        PrePensionStatus$is_tier_two[i] <- FALSE
        PrePensionStatus$is_tier_three[i] <- FALSE
        PrePensionStatus$is_pension[i] <- FALSE
      } else if (PrePensionStatus$combined_balance[i] >= tier_two_threshold & 
                 PrePensionStatus$combined_balance[i] < tier_three_threshold& 
                 PrePensionStatus$date[i] < beneficiary$date_of_pension){
        # Tier 2
        PrePensionStatus$distribution[i] <- max_monthly_contribution * 3
        PrePensionStatus$is_tier_one[i] <- FALSE
        PrePensionStatus$is_tier_two[i] <- TRUE
        PrePensionStatus$is_tier_three[i] <- FALSE
        PrePensionStatus$is_pension[i] <- FALSE
      } else if (PrePensionStatus$combined_balance[i] >= tier_three_threshold & 
                 PrePensionStatus$date[i] < beneficiary$date_of_pension) {
        
        PrePensionStatus$distribution[i] <- 
          .tierThreeDistribution(PrePensionStatus$annual_income, 
                                 poverty_floor = poverty_floor,
                                 income_multiplier_cap = income_multiplier_cap)
        PrePensionStatus$is_tier_one[i] <- FALSE
        PrePensionStatus$is_tier_two[i] <- FALSE
        PrePensionStatus$is_tier_three[i] <- TRUE
        PrePensionStatus$is_pension[i] <- FALSE
      } 
    }
    
  }
  
  # Post Pension Activity -------------------------------------------
  
  snapshot <- utils::tail(PrePensionStatus$combined_balance, 1)
  monthly_pension <- snapshot * .04 / 12
  
  PostPensionStatus <- Status[Status$date >= beneficiary$date_of_pension, ]
  
  for (i in seq_len(nrow(PostPensionStatus))) {
    PostPensionStatus$distribution <- rep(monthly_pension, 
                                          nrow(PostPensionStatus))
    post_distribution_balance <- 
      if (i == 1) {
        snapshot - monthly_pension
      } else {
        PostPensionStatus$combined_balance[i - 1] - 
          PostPensionStatus$distribution[i]
      }
    PostPensionStatus$subaccount_interest[i] <- 
      post_distribution_balance * 
      PostPensionStatus$subaccount_rate[i]
    
    PostPensionStatus$combined_balance[i] <- 
      post_distribution_balance + PostPensionStatus$subaccount_interest[i]
    
  }
  
  
  
  beneficiary$MonthlyStatus <- rbind(PrePensionStatus,
                                     PostPensionStatus)
  
  beneficiary
}


.tierThreeDistribution <- function(income, 
                                   poverty_floor, 
                                   income_multiplier_cap) {
  poverty_floor + min(c(income * .5, income_multiplier_cap / 2)) / 12
}
