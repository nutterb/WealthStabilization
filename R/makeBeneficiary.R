#' @name makeBeneficiary
#' @title Make a Beneficiary Object
#'
#' @description Creates an object to store projections of wealth fund 
#'   utilization.
#'   
#' @param name `character(1)`. The name of the beneficiary
#' @param date_of_birth `Date(1)`. The beneficiary's date of birth. 
#' @param date_of_entry `Date(1)`. The date on which the beneficiary signs 
#'   the contract to join the trust. Defaults to the 18th birthday.
#' @param date_of_pension `Date(1)`. The date on which the beneficiary begins
#'   receiving pension. Defaults to the 60th birthday.
#' @param date_of_death `Date(1)`. The date on which the beneficiary dies.
#'   Defaults to the 100th birthday.
#' @param starting_balance `numeric(1)`. The balance the beneficiary has saved
#'   to a Roth account on `date_of_entry`.
#' 
#' @return Returns an object of class `beneficiary`. The objects has the
#' following elements
#' 
#' * `name`: The name of the beneficiary
#' * `date_of_birth`: The beneficiary's date of birth. 
#' * `date_of_entry`: The date the beneficiary signs the contract to join the
#'    trust. 
#' * `date_of_pension`: The date the beneficiary begins receiving pension 
#'    benefits.
#' * `date_of_death`: The date the beneficiary dies.
#' * `MonthlyStatus`: A `data.frame` giving monthly status of the beneficiary
#'    account and utiliziation up to age 100.
#'    
#' @export

makeBeneficiary <- function(name, 
                            date_of_birth, 
                            date_of_entry = date_of_birth + lubridate::years(18),
                            date_of_pension = date_of_birth + lubridate::years(60),
                            date_of_death = date_of_birth + lubridate::years(100),
                            starting_balance = 0) {
  
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertString(x = name, 
                          add = coll)
  
  checkmate::assertDate(x = date_of_birth, 
                        len = 1, 
                        add = coll)
  
  checkmate::assertDate(x = date_of_entry, 
                        len = 1, 
                        add = coll)
  
  checkmate::assertDate(x = date_of_pension, 
                        len = 1, 
                        add = coll)
  
  checkmate::assertDate(x = date_of_death, 
                        len = 1, 
                        add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Initialize the data frame ---------------------------------------
  
  date_fifty_nine <- 
    date_of_birth + lubridate::years(59)
  
  date_one_hundred <- date_of_birth + lubridate::years(100)
  
  MonthlyStatus <- 
    data.frame(date = seq(date_of_entry, 
                          date_one_hundred, 
                          by = "1 month"))
  
  MonthlyStatus <- MonthlyStatus[MonthlyStatus$date >= date_of_entry, , 
                                 drop = FALSE]
  
  # Additional columns of data --------------------------------------
  
  MonthlyStatus$annual_income <- rep(0, nrow(MonthlyStatus))
  MonthlyStatus$contribution <- rep(0, nrow(MonthlyStatus))
  MonthlyStatus$matched_contribution <- rep(0, nrow(MonthlyStatus))
  MonthlyStatus$personal_balance <- rep(0, nrow(MonthlyStatus))
  MonthlyStatus$personal_rate <- rep(0.06/12, nrow(MonthlyStatus))
  MonthlyStatus$personal_interest <- rep(0, nrow(MonthlyStatus))
  MonthlyStatus$subaccount_balance <- rep(0, nrow(MonthlyStatus))
  MonthlyStatus$subaccount_rate <- rep(0.06/12, nrow(MonthlyStatus))
  MonthlyStatus$subaccount_interest <- rep(0, nrow(MonthlyStatus))
  MonthlyStatus$combined_balance <- rep(0, nrow(MonthlyStatus))
  MonthlyStatus$distribution <- rep(0, nrow(MonthlyStatus))
  MonthlyStatus$is_tier_one <- rep(FALSE, nrow(MonthlyStatus))
  MonthlyStatus$is_tier_two <- rep(FALSE, nrow(MonthlyStatus))
  MonthlyStatus$is_tier_three <- rep(FALSE, nrow(MonthlyStatus))
  MonthlyStatus$is_pension <- rep(FALSE, nrow(MonthlyStatus))

  # Return the object -----------------------------------------------
  structure(
    list(name = name, 
         date_of_birth = date_of_birth, 
         date_of_entry = date_of_entry, 
         date_of_pension = date_of_pension, 
         date_of_death = date_of_death,
         starting_balance = starting_balance,
         MonthlyStatus = MonthlyStatus), 
    class = "beneficiary"
  )
}
