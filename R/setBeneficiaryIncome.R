#' @name setBeneficiaryIncome
#' @title Set Beneficiary Annual Income
#' 
#' @description Functions for setting annual earned income for a beneficiary
#'   within the beneficiary object. 
#'   
#' @param beneficiary An object of class `beneficiary`
#' @param ... named pairings where the name is the year an income level starts. 
#'   income will be propagated to future years until the next year in which 
#'   income is given. use `2030 = 25000, 2035 = 30000` to set the income at 
#'   $25,000 for 2030, 2031, 2032, 2033, and 2034. Income will be $30,000 for
#'   2035 and all years after. 
#'   
#' @export

setBeneficiaryIncome <- function(beneficiary, 
                                 ...) {
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertClass(x = beneficiary, 
                         classes = "beneficiary", 
                         add = coll)

  income_args <- list(...)
  
  years <- as.numeric(names(income_args))
  
  if (any(is.na(years))) {
    coll$push("Names of ... could not be coerced to numeric")
  }
  
  checkmate::assertList(x = income_args, 
                        types = "numeric", 
                        add = coll)
  
  checkmate::reportAssertions(coll)

  # Functionality ---------------------------------------------------
  
  IncomeSetting <- 
    data.frame(year = years, 
               income = unlist(income_args))
  IncomeSetting <- IncomeSetting[order(IncomeSetting$year), ]
  
  Status <- beneficiary$MonthlyStatus
  
  year <- lubridate::year(Status$date)
  
  for (i in seq_along(IncomeSetting)) {
    Status$annual_income[year >= IncomeSetting$year[i]] <- 
      IncomeSetting$income[i]
  }
  
  beneficiary$MonthlyStatus <- Status
  
  beneficiary
}
