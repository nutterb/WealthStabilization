#' @name setIndividualContribution 
#' @title Set Individual Contributions for a Date Range
#' 
#' @description Sets the value of the beneficiary contribution to their 
#'   personal Roth account for a date range. 
#'   
#' @param beneficiary A `beneficiary` object. 
#' @param contribution `numeric(1)`. The value of the monthly contribution 
#'   for the date range. 
#' @param start_date `Date(1)`. The start of the date range for which the 
#'   monthly contribution will be applied. 
#' @param end_date `Date(1)`. The end of the date range for which the montly
#'   contribution will be applied.
#'   
#' @export

setIndividualContribution <- function(beneficiary, 
                                      contribution, 
                                      start_date, 
                                      end_date) {
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertClass(x = beneficiary, 
                         classes = "beneficiary", 
                         add = coll)
  
  checkmate::assertNumeric(x = contribution, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertDate(x = start_date, 
                        len = 1, 
                        add = coll)
  
  checkmate::assertDate(x = end_date, 
                        len = 1, 
                        add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  w <- which(beneficiary$MonthlyStatus$date >= start_date & 
               beneficiary$MonthlyStatus$date <= end_date)
  
  beneficiary$MonthlyStatus$contribution[w] <- rep(contribution, 
                                                   length(w))
  
  beneficiary <- recalculateStatus(beneficiary)
  
  beneficiary
}
