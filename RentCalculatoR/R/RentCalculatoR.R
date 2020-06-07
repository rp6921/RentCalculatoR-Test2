#' @title CHANGES IN RENTS CALCULATOR (FOR SWITZERLAND)
#'
#' @description With the package RentCalculatoR you can calculate the change in rent of residential real estate according to Swiss law. Changes in value-adding investments, depreciation, interests and reference interest rates can be taken into account.
#'
#' @param act_rent_CHF
#' @param investment_CHF
#' @param increasing_share
#' @param lifespan
#' @param maintenance_rate
#'
#' @return act_data
#' @return return_invest
#'
#' @examples RentInformations()
#' @examples RentInvestCalculatoR(1000,100000,50,12)
#' @examples RentInvestCalculatoR(0.000001,500000,100,50,8)
#'
#' @export RentInformations


# To write this package we used the mentioned sources in the Bibliography at the end of this file.

# To ignore the warnings during usage
options(warn=-1)
options("getSymbols.warning4.0"=FALSE)

##########################################################################################################################
#
#  PART A: RentInformations(), returns act_data(c(act_mortgage, act_date, act_infl_rate))
#
##########################################################################################################################

# The RentInformations function gets the relevant information from the system or from scraping of the
# webpage of the Department of Statistics. They are coded in PART A.
#    - current relevant and official mortgage rent (mortgage, in %); from https://www.bwo.admin.ch/bwo/de/home/
#      mietrecht/referenzzinssatz/entwicklung-referenzzinssatz-und-durchschnittszinssatz.html
#    - current date for the calculation on inflation and on general cost increases
#    - official information about the inflation rates from xlsx-file on https://www.bfs.admin.ch/bfsstatic/
#     dam/assets/12827290/master/


RentInformations <- function(){

# We use the following packages. They will be  installed in case you don't have them

if ("xml2" %in% rownames(installed.packages()) == FALSE) {
  install.packages("xml2")
}

if ("rvest" %in% rownames(installed.packages()) == FALSE) {
  install.packages("rvest")
}

if ("tidyverse" %in% rownames(installed.packages()) == FALSE) {
  install.packages("tidyverse")
}

if ("readxl" %in% rownames(installed.packages()) == FALSE) {
  install.packages("readxl")
}


library(xml2)
library(rvest)
library(tidyverse)
library(readxl)


#############################
# MORTGAGE - getting the actual mortgage (act_mortgage) from the Federal Office for Housing
#############################

# get the mortgage rates from the website of the statistics of the government
mortgage_url<- "https://www.bwo.admin.ch/bwo/de/home/mietrecht/referenzzinssatz/entwicklung-referenzzinssatz-und-durchschnittszinssatz.html"
mortgage_urlHtml <- mortgage_url %>% read_html()
mortgage_data <- mortgage_urlHtml %>% html_table(header=TRUE) %>% as.data.frame

# clean up the data: set column names, define data types
mortgage_data_clean <- data.frame(mortgage_data[,(1:2)])
colnames(mortgage_data_clean) <- c("mortgage_rate", "valuable_from")
mortgage_data_clean$mortgage_rate <- parse_number(mortgage_data_clean$mortgage_rate, locale = locale(decimal_mark=","))
mortgage_data_clean$valuable_from <- strptime(mortgage_data_clean$valuable_from, format="%d.%m.%Y")
mortgage_data_clean$valuable_from <- as.Date(mortgage_data_clean$valuable_from)

# clean the line with additional Information, because the different way in measuring is not relevant to the calculations
mortgage_data_clean <- mortgage_data_clean[complete.cases(mortgage_data_clean),]
act_mortgage <- mortgage_data_clean$mortgage_rate[1]

#############################
# ACTUAL DATE - getting the actual date (act_date) out of the system
#############################
act_date <- Sys.Date()

#############################
# INFLATION RATE - getting the inflation rate (act_infl_rate) from the Department of Statistics
#############################
# Inflation index from as xlsx download of the department for statistics

URL_inflation <- "https://www.bfs.admin.ch/bfsstatic/dam/assets/13047088/master"
download.file(URL_inflation, destfile = "inflation_rates.xlsx")
inflation_base2015 <- read_excel("inflation_rates.xlsx", sheet = "2015",
                                 col_names =  c("year", (1:17)))

# clean the spreadsheet of 2015 indices
inflation_clean <- data.frame(inflation_base2015[,(1:13)])
inflation_clean$year <- as.Date.character(inflation_clean$year, format = "%Y")
inflation_clean$year <- as.numeric(format(inflation_clean$year, "%Y"))

# take only cases where year has a value and sort from actual to past years
years_only <- complete.cases(inflation_clean$year)
inflation_clean <- cbind(years_only, inflation_clean)
inflation_clean <- subset(inflation_clean, years_only == TRUE)
inflation_clean <- inflation_clean [,(2:14)]
inflation_clean <- inflation_clean[order(inflation_clean$year, decreasing = TRUE),]

# filter out the actual inflation rate
act_inflation_rate <- inflation_clean[1,]
act_inflation_rate <- gather(act_inflation_rate)
act_inflation_rate <- subset(act_inflation_rate, !is.na(act_inflation_rate$value))
act_inflation_rate <- as.character(act_inflation_rate$value)
act_inflation_rate <- as.numeric(dplyr::last(act_inflation_rate))


#############################
# PRINT RESULTS
#############################
# overview of the results and report
actual_data <- c(act_mortgage, as.character(act_date), act_inflation_rate)
actual_answers <- c("The actual mortgage rate in % is:", "The actual date is:",
                    "And the actual inflation rate is (points on 2015 basis)")
actual_print_result <- as.table(cbind(actual_answers, actual_data))
print(actual_print_result)

# return the actual data in a dataframe
act_data <- data.frame(actual_data)
act_data <- cbind(c("act_mortgage", "act_date", "act_inflation_rate"), act_data)
colnames(act_data) <- c("name", "value")
row1 <- as.vector(act_data$name)
row2 <- as.vector(act_data$value)
act_data <- rbind(row2)
colnames(act_data) <- row1
return(act_data)

}



# Example to PART A
# -------------------
# If you would like to know what the actual relevant data for rents is.
# This output will be used from the other functions in Part B and C as well.
# act_results <- RentInformations()


###########################################################################################################################
#
#  PART B: RentInvestCalculatoR(act_rent_CHF, investment_CHF, increasing_share, lifespan, maintenance_rate)
#           returns return_invest (c("value_incr_share_CHF", "depreciation_CHF","interest_CHF",
#                                    "maintenance_CHF", "total_add_rent_monthly_CHF"))
#
##########################################################################################################################


# To calculate the changings due to investments we need from the user (PART B of the package):
#    - actual net rent (act_rent_CHF, integer)
#    - total investment (investment_CHF, integer)
#    - value-increasing share (increasing_share, in %)
#    - lifespan (lifespan, in years)
#    - maintenance allowance (maintenance_rate, 10 % by default due to a high court judgment)

# And we use the acutual mortgage (act_mortgage) from the RentInformations function (see in PART A)


#' @export RentInvestCalculatoR

  RentInvestCalculatoR <- function(act_rent_CHF, investment_CHF, increasing_share,
                                   lifespan, maintenance_rate=10){

    # calling the results of the RentInformations function (PART A) and take the actual mortgage
    act_data <- as.data.frame(RentInformations( ))
    act_mortgage <- as.numeric(act_data$act_mortgage)

    # the increasing share and the depreciation of the investment in CHF
    value_incr_share_CHF <- investment_CHF/100*increasing_share
    depreciation_CHF <- value_incr_share_CHF/lifespan

    # the allowed interest by law is 0.5 % above the actual mortgage rate (dived to 2 paries)
    allowed_interest <- (act_mortgage+.5)/2
    # this leads to a change due to interests
    interest_CHF <- value_incr_share_CHF*allowed_interest/100

    # the intermediate result for the change is the sum of the depreciation and the interests
    int_result <- depreciation_CHF+interest_CHF

    # the allowed maintenance increase is 10 % of the intermediate result
    maintenance_CHF <- int_result*maintenance_rate/100

    #############################
    # PRINT RESULTS
    #############################

    # total rent results and report
    total_add_rent_monthly_CHF <- (int_result + maintenance_CHF)/12
    total_new_rent_monthly_CHF <- (12*act_rent_CHF + int_result + maintenance_CHF)/12
    rent_answers <- c("Your actual rent per month in CHF is:", "The additional rent per month in CHF is:",
                      "And the new total rent per month in CHF is:")
    rent_summary <- c(act_rent_CHF, total_add_rent_monthly_CHF, total_new_rent_monthly_CHF)
    rent_summary <- round(rent_summary/5, digits = 2)*5   # round to 5 cts.
    rent_result <- as.table(cbind(rent_answers, rent_summary))
    print("*******************************************************************************")

    # return the results in a dataframe
    return_invest <- as.table(c(value_incr_share_CHF, depreciation_CHF,interest_CHF, maintenance_CHF,
                                total_add_rent_monthly_CHF, total_new_rent_monthly_CHF))
    return_invest <- round(return_invest/5, digits = 2)*5   # round to 5 cts.
    return_invest <- as.data.frame(return_invest)
    colnames (return_invest) <- c("name", "value")
    row3 <- as.vector(return_invest$name)
    CHF <- as.vector(return_invest$value)
    return_invest <- rbind(CHF)
    colnames(return_invest) <- c("value_incr_share_CHF", "depreciation_CHF","interest_CHF", "maintenance_CHF",
                                 "total_add_rent_monthly_CHF", "total_new_rent_monthly_CHF")
    print(return_invest)
  }



  # Examples to PART B
  # ----------------------------------------------------------------------------------------------------------

  # EXAMPLE for standard use
  # ----------------------------------------------------------------------------------------------------------
  # You put in the needed data of your actual rent and of the investment into the calculator.
  # If you don't give a value for the maintenance rate, the calculator uses 10 % by default

  # To test it for a actual rent of 1000 CHF, an investment of 100000 CHF,
  # a value increasing share of 50 % and a livespan of 12 years:
  # RentInvestCalculatoR(1000,100000,50,12)


  # EXAMPLE with a quasi zero value for actual_rent (e.g. for calculating initial rents of new buildings)
  # ----------------------------------------------------------------------------------------------------------
  # The starting values of the actual rent basically can't be zero. If you would like to calculate the initial
  # rent for a new building, you have to set the actual rent to a very small value. Of course you can also use
  # it, if you buy an appartment and you want to know how much rent you should get out of it. The calculator
  # gives you here an approximation, because you have to set an average lifespan over all components of the
  # building (see also EXAMPLE with different components for more precise calculations).

  # To test it for a new building let's say we would like to calculate the rent for a unit (appartment).
  # The unit is valid an investment of 500000 CHF,  the value increasing share is 100 % because there was no
  # value before, the average lifespan will be 50 years and the maintanence is set to 8 % because in the
  # in the first years you still have guarantee and would not spend as much on maintenance.
  # RentInvestCalculatoR(0.000001,500000,100,50,8)


  # EXAMPLE with different components
  # ----------------------------------------------------------------------------------------------------------
  # If you would like to do the calculation for different components, you can use the caluluator of course
  # different times and add the values up. You can look up different lifespans of components on
  # https://www.mietrecht.ch/index.php?id=32 (unfortunately only available in german language).

  # @ Giuliano: Could you describe this example with a table
  # It will work like this "investment_name <- RentInvestCalculatoR(...,...,..., values of the table)
  # investment_name  |   act_rent_CHF | investment_CHF  | increasing_share | lifespan    |  maintenance_rate
  #  structure of building      0.001        250000          100 %             100 years      10 % by default
  #  kitchen                    0.001         30000           100 %             15 years      8 %
  #  bathroom                   0.001         50000           100 %             30 years      8 %
  #  floors                     0.001         25000           100 %             12 years      8 %
  #  windows                    0.001         40000           100 %             20 years      8 %
  #   Total                     0.001       sum(????)        sum(????)         average(????)  average (????)



  ##############################################################################################################
  # BIBLIOGRAPHY
  ##############################################################################################################

  # Topic related informations about rents in Switzerland
  # https://www.mietrecht.ch

  # Creating a package
  # https://www.analyticsvidhya.com/blog/2017/03/create-packages-r-cran-github/
  # http://portal.stats.ox.ac.uk/userdata/ruth/APTS2012/Rcourse10.pdf
  # https://cran.r-project.org/doc/contrib/Leisch-CreatingPackages.pdf
  # https://support.rstudio.com/hc/en-us/articles/200486508-Building-Testing-and-Distributing-Packages
  # https://support.rstudio.com/hc/en-us/articles/200486518

  # Writing documentation
  # https://support.rstudio.com/hc/en-us/articles/200532317-Writing-Package-Documentation

  # Debugging
  # https://stat.ethz.ch/pipermail/r-help/2014-May/374864.html
  # https://stackoverflow.com/questions/26697727/what-does-error-in-namespaceexportns-exports-undefined-exports-mean

  # Publishing on a GitHub-Account
  # https://www.analyticsvidhya.com/blog/2017/03/create-packages-r-cran-github/

