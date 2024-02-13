# This is a tutorial on how to scrape data from a static website
# Two examples:
# Table extraction
# Query  into extraction

# Table example -----------------------------------------------------------

# https://www.cbr.washington.edu/sacramento/workgroups/delta_smelt.html
# Request, Parse, Clean

library(rvest)
library(dplyr)
library(stringr)
library(readr)

# Request -----------------------------------------------------------------

tableLink <- session("https://www.cbr.washington.edu/sacramento/workgroups/delta_smelt.html")

# Parse -------------------------------------------------------------------

# For some features, there are pre-made functions that does the parsing for us, e.g., tables
# This is because tables are structured the same across all webpages, if they are displayed as such
# Leverage this fact to be able to create a generic function that pulls from the table(s)
tables <- tableLink %>% 
  html_table()

tables

# Clean -------------------------------------------------------------------

# The header is actually a table, and therefore is parsed by the function
# Can see this when you inspect the webpage with the html tag: <table>
View(tables[[1]])

# For "Delta Smelt Releases" table: 5th one
tables[[5]]

# All tables are read in, however, not all are read in 100% correct
# Trouble with headers at times, generally merged rows or large cells

# "Daily Data for Last 2 Weeks"
tables[[10]]

tables[[10]] %>% 
  setNames(.[1, ]) %>%
  slice(-c(1:2))

# "Hourly Data for Last 7 Days"
tables[[11]] %>% 
  setNames(.[1, ]) %>%
  slice(-c(1:2)) %>% 
  mutate(Date = as.Date(Date),
         across(2:4, as.numeric))

# Theoretically, more efficient way to do this is to specify exactly
# which table you want during the request step:

tableLink %>% 
  html_element("#deltasmelt-table") %>% 
  html_table() %>% 
  setNames(.[1, ]) %>%
  slice(-c(1:2))

bench::mark(allTables = tableLink %>% 
              html_table() %>% 
              .[[10]] %>% 
              setNames(.[1, ]) %>%
              slice(-c(1:2)),
            selectedTable = tableLink %>% 
              html_element("#deltasmelt-table") %>% 
              html_table() %>% 
              setNames(.[1, ]) %>%
              slice(-c(1:2)))

# You can also just scrape the CSV link and read that in directly
tableLink %>% 
  html_elements("a") %>% 
  html_attr("href") %>% 
  str_subset(".csv") %>% 
  .[1] %>% 
  paste0("https://www.cbr.washington.edu", .) %>% 
  read_csv()

# Note that a csv file is structured and that metadata can be read by the function
# This means we don't have to fix the column names and specify the data types

tableLink %>% 
  html_element("body > div.pindent > div.xsmall.addmargin.noprint > a:nth-child(1)") %>% 
  html_attr("href") %>% 
  paste0("https://www.cbr.washington.edu", .) %>% 
  read_csv()

# Forms -------------------------------------------------------------------

# Request
queryLink <- session("https://www.cbr.washington.edu/sacramento/data/query_loss_detail.html")

# Parse -------------------------------------------------------------------

# Identifying the form
formToFill <- html_form(queryLink)[[1]]

# Several things to fill out here:
# Radio buttons, outputFormat, dnaOnly, age
# Selectors, water year and species
# Checkbox, query link

# Finding relevant form metadata:
str(formToFill)
names(formToFill$fields$year)
formToFill$fields$year
# Years available: all, 1993-2024
formToFill$fields$year$options

# Can do it manually; html_form() does this parsing for you
queryLink %>% 
  html_elements("#year-select") %>% 
  html_elements("option") %>% 
  html_attr("value")

# Species: speciesCode:label
data.frame(
  name = names(formToFill$fields$species$options),
  value = as.character(formToFill$fields$species$options)
)

# Radios: outputFormat = default or csv; dnaOnly = yes or no
formToFill$fields$outputFormat$type
fields <- sapply(formToFill$fields, function(x) x$type)

# isolate the values for each radio
outputFormatIndex <- which(names(fields) %in% "outputFormat")

formToFill$fields[outputFormatIndex] %>% 
  sapply(function(x) x$value)

# Filling out the form
# For selectors, easy
# For radios and checkboxes, have to delete fields

# Want outputFormat = csv, so remove the field 'outputFormat: default'
formToFill$fields[1] <- NULL
# Want dnaOnly = no
formToFill$fields[5] <- NULL
# Want age = no (no restrictions)
formToFill$fields[6:7] <- NULL
# query box is actually default checked; remove if don't want
formToFill$fields[8]
# Keep in mind the sequence of operations here; the indices will change as you remove others

formToFill

filledForm <- formToFill %>%
  html_form_set(year = 2024,
                species = "1:f") %>%
  session_submit(x = queryLink)

# Returned page is a page with the query link and not the link itself. Need to pull it
filledForm %>% 
  html_elements("body > div.pindent > p") %>% 
  html_text()

# Can wrap everything into a function:
pullLossSacPas <- function(outputFormat, year, species, dnaOnly, age, query = c(T, F),
                           form,
                           url = "https://www.cbr.washington.edu/sacramento/data/query_loss_detail.html") {
  
  # Checking first
  # Do you want a query returned or not?
  if (all(c(T, F) %in% query)) {
    message("No preference chosen for the ", shQuote("query"), " argument. Defaulting to T.")
    query <- T
  }
  
  # Request
  page <- session(url)
  
  # Pull form; only 1 form on the page for now
  pageForm <- html_form(page)
  numberForms <- length(pageForm)
  
  if (numberForms > 1) {
    message("There are ", numberForms, " on the webpage. Choose the right one via the ", shQuote("form"), " argument.")
    return(pageForm)
  } else form <- 1
  pageForm <- pageForm[[form]]
  
  # Are you missing any fields?
  allOptionsNames <- c("outputFormat", "year", "species", "dnaOnly", "age")
  
  # Function to find options
  findValidValues <- function(missing, fields = pageForm$fields) {
    formFields <- pageForm$fields[names(pageForm$fields) %in% missing]
    
    selectors <- (sapply(formFields, '[[', "type") %in% "select") %>% 
      formFields[.] %>% 
      sapply(., '[[', "options")
    
    otherFields <- (!sapply(formFields, '[[', "type") %in% "select") %>% 
      formFields[.] %>% 
      sapply(., '[[', "value")
    
    message("Argument(s): ", paste(shQuote(c(unique(names(otherFields)), names(selectors))), collapse = ", "), " need a valid argument.")
    
    if (length(otherFields) > 0) print(otherFields)
    if (length(selectors) > 0) print(selectors)
  }
  
  if (any(!allOptionsNames %in% names(as.list(match.call())[-1]))) {
    missingArguments <- allOptionsNames[!allOptionsNames %in% names(as.list(match.call()[-1]))]
    
    findValidValues(missingArguments)
    stop("Choose a valid value from above.", call. = F)
  }
  
  # Are all specified options still correct? Did they change the option names?
  allOptions <- c(outputFormat, year, species, dnaOnly, age)
  
  badOptions <- !allOptions %in% sapply(pageForm$fields, function(x) x$value)
  if (any(badOptions)) {
    findValidValues(allOptionsNames[badOptions])
    stop("Choose a valid value from above.", call. = F)
  }
  
  # Select the radios
  radioSelection <- function(selectionValue, radioName = deparse(substitute(selectionValue))) {
    
    index <- which(names(pageForm$fields) %in% radioName)
    values <- sapply(pageForm$fields[index], function(x) x$value)
    
    if (length(index) == 0) {
      message("Radio ", shQuote(radioName), " no longer exists and will be ignored.")
      return(pageForm)
    }
    
    if (missing(selectionValue)) {
      cat(values, "\n")
      stop("Choose a value for ", shQuote(radioName), " listed above.", call. = F)
    } else {
      pageForm$fields[index[which(!values %in% selectionValue)]] <- NULL
    }
    pageForm
  }
  # First, outputFormat radio
  pageForm <- radioSelection(outputFormat)
  # Second, dnaOnly radio
  pageForm <- radioSelection(dnaOnly)
  # Third, age radio
  pageForm <- radioSelection(age)
  
  # Select year and Species
  if (missing(year)) {
    message("Choose a water year: ")
    return(as.character(pageForm$fields$year$options))
  } 
  
  if (missing(species)) {
    message("Choose a species: ")
    return(data.frame(
      name = names(pageForm$fields$species$options),
      value = as.character(pageForm$fields$species$options)
    ))
  }
  
  # Select if a query link is required or not
  if (!query) {
    pageForm$fields$datalink <- NULL
  }
  
  # Any other necessary field missing?
  fieldNames <- names(pageForm$fields)[names(pageForm$fields) != ""]
  newFields <- fieldNames[duplicated(fieldNames)]
  if (length(newFields) > 0) {
    message("Additional form fields were found. You may need to fix this function. These are their options: ")
    print(
      lapply(newFields, function(x) {
        fieldInterest <- pageForm$fields[names(pageForm$fields) %in% x]
        sapply(1:length(fieldInterest), function(x) fieldInterest[[x]]$value)
      }) %>% 
        setNames(newFields)
    )
  }
  
  queriedPage <- pageForm %>% 
    html_form_set(year = year, 
                  species = species) %>% 
    session_submit(x = page)
  
  if (query) {
    queryLink <- queriedPage %>% 
      html_elements("body > div.pindent > p") %>% 
      html_text()
    
    return(queryLink)
  } else queriedPage
}

pullLossSacPas(outputFormat = "csv", dnaOnly = "no", age = "no", year = 2024, species = "1:f", query = T) %>% 
  read_csv()
