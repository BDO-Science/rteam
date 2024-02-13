# Trying to scrape from an active website
library(rvest)
library(dplyr)

# SacPas Temp profiler

webpage <- session("https://www.cbr.washington.edu/shiny/TEMPMAKER/")

# Try and scrape the "Download" link
# Try and scrape the image
# Need the image url to use in download.file

# Both of these requires the url to the resource
# Can see below that these links are unidentified when using rvest (static),
# even though you can "see" it in your Inspect panel from your browser
webpage %>% 
  html_elements("a") %>% 
  html_attr("href")

webpage %>% 
  html_element("#downloadtemps") %>% 
  html_attr("href")

# This is as opposed to the real time monitoring DS page, which is a static page
webpageDS <- session("https://www.cbr.washington.edu/sacramento/workgroups/delta_smelt.html")

webpageDS %>% 
  html_element("body > div.pindent > img:nth-child(34)") %>% 
  html_attr("src") %>% 
  paste0("https://www.cbr.washington.edu", .) %>% 
  download.file("deltasmelt_wtemp.png", mode = "wb")

shell.exec("deltasmelt_wtemp.png")

