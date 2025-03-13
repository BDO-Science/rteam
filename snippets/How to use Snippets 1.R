#How to use Snippets.R----

# Nick Bertrand
# Start Date: 3-22-2024

#About----
#Project: R studio tutorials
#Purpose:Introducing the wonders of snippets


# ## Using Snippets in R Studio
# 
# Snippets are means to set template code chunks for frequently used elements in scripts. 
# To access the options to create a snippet follow the selections below within R Studio
# 
# Tools => Global Options => Code => Tab Editing => Snippets "Edit Snippets".
# 
# Note that all Snippets are offset by a tab within the edit snippets window.
# 
# ## Example Snippets (These should copy and paste into the snippets window and work)

snippet TemplateFileFolders
  library(tidyverse)
  # Set working directory ----
  #set to location of root object to highest tier directory
  getwd()
  root <- "C:/Users/nbertrand/OneDrive - DOI/Desktop/Bertrand/GitHub/ADD DIRECTORY NAME HERE"
  setwd(root)
  
  #create directories in new project folder
  #vector of folder names to be created
  folderlist <- c("Data","R_scripts","Table_Output", "Viz_Output")
  #function to iterate through folderlist
  makefolder_fun <- function(folderlist){
    dir.create(file.path(root, folderlist))
  }
  #map function through the vector of folder names
  map(folderlist,makefolder_fun)
  #delete these lines after folders are created
snippet TemplateHeader
  #01_FileName.R----
  
  # Nick Bertrand
  # Start Date: `r paste(date())`
  
  #About----
  #Project: 
  #Purpose:
  
  #Libraries ----
  library(tidyverse)
  library(readr)
  
  # Set working directory ----
  #set to location of root object to highest tier directory
  getwd()
  root <- "C:/Users/nbertrand/OneDrive - DOI/Desktop/Bertrand/GitHub/ADD DIRECTORY NAME HERE"
  setwd(root)
  getwd()
  #these root object use directories 
  data_root<-file.path(root,"Data")
  code_root <- file.path(root,"R_scripts")
  table_output_root <- file.path(root,"Table_Output")
  viz_output_root <- file.path(root,"Viz_Output")
snippet Templateggplot
  #ggplot pallet
  my_palette=c('#003E51','#007396', '#C69214', '#DDCBA4','#FF671F', '#215732',
               '#4C12A1','#9a3324', "#88CCEE","#AA4499")
  
  ggplot(iris, aes(x=Petal.Length,y= Petal.Width, color=Species)) +
    geom_point()+
    labs(title = "Density plot of the % Delta Inflow grouped by Water Year Type")+
    theme(legend.title=element_blank())+
    xlab("% Inflow Exported")+
    facet_wrap(~Species)+
    scale_colour_manual(values = my_palette)