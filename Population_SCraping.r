# Create initial data frame
library(rgdal)
library(raster)
library(httr)
library(XML)
library(RCurl)
library(dplyr)
library(MASS)
library(reshape2)
library(reshape)
library(yaml)
library(sf)
library(sp)
library(maptools)
library(config)
library(ncdf4)
library(RNetCDF)
library(terra)
Lisks_To_Scrape <- function(Path) {
Output <- data.frame(
  Link = 'https://hub.worldpop.org/geodata/summary?id=',
  ID = c(        '11849', '12010', '12002', '11954', '11851', '11872', '11874', '12011', '11882', '12015', '11899', '11888', '11907', '12001', '11916', '11918', '11925', '11919', '11930', '11936', '12008', '11946'),
  Countriey = c('Algeria', 'Bahrain', 'Comoros', 'Djibouti', 'Egypt', 'Iraq', 'Jordan', 'Kuwait', 'Lebanon', 'Libya', 'Mauritania', 'Morocco', 'Oman', 'Palestine', 'Qatar', 'Saudi Arabia', 'Somalia', 'Sudan', 'Syria', 'Tunisia', 'UAE', 'Yemen')
)

# Create an empty list
c <- list()

# Generate data frames for each year
for (i in 1:20) {
  cd <- data.frame(Output, Factor = i * 249)
  c[[i]] <- cd
}

# Assign names to list elements based on years
names(c) <- c('2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')

# Melt the list of data frames into a single data frame
melt(c) -> Result

# Rename the columns of the melted data frame
names(Result) <- c('Link', 'ID', 'Countriey', 'variable', 'value', 'Year')

# Select desired columns and assign to a new data frame
Result %>% dplyr::select('Link', 'ID', 'value', 'Countriey', 'Year') -> Result_ADDD

# Perform data transformations on the Result_ADDD data frame
Final <- Result_ADDD %>%
  mutate(
    ID = as.numeric(ID),     # Convert ID column to numeric if it's not already
    value = as.numeric(value) # Convert value column to numeric if it's not already
  ) %>%
  mutate(
    Add_ID = ID + value,                   # Compute Add_ID column
    Final_link = paste0(Link, Add_ID)      # Compute Final_link column
  )

# Select desired columns and assign to a new data frame
Final %>% dplyr::select('Countriey', 'Year', 'Final_link') -> Final_DF

# Rename columns of Final_DF
names(Final_DF) <- c('Countriey', 'Year', 'Link')

# Update Link and Year columns in the Output data frame
Output$Link <- paste0(Output$Link, Output$ID)
Output$Year <- '2000'

# Select desired columns and assign to a new data frame
Output %>% dplyr::select('Countriey', 'Year', 'Link') -> Output_2000

# Assign IDs to the Output data frame
Output$ID <- 1:nrow(Output)

bind_rows(Output,Final_DF)->Result_DT

Result_DT%>%dplyr::select('Link','Countriey', 'Year')->F1
F1$Countriey_folder<-paste0(Path,'\\',F1$Countriey )
F1$Years_path<-paste0(Path,'\\',F1$Countriey ,'\\',F1$Year)

lapply(F1$Countriey_folder,function(x){dir.create(x)})
lapply(F1$Years_path,function(x){dir.create(x)})->Final_Output

#filtered_data <- F1 %>% filter(!(Countriey == "Algeria" & Year == "2000"))
split(F1,F1$Link)->filtered_data_SP
Download_Data <- function(Maindir, URL, Filter) {
  setwd(Maindir)
  resource <- GET(URL)
  parse <- htmlParse(resource)
  links <- xpathSApply(parse, path = "//a", xmlGetAttr, "href")
  
  # Get the list of URLs in R 
  linkslist <- lapply(links, function(x) {
    if (!startsWith(x, "http")) {
      paste0(URL, x)
    } else {
      x
    }
  })
  
  # Convert the list to a data frame 
  Organise_linkslist <- data.frame(URL = unlist(linkslist))
  Organise_linkslist$Images <- basename(Organise_linkslist$URL)
  Organise_linkslist$Validate <- grepl(Filter, Organise_linkslist$Images)
  
  # Get only the filtered datasets 
  Organise_linkslist <- Organise_linkslist[Organise_linkslist$Validate == TRUE, , drop = FALSE]
  
  # Split the Images 
  Images <- Organise_linkslist
  
  split(Images, Images$Images) -> Organise_linkslist_split
  
  for (i in Organise_linkslist_split) {
    filename <- basename(i[['URL']])
    print(filename)
    
    # Check if the file exists
    if (!file.exists(filename)) {
      # If the file does not exist, download it
      download.file(i[['URL']], filename, mode = 'wb', overwrite = FALSE)
      message(paste("Downloaded", filename))
    } else {
      # If the file exists, skip the download
      message(paste("Skipping download of", filename, "- file already exists"))
    }
  }
}
lapply(filtered_data_SP,function(x){Download_Data(
  
  URL = x$Link,
  Maindir = x$Years_path,
  Filter = ".tif")
  
})
}
Result_DT<-Lisks_To_Scrape(Path='E:\\Population')



