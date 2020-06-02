# Script set up to Extract and Format PDF Files from https://ohiolmi.com/Home/UIClaims
library(pdftools)
library(tidyverse)

# Set Start and End Dates for the weeks to extract
Start_Date <- '2020-01-01'
End_Date <- '2020-05-23'

# Generate List of Weeks to Pull using Start and End Dates
Week_List <-
  unique(strftime(seq(
    as.Date(Start_Date), as.Date(End_Date), 'days'
  ), format = '%g%V'))

# Build URLs for PDF files, will work as long as URLs remain consistent
Url_List <-
  paste('https://ohiolmi.com/portals/206/UC/weekly/UC236cw_',
        Week_List,
        '.pdf',
        sep = '')

# Define Function Here

Scrape_PDF <- function(PDFUrl) {
  # Extract Text from PDF
  text <- pdf_text(PDFUrl)[[2]]
  df_Title <- pdf_data(PDFUrl)[[1]]
  WeekEnding <-
    df_Title$text[3:5] %>% paste(collapse = ' ') %>% as.Date(format = '%B %d, %Y')
  # Read Initial Text into a Single Dataframe Column, Skip 7 unnecessary Rows and include only the 46 rows of 
  df_Text <-
    read_delim(
      text,
      delim = '\r\n',
      skip =  7,
      col_names = 'column',
      n_max = 46
    )
  
  # Replace elipsis with periods for County Name.....
  df_Text$column <- gsub("…", ".", df_Text$column)  %>%
    # Add space between period and first numerical value (Issue found in Week 16 2020 file)
    gsub("\\.([0-9])", " \\1", .) %>%
    # Remove Unnecessary Periods
    gsub("\\.", "", .) %>%
    # Remove Extra Spaces
    gsub("[ ]{2,}", " ", .) %>%
    # Replace Spaces in Particular Cases
    gsub('Out of State', 'OutOfState', .) %>%
    gsub('Interstate Liable', 'InterstateLiable', .) %>%
    gsub('Interstate Agent', 'InterstateAgent', .) %>%
    gsub('Van Wert', 'VanWert', .) %>%
    # Remove commas from Numerics
    gsub(",","",.)
  # Seperate Text into Initial Table in same format as PDF (2 Tables "side by side")
  df_Dirty_Table <-
    separate(
      df_Text,
      column,
      into = c(
        'County',
        'Initial_Claims',
        'Continued_Claims',
        'County.2',
        'Initial_Claims.2',
        'Continued_Claims.2'
      ),
      sep = ' '
    )
  # Bind "side by side" tables into single table, add the "Week Ending Date", and remove empty rows
  df_Clean_Table <-
    df_Dirty_Table[1:3] %>% bind_rows(
      select(
        df_Dirty_Table,
        'County' = County.2,
        'Initial_Claims' = Initial_Claims.2,
        'Continued_Claims' = Continued_Claims.2
      )
    ) %>% mutate('Week_Ending' = WeekEnding) %>% drop_na(County)
  
  # Re Add Spaces in County Names where appropriate
  df_Clean_Table$County <-
    gsub("(.)([[:upper:]])", "\\1 \\2", df_Clean_Table$County)
  return(df_Clean_Table)
  

}

# Map over Defined Function with List of URLs
df_Export <- map_dfr(Url_List, Scrape_PDF)

write.csv(df_Export, 'UnemploymentByCounty.csv', row.names = FALSE, quote = FALSE)