#### Preamble ####
# Purpose: Gather data from the Kenyan census PDF
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Date: 22 May 2020
# Ideas: -
# Issues: -


#### Workspace set-up ####
library(janitor)
library(pdftools)
library(tidyverse)
library(stringi)


#### Get the data ####
download.file(url = "PASTE_THE_URL_HERE",
              destfile = "inputs/kenya_census.pdf")


#### Create a sample ####
all_pages <- pdftools::pdf_text("inputs/kenya_census.pdf")

page_26 <- stringi::stri_split_lines(all_pages[[PASTE_THE_PAGE_HERE]])[[1]] 

# Get rid of the top matter
page_26_no_header <- page_26[PASTE_THE_FIRST_ROW_OF_INTEREST_HERE:length(page_26)]

# Get rid of the bottom matter
page_26_no_header_no_footer <- page_26_no_header[1:PASTE_THE_LAST_ROW_OF_INTEREST_HERE]

# Convert into a tibble
demography_data <- tibble(all = page_26_no_header_no_footer)

# # Split columns
demography_data <-
  demography_data %>%
  mutate(all = str_squish(all)) %>% # Any space more than two spaces is squished down to one
  mutate(all = str_replace(all, "10 -14", "10-14")) %>% 
  mutate(all = str_replace(all, "Not Stated", "NotStated")) %>% # Any space more than two spaces is squished down to one
  separate(col = all,
           into = c("age", "male", "female", "intersex", "total"),
           sep = " ", # Just looking for a space. Seems to work fine because the tables are pretty nicely laid out
           remove = TRUE,
           fill = "right"
  )

# Fix the types
demography_data <-
  demography_data %>%
  filter(age != "Age") %>% 
  mutate_at(vars(male, female, intersex, total), ~str_remove_all(., "INSERT_CHARACTER")) %>%
  mutate_at(vars(male, female, intersex, total), ~as.integer(.))


# Save
write_csv(demography_data, "outputs/cleaned_page_26.csv")


