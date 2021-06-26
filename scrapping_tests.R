library(tidyverse)
library(rvest)
library(polite)
library(tesseract)
library(here)

#### Functions ####

# Function to obtain links for UNGA resolutions out of a session table
# e.g. https://www.un.org/depts/dhl/resguide/r1_resolutions_table_eng.htm

# Parameters: 
# p: path of the UNGA Session table
# s: {polite}'s webscrapping session
tibble_links <- function(p, s){
  
  # Scrape UN session table
  scrape_resul <-  nod(s, p) %>% 
    scrape() 
  
  # Some tables are scraped in raw format, and thus have to be converted into html. 
  if("raw" %in% class(scrape_resul)){
    scrape_resul <- read_html(scrape_resul)
  }
  
  # We are interested in the first colum of the table
  scrape_resul %>% 
    html_nodes("td:nth-child(1) a") %>% 
    map_dfr(function(y) 
      tibble(UNRES = html_text(y) %>% str_trim(), 
             LINK = html_attr(y,"href"))) %>% 
    filter(!is.na(LINK))
}

# Use {purrr}'s possibly to identify errors without losing everything else
possibly_tibble_links <- possibly(tibble_links,otherwise = NULL)

# Function to download the pdf of a UNGA resolution
# Parameters
# s: {polite}'s webscrapping session
# un_path: UN website path to where the embed pdf is located
# ...: other arguments passed to {polite}'s rip. This notably include destfile, path, and overwrite. 
download_unga_resol <- function(s, un_path, ...){
  
  # Get full page
  pag_un <- s %>% 
    nod(un_path) 
  
  # Obtain actual link to the pdf (since it is embed)
  link_pdf <- pag_un %>% 
    scrape() %>% 
    html_node(".embed-responsive-item") %>% 
    html_attr("src") 
  
  # Download pdf
  pag_un %>% 
    nod(path = link_pdf) %>%
    rip(...)
  
}

# Use {purrr}'s possibly to identify errors without losing everything else
possibly_download_unga_resol <- possibly(download_unga_resol, otherwise = "ERROR")


#### UNGA voting data ####

# Sources 
#
# Accesed and downloaded on June 15, 2021
# available at https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LEJUQZ
#
# Raw UN Data
#   Erik Voeten 
#   "Data and Analyses of Voting in the UN General Assembly" 
#   Routledge Handbook of International Organization, edited by Bob Reinalda (published May 27, 2013)
#
# Ideal Point Data
#   Bailey, Michael A., Anton Strezhnev, and Erik Voeten. 2017. 
#   Estimating dynamic state preferences from united nations voting data. 
#   Journal of Conflict Resolution 61 (2): 430-56.

# Loading completeVotes
load(here("data_voeten","UNVotes-1.RData")) 

# Only keep resolution level data for now. 
# Taking care of changing the unres field to facilitate matches with UN identifiers 
# since they always have the same format of A/RES/[S]/[NUM][...]
votes_prematch <- completeVotes %>% 
  distinct(rcid, year, session, importantvote, date, 
           unres, amend, para, short, descr, 
           me, nu, di, hr, co, ec, 
           ident, resid) %>% 
  mutate(unres_match = if_else(str_detect(unres,"^R/"),
                               str_replace(unres,"^R/","A/RES/"),
                               unres),
         unres_base = str_extract(unres_match,"^A/RES/(\\d{1,2})/(\\d+)"),
         unres_num = str_remove(unres_base, paste0("A/RES/",session,"/")) %>% as.numeric) %>% 
  relocate(unres_match,.after=unres) %>% 
  as_tibble()

# Some votes don't have a base resolution, come from other sessions or have some different format
votes_check_unres <- filter(votes_match, is.na(unres_num))

# We keep, for now, only those matchable
votes_match <- filter(votes_prematch, !is.na(unres_num))
  
#### UN webscrapping session ####

# Accesed and downloaded on June 23, 2021
# One first thing to notice is that the URLs format for English tables changed from around 2013.
session_tables <- bow("https://www.un.org")
list_tables <- paste0("depts/dhl/resguide/r",1:67,"_resolutions_table_eng.htm") %>% 
  c(paste0("depts/dhl/resguide/r",68:75,"_resolutions_table_en.htm"))

# Scrape all UNGA tables 
table_links <- seq_along(list_tables) %>% 
  map_dfr(~ possibly_tibble_links(p = list_tables[.x], s = session_tables) %>% 
            mutate(UNSES = .x))

# Prepare links for matching. Notably, we 
# 1) Verify that all UN identifiers have the format "A/RES/[...]" 
#     (errors found on sessions 60, 65, and 70).
# 2) Check which Roman numerals within parenthesis, if any, are present.
#     (these were found to be UNGA sessions 1-30)
# 3) Make appropriate corrections to the UN identifier so that it has the format "A/RES/[S]/[...]"
#     (additional corrections needed for some rows from UNGA sessions 30 and 40)
# 4) Extract the resolution identifier "base" that is "A/RES/[S]/[NUM]" without letters
# 5) Remove rows incorrectly scraped as resolutions 
#     (2 rows from UNGA session 70)
links_match <- table_links %>% 
  mutate(ID_FAZH = str_pad(row_number(), 6,"left", 0),
         VERIF = str_sub(UNRES,1,6) == "A/RES/", 
         ROMANOS = str_extract(UNRES,"\\([IVXL]*\\)"),
         CORR = case_when(VERIF ~ UNRES,
                          str_sub(UNRES,1,2) %in% c(">A","AA") ~ str_sub(UNRES,2),
                          T ~ NA_character_),
         CORR = if_else(is.na(ROMANOS), 
                        CORR, 
                        str_remove(CORR,"\\([IVXL]*\\)") %>%  
                          str_replace("^A/RES", paste0("A/RES/",UNSES))) %>% 
           str_trim(),
         CORR = {if_else(str_detect(CORR, "/4 0/"), 
                         str_replace(CORR, "/4 0/", "/40/"), 
                         CORR)},
         RES_BASE = str_extract(CORR,"^A/RES/(\\d{1,2})/(\\d+)")) %>%
  filter(!is.na(CORR)) 


#### UN direct match of resolutions ####

direct_match <- inner_join(votes_match, links_match, by = c("unres_match" = "CORR"))

session_pdf <-  bow("https://undocs.org")

# Accesed and downloaded on June 23, 2021 (takes around 10 hours)
first_download_log <- direct_match %>% 
  select(LINK, unres_match, UNSES) %>% 
  pmap_chr(
    ~ possibly_download_unga_resol(
      s = session_pdf, 
      un_path = ..1,
      destfile = paste0(str_replace_all(..2,"\\/","_"), ".pdf"), 
      path = here("resol_pdf",paste0(str_pad(..3,2,"left",0))), 
      overwrite = TRUE)
  ) %>% 
  tibble(RESUL = .) %>% 
  mutate(DATE = Sys.Date()) %>% 
  bind_cols(select(direct_match,UNSES,unres_match,LINK),.)
# Save not to lose progress
save(first_download_log, 
     file = here("resol_pdf", "00_logs", 
                 paste0("first_download_log_", 
                        max(first_download_log$DATE) %>% str_replace_all("-","_"),
                        ".Rdata")
                 )
     )
