library(tidyverse)
library(rvest)
library(polite)
library(tesseract)
library(here)

#### Functions ####

tibble_links <- function(p, s){
  
  scrape_resul <-  nod(s, p) %>% 
    scrape() 
  
  if("raw" %in% class(scrape_resul)){
    scrape_resul <- read_html(scrape_resul)
  }
  
  scrape_resul %>% 
    html_nodes("td:nth-child(1) a") %>% 
    map_dfr(function(y) 
      tibble(UNRES = html_text(y) %>% str_trim(), 
             LINK = html_attr(y,"href"))) %>% 
    filter(!is.na(LINK))
}

possibly_tibble_links <- possibly(tibble_links,otherwise = NULL)

download_unga_resol <- function(s, un_path, ...){
  
  pag_un <- s %>% 
    nod(un_path) 
  
  link_pdf <- pag_un %>% 
    scrape() %>% 
    html_node(".embed-responsive-item") %>% 
    html_attr("src") 
  
  pag_un %>% 
    nod(path = link_pdf) %>%
    rip(...)
  
}

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

votes_match <- completeVotes %>% 
  distinct(rcid, year, session, importantvote, date, 
           unres, amend, para, short, descr, 
           me, nu, di, hr, co, ec, 
           ident, resid) %>% 
  mutate(unres_match = if_else(str_detect(unres,"^R/"),
                               str_replace(unres,"^R/","A/RES/"),
                               unres)) %>% 
  relocate(unres_match,.after=unres)
  
#### UN webscrapping session ####

session_tables <- bow("https://www.un.org")
list_tables <- paste0("depts/dhl/resguide/r",1:67,"_resolutions_table_eng.htm") %>% 
  c(paste0("depts/dhl/resguide/r",68:75,"_resolutions_table_en.htm"))
map(~possible_tables(s = session_tables, p =.x))

prueba_links <- seq_along(list_tables) %>% 
  map_dfr(
    ~ 
      possibly_tibble_links(p = list_tables[.x], s = session_tables) %>% 
      mutate(UNSES = .x))

links_match <- prueba_links %>% 
  mutate(ID_FAZH = str_pad(row_number(), 6,"left", 0),
         VERIF = str_sub(UNRES,1,6) == "A/RES/", 
         ROMANOS = str_extract(UNRES,"\\([IVXL]*\\)"),
         CORR = case_when(VERIF ~ UNRES,
                          str_sub(UNRES,1,2) %in% c(">A","AA") ~ str_sub(UNRES,2),
                          T ~ NA_character_) %>% 
           {if_else(is.na(ROMANOS), .,
                    str_remove(.,"\\([IVXL]*\\)") %>% 
                      str_replace("^A/RES", paste0("A/RES/",UNSES)))}
         
         ) %>%
  filter(!is.na(CORR)) 


#### UN direct match of resolutions ####

direct_match <- inner_join(votes_match, links_match, by = c("unres_match" = "CORR"))

session_pdf <-  bow("https://undocs.org")

aux_descarga <- direct_match %>% 
  select(LINK, unres_match, UNSES) %>% 
  pmap_chr(
    ~ possibly_download_unga_resol(
      s = session_pdf, 
      un_path = ..1,
      destfile = paste0(str_replace_all(..2,"\\/","_"), ".pdf"), 
      path = here("resol_pdf",paste0(str_pad(..3,2,"left",0))), 
      overwrite = TRUE)
  )
