library(tidyverse)
library(magrittr)
library(here)
library(pdftools)

#### Functions ####

extract_toc <- function(x, vol = 1){
  
  # We extract page 3 of the document x
  raw_toc <- x[[3]] %>% 
    str_split("\n") %>% 
    unlist() %>% 
    str_trim()
  
  # The section for the 4th Committee is expected to be divided between lines 
  # We then extract the table for the main sections 
  # We assume the first two lines are "Content" and "Section/Page"
  # This is different depending on the volume
  
  if(vol == 1){
    #  In volume 1
    
    #  The 4th Committee section is divided between lines 5 and 6 of the raw toc 
    raw_toc[5] <- paste(raw_toc[5:6],collapse = " ")
    raw_toc <- raw_toc[-6]
    
    # we have 7 main sections assuming (corrected) lines 3 to 9 
    # we also identify the page where annexes start, assuming it's line 11
    
    clean_toc <- raw_toc[3:9] %>% 
      str_split_fixed(pattern = "(\\.{2,})", n = 2) %>% 
      as.data.frame() %>% 
      tibble() %>% 
      transmute(Section = str_trim(V1), 
                Start_Page = str_trim(V2) %>% as.integer,
                Annex_Page = str_extract(raw_toc[11],"\\d+$") %>% as.integer)
    
  } else {
    #  In volume 3 
    
    #  The 4th Committee section is divided between lines 4 and 5 of the raw toc 
    raw_toc[4] <- paste(raw_toc[4:5],collapse = " ")
    raw_toc <- raw_toc[-5]
    
    # we have 3 main sections assuming (corrected) lines 3 to 5 
    # we also identify the page where decisions start, assuming it's line 6
    
    clean_toc <- raw_toc[3:5] %>% 
      str_split_fixed(pattern = "(\\.{2,})", n = 2) %>% 
      as.data.frame() %>% 
      tibble() %>% 
      transmute(Section = str_trim(V1), 
                Start_Page = str_trim(V2) %>% as.integer,
                Annex_Page = str_extract(raw_toc[6],"\\d+$") %>% as.integer)
  }
  
  return(clean_toc)
  
}

soft_check_toc <- function(toc, shift = 4, all_pages = vol_1_all, n_check = 20){
  toc %>% 
    mutate(Soft_Check_Start = map_chr(Start_Page, 
                                      ~ all_pages[.x + shift] %>% 
                                        str_trim() %>% 
                                        str_sub(end = n_check)) %>% 
             equals(str_sub(Section, end = n_check)),
           Soft_Check_Prev = map_chr(Start_Page, 
                                     ~ all_pages[.x + shift - 1] %>% 
                                       str_trim() %>% 
                                       str_sub(end = n_check)) %>% 
             equals(str_sub(Section, end = n_check)),
           Soft_Check = Soft_Check_Start & !Soft_Check_Prev,
           i = row_number()) %>% 
    filter(!Soft_Check)
}

remove_header_footer <- function(raw_page, header){
  
  # Ensure we have a trimmed page
  raw_page <- str_trim(raw_page)
  
  # Verify headers, if they don't match, return NA
  end_header <- nchar(header)
  check_header <- str_sub(string = raw_page, end = end_header) == header
  end_header <- if_else(check_header, end_header, NA_integer_)
  
  # The footer is usually the document page on its own line, 
  # The function returns NA if it doesn't locate this pattern.
  start_footer <- str_locate(raw_page,"\n\\s*\\d+$")[,"start"]
  
  new_page <- str_sub(raw_page,start = end_header + 1, end = start_footer - 1) %>% 
    unlist() %>% 
    str_trim()
  
  return(new_page)
  
}

generate_raw_sec_toc <- function(master_tibble, vol = 1){
  
  raw_sec_toc <- master_tibble %>% 
    transmute(Session,Year,Section,
              Raw_toc = str_remove(Section_toc,"^Contents\n") %>% 
                str_remove_all("Resolution\nnumber\\s+Title\\s+Page\n")) %>% 
    split(.$Section) %>% 
    map_dfr(~tibble(Raw_toc = str_split(.x$Raw_toc,"\n") %>% unlist) %>% 
              bind_cols(select(.x,-Raw_toc),.)) %>% 
    mutate(Starts_With_Resol = str_detect(Raw_toc, paste0("^",Session,"/\\d+\\.")),
           Starts_With_SubRes = str_detect(Raw_toc, 
                                           if_else(vol == 1, "^\\s*[A-Z]\\.", "^\\s*Resolution [A-Z]\\.")
                                           ),
           Ends_With_Page = str_detect(Raw_toc,"(\\.{2,})\\s*\\d+$"), 
           Line_Type = case_when(Starts_With_Resol & Ends_With_Page ~ "Base",
                                 Starts_With_Resol & !Ends_With_Page ~ "Root",
                                 Starts_With_SubRes & Ends_With_Page ~ "Sub_Final",
                                 Starts_With_SubRes & !Ends_With_Page ~ "Sub_Temp",
                                 Ends_With_Page ~ "Extra_Final",
                                 !Ends_With_Page ~ "Extra_Temp")) %>%
    separate(col = Raw_toc, into = c("UNRES_Raw","Aux"), sep="\\s", 
             remove = FALSE, fill = "left", extra = "merge") %>% 
    mutate(UNRES_Raw = str_remove(UNRES_Raw,"\\.$"), 
           Aux = str_trim(Aux)) %>% 
    separate(col = Aux, into = c("Title_prelim","Start_Page"), sep = "\\.{2,}", 
             fill = "right")
  
  for(i in which(raw_sec_toc$UNRES_Raw == "")){
    raw_sec_toc$UNRES_Raw[i] <- raw_sec_toc$UNRES_Raw[i - 1]
  }
  
  raw_sec_toc <- group_by(raw_sec_toc, UNRES_Raw) %>% 
    mutate(N_Sub = n() - 1) %>% 
    ungroup()
  
  for(i in which(raw_sec_toc$Line_Type == "Root")){
    
    aux_i <- i + raw_sec_toc$N_Sub[i]
    raw_sec_toc$Start_Page[i] <- raw_sec_toc$Start_Page[aux_i]
    raw_sec_toc$Title_prelim[i] <- paste(raw_sec_toc$Title_prelim[i:aux_i], collapse = " ")
    
  }
  
  for(i in which(raw_sec_toc$Line_Type %in% "Root")){
    
    aux_i <- i + raw_sec_toc$N_Sub[i]
    raw_sec_toc$Start_Page[i] <- raw_sec_toc$Start_Page[aux_i]
    raw_sec_toc$Title_prelim[i] <- paste(raw_sec_toc$Title_prelim[i:aux_i], collapse = " ")
    
  }
  
  raw_sec_toc <- filter(raw_sec_toc, !Line_Type %in% c("Extra_Temp","Extra_Final"))
  
  return(raw_sec_toc)
  
}

#### General data ####

shift <- 4

vol_1_all <- list.files(here("resol_pdf")) %>% 
  keep(~str_detect(.x,"Vol.I\\)")) %>% 
  rev() %>% 
  map(~pdf_text(here("resol_pdf",.x)))

vol_3_all <- list.files(here("resol_pdf")) %>% 
  keep(~str_detect(.x,"Vol.III\\)")) %>% 
  rev() %>% 
  map(~pdf_text(here("resol_pdf",.x)))

#### Table of Contents checking ####

# Volume I

vol_1_toc_check <- seq_along(vol_1_all) %>% 
  map(~extract_toc(vol_1_all[[.x]], vol = 1) %>% 
        mutate(Session = 76 - .x,
               Year = 2021 - .x))

seq_along(vol_1_toc_check) %>% 
  map(~soft_check_toc(toc = vol_1_toc_check[[.x]],
                      all_pages = vol_1_all[[.x]]))

vol_1_toc_check[[1]]$Start_Page[2] <- 223
soft_check_toc(toc = vol_1_toc_check[[6]], shift = 3, all_pages = vol_1_all[[6]])

seq_along(vol_1_toc_check) %>% 
  map(~soft_check_toc(toc = vol_1_toc_check[[.x]],
                      shift = if_else(.x == 6, 3, 4),
                      all_pages = vol_1_all[[.x]]))

# Volume 3

vol_3_toc_check <- seq_along(vol_3_all) %>% 
  map(~extract_toc(vol_3_all[[.x]], vol = 3) %>% 
        mutate(Session = 75 - .x,
               Year = 2020 - .x))

seq_along(vol_3_toc_check) %>% 
  map(~soft_check_toc(toc = vol_3_toc_check[[.x]],
                      all_pages = vol_3_all[[.x]]))


#### Table of Contents Construction ####

# Volume I

vol_1_toc <- vol_1_toc_check %>% 
  map(~.x %>% 
        mutate(End_Page = lead(Start_Page, default = unique(Annex_Page))-1) %>% 
        select(Section, Session, Year, Start_Page, End_Page))

vol_1_raw <- map(vol_1_all,str_trim)

# The shift is different for session 69 since there is no blank page 4
for(s in seq_along(vol_1_raw)){
  print(s)
  aux_i <- vol_1_toc[[s]]$Start_Page[3] + if_else(s == 6, 3, shift)
  str_detect(string = vol_1_raw[[s]][aux_i], 
             pattern = "(?<=Special Political)\n\\s*(?= and Decol)") %>% print
  vol_1_raw[[s]][aux_i] <- str_remove(string = vol_1_raw[[s]][aux_i], 
                                      pattern = "(?<=Special Political)\n\\s*(?= and Decol)")
  
  aux_i <- vol_1_toc[[s]]$Start_Page[6] + if_else(s == 6, 3, shift)
  str_detect(string = vol_1_raw[[s]][aux_i], 
             pattern = "_+\n(\\*||∗)\n*[[:print:]]*(?=\n\\s+\\d+$)") %>% print
  str_detect(string = vol_1_raw[[s]][aux_i], 
             pattern = "(?<=Committee)(\\*||∗)") %>% print
  str_detect(string = vol_1_raw[[s]][aux_i], 
             pattern = "(((?<=Page\n)(\\*||∗)\n)|((?<=number\n)(\\*||∗)\n))") %>% print
  vol_1_raw[[s]][aux_i] <- str_remove(string = vol_1_raw[[s]][aux_i], 
                                      pattern = "_+\n(\\*||∗)\n*[[:print:]]*(?=\n\\s+\\d+$)") %>% 
    str_remove(pattern = "(?<=Committee)(\\*||∗)") %>% 
    #There may be a hidden * or  at the top of the toc
    str_remove(pattern = "(((?<=Page\n)(\\*||∗)\n)|((?<=number\n)(\\*||∗)\n))")
}

# Volume III

vol_3_toc <- vol_3_toc_check %>% 
  map(~.x %>% 
        mutate(End_Page = lead(Start_Page, default = unique(Annex_Page))-1) %>% 
        select(Section, Session, Year, Start_Page, End_Page))

vol_3_raw <- map(vol_3_all,str_trim)

for(s in seq_along(vol_3_raw)){
  print(s)
  aux_i <- vol_3_toc[[s]]$Start_Page[2] + shift
  str_detect(string = vol_3_raw[[s]][aux_i],
             pattern = paste0("(",
                              "((?<=Special Political)\n\\s*(?= and Decol))",
                              "|",
                              "((?<=and Decolonization)\n\\s*(?= Committee ))",
                              ")")) %>% print
  vol_3_raw[[s]][aux_i] <- str_remove(string = vol_3_raw[[s]][aux_i],
                                      pattern = paste0("(",
                                                       "((?<=Special Political)\n\\s*(?= and Decol))",
                                                       "|",
                                                       "((?<=and Decolonization)\n\\s*(?= Committee ))",
                                                       ")"))
  
  aux_i <- vol_3_toc[[s]]$Start_Page[3] + shift
  str_detect(string = vol_3_raw[[s]][aux_i],
             pattern = paste0("(",
                              "_+\n(\\*||∗)\n*[[:print:]]*(?=\n\\s+\\d+$)",
                              "|",
                              "_+\n(\\*||∗)\n*[[:print:]]*\nCommittee\\.(?=\n\\s+\\d+$)",
                              ")")) %>% print
  str_detect(string = vol_3_raw[[s]][aux_i],
             pattern = "(?<=Committee)(\\*||∗)") %>% print
  str_detect(string = vol_3_raw[[s]][aux_i],
             pattern = "(((?<=Page\n)(\\*||∗)\n)|((?<=number\n)(\\*||∗)\n))") %>% print
  vol_3_raw[[s]][aux_i] <- str_remove(string = vol_3_raw[[s]][aux_i],
                                      pattern = paste0("(",
                                                       "_+\n(\\*||∗)\n*[[:print:]]*(?=\n\\s+\\d+$)",
                                                       "|",
                                                       "_+\n(\\*||∗)\n*[[:print:]]*\nCommittee\\.(?=\n\\s+\\d+$)",
                                                       ")")) %>%
    str_remove(pattern = "(?<=Committee)(\\*||∗)") %>%
    #There may be a hidden * or  at the top of the toc
    str_remove(pattern = "(((?<=Page\n)(\\*||∗)\n)|((?<=number\n)(\\*||∗)\n))")

}

#### Construct Master Tibbles ####

vol_1_master_tibble <- seq_along(vol_1_toc) %>% 
  map(function(s) vol_1_toc[[s]] %>% 
        mutate(Raw_Pages = pmap(list(Start_Page, End_Page, Section),
                                ~ vol_1_raw[[s]][..1:..2 + if_else(s == 6, 3, shift)] %>% 
                                  discard(~.x == "") %>% 
                                  remove_header_footer(paste0(..3,"\n"))),
               First_Resol = map2_int(Raw_Pages,Session,
                                      ~str_detect(.x,paste("^RESOLUTION",.y)) %>% 
                                        which %>% 
                                        min),
               Section_toc = map2_chr(Raw_Pages,First_Resol,
                                      ~.x[1:(.y-1)] %>% 
                                        paste(collapse = "\n")),
               Section_res = map2_chr(Raw_Pages,First_Resol,
                                      ~.x[.y:length(.x)] %>% 
                                        paste(collapse = "\n"))) %>% 
        select(-Raw_Pages,-First_Resol))

vol_3_master_tibble <- seq_along(vol_3_toc) %>% 
  map(function(s) vol_3_toc[[s]] %>% 
        mutate(Raw_Pages = pmap(list(Start_Page, End_Page, Section),
                                ~ vol_3_raw[[s]][..1:..2 + shift] %>% 
                                  discard(~.x == "") %>% 
                                  remove_header_footer(paste0(..3,"\n"))),
               First_Resol = map2_int(Raw_Pages,Session,
                                      ~str_detect(.x,paste("^RESOLUTION",.y)) %>% 
                                        which %>% 
                                        min),
               Section_toc = map2_chr(Raw_Pages,First_Resol,
                                      ~.x[1:(.y-1)] %>% 
                                        paste(collapse = "\n")),
               Section_res = map2_chr(Raw_Pages,First_Resol,
                                      ~.x[.y:length(.x)] %>% 
                                        paste(collapse = "\n"))) %>% 
        select(-Raw_Pages,-First_Resol))

#### Construct Checking TOC tibble ####

vol_1_raw_sec_toc <- map(vol_1_master_tibble,generate_raw_sec_toc)

for(s in seq_along(vol_1_raw_sec_toc)){
  vol_1_raw_sec_toc[[s]]$Session %>% unique %>% print
  unique(vol_1_raw_sec_toc[[s]]$UNRES_Raw) %>% length %>% print
  filter(vol_1_raw_sec_toc[[s]], str_detect(Line_Type,"Sub")) %>% print
}

## THIS IS FAILING ##
# vol_3_raw_sec_toc <- map(vol_3_master_tibble,generate_raw_sec_toc,vol=3)
# 
# for(s in seq_along(vol_3_raw_sec_toc)){
#   vol_3_raw_sec_toc[[s]]$Session %>% unique %>% print
#   unique(vol_3_raw_sec_toc[[s]]$UNRES_Raw) %>% length %>% print
#   filter(vol_3_raw_sec_toc[[s]], str_detect(Line_Type,"Sub")) %>% print
# }

#### Recover Resolution Texts ####

vol_1_resol_text_df <- vol_1_master_tibble %>% 
  map_dfr(~ .x$Section_res %>% 
            paste(collapse = "\n") %>% 
            str_split(pattern = "(?=RESOLUTION)") %>% 
            unlist %>% 
            tibble(Resolution_Text = .) %>% 
            slice(-1) %>% 
            separate(col = Resolution_Text, into = c("Resolution_Header","Aux"), 
                     sep="\n", extra = "merge") %>% 
            mutate(UNRES = str_extract(Resolution_Header,"\\d+/\\d+"), 
                   Aux = str_trim(Aux),
                   Aux_Pos = map2(Aux,UNRES,~str_locate(.x, pattern = paste0(.y,"\\.\\s+"))), 
                   Start_Pos = map_int(Aux_Pos,~.x[1,"start"]), 
                   End_Pos = map_int(Aux_Pos,~.x[1,"end"]), 
                   Resol_Meta = map2_chr(Aux,Start_Pos,~str_sub(.x,end = .y - 1)), 
                   Resol_Text = map2_chr(Aux,End_Pos, ~ str_sub(.x,start = .y + 1)),
                   Resol_Text_WPOP = Resol_Text %>% 
                     str_remove_all("(?<=\n)\\s+[A-Z][a-z]*ing") %>% 
                     str_remove_all("(?<=\n)\\s+\\d+\\.\\s+[A-Z][a-z]*")) %>% 
            select(UNRES,Resol_Meta,Resol_Text,Resol_Text_WPOP))

# FAILS IN SOME REGARDS (HIDDEN CHARACTERS AND RES/73/293)
vol_3_resol_text_df <- vol_3_master_tibble %>% 
  map_dfr(~ .x$Section_res %>% 
            paste(collapse = "\n") %>% 
            str_split(pattern = "(?=RESOLUTION)") %>% 
            unlist %>% 
            tibble(Resolution_Text = .) %>% 
            slice(-1) %>% 
            separate(col = Resolution_Text, into = c("Resolution_Header","Aux"), 
                     sep="\n", extra = "merge") %>% 
            mutate(UNRES = str_extract(Resolution_Header,"\\d+/\\d+"), 
                   Aux = str_trim(Aux),
                   Aux_Pos = map2(Aux,UNRES,~str_locate(.x, pattern = paste0(.y,"\\.\\s+"))), 
                   Start_Pos = map_int(Aux_Pos,~.x[1,"start"]), 
                   End_Pos = map_int(Aux_Pos,~.x[1,"end"]), 
                   Resol_Meta = map2_chr(Aux,Start_Pos,~str_sub(.x,end = .y - 1)), 
                   Resol_Text = map2_chr(Aux,End_Pos, ~ str_sub(.x,start = .y + 1)),
                   Resol_Text_WPOP = Resol_Text %>% 
                     str_remove_all("(?<=\n)\\s+[A-Z][a-z]*ing") %>% 
                     str_remove_all("(?<=\n)\\s+\\d+\\.\\s+[A-Z][a-z]*")) %>% 
            select(UNRES,Resol_Meta,Resol_Text,Resol_Text_WPOP))


resol_text_df <- bind_rows(mutate(vol_1_resol_text_df, Vol = "1"),
                           mutate(vol_3_resol_text_df, Vol = "3")) %>% 
  mutate(ID_FAZH = paste(Vol,UNRES,sep="/"))

save(resol_text_df, file = here("resol_data","resol_text_df.RData"))
