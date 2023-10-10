
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rvest)

# Data --------------------------------------------------------------------

akuma_no_mi <-  readRDS("akuma_no_mi.rds")

techniques_info <- lapply(1:(akuma_no_mi %>%  nrow()), function(i){
  akuma_name <- akuma_no_mi$Akuma_name[i]
  url <- akuma_no_mi$Url[i]
  link_akuma <- paste0("https://onepiece.fandom.com", url)
  
  page <- link_akuma %>% read_html()
  
  all_text <- page %>% 
    html_elements("li") %>% 
    html_text() %>% 
    str_replace_all('[\r\n\t]', '')
  
  kanjiid = page %>% 
    html_elements("li") %>% as.character() %>%  str_detect("kanji")
  
  all_text2 = all_text[kanjiid]
  
  df <- data.frame(Technique = all_text2) %>%  
    separate(Technique,sep = "\\(", c("Name_technique", "A") ,
             extra = "merge") %>%  
    separate(A,sep = "\\)( ,|:?) ", c("Kanji", "Info_technique") ,
             extra = "merge")  %>% 
    mutate(Akuma_name = akuma_name)
  
  df
})

techniques_info_df <- do.call(rbind, techniques_info)

saveRDS(techniques_info_df, file = "akuma_techniques.rds")
