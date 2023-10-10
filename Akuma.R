
# library -----------------------------------------------------------------

library(tidyverse)
library(rvest)



# initial page ------------------------------------------------------------

link <-  "https://onepiece.fandom.com/wiki/Devil_Fruit"

devil_fruit_page <- link %>% read_html()


# Content -----------------------------------------------------------------

names <- devil_fruit_page %>%  
  html_elements("table") %>% 
  html_elements("div") %>% 
  html_elements("a") %>% 
  html_attr("title")

urls <- devil_fruit_page %>%  
  html_elements("table") %>% 
  html_elements("div") %>% 
  html_elements("a") %>% 
  html_attr("href")

all_info <-  data.frame(Names = names, Urls = urls, stringsAsFactors = FALSE) %>%  
  mutate( akuma_no_mi = Names %>% 
            str_to_lower() %>% 
            str_detect(., "no mi") %>% replace_na(FALSE)
          ) 

all_info_akuma <-  all_info %>%
  filter(akuma_no_mi == TRUE)


# Akuma no mi info --------------------------------------------------------

akuma_no_mi_info <- lapply(all_info_akuma$Urls, function(url){
  
  link_akuma <- paste0("https://onepiece.fandom.com", url)
  
  akuma_page <-  link_akuma %>% 
    read_html() %>% 
    html_elements(css = ".portable-infobox")
  
  
  akuma_name <- akuma_page %>% 
    html_elements("h2") %>% 
    html_text() %>%
    .[1]
  if ( length(akuma_name) == 0) { akuma_name = NA}
  
  
  english_name <- akuma_page %>% 
    html_elements(xpath = "//*[contains(@data-source, 'ename')]") %>% 
    html_elements("div") %>% 
    html_text() %>% .[1]
  if ( length(english_name) == 0) { english_name = NA}
  
  meaning <- akuma_page %>% 
    html_elements(xpath = "//*[contains(@data-source, 'meaning')]") %>% 
    html_elements("div") %>% 
    html_text() %>% .[1]
  if ( length(meaning) == 0) { meaning = NA}
  
  fruit_debut_manga <- akuma_page %>% 
    html_elements(xpath = "//*[contains(@data-source, 'fruit')]") %>% 
    html_elements("div") %>% 
    html_elements("a") %>% 
    html_text() %>% .[1]
  if ( length(fruit_debut_manga) == 0) { fruit_debut_manga = NA}
  
  fruit_debut_anime <- akuma_page %>% 
    html_elements(xpath = "//*[contains(@data-source, 'fruit')]") %>% 
    html_elements("div") %>% 
    html_elements("a") %>% 
    html_text() %>%  .[2]
  if ( length(fruit_debut_anime) == 0) { fruit_debut_anime = NA}
  
  usage_debut_manga <- akuma_page %>% 
    html_elements(xpath = "//*[contains(@data-source, 'first')]") %>% 
    html_elements("div") %>% 
    html_elements("a") %>% 
    html_text() %>% .[1]
  if ( length(usage_debut_manga) == 0) { usage_debut_manga = NA}
  
  usage_debut_anime <- akuma_page %>% 
    html_elements(xpath = "//*[contains(@data-source, 'first')]") %>% 
    html_elements("div") %>% 
    html_elements("a") %>% 
    html_text() %>% .[2]
  if ( length(usage_debut_anime) == 0) { usage_debut_anime = NA}
  
  type <- akuma_page %>% 
    html_elements(xpath = "//*[contains(@data-source, 'type')]") %>% 
    html_elements("div") %>% 
    html_text()
  if ( length(type) == 0) { type = NA}
  
  current_user <- akuma_page %>% 
    html_elements(xpath = "//*[contains(@data-source, 'user')]") %>% 
    html_elements("div") %>% 
    html_text()
  if ( length(current_user) == 0) { current_user = NA}
  
  previous_user <- akuma_page %>% 
    html_elements(xpath = "//*[contains(@data-source, 'previous')]") %>% 
    html_elements("div") %>% 
    html_text()
  if ( length(previous_user) == 0) { previous_user = NA}
  
  info_akuma_all <- data.frame(Akuma_name = akuma_name, English_name = english_name,
                               Akuma_meaning = meaning, Debut_manga = fruit_debut_manga, 
                               Debut_anime = fruit_debut_anime, Usage_manga = usage_debut_manga,
                               Usage_anime = usage_debut_anime, Akuma_type = type,
                               Current_user = current_user, Previous_user = previous_user,
                               Url = url)
  info_akuma_all
  
})

akuma_no_mi_info_all <- do.call(rbind,akuma_no_mi_info)


saveRDS(akuma_no_mi_info_all, file = "akuma_no_mi.rds")



# image -------------------------------------------------------------------


akuma_image <- lapply(akuma_no_mi$Url, function(url){
  
  link_akuma <- paste0("https://onepiece.fandom.com", url)
  
  page  <-  link_akuma %>% 
    read_html()
  
  image <- page %>% 
    html_elements("img.pi-image-thumbnail") %>% 
    html_attr("src")
  if ( length(image) == 0) { image = NA}
  
  akuma_page <-  link_akuma %>% 
    read_html() %>% 
    html_elements(css = ".portable-infobox")
  
  
  akuma_name <- akuma_page %>% 
    html_elements("h2") %>% 
    html_text() %>%
    .[1]
  if ( length(akuma_name) == 0) { akuma_name = NA}
  
  data.frame(Akuma_name = akuma_name, Image = image)
  
})



akuma_image_all <- do.call(rbind,akuma_image)
akuma_no_mi <- akuma_no_mi %>% left_join(akuma_image_all)
akuma_no_mi <- akuma_no_mi %>%  
  mutate( Image = Image %>% 
            str_replace_all(., "/revision", " ")
  
) %>% 
  separate(Image,sep = " ", c("image_url", "discart")) 
  
akuma_no_mi <- akuma_no_mi %>% select(-discart)


saveRDS(akuma_no_mi, file = "akuma_no_mi.rds")

akuma_no_mi <- read_rds("akuma_no_mi.rds")
