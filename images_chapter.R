
# libries -----------------------------------------------------------------

library(tidyverse)
library(rvest)


# Data --------------------------------------------------------------------

# Get urls from chapters

general_url <- "https://w63.1piecemanga.com/"

general_page <- general_url %>% 
  read_html()

chapter_number  <- general_page %>% 
  html_elements(css = ".comic-thumb-title") %>%
  html_text()

url_chapters <- general_page %>% 
  html_elements(css = "a") %>%
  html_attr('href') 

url_chaptersid <- url_chapters %>%  str_detect(., 'one-piece-chapter')

url_chapters <- url_chapters[url_chaptersid]


managa_url <-  tibble(Chapter =chapter_number,   url = url_chapters) %>% 
  mutate(Chapter = Chapter %>% 
           str_replace_all("One Piece, Chapter ", '') %>% 
           str_replace_all('[:alpha:]+', '') %>% 
           str_replace_all("(–|-|!|,|’|\\( )", '') %>% 
           str_replace_all("\\)", '') %>% 
           str_trim() %>% as.numeric()
           ) %>% 
  mutate(Chapter = ifelse(Chapter == 10465, 1046.5, Chapter))

saveRDS(managa_url, file = "manga_urls.rds")

# Getting jpeg manga ------------------------------------------------------

get_imagesulr <- function(id){
  chapter <- managa_url %>% 
    filter(Chapter == id) %>% 
    pull(url)
  
  page_chapter <-  chapter %>% 
    read_html()
  
  chapter_pages <- page_chapter %>% 
    html_elements('img') %>% 
    html_attr('src')
  
  tibble(Chapter_number = id, pages_url = chapter_pages)
}

images_manga <- 1:max(managa_url$Chapter) %>% map_df(get_imagesulr)
saveRDS(images_manga, file = "imagens_manga_url.rds")


# Tidying up images ulr ---------------------------------------------------

images_manga <- readRDS("imagens_manga_url.rds")

chapter_pages = images_manga %>%  
  group_by(Chapter_number) %>% 
  summarise(count_pages_chapter = n()) %>% 
  ungroup() 

# saving images -----------------------------------------------------------

images_manga = images_manga %>% 
  mutate(pages = 1:(images_manga %>% nrow())) %>% 
  group_by(Chapter_number) %>% 
  mutate(pages_rank = pages %>% rank()) 


getting_manga_page_jpg <-  function(chapter){
  df <- images_manga %>% filter(pages == chapter)
  url_manga <- df  %>% pull(pages_url)
  chap <- df %>% pull(Chapter_number)
  page <- df %>% pull(pages_rank)
  
  
  name_page = paste0("op_chap_", chap, "_pg_", page, ".jpg" )
  download.file(url_manga, paste0("C:/Users/erika.borelli_skyspe/Documents/Brasil/OnePiece/manga/",name_page), mode = 'wb')
  print(paste0("Saved: ", name_page))
}

ids = sample(59:18965, 3782)
ids[2216:3782] %>% map(getting_manga_page_jpg)

which(ids == 18605)
ids[84]
getting_manga_page_jpg(7209)
ids %>%  length()
