
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rvest)


# Principal page ----------------------------------------------------------


link <-  'https://onepiece.fandom.com/wiki/Story_Arcs'

page_arcs <- link %>% 
  read_html()


all_tables <- page_arcs %>% 
  html_table() 



saga_lista = function(id){
sea_of_survival <- all_tables %>% .[[id]]
names(sea_of_survival) <- sea_of_survival[1,]
names(sea_of_survival)[1] <- 'type'
sea_of_survival <-  sea_of_survival %>% 
  filter(row_number() != 1)  %>% 
  mutate(type = ifelse(type == '', 'Arc', type)
         ) %>% na.exclude()



# Cleaning data -----------------------------------------------------------

try1 <- sea_of_survival %>% 
  pivot_longer(!type, names_to = 'Saga', values_to = 'values') %>%
  pivot_wider(names_from = 'type', values_from = 'values') 


organizing <- function(saga,column_number){
  values = try1 %>%
    filter(Saga == saga) %>%
    select(all_of(column_number)) %>%
    pull() %>% unlist()
  column_name = names(try1)[column_number]
  df = tibble(Values = values) 
  names(df)[1] = column_name
  df
}

organizing2 <-  function(saga){
  list_all = map2(saga, 2:5, organizing )
  df = do.call(cbind, list_all) %>% 
    mutate(Saga = saga) 
  df
}

sea_of_survival2 <- map_df(try1 %>% pull(Saga), organizing2) %>% 
  filter(Arc != '') %>% 
  mutate(Episode_type = case_when(
    Chapters %>%  str_detect('Special') ~ 'Special', 
    Chapters %>%  str_detect('Filler arc') ~ 'Filler',
    Chapters %>% str_detect('(covers)') ~ 'Covers',
    .default = 'Regular')
  ) %>% 
  mutate_at(c('Chapters', 'Volumes'), ~ str_replace_all(.,'[:alpha:]', '') %>% 
              str_replace("\\(", '') %>% 
              str_replace("\\)", '') %>% 
           str_trim()
         ) %>% 
  separate_longer_delim(Episodes, delim = ",") %>% 
  mutate(Episodes = Episodes %>% str_trim()) %>%  
    mutate(Episode_lenght = Chapters %>% str_length()) %>% 
    mutate(Chapters = ifelse(Episode_lenght == 0, NA, Chapters),
           Volumes = ifelse(Episode_lenght == 0, NA, Volumes)) %>% 
    separate(Chapters, c("Chap_begin", "Chap_end")) %>% 
    separate(Volumes, c('Vol_begin', 'Vol_end')) %>% 
    separate(Episodes, c("Ep_begin", "Ep_end")) 

sea_of_survival2
}

sea <- map_df(1:2, saga_lista)

saveRDS(sea, file = "sagas_chapter.rds")
