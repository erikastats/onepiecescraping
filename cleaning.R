
# Libraries ---------------------------------------------------------------

library(tidyverse)


# Data --------------------------------------------------------------------

akuma_techniques <- readRDS("akuma_techniques.rds")
akuma_no_mi <- readRDS("akuma_no_mi.rds")

# Cleaning ----------------------------------------------------------------


akuma_techniques %>% 
  mutate(Name_technique = Name_technique %>% 
           str_replace_all("\'", "") %>% 
           str_replace_all('\"', '') %>% 
          str_trim(),
         Info_technique = Info_technique %>% 
           str_replace_all("[:digit:]\\]", '')
           ) %>% 
  unique() %>% view()


akuma_no_mi_unique <- akuma_no_mi %>% 
  distinct(Akuma_name, .keep_all =  TRUE) %>%
  mutate(Akuma_type = Akuma_type %>% 
           str_replace_all("[:digit:]", '') %>% 
           str_replace_all("\\[", '') %>% 
           str_replace_all("\\]", ''),
         Akuma_type_general = case_when(
           Akuma_type %>% str_detect("Paramecia") ~ 'Paramecia', 
           Akuma_type %>% str_detect("Zoan") ~ 'Zoan',
           Akuma_type %>% str_detect("Logia") ~ 'Logia',
           Akuma_type %>% str_detect("Unknown") ~ 'Unknown'
         ),
         Akuma_sutype = case_when(
           Akuma_type %>%  str_detect("Mythical") ~ 'Mythical',
           Akuma_type %>%  str_detect("Special") ~ 'Special',
           Akuma_type %>%  str_detect("Ancient") ~ 'Ancient',
           Akuma_type %>%  str_detect("Ancient") ~ 'Ancient',
         )
         
         )
  
