library(magick)
library(gtools)
l <- list.files("plots",".png$", full.names = T)
l <- mixedsort(l)
imgs <- l

split(imgs, (seq_along(imgs)-1)%/%6) %>%
  lapply(function(x){
    lapply(x, image_read) %>% 
      {Reduce(c, .)} %>% 
      image_montage() %>% 
      image_trim()
  }) %>% 
  {Reduce(c, .)} %>% 
  image_append(stack = TRUE) %>% 
  image_convert("png") %>% 
  image_write("plots/0_collage.png", quality = 100)
