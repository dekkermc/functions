


theme_isar<- function(){ 
  font <- "Helvetica"    #assign font family up front
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 20,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 14),               #font size
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 12,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        face = 'bold',
        family = font,            #font family
        size = 14),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 12),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10))
      
      
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    ) 
  
}



ggplot(mtcars, aes(gear_fac, fill = gear_fac)) +
  geom_bar(stat = "count") +
  theme_isar() 



ggplot(mtcars, aes(x = disp, y = hp, color = qsec)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "#072C52",
                      high = "#148AFF")



  scale_fill_manual(values = c("red", "blue", "grey", "orange", "black", "green")) +
    
    
  scale_fill  
    

isar_colors_discrete <- c("#0B5394", "#E66831", "#ffda3f", "#996633", "#1a9850", "#2b8cbe", "#e5f5ff")
  
  
ggplot(mtcars, aes(factor(carb), fill = factor(carb))) +
  geom_bar(stat = "count") +
  scale_fill_manual(values = c("#0B5394", "#E66831", "#ffda3f", "#996633", "#1a9850", "#2b8cbe", "#e5f5ff")) +
  theme_isar()
  

ggplot(mtcars, aes(qsec)) +
  geom_density(color = "red",
               fill = "red",
               alpha = .5) +
  labs(caption = "<img src='https://upload.wikimedia.org/wikipedia/commons/f/f7/Stack_Overflow_logo.png' width='100'/>") +
  theme(plot.caption = element_markdown())
  
  
  