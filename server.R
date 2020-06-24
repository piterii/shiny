library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(ggrepel)


part_categories <- read.csv('part_categories.csv')
sets <- read.csv('sets.csv')
parts <- read.csv('parts.csv')
themes <- read.csv('themes.csv')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  #plot1
  output$plot1 <- renderPlot({
    
    sets %>% select(year, num_parts) %>% group_by(year) %>% summarise(sum_parts = sum(num_parts)) ->parts_year
    parts_year %>% filter(year>input$range[1]) %>% filter(year<input$range[2]) ->parts_year
    
    ggplot(parts_year, aes(x = parts_year$year, y = parts_year$sum_parts))+
      geom_smooth(model = lm, linetype = 'dotted', color = 'blue', fill = 'blue', alpha =0.1)+
      geom_point(aes(color = 'Parts'), size = 3)+
      geom_line(aes( color = 'Parts'), size = 1)+
      ggtitle("Number of parts released each year ")+
      xlab("Year")+
      ylab("Number of parts")+
      theme(panel.background = element_rect("white"),
            axis.line = element_line(size = 0.5, linetype = 'solid', color = "black"),
            axis.text.x = element_text(angle = 30),
            plot.title = element_text(hjust = 0.5),
            legend.position = 'right',
            legend.title = element_blank())+
      scale_x_continuous(breaks = seq(1950,2015,by = 5), 
                         limits = c(input$range[1],input$range[2]))+
      scale_y_continuous(breaks = seq(0,200000, by = 20000))+
      scale_colour_manual(values=c("darkblue"))
  })
  
  #plot2
  output$plot2 <- renderPlot({
    # Average parts in sets released over time
    sets %>% filter(year>input$range[1]) %>% filter(year<input$range[2]) -> sets_plot2
    sets_plot2 %>% select(year, num_parts) %>% group_by(year) %>% summarise(avg_parts = mean(num_parts)) -> parts_year_avg
    sets_plot2 %>% select(year) %>% group_by(year) %>% count(year) -> sets_year_count
    
    ggplot()+
      geom_line(aes(x = parts_year_avg$year, y = parts_year_avg$avg_parts, color = 'Parts'), size = 2)+
      geom_line(aes(x = sets_year_count$year, y = sets_year_count$n, color = 'Sets'), size = 2)+
      ggtitle("Number of sets released and average amount of parts in them")+
      xlab("Year")+
      ylab("Number of sets")+
      theme(panel.background = element_rect("white"),
            axis.line = element_line(size = 0.5, linetype = 'solid', color = "black"),
            axis.text.x = element_text(angle = 30),
            plot.title = element_text(hjust = 0.5),
            legend.position = 'right',
            legend.title = element_blank())+
      scale_x_continuous(breaks = seq(1950,2015,by = 5), 
                         limits = c(input$range[1],input$range[2]))+
      scale_y_continuous(breaks = seq(0,1000, by = 100))+
      scale_colour_manual(values=c("lightblue","darkblue"))
  })
  
  
  #plot 3 
  output$plot3 <- renderPlot({
    # top 20 sets
    
    if (input$radio2=="All") {
      sets %>% filter(year>input$range2[1]) %>% filter(year<input$range2[2]) -> temp
      temp<- left_join(temp, themes, by = c("theme_id" = 'id')) 
      
      temp %>% select(name.x, num_parts) %>% arrange(desc(num_parts)) %>% distinct() %>% slice(0:20) %>% arrange(num_parts) -> top_sets
      
    }
    
    if (input$radio2!="All") {
      
      sets %>% filter(year>input$range2[1]) %>% filter(year<input$range2[2]) -> temp
      temp<- left_join(temp, themes, by = c("theme_id" = 'id')) 
      temp %>% filter(name.y == input$radio2)%>%
        select(name.x, num_parts) %>% arrange(desc(num_parts)) %>% distinct() %>% slice(0:20) %>% arrange(num_parts) -> top_sets
      
    }
    top_sets$name <- as.character(top_sets$name)
    
    
    
    ggplot(top_sets, aes(x=name.x, y=num_parts))+
      geom_bar(stat = "identity", fill = 'navyblue')+
      scale_x_discrete(limits = top_sets$name)+
      coord_flip()+
      ggtitle("Top 20 largest sets")+
      ylab("Number of parts")+
      geom_text(aes(label = num_parts), hjust = 1.5, color = 'white')+
      theme_minimal()+
      theme(legend.position = 'none',
            panel.grid = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5)
      )
  })
  
  #plot 4
  output$plot4 <- renderPlot({
    
    sets %>% filter(year>input$range2[1]) %>% filter(year<input$range2[2]) %>% select(theme_id) %>% group_by(theme_id) %>% count(theme_id) %>% arrange(desc(n)) -> popular_themes
    popular_themes <- left_join(popular_themes, themes, by = c("theme_id" = 'id'))
    popular_themes %>% group_by(name) %>% select(name, n) %>% summarise(n = sum(n)) %>% arrange(desc(n)) %>% slice(0:15) %>%arrange(n) -> popular_themes
    sets %>% filter(year>input$range2[1]) %>% filter(year<input$range2[2]) %>% select(theme_id, num_parts) %>% group_by(theme_id) %>% summarise(avg = mean(num_parts)) -> avg_parts_theme
    avg_parts_theme <- left_join(avg_parts_theme, themes %>%select(id, name) , by = c("theme_id" = 'id')) 
    avg_parts_theme %>% select(name, avg) ->avg_parts_theme
    avg_sum_themes <- left_join(popular_themes, avg_parts_theme, by = c('name' ='name'))
    avg_sum_themes %>% group_by(name) %>% summarise(n=max(n), avg = max(avg)) -> avg_sum_themes
    

    
    ggplot(avg_sum_themes, aes(x = avg, y = n))+
      geom_point(aes(color = 'Parts'), size = 5, color = 'darkblue')+
      geom_point(aes(color = 'Parts'), size = 3, color = 'white')+
      geom_label_repel(aes(label = name),
                       box.padding   = 0.35, 
                       point.padding = 0.5,
                       segment.color = 'grey50')+
      ggtitle("Complexity of themes")+
      xlab("Average number of elements in sets")+
      ylab("Number of sets")+
      theme(panel.background = element_rect("white"),
            axis.line = element_line(size = 0.5, linetype = 'solid', color = "black"),
            plot.title = element_text(hjust = 0.5),
            legend.position = 'none')+
      scale_colour_manual(values=c("lightblue","darkblue"))
  })
  

  
  output$plot5 <- renderPlot({ 
    
    sets_themes <- left_join(sets, themes, by = c("theme_id" = 'id'))
    sets %>% select(theme_id) %>% group_by(theme_id) %>% count(theme_id) %>% arrange(desc(n)) -> popular_themes
    
    popular_themes <- left_join(popular_themes, themes, by = c("theme_id" = 'id'))
    
    popular_themes %>% group_by(name) %>% select(name, n) %>% summarise(n = sum(n)) %>% arrange(desc(n)) %>% slice(0:10) %>%arrange(n) -> popular_themes
    
    g<- ggplot(data = sets_themes[sets_themes$name.y %in% popular_themes$name[5:10],])+
      geom_boxplot(aes(x = name.y, y = num_parts), color = 'navyblue', fill = 'lightblue', varwidth  = TRUE )+
      scale_y_continuous(limits = c(0,1500))+
      ylab("Number of parts")+
      xlab("Theme")+
      ggtitle("Sets sizes depending on themes")+
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5))
    

    
    if (input$radio3 == "yes") {

        g <- g+ geom_jitter(aes(x = name.y, y = num_parts), width = 0.2, alpha = 0.3)

    }
    
    g

})
  
  
})






