function(input,output){
  
  output$plot_engagement <- renderPlotly({
    you_agg <- youtube_clean %>% 
      group_by(category_id) %>% 
      summarise(mean_likes = round(mean(likes),3),
                mean_comment = round(mean(comment_count),3),
                mean_dislikes = round(mean(dislikes),3)) %>% 
      pivot_longer(cols = c(mean_likes, mean_comment, mean_dislikes))
    
    plot_agg <- you_agg %>% 
      group_by(category_id) %>% 
      mutate(value = value / sum(value)) %>% 
      ungroup() %>% 
      arrange(desc(name), value) %>% 
      mutate(category_id = factor(category_id, levels = unique(category_id))) %>% 
      ggplot(aes(x = value, 
                 y = category_id, 
                 fill = name,
                 text = glue("Probability: {round(value,4)}
                             Engagement: {name}"))) +
      geom_col() +
      labs(x = NULL,
           y = NULL,
           title = "Videos Engagement per Category") +
      scale_fill_manual(values = c("#343aeb", "#c12fed", "#d60f5f")) +
      theme(legend.position = "none") +
      theme_lbb3
    
    ggplotly(plot_agg, tooltip = "text")
  }
  )
  
  output$plot_topc <- renderPlotly({
    top_channel <- youtube_clean %>% 
      group_by(channel_title) %>% 
      summarise(sum_views = sum(views)) %>% 
      arrange(desc(sum_views)) %>% 
      head(20)
    
    inputcat <- as.integer(input$numbercat)
    ord_channel <- top_channel[1:inputcat,]
    
    plot_top <- ord_channel %>% 
      ggplot(aes(x = sum_views/10000,
                 y = reorder(channel_title, sum_views),
                 text = glue("Channel Name: {channel_title}
                             Total Views: {sum_views/10000}"))) +
      geom_col(fill = "#3ea6f0") +
      labs(x = NULL,
           y = NULL,
           title = "Top Channel by Total Views (per 10,000)") +
      theme_lbb3
    
    ggplotly(plot_top, tooltip = "text")
  })
  
  output$plot_perform <- renderPlotly({
    pos_index <- youtube_clean %>% 
      mutate(positive_index = (likes - dislikes)/views) %>%
      filter(year_trending == input$tahun) %>% 
      group_by(publish_wday) %>% 
      summarise(mean_positive_index = mean(positive_index))
    
    plot_pos <- pos_index %>% 
      ggplot(aes(x = publish_wday, 
                 y = mean_positive_index,
                 text = glue("Publish Weekday: {publish_wday}
                         Positive Index: {round(mean_positive_index,4)}"))) + 
      geom_col(fill = "#c05ed6") +
      labs(x = NULL,
           y = NULL,
           title = "Positive Index per Weekday") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_lbb3
    
    ggplotly(plot_pos, tooltip = "text")
  })
  
  output$plot_corr <- renderPlotly({
    pilih <- input$numvar %>% 
      str_to_lower() %>% 
      str_replace_all(pattern = " ", replacement = "_")
    
    plot_dist <- youtube_clean %>% 
      filter(views <= 10000000) %>% 
      mutate(views = views/1000,
             likes = likes/1000,
             dislikes = dislikes/1000,
             comment_count = comment_count/1000) %>% 
      ggplot(aes_string(x = "views",
                        y = pilih)) +
      geom_jitter(aes(col = category_id,
                      text = glue("{str_to_upper(category_id)}
                                  Views: {views}
                                  Y-axis variable: {input$numvar}"))) +
      geom_smooth() +
      labs(y = glue("{input$numvar}"),
           x = "Views",
           title = "Engagement Correlation Plot (per 1,000)") +
      scale_color_brewer(palette = "Spectral") +
      theme_lbb3 +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = "none")
    ggplotly(plot_dist, tooltip = "text")
  })
  
  output$data_youtube <- renderDataTable({
    oldName <- colnames(youtube)
    newName <- youtube %>% 
      colnames() %>% 
      str_replace_all(pattern = "_", replacement = " ") %>% 
      str_to_title()
    
    colnames(youtube)[which(names(youtube) == oldName)] <- newName
    
    datatable(data = youtube,
              options = list(scrollX = T))
    
  })
}