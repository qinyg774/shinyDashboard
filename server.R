library(DT)
library(shiny)
library(googleVis)
library(ggplot2)
library(dplyr)
library(tidyr)

shinyServer(function(input, output, session) {
  
  output$size1 <- renderPlot(
    dt %>% 
      filter(status == "Alive") %>%
      filter(spc_common %in% input$species | input$species == "All") %>%
      filter(diameter <= quantile(diameter, .9)) %>%
      ggplot(aes(x = diameter)) + 
      geom_density(aes(color = borough)) + 
      ggtitle("Tree Size Distribution (Alive Only)") +
      theme(plot.title = element_text(size = rel(2), hjust = 0.5))
  )

    
  output$species2 <- renderGvis(
    dt %>%
      filter(!(spc_common == "")) %>%
      filter(borough == input$boro | input$boro == "All") %>%
      filter(status == "Alive" | input$alive_only == FALSE) %>%
      group_by(spc_common) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      top_n(input$top) %>%
      gvisBarChart(options = list(title = " Top Species by Borough", 
                                width = "1000px", 
                                height = "500px")) 
  )

  
  health_heatmap <- reactive({
    temp <- dt %>% 
      filter(status == "Alive" & health != "" & spc_common != "") %>%
      filter(curb_loc == input$site | input$site == "All") %>%
      group_by(spc_common) %>%
      summarise(perc_poor = sum(ifelse(health == "Poor", 1, 0))/n(),
                perc_poor_fair = sum(ifelse(health == "Poor"|health == "Fair", 1, 0))/n(),
                perc_root_stone = sum(ifelse(root_stone == "Yes", 1, 0))/n(),
                perc_root_grate = sum(ifelse(root_grate == "Yes", 1, 0))/n(),
                perc_root_other = sum(ifelse(root_other == "Yes", 1, 0))/n(),
                perc_trunk_wire = sum(ifelse(trunk_wire == "Yes", 1, 0))/n(),
                perc_trnk_light = sum(ifelse(trnk_light == "Yes", 1, 0))/n(),
                perc_trnk_other = sum(ifelse(trnk_other == "Yes", 1, 0))/n(),
                perc_brch_light = sum(ifelse(brch_light == "Yes", 1, 0))/n(),
                perc_brch_shoe = sum(ifelse(brch_shoe == "Yes", 1, 0))/n(),
                perc_brch_other = sum(ifelse(brch_other == "Yes", 1, 0))/n()
      ) %>% gather("health_indicator", "value", 2:12)
    
    lvls <- temp %>% 
      filter(health_indicator == input$sort_hi) %>% arrange(desc(value)) %>% pull(1)
    
    temp$spc_common <- as.factor(temp$spc_common)
    
    for (lvl in lvls) {temp$spc_common <- relevel(temp$spc_common, lvl)}
    
    temp
    
    })
  
  
  output$health_heatmap <- renderPlot({
    health_heatmap() %>%
      filter(spc_common %in% input$species_multi) %>%
      ggplot(aes(x = reorder(health_indicator, match(health_indicator, health_indicators)), y = spc_common)) + 
      geom_tile(aes(fill = value))  + 
      scale_fill_gradient(low = "green", high = "red") +
      theme(axis.text.x = element_text(angle = 90, size = rel(1.5)),
            axis.title.x = element_blank(),
            axis.text.y = element_text(angle = 45, size = rel(1.5)),
            axis.title.y = element_blank(),
            legend.position = "none"
      )
  })

  output$table <-renderDataTable({
    datatable(dt, rownames = FALSE) %>% formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })

})