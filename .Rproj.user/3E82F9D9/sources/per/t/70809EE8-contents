source("global.R")


options(shiny.maxRequestSize=100*1024^2)

server <-  function(input, output, session){
  
  values <- reactiveValues(over_time = NULL, 
                           related = NULL,
                           dma = NULL, 
                           region = NULL, 
                           product = NULL,
                           time = NULL,
                           dates = NULL,
                           google_search = NULL)
  
  output$google_pro <- renderUI({
    selectInput("product", "Google product", choices = c("web", "news", "images", "froogle", "youtube"), selected = "web")
  })
  
  output$time <- renderUI({
    selectInput("time", "Time Period", choices = c("Past 12 months" = "today 12-m",
                                                   "Last hour" = "now 1-H", 
                                                   "Last four hours" = "now 4-H", 
                                                   "Last day" = "now 1-d", 
                                                   "Last seven days" = "now 7-d",
                                                   "Past 30 days" = "today 1-m",
                                                   "Past 90 days" = "today 3-m",
                                                   "Last five years" = "today+5-y",
                                                   "2004 to present" = "all", 
                                                   "------------------",
                                                   "Specific two dates" = "specific"), selected = "today 12-m", multiple = F)
  })
  
  output$spe_date <- renderUI({
    req(input$time)
    if(input$time == "specific")
      dateRangeInput(
        inputId = "dates",
        label = "Date range",
        start = "2018-04-15",
        end = Sys.Date())
  })

  times <- reactive({
    req(input$time)
    if(input$time == "specific"){
      times <- paste0(input$dates, collapse  = " ")
    }else{
      times <- input$time
    }
    return(times)
  })
  
  output$title_panel <- renderText({
    paste0("Google Trend: ", input$google_search)
  })

  output$bton <- renderUI({
    actionButton("submit", "Add for searching")
  })
  

  
  observe({
    if(is.null(values$google_search) & is.null(values$time) & is.null(values$product)){
      values$google_search <- c("Amazon","Walmart","Best Buy","Target")
      values$time <- "today 12-m"
      values$product <- "web"
    }
  })
  
  observeEvent(input$submit,{
    values$google_search <- input$google_search
    values$time <- times()
    values$product <- input$product
  })
  
  data_out <- reactive({
    req(values$google_search, values$time, values$product)
    
    text <- values$google_search
    text <- c(unlist(strsplit(text, ",")))
    text <- text[!text %in% c("", " ", "   ")]
    text <- trimws(text)
 
    result <- gtrends(text, geo = "US", time = values$time, gprop = values$product)
    
    values$related_topics_out <- result$related_topics
    values$related_queries_out <- result$related_queries
    values$interest_by_dma_out <- result$interest_by_dma
    values$interest_by_city_out <- result$interest_by_city
    
    interest_over_time <- result$interest_over_time
    interest_over_time <- interest_over_time[!duplicated(interest_over_time[,c("date","keyword")]),]
    interest_over_time$group <- gsub( '\\s+', '',interest_over_time$keyword)
    interest_over_time$hours <- get_time(interest_over_time$date)
    interest_over_time$wdate <- wday(interest_over_time$date, label = T)
    interest_over_time$dates <- ymd(interest_over_time$date)
    interest_over_time$weeks <- nth_week(interest_over_time$date)
    interest_over_time$month <- month(interest_over_time$date, label = T)
    interest_over_time$year <- year(interest_over_time$date)
    
    interest_by_region <- result$interest_by_region
    if(nrow(interest_by_region) >= 1){
      interest_by_region <- interest_by_region[!duplicated(interest_by_region[,c("location","keyword")]),]
      interest_by_region$group <- gsub( '\\s+', '',interest_by_region$keyword)
      interest_by_region <- interest_by_region[ interest_by_region$location %in% c(states$ID),]
      interest_by_region <- interest_by_region[order(interest_by_region$location,match(interest_by_region$location,states$ID)),]
      interest_by_region <- data.frame(do.call("rbind",lapply(split(interest_by_region, interest_by_region$location), function(i) {
        i[,"hits_1"] = max(i[,"hits"])
        i[,"per"] = round((i[,"hits"]/sum(i[,"hits"]))*100,1)
        i
      })))
    }
    interest_by_dma <- result$interest_by_dma
    interest_by_dma <- interest_by_dma[!duplicated(interest_by_dma[,c("location","keyword")]),]
    interest_by_dma$group <- gsub( '\\s+', '',interest_by_dma$keyword)
    
    related_queries <- result$related_queries
    related_queries$group <- gsub( '\\s+', '',related_queries$keyword)
    find <- unique(tolower(related_queries$keyword))
    replace <- gsub('\\s+',"",find )
    
    for(i in 1:length(find)){
      for(j in 1:nrow(related_queries)){
        related_queries[j,"value"] <-  gsub(find[[i]], replace[[i]], related_queries[j,"value"] )
        related_queries
      }
    }

    out <- list(interest_over_time, interest_by_region, interest_by_dma, related_queries)
    return(out)
  })
  
  observe({
    values$over_time <- data_out()[[1]]
    values$region <- data_out()[[2]]
    values$dma <- data_out()[[3]]
    values$related <- data_out()[[4]]
  })
  
#================================================
  
  
#================================================
  

observe({
  req(values$region)
  interest_by_region <- values$region

  if(nrow(interest_by_region) >= 1){
    ldf <- split(interest_by_region, interest_by_region$keyword)
    ldf <- lapply(seq_along(ldf), function(i, color, dt){
      dt <- dt[[i]]
      color_1 <- color[[i]]
      if(isTRUE(color_1 == "YlOrRd")){
        palette_1 = brewer.pal(4, color_1)
      }else{
        palette_1 = brewer.pal(8, color_1)
      }
      pal <- colorNumeric(palette=palette_1,domain=dt$hits)
      dt[,"pallete"] <- color[[i]]
      dt[,"colors"] <- pal(dt$hits)
      dt[[i]] <- dt
    },color = list_color[1:length(ldf)], dt = ldf)
    single_color_1 <- single_color[1:length(ldf)]
    color_table <- data.frame(do.call("rbind", ldf))
    color_table <- color_table[color_table$hits == color_table$hits_1,]
    color_table <- color_table[!duplicated(color_table$location),]
    color_table <- color_table[order(color_table$location,match(color_table$location,states$ID)),]
    pop <- c()
    final <- c()
    for(i in 1:length(unique(interest_by_region$keyword))){
      text <- sprintf("<strong style='font-size:16px;text-align:left;color: %s;'>%s</strong>", single_color_1[[i]], interest_by_region$keyword[[i]])
      value <- sprintf("<strong style='font-size:14px; padding-left:40px;text-align:left;color: %s;'>%5.2f%%</strong>", single_color_1[[i]],
                       ldf[[i]]$per[match(states$ID, ldf[[i]]$location)])
      pop[[i]] <- paste0(text,"  ",value)
    }

    # df_args <- c(pop, sep="<br>")
    # do.call(paste, df_args)

    final  <- Reduce(function(...) paste(..., sep = "<br>"),pop)

    for(i in 1:length(final)){
      final[[i]] <- paste0("<strong style='font-size:20px;color: #084594'> ",states$ID[[i]], "</strong>","<br>","<br>",final[[i]],"<br>","<br>",
                           "<span style='font-size:12px;color: #084594'> Percentages calculated out of searches <br> for all ",length(ldf)," terms in " ,states$ID[[i]]," </span>")
    }

    output$map <- renderLeaflet({
      # w$show()
      # Sys.sleep(1)
      # w$hide()
     
      fig <- leaflet(states) %>%
        addTiles() %>%
        addProviderTiles("OpenStreetMap.Mapnik") %>% 
        #addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
        addPolygons(
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    fillColor = color_table$colors,
                    popup = final,
                    group = "states") %>% addResetMapButton() %>%
        addLabelOnlyMarkers(lng = ~X, lat = ~Y, label = ~ID,
                            labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE,
                                                        
                                                        offset = c(0,0),
                                                        style = list(
                                                          "color" = "gray", 
                                                          "font-family" = "serif",
                                                          "font-style" = "normal",
                                                          "box-shadow" = "1px 1px rgba(0,0,0,0.25)",
                                                          "font-size" = "12px",
                                                          "border-color" = "rgba(0,0,0,0.5)",
                                                          "padding" = "2px" 
                                                        ))) %>%
        # these markers will be "invisible" on the map:
        addMarkers(
          data = states, lng = ~X, lat = ~Y, label = ~ID,
          group = 'cities', # this is the group to use in addSearchFeatures()
          # make custom icon that is so small you can't see it:
          icon = makeIcon( 
            iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
            iconWidth = 1, iconHeight = 1
          )
        ) %>%
      addSearchFeatures(
        targetGroups = "cities", # group should match addMarkers() group
        options = searchFeaturesOptions(
          zoom=7, openPopup = TRUE, firstTipSubmit = TRUE, 
          autoCollapse = TRUE, hideMarkerOnCollapse = TRUE,
        )) %>% 
        addControl("<P><B>Hint!</B> Search for ...<br/><ul><li>New York</li>
          <li>Boston</li><li>Hartford</li><li>Philadelphia</li><li>Pittsburgh</li>
          <li>Providence</li></ul></P>",className = "fieldset {border: 0;}", position = 'bottomright'
        )
      values$plot_region <- fig
    })
  }
})

  
observe({
  req(values$over_time)
  df <- values$over_time
  df1 <- df %>% group_by(keyword) %>% summarise(mean(hits,na.rm = TRUE))
  names(df1) <- c("keyword","hits")
  
  p <- df  %>% plotly::plot_ly(
    x = ~date,
    y = ~hits,
    source = "event",
    type = 'scatter',
    mode = 'lines',
    color = ~ keyword,
    label = ~date,
    texttemplate = '<br>%{keyword}</b>%{hits}', 
    showlegend = T)
  p <- p %>% plotly::layout(hovermode = "x unified",
                    hoverlabel=list(bgcolor="white"),
                    legend = list(orientation = "h",   # show entries horizontally
                                  xanchor = "center",  # use center of legend as anchor
                                  x = 0.5, y = max(df$hits)),
                    font = list(color = 'gray',family = 'sans serif',size = 12),
                    hoverlabel = list(font=list(size=13)),
                    title = list(text = paste0(unique(df$keyword), collapse = " vs "),
                                 font = list(
                                   family = 'sans serif',
                                   size = 20,
                                   color = "black")),
                    margin =list(l=50,r=20,b=100,t=100,pad=10),
                    xaxis = list(
                      tickfont = list(
                        family = 'sans serif',
                        size = 17,
                        color = "black"
                      ),
                      titlefont = list(
                        family = 'sans serif',
                        size = 17,
                        color = "black"
                      ),
                      title = "Date",
                      zeroline = FALSE,
                      tickmode = "array",
                      color = "black"
                    ),
                    yaxis = list(
                      tickfont = list(
                        family = 'sans serif',
                        size = 17,
                        color = "black"),
                      titlefont = list(
                        family = 'sans serif',
                        size = 17,
                        color = "black"),
                      title = "Search Hits",
                      zeroline = FALSE,
                      color = "black"))
  
  p1 <- df1 %>% plotly::plot_ly(
    x=~keyword,
    y=~hits,
    color= ~ keyword,
    texttemplate = '<br>%{keyword}</b>%{hits}',
    showlegend = F,shareY = TRUE)
  p1 <- p1 %>% plotly::layout(
    font = list(color = 'gray',family = 'sans serif',size = 12),
    hoverlabel = list(font=list(size=13)),
    title = list(text = paste0(unique(df$keyword), collapse = " vs "),
                 font = list(
                   family = 'sans serif',
                   size = 20,
                   color = "black")),
    margin =list(l=50,r=20,b=100,t=100,pad=10),
    xaxis = list(
      showticklabels = FALSE,
      tickfont = list(
        family = 'sans serif',
        size = 17,
        color = "black"
      ),
      titlefont = list(
        family = 'sans serif',
        size = 17,
        color = "black"
      ),
      title = "Average",
      zeroline = FALSE,
      tickmode = "array",
      color = "black"
    ),
    yaxis = list(
      tickfont = list(
        family = 'sans serif',
        size = 17,
        color = "black"),
      titlefont = list(
        family = 'sans serif',
        size = 17,
        color = "black"),
      title = "Search Hits",
      zeroline = FALSE,
      color = "black"))
  
  fig <- subplot(p1,p,nrows = 1, widths = c(0.2, 0.8),shareY = TRUE, titleX = TRUE)
  values$plot_over_time <- fig
  
  output$plot_out <- renderPlotly({
    values$plot_over_time
  })
  
})

observe({
  req(values$related)
  dt <- values$related
  if(nrow(dt)>=1){
    topqueries <- dt %>%
      filter(related_queries == 'top') %>%
      unnest_tokens(bigram, value, token = 'ngrams', n = 3) %>%
      separate(bigram, c("word1", "word2"), sep = " ") %>%
      filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
      count(word1, word2, sort = TRUE) %>%
      filter(!is.na(word1), !is.na(word2)) %>%
      graph_from_data_frame()
    
    # Find group membership
    wt <- cluster_walktrap(topqueries, steps = 6)
    members <- membership(wt)
    
    # Convert igraph to list for networkD3
    sj_list <- igraph_to_networkD3(topqueries, group = members)
    fig <- networkD3::forceNetwork(Links = sj_list$links, 
                                   Nodes = sj_list$nodes, 
                                   Source = 'source', 
                                   Target = 'target', 
                                   NodeID = 'name', 
                                   Group = 'group',
                                   Value = "value",
                                   #linkDistance = 200,
                                   #linkColour = 'source',
                                   height = 650, 
                                   width = 700, 
                                   fontSize = 20, 
                                   linkDistance = networkD3::JS("function(d) { return 200*d.value; }"), 
                                   linkWidth = networkD3::JS("function(d) { return d.value/5; }"),
                                   opacity = 1, zoom = TRUE, opacityNoHover = 0.9)
    values$plot_network <- fig
    
    output$forceRadius <- renderForceNetwork({
      values$plot_network 
    })
  }

})

output$download_network <- downloadHandler(
  filename = function() {'google_trend_network.html'},
  content = function(file) {
    
    htmlwidgets::saveWidget(as_widget(values$plot_network), file)
    
  }
)

output$download_region <- downloadHandler(
  filename = function() {'google_trend_region.html'},
  content = function(file) {
    htmlwidgets::saveWidget(as_widget(values$plot_region), file)
  }
)
  
  output$plot_out_ui <- renderUI({
    req(nrow(values$over_time)>=1)
    div(
      style = "position: relative",
      box(
        id = "hamd_mmrm",
        width = NULL,
        height = 590,
        title = "Interest over time",
          div(style="max-height:600px;position: relative",plotlyOutput("plot_out", width = "100%",height= "500px")),
          div(
            style = "position: absolute; right: 1em; top: 1em;",
            tags$style(".glyphicon{display: none;}"),
            dropdown(
              downloadButton('download_mmrm', 'Download Graph'),
              size = "xs",
              icon = icon("download"),
              up = FALSE,
              right = TRUE
            )
          )
        )
      )
  })
  
  output$plot_region <- renderUI({
    req(nrow(values$region)>=1,nrow(values$related)>=1)
    div(
      style = "position: relative",
      box(
        id = "region",
        width = 7,
        height = 590,
        title = "Compared breakdown by Region",
        div(style="max-height:600px;position: relative",leafletOutput("map", width = "100%",height= "500px")),
        div(
          style = "position: absolute; right: 1em; top: 1em;",
          tags$style(".glyphicon{display: none;}"),
          dropdown(
            downloadButton('download_region', 'Download Graph'),
            size = "xs",
            icon = icon("download"),
            up = FALSE,
            right = TRUE
          )
        )
      ),
      box(
        id = "network",
        width = 5,
        height = 590,
        title = "Relative Network",
        div(style="max-height:600px;position: relative",forceNetworkOutput("forceRadius", width = "100%",height= "500px")),
        div(
          style = "position: absolute; right: 1em; top: 1em;",
          tags$style(".glyphicon{display: none;}"),
          dropdown(
            downloadButton('download_network', 'Download Graph'),
            size = "xs",
            icon = icon("download"),
            up = FALSE,
            right = TRUE
          )
        )
      )
    )
  })
  
  output$download_mmrm <- downloadHandler(
    filename = function() {'google_trend_plot.html'},
    content = function(file) {
      
      htmlwidgets::saveWidget(as_widget(values$plot_over_time), file)
      
    }
  )
  
  
  output$boxes <- renderUI({
    req(nrow(values$over_time)>=1)
    
    dt <- values$over_time
    rank <- dt %>% group_by(keyword) %>% summarise(mean(hits,na.rm = TRUE))
    names(rank) <- c("keyword","hits") 
    rank <- rank[order(rank$hits, decreasing = TRUE),]
    rank$rank_out <- c(1:nrow(rank))

     lapply(1:length(unique(dt$group)), function(a, name, group, dt, rank) {
       df1 <- dt[dt$group == group[[a]],]
       rank <- rank[rank$keyword == name[[a]],]
 
      output[[name[[a]]]] <- renderText(
        HTML(paste0("<span style='font-size:30px;'>",name[[a]],"</span><span style='font-size:18px;'> </span>"))
      )
      output[[paste("1",name[[a]])]] <- renderText(
        HTML(paste0("<span style='font-size:16px;color:white;'>",min(df1$date), " to ", max(df1$date),"</span><span style='font-size:16px;'> </span>"))
      )
      output[[paste("2",name[[a]])]] <- renderText(
        HTML(paste0("<span style='font-size:18px;color:white;'>","Max: ","</span><span style='font-size:16px;color:#ff7f0e;'>",max(df1$hits,na.rm = TRUE),"</span> hits on <span style='font-size:16px;color:white;'>",df1$date[df1$hits == max(df1$hits,na.rm = TRUE)][1],"</span>"))
      )
      output[[paste("3",name[[a]])]] <- renderText(
        HTML(paste0("<span style='font-size:18px;color:white;'>","Average: ","</span><span style='font-size:16px;color:#ff7f0e;'>",round(mean(df1$hits,na.rm = TRUE),0),"</span> hits <span style='font-size:16px;color:white;'>"))
      )
      
      output[[paste("5",name[[a]])]] <- renderText(
        HTML(paste0("<span style='font-size:18px;color:white;'>","Rank: ","</span><span style='font-size:16px;color:#ff7f0e;'>",rank$rank_out," </span>"))
      )
      
      # output[[paste("4",name[[a]])]] <- renderPlotly({
      #   p
      # })
      
      box(
        width = 3,
        title = htmlOutput(paste("1",name[[a]]),style = "text-align:left;"),
        splitLayout(cellWidths = c("70%", "30%"),
                    tagList(
                    htmlOutput(name[[a]],style = "text-align:center;color:#32907c"),
                    htmlOutput(paste("2",name[[a]]),style = "text-align:left;padding-left:10px;"),
                    htmlOutput(paste("3",name[[a]]),style = "text-align:left;padding-left:10px;"),
                    htmlOutput(paste("5",name[[a]]),style = "text-align:left;padding-left:10px;"),br()
                    ),
                    div(style ="text-align:right;height:150px;", 
                        tags$style("#dropdown-menu-mydropdown2 {background-color: transparent;border-color: white;min-width:100px}.btn-custom {background-color: transparent; color: #FFF;}.caret{display: none;}"),
                        dropdownButton(
                          inputId = "mydropdown2",
                          label = "",
                          icon = icon("fas fa-ellipsis-v"),
                          status = "custom",
                          circle = FALSE,
                           actionButton(paste0("delete_",group[[a]]),"Remove ", 
                                       #onclick = "Shiny.onInputChange('btnLabel', this.id);", 
                                       # This works perfectly
                                       onclick = 'Shiny.setInputValue(\"btnRem\", this.id, {priority: \"event\"})',
                                       # onclick = 'Shiny.setInputValue(\"btnLabel\",this.id)', 
                                       style = "background-color: transparent;color:white;border-color: transparent;height:20px"),br(),
                          actionButton(paste0("explore_",group[[a]]),"Explore",
                                       onclick = 'Shiny.setInputValue(\"btnExp\", this.id, {priority: \"event\"})',
                                       style = "background-color: transparent;color:white;border-color: transparent;height:20px")
                        )
                    )
        )#,plotlyOutput(paste("4",name[[a]]), height= "100px")
        
        )
      
    }, name = unique(dt$keyword), group  = unique(dt$group), dt = dt, rank= rank)
  })
  

#https://stackoverflow.com/questions/40038749/r-shiny-how-to-write-loop-for-observeevent

observeEvent(input$btnRem,{
  #print(input$btnRem)
  dt <- values$over_time
  dt <- dt %>% filter(group != sub("delete_","",input$btnRem))
  values$over_time <- dt
  
  dt1 <- values$related
  dt1 <- dt1 %>% filter(group != sub("delete_","",input$btnRem))
  values$related <- dt1
  
  dt2 <- values$region
  dt2 <- dt2 %>% filter(group != sub("delete_","",input$btnRem))
  values$region <- dt2
  
})

observeEvent(input$btnExp,{
  print(input$btnExp)
  dt1 <- values$related
  dt <- dt1 %>% filter(group == sub("explore_","",input$btnExp))
  Top <- dt$value[dt$related_queries == "top"]
  Rising = dt$value[dt$related_queries == "rising"]
  sq <- seq(max(length(Top), length(Rising)))
 
  queries <-  data.frame(Top = Top[sq], Rising = Rising[sq])
  queries[is.na(queries)] <- ""
   
  values$related_explore <- queries
  print(values$related_explore)
  if(nrow(dt)>=1){
    topqueries <- dt %>%
      #filter(related_queries == 'top') %>%
      unnest_tokens(bigram, value, token = 'ngrams', n = 3) %>%
      separate(bigram, c("word1", "word2"), sep = " ") %>%
      filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
      count(word1, word2, sort = TRUE) %>%
      filter(!is.na(word1), !is.na(word2)) %>%
      graph_from_data_frame()
    
    # Find group membership
    wt <- cluster_walktrap(topqueries, steps = 6)
    members <- membership(wt)
    
    # Convert igraph to list for networkD3
    sj_list <- igraph_to_networkD3(topqueries, group = members)
    fig <- networkD3::forceNetwork(Links = sj_list$links, 
                                   Nodes = sj_list$nodes, 
                                   Source = 'source', 
                                   Target = 'target', 
                                   NodeID = 'name', 
                                   Group = 'group',
                                   Value = "value",
                                   #linkDistance = 200,
                                   #linkColour = 'source',
                                   height = 450, 
                                   width = 450, 
                                   fontSize = 20, 
                                   linkDistance = networkD3::JS("function(d) { return 120*d.value; }"), 
                                   linkWidth = networkD3::JS("function(d) { return d.value/6; }"),
                                   opacity = 1, zoom = TRUE, opacityNoHover = 0.9)
    values$plot_network_explore <- fig
 
    showModal(modalDialog(
      br(),
      output$forceRadius_explore <- renderForceNetwork({
        values$plot_network_explore 
      }),
      title = paste0(sub("explore_","",input$btnExp)," Relative Network"),
      easyClose=FALSE,
      size = "l",
      footer = div(actionButton("open_query",label = "Queries Table"),
                   actionButton("closeModal",label = "Close"))
    ))
  }

})

observeEvent(input$closeModal, {
  removeModal()
})

observeEvent(input$open_query, {
  showModal(modalDialog(
    br(),
    output$open_query_explore <- renderDT({
       dt <- values$related_explore
       datatable(dt,
                 filter = "top",
                 rownames = FALSE,
                 options = list(
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#32393e', 'color': 'white'});",
                     "}"),pageLength = 10, orderClasses = TRUE, searching = TRUE, paging = TRUE),
                 container = tags$table(
                   class="stripe row-border hover",
                   tags$thead(tags$tr(lapply(colnames(dt), tags$th)))
                 )
       ) %>% formatStyle(columns=colnames(dt),color='white',background = '#32393e',target = 'row')
    }),
    title = paste0(sub("explore_","",input$btnExp)," Relative Network Table"),
    easyClose=FALSE,
    size = "l",
    footer = actionButton("closeModal",label = "Close")
  ))
})


observe({
  output$related_queries_out <- renderDT({
    dt <- values$related_queries_out
    datatable(dt,
              filter = "top",
              rownames = FALSE,
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#32393e', 'color': 'white'});",
                  "}"),pageLength = 10, orderClasses = TRUE, searching = TRUE, paging = TRUE),
              container = tags$table(
                class="stripe row-border hover",
                tags$thead(tags$tr(lapply(colnames(dt), tags$th)))
              )
    ) %>% formatStyle(columns=colnames(dt),color='white',background = '#32393e',target = 'row')
  })
  
  output$interest_by_dma_out <- renderDT({
    dt <- values$interest_by_dma_out
    datatable(dt,
              filter = "top",
              rownames = FALSE,
              options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#32393e', 'color': 'white'});",
        "}"),pageLength = 10, orderClasses = TRUE, searching = TRUE, paging = TRUE),
      container = tags$table(
        class="stripe row-border hover",
        tags$thead(tags$tr(lapply(colnames(dt), tags$th)))
      )
    ) %>% formatStyle(columns=colnames(dt),color='white',background = '#32393e',target = 'row')
  })
  output$queries_out <- renderUI({
  div(
    style = "position: relative",
    box(
      id = "related_queries",
      width = 6,
      height = 620,
      title = "Compared breakdown by Related Queries",
      div(DTOutput("related_queries_out"))
    ),
    box(
      id = "related_queries",
      width = 6,
      height = 620,
      title = "Compared breakdown by Metropolitan Areas",
      div(DTOutput("interest_by_dma_out"))
    ))
  })
})
 
  
  
}# end server

