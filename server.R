#########################################################################################
#load necessary packages
#########################################################################################
require("leaflet")
require("dplyr")
require("plotly")
require("ggplot2")
require("dplyr")
require("reshape2")
require("tidyverse")

#########################################################################################
#load data sets used
#########################################################################################


#Genreal data
dados <- read_csv("2018_09_04_dados.csv")


#Convert data to data
dados <- dados %>%
    mutate(Data = as.Date(Data, format = "%d/%m/%Y"))



#iNext data
sample_coverage <- read_csv("sample_coverage.csv")
sample_coverage <- sample_coverage %>%
    mutate(lty = factor(method, c("interpolated", "observed", "extrapolated"),
                        c("interpolation", "interpolation", "extrapolation")))


hill_numbers <- read.csv("hill_numbers.csv")
hill_numbers <- hill_numbers %>%
    mutate(lty = factor(method, c("interpolated", "observed", "extrapolated"),
                        c("interpolation", "interpolation", "extrapolation")))


hill_ci <- read_csv("hill_ci.csv")


#Permanova data
perm_all <- read_csv("Perm_res_all.csv")
perm_adu <- read_csv("Perm_res_adult.csv")
perm_nym <- read_csv("Perm_res_nymph.csv")

#########################################################################################
#Manipulating data for sites map
#########################################################################################

#Couting the n of individulas in each point
map_plot <- dados %>% select("Ambiente", "Especie", "Lat", "Long") %>%
    group_by_all() %>%
    summarise(N = n())

map_plot <- map_plot %>% spread(Especie, N, fill = 0)

#Cretae a new data frame
dados_riq <- data.frame(lapply(map_plot[5:12], function(x) ifelse(x >= 1,1,0)))

dados_riq <- dados_riq %>% 
    mutate(riq = rowSums(dados_riq))

dados_amb <- map_plot %>% select("Ambiente", "Lat", "Long")
dados_amb <- data.frame(dados_amb)

map_plot <- cbind(dados_amb, dados_riq)

map_plot <- map_plot %>% 
    group_by(Lat, Long, Ambiente) %>%
    summarise(riq = sum(riq))

pal <- colorFactor(c("#2b8cbe", "#006d2c"), domain = c("Periurbano", "Rural"))

#########################################################################################
#Manipulating data descriptive analysis
#########################################################################################
gg_esp <- dados %>%
    group_by(Especie) %>%
    summarise(count = n()) %>%
    rename(Abundance = count)

#Creating a plot representing proportion of each stage
gg_prop <- dados %>%
    group_by(Especie, Estagio) %>%
    summarise(Abundance = n())

gg_prop <- dcast(Especie ~ Estagio, data = gg_prop)


gg_prop <- gg_prop %>%
    replace(., is.na(.), 0) %>%
    mutate(Total = Adulto + Ninfa)
    


#Change to long format
gg_prop <- melt(gg_prop, id.vars = c("Especie", "Total"))
gg_prop <- gg_prop %>%
    mutate(Proportion = round(value/Total, 2))

#########################################################################################
#Manipulating data for time series analysis
#########################################################################################
ts_plot <- dados %>%
    mutate(Estagio = ifelse(Estagio == "Adulto", "Adult", "Nymph")) %>%
    mutate(Data = format(as.Date(Data), "%Y-%m")) %>%
    group_by(Data, Especie, Estagio) %>%
    summarise(Abundance = n())

#########################################################################################
#Creating functionality
#########################################################################################

server <- function(input, output){
    
    
    
    #Plot the map
    output$mapa <- renderLeaflet({
        leaflet(data = map_plot) %>%
            addTiles() %>%
            setView(lng = -63.87, lat = -8.75, zoom = 10) %>%
            addCircleMarkers(radius = ~riq * 2, color = ~pal(Ambiente), stroke = FALSE, fillOpacity = 0.8,
                             label = ~paste("Number of species colected: ", riq)) %>%
            addLegend("bottomright", colors = c("#2b8cbe", "#006d2c"), 
                      label = c("Periurbano", "Rural"), title = "Number of Species")
    })
    
    output$wihtakker <- renderPlotly({
        ggplotly(ggplot(data = gg_esp, aes(x = reorder(Especie, Abundance), y = Abundance)) +
                     geom_bar(stat = "identity") +
                     coord_flip() +
                     theme_bw() +
                     labs(y = "Abundance", x = "Thick Species"),
                 tooltip = "y")
                

        })
    
    
    output$prop_plot <- renderPlotly({
        ggplotly(ggplot(data = gg_prop, aes(x = reorder(Especie,Total), y = Proportion,
                                            fill = variable)) +
                     geom_bar(stat = "identity") +
                     coord_flip() +
                     theme_bw() +
                     labs(y = "Proportion of Individuals", x = "Thick Species"),
                 tooltip = "y")
    })
    
    
    #Function to filter data based on input
    data_to_plot <- reactive({
        if(input$sel_stage == "All" & input$sel_species == "All"){
            data <- ts_plot
            data <- data %>% group_by(Data, Estagio) %>% summarise(Abundance = sum(Abundance))
        }
        else if(input$sel_stage == "All" & input$sel_species != "All"){
            data <- ts_plot[ts_plot$Especie == input$sel_species,]
            
        }
        else if(input$sel_stage != "All" & input$sel_species == "All"){
            data <- ts_plot[ts_plot$Estagio == input$sel_stage,]
            data <- data %>% group_by(Data) %>% summarise(Abundance = sum(Abundance))
        }
        else{
            data <- ts_plot[ts_plot$Especie == input$sel_species & ts_plot$Estagio == input$sel_stage, ]
        }
        data
    })
    
    #Plot ts
    output$ts_plot <- renderPlotly({
        ts <- data_to_plot()
        if(input$sel_stage != "All" & input$sel_species == "All"){
            ggplotly(ggplot(data = ts,aes(x = Data, y = Abundance)) +
                         geom_bar(stat = "identity") +
                         theme_bw() +
                         labs(y = "Abundance", x = "Data of the collect"),
                     tooltip = "y")
            
        }
        else{
            ggplotly(ggplot(data = ts,aes(x = Data, y = Abundance, fill = Estagio)) +
                         geom_bar(stat = "identity") +
                         theme_bw() +
                         labs(y = "Abundance", x = "Data of the collect"),
                     tooltip = "y")
            
        }
        
    })
    
    #Function to filter data based on input
    hill_data <- reactive({
        if(input$sel_hill == "Sample coverage"){
            data_sub <- sample_coverage
        }
        else{
            data_sub <- hill_numbers
            data_sub <- data_sub[data_sub$order == input$sel_hill, ]
            
        }
        data_sub
    })
    
    #Plot iNext graphs
    output$hill_plot <- renderPlot({
        data_sub <- hill_data()
        if(input$sel_hill == "Sample coverage"){
            ggplot(data_sub, aes(x = x, y = y, col = site)) +
                theme_bw(base_size = 24) +
                geom_line(aes(linetype = lty), lwd = 1.5) +
                theme(legend.position = "bottom",
                      legend.title = element_blank()) +
                labs(x = "Number of individuals", y = "Sample coverage") +
                geom_ribbon(aes(ymin = y.lwr, ymax = y.upr, fill = site, colour = NULL), 
                            alpha = 0.2)
            
        }
        else{
            ggplot(data_sub, aes(x = x, y = y, col = site)) +
                theme_bw(base_size = 24) +
                geom_line(aes(linetype = lty), lwd = 1.5) +
                theme(legend.position = "bottom",
                      legend.title = element_blank()) +
                labs(x = "Number of individuals", y = "Species diversity") +
                geom_ribbon(aes(ymin = y.lwr, ymax = y.upr, fill = site, colour = NULL), 
                            alpha = 0.2)
            
        }
        
            
            
    })
    
    #Function to read ci
    table_ci <- reactive({
        if(input$sel_hill == "Sample coverage"){
            data_ci <- hill_ci[is.na(hill_ci$Diversity),]
            
        }
        else{
            data_ci <- hill_ci
            data_ci <-  data_ci[data_ci$Diversity == input$sel_hill, ]
            
        }
        data_ci
    })
    
    #Retrun table
    output$table1 <- renderTable({
        table_ci()
    })
    
    #Function to read permanova data
    Permanova_table <- reactive({
        if(input$sel_com == "All"){
            data_perm <- perm_all
            
        }
        else if(input$sel_com == "Adult"){
            data_perm <- perm_adu
            
        }
        else{
            data_perm <- perm_nym
        }

    })
    
    #Retrun table
    output$table2 <- renderTable({
        Permanova_table()
    })
    
    
    #Return image
    output$image <- renderImage({
        if(input$sel_com == "All"){
            return(
                list(
                    src = "www/All_venn.png",
                    filetype = "image/png",
                    alt = "Venn All",
                    width = 640,
                    height = 480,
                    align = "center"
                )
            )

        }
        else if(input$sel_com == "Adult"){
            return(
                list(
                    src = "www/Adult_venn.png",
                    filetype = "image/png",
                    alt = "Venn All",
                    width = 640,
                    height = 480,
                    align = "center"
                )
            )
            
        }
        else{
            return(
                list(
                    src = "www/Ninfa_venn.png",
                    filetype = "image/png",
                    alt = "Venn All",
                    width = 720,
                    height = 480,
                    align = "center"
                )
            )
            
            
        }
    },deleteFile = FALSE)
    
    
}




