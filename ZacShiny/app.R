library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(RColorBrewer)
library(shinybusy)
library(ggrepel)
library(leaflet)
library(readxl)
library(robotstxt)
library(rvest)
library(knitr)
library(pdftools)
library(janitor)
library(viridis)
library(ggnetwork)
library(igraph)
library(datasets)
library(mdsr)
library(gapminder)
library(maps)


##getting our data into Shiny App
state_co2_adjusted<-read_csv("state_co2_adjusted")
stateCO2_perCapita<-read_csv("stateCO2_perCapita")
state_renewables<-read_csv("state_renewables")
US_renewables<-read_csv("US_renewables")
state_energy_consumption<-read_csv("state_energy_consumption")
energy<-read_csv("state_energy_consumption")
state_co2_2 <- read.csv("state_co2_adjusted2")
stateCO2_perCapita2<-read_csv("stateCO2_perCapita2")
new_energy<- read_csv("combined_energy_locations") 
coal_adjusted <- read_csv("coal_adjusted")
geothermal_adjusted <- read_csv("geothermal_adjusted")
hydroelectric_adjusted <- read_csv("hydroelectric_adjusted")
natural_gas_adjusted <- read_csv("natural_gas_adjusted")
nuclear_adjusted <- read_csv("nuclear_adjusted")
petroleum_adjusted <- read_csv("petroleum_adjusted")
solar_adjusted <- read_csv("solar_adjusted")
wind_adjusted <- read_csv("wind_adjusted")

##This is necessary for renewables section of app
US_renewables_col_names<-colnames(US_renewables)
state_renewables_col_names<-colnames(state_renewables)

line_var_values<-c("coal", "natural_gas", "crude_oil", "nuclear", "biofuels","wood_waste", "other", "total")
line_var_names<-c("Coal", "Natural Gas", "Crude Oil", "Nuclear", "Biofuels","Wood Waste", "Other Renewables", "Total" )
names(line_var_values) <- line_var_names

### This for US renewables section of app
US_line_var_values<-c("Total","Coal", "Petroleum_Liquids", "Petroleum_Coke", "Natural_Gas", "Other_Gases","Nuclear", "Hydroelectric", "Other_Renewables","Hydroelectric_pumped", "Other")
US_line_var_names<-c("Total", "Coal", "Petroleum Liquids", "Petroleum Coke", "Natural Gas", "Other Gases","Nuclear", "Hydroelectric", "Other Renewables","Hydroelectric Pumped", "Other" )
names(US_line_var_values) <- US_line_var_names




#####For Choropleths
state_co2 <- read_csv("state_co2_adjusted")
stateCO2_perCapita <- read_csv("stateCO2_perCapita")

usa_states <- map_data(map = "state"
                       , region = ".")
state_info <- data.frame(state_full = tolower(state.name)
                         , State = state.abb
                         , Region = state.region)


#for co2
state_co2 <- state_co2 %>%
  mutate(State = tolower(state.name))

state_co2_map <- state_co2 %>%
  left_join(state_info, by = "State") %>%
  right_join(usa_states, by = c("State" = "region")) %>%
  rename(Y1990 = "1990", Y2018 = "2018")

#for per capita
stateCO2_perCapita <- stateCO2_perCapita %>%
  mutate(State = tolower(state.name))
perCapita_co2_map <- stateCO2_perCapita %>%
  left_join(state_info, by = "State") %>%
  right_join(usa_states, by = c("State" = "region")) %>%
  rename(Y1990 = "1990", Y2018 = "2018") 

#for geothermal
geothermal_adjusted2 <- geothermal_adjusted %>%
  group_by(State) %>%
  summarize(Energy_Produced = sum(Energy_Produced)) %>%
  rename(state = State) %>%
  mutate(State = tolower(state))

geo_map <- geothermal_adjusted2 %>%
  left_join(state_info, by = "State") %>%
  right_join(usa_states, by = c("State" = "region"))


#for hydro
hydroelectric_adjusted2 <- hydroelectric_adjusted %>%
  group_by(State) %>%
  summarize(Energy_Produced = sum(Energy_Produced)) %>%
  rename(state = State) %>%
  mutate(State = tolower(state))

hydro_map <- hydroelectric_adjusted2 %>%
  left_join(state_info, by = "State") %>%
  right_join(usa_states, by = c("State" = "region"))

#for natural gas
natural_gas_adjusted2 <- natural_gas_adjusted %>%
  group_by(State) %>%
  summarize(Energy_Produced = sum(Energy_Produced)) %>%
  rename(state = State) %>%
  mutate(State = tolower(state))

ng_map <- natural_gas_adjusted2 %>%
  left_join(state_info, by = "State") %>%
  right_join(usa_states, by = c("State" = "region"))

#for coal
coal_adjusted2 <- coal_adjusted %>%
  group_by(State) %>%
  summarize(Energy_Produced = sum(Energy_Produced)) %>%
  rename(state = State) %>%
  mutate(State = tolower(state))

coal_map <- coal_adjusted2 %>%
  left_join(state_info, by = "State") %>%
  right_join(usa_states, by = c("State" = "region"))

#for nuclear
nuclear_adjusted2 <- nuclear_adjusted %>%
  group_by(State) %>%
  summarize(Energy_Produced = sum(Energy_Produced)) %>%
  rename(state = State) %>%
  mutate(State = tolower(state))

nuclear_map <- nuclear_adjusted2 %>%
  left_join(state_info, by = "State") %>%
  right_join(usa_states, by = c("State" = "region"))

#for petroleum
petroleum_adjusted2 <- petroleum_adjusted %>%
  group_by(State) %>%
  summarize(Energy_Produced = sum(Energy_Produced)) %>%
  rename(state = State) %>%
  mutate(State = tolower(state))

petroleum_map <- petroleum_adjusted2 %>%
  left_join(state_info, by = "State") %>%
  right_join(usa_states, by = c("State" = "region"))

#for solar
solar_adjusted2 <- solar_adjusted %>%
  group_by(State) %>%
  summarize(Energy_Produced = sum(Energy_Produced)) %>%
  rename(state = State) %>%
  mutate(State = tolower(state))

solar_map <- solar_adjusted2 %>%
  left_join(state_info, by = "State") %>%
  right_join(usa_states, by = c("State" = "region"))


#for wind
wind_adjusted2 <- wind_adjusted %>%
  group_by(State) %>%
  summarize(Energy_Produced = sum(Energy_Produced)) %>%
  rename(state = State) %>%
  mutate(State = tolower(state))

wind_map <- wind_adjusted2 %>%
  left_join(state_info, by = "State") %>%
  right_join(usa_states, by = c("State" = "region"))





ui <- navbarPage(theme = shinytheme("sandstone"),
                       setBackgroundColor(
                         color = c("#218766", "#adccb0 "),
                         gradient = "linear",
                         direction = "bottom"
                       ),
                       title="Energy and CO2 Production in the US",
navbarMenu("Choropleths",
           tabPanel(
             title="Petroleum",
               mainPanel(plotOutput(outputId = "petroleum_choro"), width = 12
               )
             ),
           
           tabPanel(
             title="Natural Gas",
             mainPanel(plotOutput(outputId = "ng_choro"), width = 12 
                 )
             ),
               
           tabPanel(
             title= "Coal",
               mainPanel(plotOutput("coal_choro"), width = 12 
                 )
             ),
  
           tabPanel(
             title = "Geothermal",
             mainPanel(plotOutput(outputId = "geo_choro"), width = 12 
             )
           ),
           
           tabPanel(
             title = "Hydroelectric",
             mainPanel(plotOutput(outputId = "hydro_choro"), width = 12 
             )
           ),
           
           tabPanel(
             title = "Nuclear",
             mainPanel(plotOutput(outputId = "nuclear_choro"), width = 12 
             )
           ),
           
           tabPanel(
             title = "Wind",
             mainPanel(plotOutput(outputId = "wind_choro"), width = 12 
             )
           ),
           
           tabPanel(
             title = "Solar",
             mainPanel(plotOutput(outputId = "solar_choro"), width = 12 
             )
           ),
           
           tabPanel(
             title = " CO2 Emissions: State",
             mainPanel(plotOutput(outputId = "state_co2_choro"), width = 12 
             )
           ),
           
           tabPanel("CO2 Emissions: Per Capita",
              mainPanel(plotOutput(outputId = "percapita_choro"), width = 12 
             )
           )
         )
       )
           


server <- function(input, output) {
  
  
  output$petroleum_choro <- renderPlot({
    ggplot(petroleum_map, aes(x = long, y = lat, group = group
                              , fill = Energy_Produced)) +
      geom_polygon(color = "white") +
      theme_void() +
      coord_fixed(ratio = 1.3) +
      labs(title = "Petroleum Energy Produced by State", 
           subtitle = "as of 2018",
           fill = "Petroleum Energy Produced (Megawatts)") +
      scale_fill_distiller(palette = "PuBu", direction = "horizantle")
  })
  
  output$geo_choro <- renderPlot({
    ggplot(geo_map, aes(x = long, y = lat, group = group
                        , fill = Energy_Produced)) +
      geom_polygon(color = "white") +
      theme_void() +
      coord_fixed(ratio = 1.3) +
      labs(title = "Geothermal Energy Produced by State", 
           subtitle = "as of 2018",
           fill = "Geothermal Energy Produced (Megawatts)") +
      scale_fill_distiller(palette = "PuBu", direction = "horizantle")
  })
  
  
  
  
  
  
}




shinyApp(ui = ui, server = server)