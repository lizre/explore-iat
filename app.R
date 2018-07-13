library(shiny)
library(rsconnect)
library(shinydashboard)
library(here)

# Import and set up data #####
library(haven)
library(dplyr)
raceiatdat <- read_spss("raceiat_N7983.sav") %>% select(Implicit, Explicit, raceomb, sex, politicalid, year, age, education)

# Break data into discrete categories for coloring-by in histogram.
raceiatdat$Preference <- cut(raceiatdat$Implicit, 
    breaks = c(-Inf, -.15, .15, Inf), 
    labels = c("Pro-Black Preference", 
               "No Preference", 
               "Pro-White Preference"), 
    right = FALSE)

raceiatdat$raceomb <- as.factor(raceiatdat$raceomb)
raceiatdat$raceomb <- recode(raceiatdat$raceomb, 
                      "5" = "Black",
                      "6" = "White") 

# Coerce to factors for ggplot
raceiatdat$sex <- as.factor(raceiatdat$sex)
raceiatdat$raceomb <- as.factor(raceiatdat$raceomb)

# Change variable names to nice ones; they are displayed as-in-dataset in correlation output.
raceiatdat <- rename(raceiatdat, 
                     politics = politicalid,
                     gender = sex,
                     race = raceomb,
                     explicit = Explicit)

# UI #####
customsidebar <-  dashboardSidebar(
  sidebarMenu(
    menuItem("Race", tabName = "Race", icon = icon("th")),
    menuItem("Gender", icon = icon("th"), tabName = "Gender"),
    menuItem("Link to data for all IAT tasks", icon = icon("th"), 
           href = "https://osf.io/y9hiq/")
  )
)

body <- dashboardBody(
      
  fluidRow(    
    tabItems(
        # Race tab content #####
        tabItem(tabName = "Race",
        fluidRow(
        column(width = 4,
            box(title = "What is the Race IAT?", width = NULL,
            solidHeader = TRUE, 
            status = "info", 
            collapsible = TRUE,
        "The Implicit Association Test (IAT) measures the strength of associations between 
        concepts (e.g., Black people, White people) and evaluations (e.g., Good, Bad). A 
        larger, more positive score represents a greater preference for White people over 
        Black people.", br(), paste("These plots represent", length(raceiatdat$Implicit), "people, 
        which is a random
        sample of .05% of people who took the Race IAT between 2007 and 2016.", "The average 
        IAT score in this sample is ", round(mean(raceiatdat$Implicit, na.rm = TRUE), 
        digits = 3), ".")
            ),
            
            box(title = "Who do you want to see IAT scores for?", width = NULL,
            solidHeader = TRUE, 
            status = "info", 
            collapsible = TRUE,
                  selectInput(inputId = "race_prace", 
                  label = "Below, choose whether to graph IAT scores by participant race",
                  choices = c("White" = "White", 
                              "Black" = "Black",
                              "All" = "all"),
                  selected = "all")
            ),
        
            box(title = "What demographic factors correlate with scores on the Race IAT?", width = NULL,
            solidHeader = TRUE, 
            status = "info", 
            collapsible = TRUE,
                  selectInput(inputId = "x", 
                  label = "Below, choose a variable to see its correlation with scores on the 
                  Race IAT",
                  choices = c("Age" = "age", 
                              "Education" = "education", 
                              "Political ideology (more negative = more conservative; more positive = more liberal)" = "politics", 
                              "Explicit preference for White over Black" = "explicit", 
                              "Year IAT was taken" = "year"), 
                  selected = "age")
            )
        
        ),
        
        column(width = 8,
         
          box(
          title = "How do people score on the IAT?", 
              solidHeader = TRUE, 
              width = NULL, 
              status = "primary",
              plotOutput(outputId = "racehist", 
                         height = 330)
          ),
         
          box(
          title = "Do IAT scores correlate with other factors?", 
              solidHeader = TRUE, 
              width = NULL, 
              status = "primary",
              textOutput(outputId = "racecorr")

          )
        )
        )
        ),
        
        # Gender tab content #####
        tabItem(tabName = "Gender",
        fluidRow(
        box(title = "What is the Gender IAT?", width = 6, height = 200,
                solidHeader = TRUE, 
                status = "info", 
                collapsible = TRUE,
        "The Implicit Association Test (IAT) measures the strength of associations between 
        concepts (e.g., Women, Men) and evaluations (e.g., Good, Bad).", br
        (), paste("These plots represent", length(raceiatdat$Implicit), "people, which is a random
        sample of .05% of people who took the Gender IAT between 2007 and 2016.")
            )
        ),
        fluidRow(
        box(
        title = "Plot 2", width = 6, solidHeader = TRUE, status = "primary",
        "Box content"
        ),
        box(
        title = "Plot 1", width = 6, solidHeader = TRUE, status = "primary",
        "Box content"
        )
      )
    )
  )
)
)

ui <- dashboardPage(
  dashboardHeader(title = "Explore the data: Implicit Association Test", 
                  titleWidth = 450
                  ),
  customsidebar,
  body
)

server <- function(input, output) {
 
# MUST put this scatter/correlation code BEFORE subsetting ggplot/histogram, or histogram will disappear in ui. Maybe subsetting breaks original dataset that scatter/correlation is based on?

  output$racecorr = renderText({
    df <- (as.data.frame(raceiatdat[, input$x])) #had to create a new dataframe because original tibble format was not computing correlation. Error: argument no numeric or logical (despite working in console. Not sure why)
    paste0("The correlation of ", input$x, " with scores on the Race IAT is ", round(cor(raceiatdat$Implicit,(df[,1]), method = "pearson", use = "complete.obs"), digits = 3), ".")
    })
    
  df_subset <- reactive({
    if (input$race_prace == "all") { 
        raceiatbyrace <- raceiatdat 
        return(raceiatbyrace)
        } else {
    raceiatbyrace <- filter(raceiatdat, race == input$race_prace)
    return(raceiatbyrace)}
  })
  
  df_subset3 <- reactive({
    if (input$race_prace == "all") { 
        raceiatbyrace3 <- raceiatdat$Implicit 
        return(raceiatbyrace3)
        } else {
    raceiatbyrace2 <- filter(raceiatdat, race == input$race_prace)
    raceiatbyrace3 <- raceiatbyrace2$Implicit
    return(raceiatbyrace3)}
  })

library(ggplot2)
output$racehist <- renderPlot({ #Save output to output list using output$, giving a name to   use in ui. Build output with render().
  ggplot(data = df_subset(), 
         aes(x = Implicit, fill = Preference)) + 
  geom_histogram(binwidth = .1, na.rm = TRUE, colour = "white") + 
      theme(text = element_text(size = 20)) +
      labs(title="Implicit Preferences for White versus Black",
      x = "(pro-Black)               Implicit               (pro-White)", 
      y = "Participants") + 
  xlim(c(-1.75, 1.75)) + 
  scale_fill_manual(values = c("#c51b8a", "#2c7fb8", "#191970")) 
  

})

}

shinyApp(ui, server)
