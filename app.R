library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(here)
here::here()

library(haven)
dat <- read_spss("/Users/lizredford/Downloads/Shiny/explore-raceiat/raceiat_N7983.sav") %>% select(Implicit, Explicit, raceomb, sex, politicalid, year, age, education)

dat$Preference <- cut(dat$Implicit, 
    breaks = c(-Inf, -.15, .15, Inf), 
    labels = c("Pro-Black Preference", 
               "No Preference", 
               "Pro-White Preference"), 
    right = FALSE)

dat$raceomb <- as.factor(dat$raceomb)
dat$raceomb <- recode(dat$raceomb, 
                      "5" = "Black",
                      "6" = "White") 

dat$sex <- as.factor(dat$sex)
dat$raceomb <- as.factor(dat$raceomb)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel(tags$h1("Explore the data: implicit racial preferences", style = "color:MidnightBlue;")),

     # Sidebar layout with a input and output definitions 
    sidebarLayout(

    # Input
    sidebarPanel(
      
      # Select variable to correlate with IAT
      selectInput(inputId = "x", 
                  label = "Select variable to correlate with IAT scores",
                  choices = c("Age" = "age", 
                              "Education" = "education", 
                              "Political ideology (more negative = more conservative; more positive = more liberal; 0 = moderate)" = "politicalid", 
                              "Explicit preference for White over Black" = "Explicit", 
                              "Year IAT was taken" = "year"), 
                  selected = "age"),
    
      # Select subset: participant race
      selectInput(inputId = "prace", 
                  label = "Select race of participants to plot in histogram",
                  choices = c("White" = "White", 
                              "Black" = "Black",
                              "All" = "all"),
                  selected = "all"),
      
      br(),
      br(),
      img(src = "pi.png", height = 40, width = 150),
      br(),
      br(),
      "Produced by", a("Liz Redford", href = "https://lizredford.weebly.com"),
        "in association with ", a("Project Implicit", href = "http://implicit.harvard.edu"),
      br(),
      br(),
      "To access the raw Race IAT datafrom 2002-2017, visit", a("the Open Science Framework",         href = "https://osf.io/52qxl/", "page."),

      br()
    
      ),
    
# Mainpanel for displaying outputs ----
    mainPanel(
    
    # Output
    textOutput("methods"),
    tags$br(),
    textOutput("results1"),
    textOutput("results2"),
    tags$br(),
    plotOutput(outputId = "iathist"),
    tags$br(),
    textOutput("results3"),
    tags$br(),
    tags$br()
#    br(),
#    plotOutput(outputId = "scatterplot")
    )
    )
  )


# Define server ----

server <- function(input, output) {

  df_subset <- reactive({
    if (input$prace == "all") { 
        datbyrace <- dat 
        return(datbyrace)
        } else {
    datbyrace <- filter(dat, raceomb == input$prace)
    return(datbyrace)}
  })
  
  df_subset3 <- reactive({
    if (input$prace == "all") { 
        datbyrace3 <- dat$Implicit 
        return(datbyrace3)
        } else {
    datbyrace2 <- filter(dat, raceomb == input$prace)
    datbyrace3 <- datbyrace2$Implicit
    return(datbyrace3)}
  })
    
  output$methods = renderText({
    paste("The Implicit Association Test (IAT) measures the strength of associations between 
    concepts (e.g., Black people, White people) and evaluations (e.g., Good, Bad). These 
    plots represent ", length(dat$Implicit), " people, which is a random sample of .05% of 
    people who took the Race IAT between 2007 and 2016." )
    })
    
  output$results1 = renderText({
    paste("The mean IAT score in this sample is ", round(mean(dat$Implicit, na.rm = TRUE), 
    digits = 3), "." )
    })

    output$results2 = renderText({
    df <- (as.data.frame(dat[, input$x])) #had to create a new dataframe because original tibble format was not computing correlation. Error: argument no numeric or logical (despite working in console. Not sure why)
    paste0("The correlation of ", input$x, " with IAT score is ", round(cor(dat$Implicit,(df[,1]), method = "pearson", use = "complete.obs"), digits = 3), ".")
    })
  
library(ggplot2)
output$iathist <- renderPlot({ #Save output to output list using output$, giving a name to   use in ui. Build output with render().
  ggplot(data = df_subset(), 
         aes(x = Implicit, fill = Preference)) + 
  geom_histogram(binwidth = .1, na.rm = TRUE, colour = "white") + 
      theme(text = element_text(size = 20)) +
      labs(title="Implicit Preferences for White versus Black",
      x = "(pro-Black)               Implicit               (pro-White)", 
      y = "Participants") + 
  xlim(c(-1.75, 1.75)) + 
  scale_fill_manual(values = c("#c51b8a", "#2c7fb8", "#191970")) + NULL #scale_fill_brewer(palette = "PiYG")
  })

output$results3 = renderText({paste("The mean IAT score for",
                                    input$prace,
                                    "participants is ",
                                    df_subset3() %>% mean(., na.rm = TRUE) %>% round(., digits = 3), "." )
    })
    
#   Create scatterplot
#   output$scatterplot <- renderPlot({
#     ggplot(data = dat, aes_string(x = input$x, y = dat$Implicit,color = input$colorby)) +
#       geom_point(na.rm=TRUE) + theme(text = element_text(size=20)) + labs(title="Implicit Preferences",
#         y = "(pro-Black)    Implicit    (pro-White)") +
#          scale_y_continuous(breaks=c(-1.5, -1, -.5, 0, .5, 1, 1.5))
#  })
  

}
# Create Shiny app
shinyApp(ui = ui, server = server)


