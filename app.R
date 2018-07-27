library(shiny)
library(rsconnect)
library(shinydashboard)
library(here)
library(numform)

# Import and set up data #####
library(haven)
library(dplyr)

# --- Import and tidy/format data -----------------------------------------------------------

# Race ------------

# Import data from Github
raceiatdat <- read_sav(file = "https://github.com/lizredford/Race-IAT-descriptives-shiny/raw/master/raceiat_N7983.sav") # transform GitHub url from 'Download' button into data frame
raceiatdat <- read_sav(file = "https://github.com/lizredford/Race-IAT-descriptives-shiny/blob/master/raceiat_N7983.sav?raw=true") # transform GitHub url from 'View Raw' hyperlink into data frame

# Alternatively, download data and import:
        # raceiatdat <- read_spss("raceiat_N7983.sav") %>% select(Implicit, Explicit, raceomb, sex, politicalid, year, age, education)
        # If NOT downloading from Github and instead using file in repo, need to also run this:
        # raceiatdat <- rename(raceiatdat, politics = politicalid)

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
                     gender = sex,
                     race = raceomb,
                     explicit = Explicit)

# Gender ------------
library(readr)

# Import data from Github

# Alternatively, download data and import:
# gendersciiatdat <- read_csv("gendersciiatdat.csv")


# UI #####
customsidebar <-  dashboardSidebar(
  sidebarMenu(
    menuItem("Race IAT", tabName = "Race", icon = icon("th")),
    menuItem("Gender IAT", icon = icon("th"), tabName = "Gender"),
    menuItem("About the IAT", icon = icon("th"), tabName = "IAT"),
    menuItem("About this dashboard", icon = icon("th"), tabName = "about"),
    menuItem("Donate to Project Implicit", icon = icon("external-link-alt"), href = "https://4agc.com/donation_pages/9dda692c-6aa1-47e7-852d-58d396ebd3af")
  )
)

body <- dashboardBody(
  tags$style(".content {
                        margin-left:20px;
                        margin-right:35px;
                        }"),
  
  fluidRow(    
    tabItems(
        # IAT info tab content #####
        
        tabItem(tabName = "about",
        "This dashboard was made by ", tags$a(href="lizredford.weebly.com", "Liz Redford"),
        "using the publicly-available Project Implicit ", tags$a(href="https://osf.io/y9hiq/", " demonstration website datasets."), br(),br(), tags$a(href="https://implicit.harvard.edu/implicit/", "Project Implicit"),
"is a non-profit organization and international collaboration between researchers who are interested in implicit social cognition - thoughts and feelings outside of conscious awareness and control.", "You can donate to Project Implicit", tags$a(href="https://4agc.com/donation_pages/9dda692c-6aa1-47e7-852d-58d396ebd3af", "here."),
br(),br(), "This dashboard works best in a browser. If on mobile, it works best positioned in landscape, but still won't appear as intended."
        ),

        tabItem(tabName = "IAT",

        h2("How does the IAT work?"),
  "The IAT measures the strength of associations between concepts (e.g., black people, gay people) and evaluations (e.g., good, bad) or stereotypes (e.g., athletic, clumsy). The main idea is that making a response is easier when closely related items share the same response key.", br(), br(),
"When doing an IAT you are asked to quickly sort words into categories that are on the left and right hand side of the computer screen by pressing the “e” key if the word belongs to the category on the left and the “i” key if the word belongs to the category on the right. The IAT has five main parts.",
br(),br(),
"Read more at", tags$a(href="https://implicit.harvard.edu/implicit/iatdetails.html", "the Project Implicit page."),

h2("Take an Implicit Association Test (IAT)"),
"Take an IAT yourself by clicking ", tags$a(href="https://implicit.harvard.edu/implicit/takeatest.html", "here.")
         
        ),
        
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
        higher score indicates a greater preference for White people over 
        Black people.", 
        br(), paste("These plots represent ", length(raceiatdat$Implicit), 
        " participants, which is a random
        sample of 0.05% of people who took the Race IAT between 2007 and 2016. The average 
        IAT score for this overall sample is ", round(mean(raceiatdat$Implicit, na.rm = TRUE), 
        digits = 3), " (SD = ", round(sd(raceiatdat$Implicit, na.rm = TRUE), digits = 3), ")", " indicating a moderate implicit preference for White over Black people.", sep = "") # sep = "" to remove paste() automatically adding spaces
            ),
            
        
            box(title = "Who do you want to see the distribution of Race IAT scores for?", 
                width = NULL,
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
          title = "How do people score on the Race IAT?", 
              solidHeader = TRUE, 
              width = NULL, 
              status = "primary",
              plotOutput(outputId = "racehist", 
                         height = 330)
          ),
         
          box(
          title = "Do Race IAT scores correlate with other factors?", 
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
        concepts (e.g., Women, Men) and evaluations (e.g., Good, Bad).", "A
        higher score indicates a greater association between men and science relative to 
        women and science.", br
        (), paste("These plots represent", length(raceiatdat$Implicit), "people, which is a random
        sample of 0.05% of people who took the Gender IAT between 2007 and 2016.")
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

# UI #####
ui <- dashboardPage(
  dashboardHeader(title = "Explore the data: Implicit Association Test", 
                  titleWidth = 450
                  ),
  customsidebar,
  body
)

# Server #####

server <- function(input, output) {
 
# MUST put this scatter/correlation code BEFORE subsetting ggplot/histogram, or histogram will disappear in ui. Maybe subsetting breaks original dataset that scatter/correlation is based on?

  output$racecorr = renderText({
    df <- (as.data.frame(raceiatdat[, input$x])) #had to create a new dataframe because original tibble format was not computing correlation. Error: argument no numeric or logical (despite working in console. Not sure why)
    
    racecorrtest <- cor.test(raceiatdat$Implicit,df[ , input$x], method = "pearson", use = "complete.obs")
    

    paste0("The correlation of ", 
           input$x, 
           " with scores on the Race IAT is r = ", 
           f_num(racecorrtest$estimate, digits = 3), #f_num removes leading 0s
           ", p ",
          if (racecorrtest$p.value < .001) {
          print("< .001")
          } else {
          print(paste0("= ", round(racecorrtest$p.value, digits = 2)))
          },
          ". This correlation is ",
          if (racecorrtest$p.value < .01) {
          print("statistically significant")
          } else {
          print("not statistically significant")
          },
          " at alpha = .01." 
                   )

    })
    
  df_subset <- reactive({
    if (input$race_prace == "all") { 
        raceiatbyrace <- raceiatdat 
        return(raceiatbyrace)
        } else {
    raceiatbyrace <- filter(raceiatdat, race == input$race_prace)
    return(raceiatbyrace)}
  })

library(ggplot2)
output$racehist <- renderPlot({ #Save output to output list using output$, giving a name to   use in ui. Build output with render().
  ggplot(data = df_subset(), 
         aes(x = Implicit, fill = Preference)) + 
  geom_histogram(binwidth = .1, na.rm = TRUE, colour = "white") + 
      theme(text = element_text(size = 20)) +
      labs(
      x = "(more pro-Black)     IAT Score     (more pro-White)", 
      y = "Number of Participants") + 
  xlim(c(-1.75, 1.75)) + 
  scale_fill_manual(values = c("#c51b8a", "#2c7fb8", "#191970")) 
  

})

}

shinyApp(ui, server)
