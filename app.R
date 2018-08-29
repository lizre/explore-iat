library(shiny)
library(rsconnect)
library(shinydashboard)
library(here)
library(numform)

# Import and set up data #####
library(haven)
library(dplyr)

# Define function that prints size of correlation coefficients
correffectsize <- function(corrtestvalue) {
 if (corrtestvalue < .09) {
      print("negligible")
    } else if (corrtestvalue > .09 & corrtestvalue < .291) {
      print("small")
    } else if (corrtestvalue > .291 & corrtestvalue < .491) {
      print("medium")
    } else {
      print ("large")
    } 
}

# Define function that conditionally prints correlation significance level
corrsig <- function(corrsigvalue) {
    if (corrsigvalue < .001) {
      print("p < .001")
    } else {
      print(paste0("p = ", round(corrsigvalue, digits = 3)))
    }
}


# --- Import and tidy/format data ----------------------------------------------------------

# Import and tidy Race data  ------------

# Import data from Github
raceiatdat <- read_sav(file = "https://github.com/lizredford/explore-iat/blob/master/raceiat_N7983.sav?raw=true") # transform GitHub url from 'View Raw' hyperlink into data frame
# raceiatdat <- read_sav(file = "https://github.com/lizredford/Race-IAT-descriptives-shiny/raw/master/raceiat_N7983.sav") # can also transform GitHub url from 'Download' button into data frame

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

# Import and tidy Gender-Science data ------------
library(readr)

# Import data from Github
gendersciiatdat <- read_csv(file = "https://github.com/lizredford/explore-iat/raw/master/gendersciiatdat.csv?raw=true") # transform GitHub url from 'View Raw' hyperlink into data frame

# Alternatively, download data and import:
# gendersciiatdat <- read_csv("gendersciiatdat.csv")

# Break data into discrete categories for coloring-by in histogram.
gendersciiatdat$Association <- cut(gendersciiatdat$implicit, 
    breaks = c(-Inf, -.15, .15, Inf), 
    labels = c("Female--Science \n Association", 
               "No Gender--Science \n Association", 
               "Male--Science \n Association"), 
    right = FALSE)

# Coerce to factors for ggplot 
gendersciiatdat$sex <- as.factor(gendersciiatdat$sex)
gendersciiatdat$sex <- recode(gendersciiatdat$sex, 
                      "f" = "Female",
                      "m" = "Male") 
gendersciiatdat$sex[gendersciiatdat$sex == "."] <- NA # Code "." as missing

gendersciiatdat$raceomb <- as.factor(gendersciiatdat$raceomb)

# Change variable names to nice ones; they are displayed as-in-dataset in correlation output.
gendersciiatdat <- rename(gendersciiatdat, 
                     gender = sex,
                     race = raceomb)


# Import and tidy Sexuality data ------------

sexualityiatdat <- read_csv(file = "https://github.com/lizredford/explore-iat/raw/master/sexualityiatdat.csv?raw=true") # transform GitHub url from 'View Raw' hyperlink into data frame

# Break data into discrete categories for coloring-by in histogram.
sexualityiatdat$Preference <- cut(sexualityiatdat$Implicit, 
    breaks = c(-Inf, -.15, .15, Inf), 
    labels = c("Pro-Gay Preference", 
               "No Sexuality Preference", 
               "Pro-Straight Preference"), 
    right = FALSE)

# Coerce to factor for ggplot
sexualityiatdat$sexualityall <- as.factor(sexualityiatdat$sexualityall)

# Change participant sexuality numeric labels to descriptive ones
sexualityiatdat$sexualityall <- recode(sexualityiatdat$sexualityall, 
                      "1" = "Heterosexual",
                      "2" = "Gay or Lesbian",
                      "3" = "Bisexual",
                      "4" = "Asexual",
                      "5" = "Questioning") 

# Change to dichotomous labels to subset by
sexualityiatdat$sexuality_dichot <- recode(sexualityiatdat$sexualityall, 
          "Heterosexual" = "Heterosexual",
          "Gay or Lesbian" = "Not heterosexual (gay, bisexual, asexual, or questioning)",
          "Bisexual" = "Not heterosexual (gay, bisexual, asexual, or questioning",
          "Asexual" = "Not heterosexual (gay, bisexual, asexual, or questioning",
          "Questioning" = "Not heterosexual (gay, bisexual, asexual, or questioning") 


# Import and tidy Age data ------------

ageiatdat <- read_csv(file = "https://github.com/lizredford/explore-iat/raw/master/ageiatdat.csv?raw=true") # transform GitHub url from 'View Raw' hyperlink into data frame

# Break data into discrete categories for coloring-by in histogram.
ageiatdat$Preference <- cut(ageiatdat$Implicit, 
    breaks = c(-Inf, -.15, .15, Inf), 
    labels = c("Pro-Old Preference", 
               "No Age Preference", 
               "Pro-Young Preference"), 
    right = FALSE)

# Coerce to factor for ggplot
ageiatdat$Age_group <- as.factor(ageiatdat$Age_group)
table(ageiatdat$Age_group)
# Change participant age group numeric labels to descriptive ones
ageiatdat$Age_group <- recode(ageiatdat$Age_group, 
                      "1" = "20 or younger",
                      "2" = "21 - 30",
                      "3" = "31 - 40",
                      "4" = "41 - 50",
                      "5" = "51 - 60",
                      "6" = "61 or older") 

# Create new variable that cuts participant ages into 3 groups to subset by
ageiatdat$Age_group_3 <- recode(ageiatdat$Age_group, 
                      "20 or younger" = "30 or younger",
                      "21 - 30" = "30 or younger",
                      "31 - 40" = "31 - 50",
                      "41 - 50" = "31 - 50",
                      "51 - 60" = "51 or older",
                      "61 or older" = "51 or older") 


# UI #####
customsidebar <-  dashboardSidebar(
  sidebarMenu(
    menuItem("Race IAT", tabName = "Race", icon = icon("th")),
    menuItem("Gender-Science IAT", icon = icon("th"), tabName = "Gender"),
    menuItem("Sexuality IAT", icon = icon("th"), tabName = "Sexuality"),
    menuItem("Age IAT", icon = icon("th"), tabName = "Age"),
    menuItem("About the IAT", icon = icon("th"), tabName = "IAT"),
    menuItem("About this dashboard", icon = icon("th"), tabName = "about"),
    menuItem("Donate to Project Implicit", icon = icon("th"), href = "https://4agc.com/donation_pages/9dda692c-6aa1-47e7-852d-58d396ebd3af")
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
br(),br(), "This dashboard works best in a browser. If on mobile, it works best positioned in landscape, but still won't appear as intended.",
br(),br(), "You can find all data and code used to create this dashboard at",
tags$a(href="https://github.com/lizredford/explore-iat", "Github.")
        ),

        tabItem(tabName = "IAT",

        h2("How does the IAT work?"),
  "The IAT measures the strength of associations between concepts (e.g., black people, gay people) and evaluations (e.g., good, bad) or stereotypes (e.g., athletic, clumsy). The main idea is that making a response is easier when closely related items share the same response key.", br(), br(),
"When doing an IAT you are asked to quickly sort words into categories that are on the left and right hand side of the computer screen by pressing the “e” key if the word belongs to the category on the left and the “i” key if the word belongs to the category on the right. The IAT has five main parts.",
br(),br(),
"Read more at", tags$a(href="https://implicit.harvard.edu/implicit/iatdetails.html", "the Project Implicit page."),

h2("Take an Implicit Association Test (IAT)"),
"Take an IAT yourself by clicking ", tags$a(href="https://implicit.harvard.edu/implicit/takeatest.html", "here; you'll get personal feedback on your preferences or associations.")
         
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
        digits = 3), " (SD = ", f_num(sd(raceiatdat$Implicit, na.rm = TRUE), digits = 2), ")", " indicating a moderate implicit preference for White over Black people.", sep = "") # sep = "" to remove paste() automatically adding spaces
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
                  selectInput(inputId = "race_corrvar", 
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
        
        # Gender-science tab content #####
        tabItem(tabName = "Gender",
        fluidRow(
        column(width = 4,
        box(title = "What is the Gender-Science IAT?", width = NULL, 
                solidHeader = TRUE, 
                status = "info", 
                collapsible = TRUE,
       "The Implicit Association Test (IAT) measures the strength of associations between 
        social concepts (e.g., Women, Men) and academic areas (e.g., Science, Liberal Arts).", "A
        higher score indicates a greater association between men and science relative to 
        women and science.", br
        (), paste("These plots represent ", length(gendersciiatdat$implicit), " people, which is a random
        sample of 2% of people who took the Gender IAT between 2007 and 2015. The average 
        IAT score for this overall sample is ", round(mean(gendersciiatdat$implicit, na.rm = TRUE), 
        digits = 3), " (SD = ", f_num(sd(gendersciiatdat$implicit, na.rm = TRUE), digits = 2), ")", " indicating a moderately strong association between men and science relative to women and science.", sep = "") # sep = "" to remove paste() automatically adding spaces
        ),
         
        box(title = "Who do you want to see the distribution of Gender-Science IAT scores for?", 
        width = NULL,
        solidHeader = TRUE, 
        status = "info", 
        collapsible = TRUE,
          selectInput(inputId = "gendersci_pgender", 
          label = "Below, choose whether to graph IAT scores by participant gender",
          choices = c("Female" = "Female", 
                      "Male" = "Male",
                      "All" = "all"),
          selected = "all")
         ),
       
         box(title = "What demographic factors correlate with scores on the Gender-Science IAT?", 
          width = NULL,
          solidHeader = TRUE, 
          status = "info", 
          collapsible = TRUE,
            selectInput(inputId = "gendersci_corrvar", 
            label = "Below, choose a variable to see its correlation with scores on the 
            Gender-Science IAT",
            choices = c("Age" = "age", 
                        "Education" = "education", 
                        "Political ideology (more negative = more conservative; more positive = more liberal)" = "politics", 
                        "Explicit associations between men and science relative to women and science" = "explicit", 
                        "Year IAT was taken" = "year"), 
            selected = "age")
            )
        
        ), # this column
        
        column(width = 8,
         
          box(
          title = "How do people score on the Gender-Science IAT?", 
              solidHeader = TRUE, 
              width = NULL, 
              status = "primary",
              plotOutput(outputId = "genderscihist", 
                         height = 330)
          ),
          
          box(
          title = "Do Gender-Science IAT scores correlate with other factors?", 
              solidHeader = TRUE, 
              width = NULL, 
              status = "primary",
              textOutput(outputId = "genderscicorr")

          )
        )
        ) #fluidRow
        
        ), # Gender tabItem

      # Sexuality tab content #####

        tabItem(tabName = "Sexuality",
        fluidRow(
        column(width = 4,
        box(title = "What is the Sexuality IAT?", width = NULL, 
                solidHeader = TRUE, 
                status = "info", 
                collapsible = TRUE,
       "The Implicit Association Test (IAT) measures the strength of associations between 
        concepts (e.g., gay, straight) and evaluations (e.g., good, bad). A 
        higher score indicates a greater preference for straight people over 
        gay people.", br
        (), paste("These plots represent ", length(sexualityiatdat$Implicit), " people, which is a random
        sample of 0.8% of people who took the Sexuality IAT between 2007 and 2015. The average 
        IAT score for this overall sample is ", round(mean(sexualityiatdat$Implicit, na.rm = TRUE), 
        digits = 3), " (SD = ", f_num(sd(sexualityiatdat$Implicit, na.rm = TRUE), digits = 2), ")", " indicating a moderate implicit preference for straight over gay people.", sep = "") # sep = "" to remove paste() automatically adding spaces
        ),
         
        box(title = "Who do you want to see the distribution of Sexuality IAT scores for?", 
        width = NULL,
        solidHeader = TRUE, 
        status = "info", 
        collapsible = TRUE,
          selectInput(inputId = "sexuality_psexuality", 
          label = "Below, choose whether to graph IAT scores by participant sexuality",
          choices = c("Heterosexual" = "Heterosexual", 
                      "Not heterosexual (gay, bisexual, asexual, or questioning)" = "Not heterosexual (gay, bisexual, asexual, or questioning)",
                      "All" = "all"),
          selected = "all")
         ),
       
         box(title = "What demographic factors correlate with scores on the Sexuality IAT?", 
          width = NULL,
          solidHeader = TRUE, 
          status = "info", 
          collapsible = TRUE,
            selectInput(inputId = "sexuality_corrvar", 
            label = "Below, choose a variable to see its correlation with scores on the 
            Sexuality IAT",
            choices = c("Age" = "age", 
                        "Education" = "education", 
                        "Political ideology (more negative = more conservative; more positive = more liberal)" = "politics", 
                        "Explicit preference for straight over gay" = "Explicit", 
                        "Year IAT was taken" = "year"), 
            selected = "age")
            )
        
        ), # this column
        
        column(width = 8,
         
          box(
          title = "How do people score on the Sexuality IAT?", 
              solidHeader = TRUE, 
              width = NULL, 
              status = "primary",
              plotOutput(outputId = "sexualityhist", 
                         height = 330)
          ),
          
          box(
          title = "Do Sexuality IAT scores correlate with other factors?", 
              solidHeader = TRUE, 
              width = NULL, 
              status = "primary",
              textOutput(outputId = "sexualitycorr")

          ) #this box
        ) # this column
        ) #fluidRow
        ), #this tab

      # Age tab content #####

        tabItem(tabName = "Age",
        fluidRow(
        column(width = 4,
        box(title = "What is the Age IAT?", width = NULL, 
                solidHeader = TRUE, 
                status = "info", 
                collapsible = TRUE,
       "The Implicit Association Test (IAT) measures the strength of associations between 
        concepts (e.g., old, young) and evaluations (e.g., good, bad). A 
        higher score indicates a greater preference for young people over 
        gay people.", br
        (), paste("These plots represent ", length(ageiatdat$Implicit), " people, which is a random
        sample of 0.8% of people who took the Age IAT between 2007 and 2015. The average 
        IAT score for this overall sample is ", round(mean(ageiatdat$Implicit, na.rm = TRUE), 
        digits = 3), " (SD = ", f_num(sd(ageiatdat$Implicit, na.rm = TRUE), digits = 2), ")", " indicating a strong implicit preference for young over old people.", sep = "") # sep = "" to remove paste() automatically adding spaces
        ),
         
        box(title = "Who do you want to see the distribution of Age IAT scores for?", 
        width = NULL,
        solidHeader = TRUE, 
        status = "info", 
        collapsible = TRUE,
          selectInput(inputId = "age_page", 
          label = "Below, choose whether to graph IAT scores by participant age group",
          choices = c("30 or younger" = "30 or younger", 
                      "31 - 50" = "31 - 50",
                      "51 or older" = "51 or older",
                      "All" = "all"),
          selected = "all")
         ),
       

         box(title = "What demographic factors correlate with scores on the Age IAT?", 
          width = NULL,
          solidHeader = TRUE, 
          status = "info", 
          collapsible = TRUE,
            selectInput(inputId = "age_corrvar", 
            label = "Below, choose a variable to see its correlation with scores on the 
            Age IAT",
            choices = c("Participant age in years" = "age", 
                        "Education" = "education", 
                        "Political ideology (more negative = more conservative; more positive = more liberal)" = "politics", 
                        "Explicit preference for young over old" = "Explicit", 
                        "Year IAT was taken" = "year"), 
            selected = "age")
            )
        
        ), # this column
        
        column(width = 8,
         
          box(
          title = "How do people score on the Age IAT?", 
              solidHeader = TRUE, 
              width = NULL, 
              status = "primary",
              plotOutput(outputId = "agehist", 
                         height = 330)
          ),
          
          box(
          title = "Do Age IAT scores correlate with other factors?", 
              solidHeader = TRUE, 
              width = NULL, 
              status = "primary",
              textOutput(outputId = "agecorr")

          ) #this box
        ) # this column
        ) #fluidRow
        ) #this tab

        ) # tabItems
        
) # fluidRow 
) # dashboardBody

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
 
### Generate reactive output: correlations ------

# Race -----

# MUST put this scatter/correlation code BEFORE subsetting ggplot/histogram, or histogram will disappear in ui. Maybe subsetting breaks original dataset that scatter/correlation is based on?

  output$racecorr = renderText({
    df <- (as.data.frame(raceiatdat[, input$race_corrvar])) #had to create a new dataframe because original tibble format was not computing correlation. Error: argument no numeric or logical (despite working in console. Not sure why)

    racecorrtest <- cor.test(raceiatdat$Implicit,df[ , input$race_corrvar], method = "pearson", use = "complete.obs")
    
   paste0("There is a ",
          correffectsize(racecorrtest$estimate),
          " correlation between ",
          input$race_corrvar, 
          " and scores on the Race IAT, r = ", 
          f_num(racecorrtest$estimate, digits = 3), #f_num removes leading 0s
          ", ",
          corrsig(racecorrtest$p.value),
          "."
          )

    })


# Gender-Science -----

# MUST put this scatter/correlation code BEFORE subsetting ggplot/histogram, or histogram will disappear in ui. Maybe subsetting breaks original dataset that scatter/correlation is based on?

  output$genderscicorr = renderText({
    df2 <- (as.data.frame(gendersciiatdat[, input$gendersci_corrvar])) #had to create a new dataframe because original tibble format was not computing correlation. Error: argument no numeric or logical (despite working in console. Not sure why)

  genderscicorrtest <- cor.test(gendersciiatdat$implicit,df2[ , input$gendersci_corrvar], method = "pearson", use = "complete.obs")
    
    paste0("There is a ",
          correffectsize(genderscicorrtest$estimate),
          " correlation between ",
          input$gendersci_corrvar, 
          " and scores on the Gender-Science IAT, r = ", 
          f_num(genderscicorrtest$estimate, digits = 3), #f_num removes leading 0s
          ", ",
          corrsig(genderscicorrtest$p.value),
          "."
          )

    })

# Sexuality correlations -----

# MUST put this scatter/correlation code BEFORE subsetting ggplot/histogram, or histogram will disappear in ui. Maybe subsetting breaks original dataset that scatter/correlation is based on?

  output$sexualitycorr = renderText({
    df <- (as.data.frame(sexualityiatdat[, input$sexuality_corrvar])) #had to create a new dataframe because original tibble format was not computing correlation. Error: argument no numeric or logical (despite working in console. Not sure why)
    
    sexualitycorrtest <- cor.test(sexualityiatdat$Implicit,df[ , input$sexuality_corrvar], method = "pearson", use = "complete.obs")
    
   paste0("There is a ",
          correffectsize(sexualitycorrtest$estimate),
          " correlation between ",
          input$sexuality_corrvar, 
          " and scores on the Sexuality IAT, r = ", 
          f_num(sexualitycorrtest$estimate, digits = 3), #f_num removes leading 0s
          ", ",
          corrsig(sexualitycorrtest$p.value),
          "."
          )

    })


# Age correlations -----

# MUST put this scatter/correlation code BEFORE subsetting ggplot/histogram, or histogram will disappear in ui. Maybe subsetting breaks original dataset that scatter/correlation is based on?

  output$agecorr = renderText({
    df <- (as.data.frame(ageiatdat[, input$age_corrvar])) #had to create a new dataframe because original tibble format was not computing correlation. Error: argument no numeric or logical (despite working in console. Not sure why)
    
    agecorrtest <- cor.test(ageiatdat$Implicit,df[ , input$age_corrvar], method = "pearson", use = "complete.obs")
    
   paste0("There is a ",
          correffectsize(agecorrtest$estimate),
          " correlation between ",
          input$age_corrvar, 
          " and scores on the age IAT, r = ", 
          f_num(agecorrtest$estimate, digits = 3), #f_num removes leading 0s
          ", ",
          corrsig(agecorrtest$p.value),
          "."
          )

    })

    
### Generate reactive output: histograms ------
  
# Race histogram -----
  
  df_subset_race <- reactive({
    if (input$race_prace == "all") { 
        raceiatbyrace <- raceiatdat 
        return(raceiatbyrace)
        } else {
    raceiatbyrace <- filter(raceiatdat, race == input$race_prace)
    return(raceiatbyrace)}
  })

library(ggplot2)
output$racehist <- renderPlot({ #Save output to output list using output$, giving a name to   use in ui. Build output with render().
  ggplot(data = df_subset_race(), 
         aes(x = Implicit, fill = Preference)) + 
  geom_histogram(binwidth = .1, na.rm = TRUE, colour = "white") + 
      theme(text = element_text(size = 20)) +
      labs(
      x = "(more pro-Black)     IAT Score     (more pro-White)", 
      y = "Number of Participants", 
      fill = "Implicit Preference") + 
  xlim(c(-1.75, 1.75)) + 
  scale_fill_manual(values = c("#c51b8a", "#2c7fb8", "#191970")) 
})

# Gender-Science histogram -----

  df_subset_gendersci <- reactive({
    if (input$gendersci_pgender == "all") { 
        gendersciiatbygender <- gendersciiatdat
        return(gendersciiatbygender)
        } else {
    gendersciiatbygender <- filter(gendersciiatdat, gender == input$gendersci_pgender)
    return(gendersciiatbygender)}
  })

output$genderscihist <- renderPlot({ #Save output to output list using output$, giving a name to   use in ui. Build output with render().
  ggplot(data = df_subset_gendersci(), 
         aes(x = implicit, fill = Association)) + 
  geom_histogram(binwidth = .1, na.rm = TRUE, colour = "white") + 
      theme(text = element_text(size = 20),
            legend.key.size = unit(2.5, 'lines')) +
      labs(
      x = " (Stronger                                (Stronger \n  female-science    IAT Score     male-science \n  association)                             association)", 
      y = "Number of Participants",
      fill = "Implicit Association") + 
  xlim(c(-1.75, 1.75)) + 
  scale_fill_manual(values = c("#c51b8a", "#2c7fb8", "#191970")) 
})

# Sexuality histogram -----
  
  df_subset_sexuality <- reactive({
    if (input$sexuality_psexuality == "all") { 
        sexualityiatbypsexu <- sexualityiatdat 
        return(sexualityiatbypsexu)
        } else {
    sexualityiatbypsexu <- filter(sexualityiatdat, sexuality_dichot == input$sexuality_psexuality)
    return(sexualityiatbypsexu)}
  })

output$sexualityhist <- renderPlot({ #Save output to output list using output$, giving a name to   use in ui. Build output with render().
  ggplot(data = df_subset_sexuality(), 
         aes(x = Implicit, fill = Preference)) + 
  geom_histogram(binwidth = .1, na.rm = TRUE, colour = "white") + 
      theme(text = element_text(size = 20)) +
      labs(
      x = "(more pro-gay)     IAT Score     (more pro-straight)", 
      y = "Number of Participants", 
      fill = "Implicit Preference") + 
  xlim(c(-1.75, 1.75)) + 
  scale_fill_manual(values = c("#c51b8a", "#2c7fb8", "#191970")) 
})

# Age histogram -----
  
  df_subset_age <- reactive({
    if (input$age_page == "all") { 
        ageiatbypage <- ageiatdat 
        return(ageiatbypage)
        } else {
    ageiatbypage <- filter(ageiatdat, Age_group_3 == input$age_page)
    return(ageiatbypage)}
  })

output$agehist <- renderPlot({ #Save output to output list using output$, giving a name to   use in ui. Build output with render().
  ggplot(data = df_subset_age(), 
         aes(x = Implicit, fill = Preference)) + 
  geom_histogram(binwidth = .1, na.rm = TRUE, colour = "white") + 
      theme(text = element_text(size = 20)) +
      labs(
      x = "(more pro-old)     IAT Score     (more pro-young)", 
      y = "Number of Participants", 
      fill = "Implicit Preference") + 
  xlim(c(-1.75, 1.75)) + 
  scale_fill_manual(values = c("#c51b8a", "#2c7fb8", "#191970")) 
})


} # server

shinyApp(ui, server)
