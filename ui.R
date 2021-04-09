# Load libraries, data -----------------------------------------------
female_inmates <- read.csv("csc_female_data.csv")


# Page 1 - Introduction ----------------------------------------------
intro_panel <- tabPanel(
  "Introduction",
  
  titlePanel("Overclassified and Overepresented: Breaking the Cycle of Trauma and Crime for Canada's Federally Incarcerated Female Inmates"),
  
  img(src = "inmate.png", width = 800, height = 400),
  br(), br(),
  
  p("Author: Laura Cline"),
  p("This is a 2-page shiny application using the Canadian Correctional Services Data on female inmates sentencing between 2012 and 2018. The application consists of an introduction page and a visualization page. The visualization page includes an interactive vertical bar chart where the user can select which variables they would like to compare for each ethnic/racial group. The data demonstrates that there is systematic racial bias in federal risk assessment tools."),
  
  p(a(href = "https://www.theglobeandmail.com/canada/article-investigation-racial-bias-in-canadian-prisons-methodology/", "Data Source (Tom Cardoso, Globe and Mail)"))
    )

# Page 2 - Character Visualization -------------------------------------------
select_values <- colnames(female_inmates)
select_values <- select_values[select_values %in% c('offender_security_level', 'dynamic_need', 'static_risk', 'motivation', 'reintegration_potential')] # Wanted columns 

sidebar_content <- sidebarPanel(
  selectInput(
    "y_var",
    label = "Score Type",
    choices = select_values,
    selected = 'offender_security_level'
  )
)

main_content <- mainPanel(
  plotOutput("plot")
)

second_panel <- tabPanel(
  "Visualization",
  titlePanel("What are the Most Common Canadian Correctional Services Scores per Race?"),
  p("Use the selector input below to choose which type of Canadian Correctional Services score you would like to see."),
  sidebarLayout(
    sidebar_content, main_content
  )
)

# User Interface -----------------------------------------------------
ui <- navbarPage(
  "Systematic Racial Bias in Canadian Risk Assessment Scores",
  intro_panel,
  second_panel)