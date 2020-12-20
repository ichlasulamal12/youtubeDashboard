header <- dashboardHeader(title = "US Youtube")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Dashboard",
             tabName = "dashboard",
             icon = icon("ethernet")),
    menuItem(text = "Correlation Plot",
             tabName = "corr",
             icon = icon("chart-line")),
    menuItem(text = "Dataset",
             tabName = "data",
             icon = icon("database")),
    menuItem(text = "Source Code",
             icon = icon("file-code"),
             href = "https://github.com/ichlasulamal12/youtubeDashboard")
  )
)

body <- dashboardBody(
  shinyDashboardThemes(
    theme = "onenote"
  ),
  tabItems(
    tabItem(tabName = "dashboard", 
            h1("Youtube Videos Dashboard", align = "center"),
            fluidRow(
              box(title = "Engagement Graph", width = 12, solidHeader = TRUE, status = "primary",
                  plotlyOutput(outputId = "plot_engagement"))
            ),
            fluidRow(
              box(title = "Select Year Input", width = 4, solidHeader = TRUE, status = "warning",
                  selectInput(inputId = "tahun",
                              label = "Choose Year",
                              choices = unique(youtube_clean$year_trending))),
              box(title = "Performances", width = 8, solidHeader = TRUE, status = "primary",
                  plotlyOutput(outputId = "plot_perform"))
            ),
            fluidRow(
              box(title = "Select Number of Channel", width = 4, solidHeader = TRUE, status = "warning",
                  sliderInput(inputId = "numbercat", 
                              label = "Number of Channel", 
                              min = 3,
                              max = 20, 
                              value = 5)),
              box(title = "Top Youtube Channel", width = 8, solidHeader = TRUE, status = "primary",
                  plotlyOutput(outputId = "plot_topc"))
            )),
    tabItem(tabName = "corr",
            radioButtons(inputId = "numvar",
                         label = "Choose Numerical Variable",
                         choices = youtube_clean %>% 
                           select_if(is.numeric) %>% 
                           select(-c(views, publish_hour, year_trending)) %>% 
                           colnames() %>% 
                           str_replace_all(pattern = "_", replacement = " ") %>% 
                           str_to_title()),
            plotlyOutput(outputId = "plot_corr")),
    tabItem(tabName = "data",
            dataTableOutput(outputId = "data_youtube"))
  )
)

dashboardPage(
  header = header,
  body = body,
  sidebar = sidebar
)
