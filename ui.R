# Load the neccessary libraries
# Checks if required packages are installed. If not, installs them. 
for (requirement in 
     c("jsonlite", "jtools", "shinydashboard", "shinydashboardPlus", "shinyjs", 
       "shinyWidgets", "shinydisconnect", "shinyFiles", "shinybusy", "zoo", 
       "lubridate", "viridis", "DBI", "RPostgres", "psych", "DT", "plotly",
       "stringr", "ggplot2", "ggthemes", "tidyr", "dplyr", "colourpicker")) {
  if (!require(requirement, character.only = TRUE)) {
    install.packages(requirement, repo="https://cran.rstudio.com")
    library(requirement, character.only = TRUE)
  }
}

# Specify Shiny UI function
shinyUI(
  
  dashboardPage(
    
    # Application title
    dashboardHeader(title = "Data Analysis Dashboard"),
    
    # Sidebar 
    dashboardSidebar(width = 240, # This specified the width of the sidebar menu
                     
                     sidebarMenu(
                       
                       
                       shinyjs::hidden(
                         
                         div(
                           id = "hiddensidebar",
                           
                           # Empty multi-input dropdown input for selecting an organization from a list
                           selectizeInput(inputId ="orgs",
                                          label = "Select an Organization:",
                                          choices = NULL, selected = NULL, multiple = T),
                           
                           # Empty multi-input dropdown input for selecting population(s) from a list
                           selectizeInput(inputId ="pops",
                                          label = "Select a Population:",
                                          choices = NULL, multiple = T),
                           
                           # Empty multi-input dropdown input for selecting survey(s) from a list
                           shinyjs::disabled(
                             selectizeInput(inputId = "surveys",
                                            label = "Select a Survey:",
                                            choices = NULL, multiple = T)
                           ),
                           
                           # Empty multi-input user_id dropdown for selecting user_ids from a list
                           shinyjs::disabled(
                             selectizeInput(inputId = "user_ids",
                                            label = "Select a user_id for removal:",
                                            choices = NULL, multiple = T)
                           ),
                           
                           # Empty range date input for selecting dates               
                           shinyjs::disabled(
                             dateRangeInput(inputId = "date", "Select a date period:", 
                                            start = as.Date(NA), end = as.Date(NA),
                                            startview = "year")
                           ),
                           
                           # Create report button
                           shinyjs::disabled(
                             actionButton(inputId = "createReport", label = "Generate Report", width = 210)
                           ), 
                           br(),
                           
                           
                           
                           # Download raw data button
                           shinyjs::disabled(
                             shinyDirButton('downloadRaw','Download Raw Data','Select a folder for output',
                                            roots = c(wd=getwd()),multiple=FALSE,
                                            style = "width: 210px")
                           )
                           
                           
                         )),
                       # Selection for Report Graph Download Theme
                       radioButtons(inputId='graphMode',label='Select Graph Theme: ',
                                    choices=list('Light'='light','Dark'='dark'),
                                    selected='dark'
                       )
                       
                       
                     ) # sidebarMenu
    ), # dashboardSidebar
    
    # Main body
    dashboardBody(
      
      # Using shinyjs library to initially hide all elements until create report button is pressed
      useShinyjs(),
      
      add_busy_spinner(spin = "fading-circle", margins = c(400,300)),
      
      disconnectMessage(),
      
      # Hide the main body of the dashboard upon load
      shinyjs::hidden(
        
        div(
          id = "hiddenbox",
          
          # Specify main tabBox  
          tabBox(width = 12,
                 
                 # Engagement tabPanel
                 ## Engagement_stats box is a placeholder for engagement.stats dataTable
                 #tabPanel("Engagement",
                 #         br(),
                 #         box(id = "engagement_stats",
                 #             title = "Statistics", status = "primary", solidHeader = T, width = 12, 
                 #             collapsible = T, 
                 #             DT::dataTableOutput("engagement.stats")
                 #         )),       
                 
                 # User Scores by Cell
                 tabPanel("User Scores by Cell",
                          
                          # Dropdown providing some user information for the section
                          dropdown(
                            p("This tab provides a detailed overview of the feedback scores users receive from their reviewers for 
                              each Cell assessed in the survey. By focusing on the scores users receive, we present a more 
                              intuitive and actionable view of the data. The statistics represent the 'typical user' or 'typical employee' 
                              at an organisation. The data comprises of all users who have received feedback from at least 3 reviewers."),
                            p("The statistics are calculated here by calculating the average score each user received from their reviwers 
                              for each question. We then calculate the average score for the cell for each user by summing the question scores 
                              and dividing by the number of questions in each cell. Finally, we get the mean of all users' cell scores and report
                              that in the table below"),
                            p("What you can do here:"),
                            tags$div(
                              tags$ul(
                                tags$li(
                                  tags$b('Report Graphs'),
                                  p('A set of boxplots visualizes the distribution of user scores for each question. These visuals 
                                    make it easy to spot patterns, such as whether scores are skewed, tightly clustered, or vary 
                                    widely among users. Plot customisation is available by clicking on the gears icon on the right.'),
                                  tags$em('Plots may display incorrectly on first run, hover over the gears icon on the right 
                                          to refresh.'),
                                  br(),
                                  p('Boxplots display:'),
                                  tags$ul(
                                    tags$li('The median (central line in the box)'),
                                    tags$li('The interquartile range (box width)'),
                                    tags$li('The range of scores (the whiskers)'),
                                    tags$li('The mean score (the dot)'),
                                    tags$li(tags$em('Outliers are removed from the plot'))
                                  )
                                ),
                                tags$li(
                                  tags$b('Summary Statistics Table'),
                                  p('A comprehensive table summarizes the key statistics for each cell/behaviour, offering 
                                    insights into the central tendency and spread of user scores. The table includes:'),
                                  tags$ul(
                                    tags$li('Mean (average score)'),
                                    tags$li('Median (middle score)'),
                                    tags$li('First Quartile (25th percentile)'),
                                    tags$li('Third Quartile (75th percentile)'),
                                    tags$li('Minimum score'),
                                    tags$li('Maximum score'),
                                    tags$li('Standard Deviation (variation in scores)'),
                                    tags$li('Interquartile Range (IQR; difference between the 75th and 25th percentiles)'),
                                    tags$li('The number of unique users for each cell/behaviour (n)')
                                    
                                  )
                                )
                              )
                            ),
                            style='unite',icon=icon('circle-question'),
                            animate=animateOptions(enter = "fadeInDown", exit = "fadeOutUp"),
                            size='xs',no_outline=T
                          ),
                          br(),
                          # Box holding "Report Graphs"
                          box(id="box_userscores_cells.graphs",
                              title="Report Graphs",status='primary',solidHeader = T,width=12,
                              collapsible=T,collapsed=T,
                              # Input and Output Fluid Layout
                              fluidRow(
                                # Box to hold Plot Inputs
                                box(width=3, title='Plot Inputs',
                                    
                                    # Cell selection input for cells
                                    tags$div(
                                      style="font-size: 12px;",
                                      selectizeInput(inputId='box_userscores_cells.graphs.cell_select',
                                                     label='Select cells:',
                                                     choices=NULL,
                                                     selected=NULL,
                                                     multiple=T,
                                                     options=list(maxItems=6))
                                    )
                                ),
                                
                                # Box holding the Plot Output
                                box(title='Plot Output',width=9,
                                    
                                    # Sidebar to hold plot customisation inputs, change `value` here to change defaults
                                    sidebar=boxSidebar(
                                      id='box_userscores_cells.graphs.options',
                                      width=75,
                                      tags$div(
                                        style="font-size: 12px;",
                                        fluidRow(
                                          column(11,
                                                 textInput(inputId='box_userscores_cells.graphs.plot.title',
                                                           value='Boxplot Distribution of User Scores',
                                                           'Plot Title'),
                                                 textInput(inputId='box_userscores_cells.graphs.plot.subtitle',
                                                           value='Mean of distribution represented by dot',
                                                           'Plot Title'),
                                                 textInput(inputId='box_userscores_cells.graphs.plot.x_title',
                                                           value='% of the time',
                                                           'X-axis Title'),
                                                 textInput(inputId='box_userscores_cells.graphs.plot.y_title',
                                                           value='Cell',
                                                           'Y-axis Title')
                                          )
                                        ),
                                        fluidRow(
                                          
                                          column(4,
                                                 numericInput(inputId='box_userscores_cells.graphs.plot.data_label',
                                                              'Data Label Text Size',
                                                              value=2.2,
                                                              min=1),
                                                 numericInput(inputId='box_userscores_cells.graphs.plot.data_auxlabel',
                                                              'Sample Size Label Text Size',
                                                              value=2.2,
                                                              min=1),
                                                 numericInput(inputId='box_userscores_cells.graphs.plot.axis_label',
                                                              'Axis Labels Text Size',
                                                              value=10,
                                                              min=6),
                                                 numericInput(inputId='box_userscores_cells.graphs.plot.axis_title',
                                                              'Axis Title Text Size',
                                                              value=12,
                                                              min=6),
                                                 numericInput(inputId='box_userscores_cells.graphs.plot.title_size',
                                                              'Title Text Size',
                                                              value=12,
                                                              min=6),
                                                 numericInput(inputId='box_userscores_cells.graphs.plot.subtitle_size',
                                                              'Subtitle Text Size',
                                                              value=10,
                                                              min=6)
                                          ),
                                          column(4,
                                                 colourInput(inputId='box_userscores_cells.graphs.plot.color1',
                                                             label='Select Plot Color 1:',
                                                             value='#23D7D2',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_cells.graphs.plot.color2',
                                                             label='Select Plot Color 2:',
                                                             value='#99F9FF',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_cells.graphs.plot.color3',
                                                             label='Select Plot Color 3:',
                                                             value='#DCFFFE',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_cells.graphs.plot.data_label_color',
                                                             label='Select Data Label Colour:',
                                                             value='black',
                                                             returnName = T,
                                                             closeOnClick = T)
                                          ),
                                          column(4,
                                                 colourInput(inputId='box_userscores_cells.graphs.plot.color4',
                                                             label='Select Plot Color 4:',
                                                             value='#D69AB2',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_cells.graphs.plot.color5',
                                                             label='Select Plot Color 5:',
                                                             value='#BD3B6F',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_cells.graphs.plot.color6',
                                                             label='Select Plot Color 6:',
                                                             value='#684564',
                                                             returnName = T,
                                                             closeOnClick = T)
                                          )
                                          
                                        )
                                      )
                                    ),
                                    # Render the graph
                                    plotOutput("box_userscores_cells.graphs.plot"),
                                    br(),
                                    # Button to generate the graph
                                    actionButton(inputId='box_userscores_cells.graphs.generate',label='Draw Graph'),
                                    br(),
                                    br(),
                                    # Download plot options
                                    fluidRow(
                                      column(width=2,
                                             numericInput(inputId='box_userscores_cells.graphs.plot.plot_width',
                                                          'Saved Plot Width',
                                                          value=8,
                                                          min=5)
                                      ),
                                      column(width=2,
                                             numericInput(inputId='box_userscores_cells.graphs.plot.plot_height',
                                                          'Saved Plot Height',
                                                          value=5,
                                                          min=5)
                                      )
                                    ),
                                    
                                    # Download button for plot
                                    downloadButton(outputId="downloaduserscores_cells",label="Save Plot as PNG")
                                ) # Plot output box
                              )
                          ), # Report Graphs Box
                          
                          # Summary Statistics Table
                          box(id='box_userscores_cells.sum',
                              title='Summary Statistics',status='primary',solidHeader = T,width=12,
                              collapsible=T,
                              div(style = 'overflow-x: scroll',DT::dataTableOutput("userscores_cells.sum")))
                 ), # Tab Panel - User Scores by Cell
                 
                 #User Scores by Question
                 tabPanel("User Scores by Question",
                          # Dropdown providing some information for the user
                          dropdown(
                            p("This tab provides a detailed overview of the feedback scores users receive from their reviewers for 
                              each behaviour assessed in the survey. By focusing on the scores users receive, we present a more 
                              intuitive and actionable view of the data. The statistics represent the 'typical user' or 'typical employee' 
                              at an organisation. The data comprises of all users who have received feedback from at least 3 reviewers"),
                            p("The statistics are calculated here by calculating the average score each user received from their reviwers 
                              for each question. Finally, we get the mean of all users' question scores and report that in the table below"),
                            p("What you can do here:"),
                            tags$div(
                              tags$ul(
                                tags$li(
                                  tags$b('Report Graphs'),
                                  p('A set of boxplots visualizes the distribution of user scores for each question. These visuals 
                                    make it easy to spot patterns, such as whether scores are skewed, tightly clustered, or vary 
                                    widely among users. Plot customisation is available by clicking on the gears icon on the right.'),
                                  tags$em('Plots may display incorrectly on first run, hover over the gears icon on the right 
                                          to refresh.'),
                                  br(),
                                  p('Boxplots display:'),
                                  tags$ul(
                                    tags$li('The median (central line in the box)'),
                                    tags$li('The interquartile range (box width)'),
                                    tags$li('The range of scores (the whiskers)'),
                                    tags$li('The mean score (the dot)'),
                                    tags$li(tags$em('Outliers are removed from the plot'))
                                  )
                                ),
                                tags$li(
                                  tags$b('Cleveland Dot Plots'),
                                  p('A Cleveland Dot Plot or Lollipop chart that is useful if you would like to display many 
                                    behaviours in a single plot. By default, the 10 lowest scoring behaviours are pre-selected. 
                                    The legend only displays in the dashboard viewer to aid with color selection but will not show 
                                    in the download. It is recommended to download this graph at least as tall as it is wide.'),
                                  tags$em('Plots may display incorrectly on first run, hover over the gears icon on the right 
                                          to refresh.')
                                ),
                                tags$li(
                                  tags$b('Summary Statistics Table'),
                                  p('A comprehensive table summarizes the key statistics for each cell/behaviour, offering 
                                    insights into the central tendency and spread of user scores. The table includes:'),
                                  tags$ul(
                                    tags$li('Mean (average score)'),
                                    tags$li('Median (middle score)'),
                                    tags$li('First Quartile (25th percentile)'),
                                    tags$li('Third Quartile (75th percentile)'),
                                    tags$li('Minimum score'),
                                    tags$li('Maximum score'),
                                    tags$li('Standard Deviation (variation in scores)'),
                                    tags$li('Interquartile Range (IQR; difference between the 75th and 25th percentiles)'),
                                    tags$li('The number of unique users for each cell/behaviour')
                                    
                                  )
                                )
                              )
                            ),
                            style='unite',icon=icon('circle-question'),
                            animate=animateOptions(enter = "fadeInDown", exit = "fadeOutUp"),
                            size='xs',no_outline=T
                          ),
                          
                          br(),
                          # Box that holds "Report Graphs"
                          box(id="box_userscores_questions.graphs",
                              title="Report Graphs",status='primary',solidHeader = T,width=12,
                              collapsible=T,collapsed=T,
                              # Inputs and Outputs Fluid Layout
                              fluidRow(
                                # Box for holding inputs
                                box(width=3,title='Plot Inputs',
                                    tags$div(
                                      style="font-size: 12px;",
                                      # Select Cell Input
                                      selectizeInput(inputId='box_userscores_questions.graphs.cell_select',
                                                     label='Select Cells:',
                                                     choices=NULL,
                                                     selected=NULL,
                                                     multiple=F),
                                      # Exclude behaviours Input
                                      selectizeInput(inputId='box_userscores_questions.graphs.question_exclude',
                                                     label='Select behaviours to exclude:',
                                                     choices=NULL,
                                                     selected=NULL,
                                                     multiple=T)
                                      
                                    )
                                ),
                                
                                # Box for holding outputs
                                box(title='Plot Output',width=9,
                                    # Sidebar in box for plot customisation options, change `value` here to change defaults
                                    sidebar=boxSidebar(
                                      id='box_userscores_questions.graphs.options',
                                      width=75,
                                      tags$div(
                                        style="font-size: 12px;",
                                        fluidRow(
                                          column(11,
                                                 textInput(inputId='box_userscores_questions.graphs.plot.title',
                                                           value='Boxplot Distribution of User Scores',
                                                           'Plot Title'),
                                                 textInput(inputId='box_userscores_questions.graphs.plot.subtitle',
                                                           value='Mean of distribution represented by dot',
                                                           'Plot Title'),
                                                 textInput(inputId='box_userscores_questions.graphs.plot.x_title',
                                                           value='% of the time',
                                                           'X-axis Title'),
                                                 textInput(inputId='box_userscores_questions.graphs.plot.y_title',
                                                           'Y-axis Title')
                                          )
                                        ),
                                        fluidRow(
                                          
                                          column(6,
                                                 numericInput(inputId='box_userscores_questions.graphs.plot.data_label',
                                                              'Data Label Text Size',
                                                              value=2.2,
                                                              min=1),
                                                 numericInput(inputId='box_userscores_questions.graphs.plot.axis_label',
                                                              'Axis Labels Text Size',
                                                              value=10,
                                                              min=6),
                                                 numericInput(inputId='box_userscores_questions.graphs.plot.axis_title',
                                                              'Axis Title Text Size',
                                                              value=12,
                                                              min=6),
                                                 numericInput(inputId='box_userscores_questions.graphs.plot.title_size',
                                                              'Title Text Size',
                                                              value=12,
                                                              min=6),
                                                 numericInput(inputId='box_userscores_questions.graphs.plot.subtitle_size',
                                                              'Subtitle Text Size',
                                                              value=10,
                                                              min=6)
                                          ),
                                          column(6,
                                                 colourInput(inputId='box_userscores_questions.graphs.plot.color1',
                                                             label='Select Plot Color 1:',
                                                             value='#23D7D2',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_questions.graphs.plot.color2',
                                                             label='Select Plot Color 2:',
                                                             value='#23D7D2',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_questions.graphs.plot.color3',
                                                             label='Select Plot Color 3:',
                                                             value='#23D7D2',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_questions.graphs.plot.color4',
                                                             label='Select Plot Color 4:',
                                                             value='#684564',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_questions.graphs.plot.data_label_color',
                                                             label='Select Data Label Colour:',
                                                             value='black',
                                                             returnName = T,
                                                             closeOnClick = T)
                                          )
                                          
                                        )
                                      )
                                    ),
                                    # Render Plot
                                    plotOutput("box_userscores_questions.graphs.plot"),
                                    br(),
                                    # Button to generate plot
                                    actionButton(inputId='box_userscores_questions.graphs.generate',label='Draw Graph'),
                                    br(),
                                    br(),
                                    # Options for plot download dimensions
                                    fluidRow(
                                      column(width=2,
                                             numericInput(inputId='box_userscores_questions.graphs.plot.plot_width',
                                                          'Saved Plot Width',
                                                          value=8,
                                                          min=5)
                                      ),
                                      column(width=2,
                                             numericInput(inputId='box_userscores_questions.graphs.plot.plot_height',
                                                          'Saved Plot Height',
                                                          value=5,
                                                          min=5)
                                      )
                                    ),
                                    # Download Button for Plot
                                    downloadButton(outputId="downloaduserscores_questions",label="Save Plot as PNG")
                                ) # Box - Plot Output
                              ) # I/O Fluid Layout
                          ), # Box - User Questions Report Graph
                          # Box for User Questions Dotplot
                          box(id='box_userscores_questions.dotplot',
                              title='Cleveland Dot Plots',status='primary',solidHeader = T,width=12,
                              collapsible=T,collapsed=T,
                              # I/O Layout for Plot
                              fluidRow(
                                # Plot Inputs
                                box(width=3,title='Plot Inputs',
                                    tags$div(
                                      style="font-size: 12px;",
                                      # Select Behaviours to Show in Dotplot
                                      selectizeInput(inputId='box_userscores_questions.dotplot.question_select',
                                                     label='Select Behaviours:',
                                                     choices=NULL,
                                                     selected=NULL,
                                                     multiple=T,
                                                     options=list(maxItems=10))
                                      
                                    )
                                ),
                                # Plot Outputs
                                box(title='Plot Output',width=9,
                                    # Sidebar for plot customisation options, change `value` here to change defaults
                                    sidebar=boxSidebar(
                                      id='box_userscores_questions.dotplot.options',
                                      width=75,
                                      tags$div(
                                        style="font-size: 12px;",
                                        fluidRow(
                                          column(11,
                                                 textInput(inputId='box_userscores_questions.dotplot.plot.title',
                                                           value='Average User Scores by Behaviour*',
                                                           'Plot Title'),
                                                 textInput(inputId='box_userscores_questions.dotplot.plot.subtitle',
                                                           value='*White line represents 95% confidence interval',
                                                           'Plot Subtitle'),
                                                 textInput(inputId='box_userscores_questions.dotplot.plot.x_title',
                                                           value='% of the time',
                                                           'X-axis Title'),
                                                 textInput(inputId='box_userscores_questions.dotplot.plot.y_title',
                                                           'Y-axis Title')
                                          )
                                        ),
                                        fluidRow(
                                          
                                          column(6,
                                                 numericInput(inputId='box_userscores_questions.dotplot.plot.data_label',
                                                              'Data Label Text Size',
                                                              value=2.2,
                                                              min=1),
                                                 numericInput(inputId='box_userscores_questions.dotplot.plot.data_auxlabel',
                                                              'Cell Label Text Size',
                                                              value=2.2,
                                                              min=1),
                                                 numericInput(inputId='box_userscores_questions.dotplot.plot.axis_label',
                                                              'Axis Labels Text Size',
                                                              value=10,
                                                              min=6)
                                                 
                                          ),
                                          column(6,
                                                 numericInput(inputId='box_userscores_questions.dotplot.plot.axis_title',
                                                              'Axis Title Text Size',
                                                              value=12,
                                                              min=6),
                                                 numericInput(inputId='box_userscores_questions.dotplot.plot.title_size',
                                                              'Title Text Size',
                                                              value=12,
                                                              min=6),
                                                 numericInput(inputId='box_userscores_questions.dotplot.plot.subtitle_size',
                                                              'Subtitle Text Size',
                                                              value=10,
                                                              min=6)
                                          )
                                        ),
                                        fluidRow(
                                          column(6,
                                                 colourInput(inputId='box_userscores_questions.dotplot.plot.cicolor',
                                                             label='Select CI Color:',
                                                             value='white',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_questions.dotplot.plot.color1',
                                                             label='Select Plot Color 1:',
                                                             value='#23d7d1',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_questions.dotplot.plot.color2',
                                                             label='Select Plot Color 2:',
                                                             value='#1ec9d7',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_questions.dotplot.plot.color3',
                                                             label='Select Plot Color 3:',
                                                             value='#2fbad9',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_questions.dotplot.plot.color4',
                                                             label='Select Plot Color 4:',
                                                             value='#42abd7',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_questions.dotplot.plot.color5',
                                                             label='Select Plot Color 5:',
                                                             value='#529cd1',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_questions.dotplot.plot.color6',
                                                             label='Select Plot Color 6:',
                                                             value='#5e8dc8',
                                                             returnName = T,
                                                             closeOnClick = T)
                                                 
                                          ),
                                          column(6,
                                                 colourInput(inputId='box_userscores_questions.dotplot.plot.color7',
                                                             label='Select Plot Color 1:',
                                                             value='#677fbb',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_questions.dotplot.plot.color8',
                                                             label='Select Plot Color 2:',
                                                             value='#6d71ac',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_questions.dotplot.plot.color9',
                                                             label='Select Plot Color 3:',
                                                             value='#70659b',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_questions.dotplot.plot.color10',
                                                             label='Select Plot Color 4:',
                                                             value='#6f5989',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_questions.dotplot.plot.color11',
                                                             label='Select Plot Color 5:',
                                                             value='#6d4e76',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='box_userscores_questions.dotplot.plot.color12',
                                                             label='Select Plot Color 6:',
                                                             value='#684564',
                                                             returnName = T,
                                                             closeOnClick = T)
                                                 
                                          )
                                        )
                                      )
                                    ),
                                    # Render Plot
                                    plotOutput("box_userscores_questions.dotplot.plot",height='500px'),
                                    br(),
                                    # Generate Plot Button
                                    actionButton(inputId='box_userscores_questions.dotplot.generate',label='Draw Graph'),
                                    br(),
                                    br(),
                                    # Plot Download Dimensions
                                    fluidRow(
                                      column(width=2,
                                             numericInput(inputId='box_userscores_questions.dotplot.plot.plot_width',
                                                          'Saved Plot Width',
                                                          value=8,
                                                          min=5)
                                      ),
                                      column(width=2,
                                             numericInput(inputId='box_userscores_questions.dotplot.plot.plot_height',
                                                          'Saved Plot Height',
                                                          value=8,
                                                          min=8)
                                      )
                                    ),
                                    # Download Button
                                    downloadButton(outputId="downloaduserscores_dotplot",label="Save Plot as PNG")
                                ) # Box - Plot Output
                              ) # I/O Fluid Layout
                          ), # Box - Dotplot Graphs
                          # Box for User Question Summary Table
                          box(id='box_userscores_questions.sum',
                              title='Summary Statistics',status='primary',solidHeader = T,width=12,
                              collapsible=T,
                              div(style = 'overflow-x: scroll',DT::dataTableOutput("userscores_questions.sum")))
                 ), # Tab Panel User Scores by Question
                 
                 
                 # Cells panel
                 tabPanel("Cells",
                          br(),
                          
                          ## Report Graphs
                          box(id = "box_cells_graphs",
                              title = 'Report Graphs', status='primary', solidHeader=T, width = 12,
                              collapsible=T,
                              collapsed=T,
                              # Dropdown showing some information for the user
                              dropdown(
                                p("This box allows you to create bar graphs representing the average score given by reviewer type for each cell. 
                                  Plot customisation is available using the gear icon on the right."),
                                style='unite',icon=icon('circle-question'),
                                animate=animateOptions(enter = "fadeInDown", exit = "fadeOutUp"),
                                size='xs',no_outline=T
                              ),
                              # I/O Fluid Layout
                              fluidRow(
                                # Box for Plot Inputs
                                box(width=3, title='Plot Inputs',
                                    tags$div(
                                      style="font-size: 12px;",
                                      # Select Cells Input
                                      selectizeInput(inputId='cells_graphs.cell_select',
                                                     label='Select cells:',
                                                     choices=NULL,
                                                     selected=NULL,
                                                     multiple=T,
                                                     options=list(maxItems=7)),
                                      # Select Reviewer Types to Show Input
                                      checkboxGroupInput(inputId='cells_graphs.relation_checkbox',
                                                         label='Select Reviewer Types',
                                                         choices=NULL,
                                                         selected=NULL),
                                      # Select Reviewer Types to Hide if <4 Reviewers Input
                                      checkboxGroupInput(inputId='cells_graphs.filter_checkbox',
                                                         label='Select relationships to filter if it has <4 reviewers: ',
                                                         choices=NULL,
                                                         selected=NULL)
                                      
                                    )
                                ),
                                
                                # Plot Output Box
                                box(title='Plot Output',width=9,
                                    # Plot Customisation Options, change `value` here to change defaults
                                    sidebar=boxSidebar(
                                      id='box_cells_graphs.options',
                                      width=75,
                                      tags$div(
                                        style="font-size: 12px;",
                                        fluidRow(
                                          column(11,
                                                 textInput(inputId='cells_graphs.plot.title',
                                                           value='Average Score by Cell and Relationship',
                                                           'Plot Title'),
                                                 textInput(inputId='cells_graphs.plot.x_title',
                                                           value='Average % of the time',
                                                           'X-axis Title'),
                                                 textInput(inputId='cells_graphs.plot.y_title',
                                                           'Y-axis Title')
                                          )
                                        ),
                                        fluidRow(
                                          
                                          column(6,
                                                 numericInput(inputId='cells_graphs.plot.data_label',
                                                              'Data Label Text Size',
                                                              value=3.5,
                                                              min=3.5),
                                                 numericInput(inputId='cells_graphs.plot.axis_label',
                                                              'Axis Labels Text Size',
                                                              value=10,
                                                              min=6),
                                                 numericInput(inputId='cells_graphs.plot.axis_title',
                                                              'Axis Title Text Size',
                                                              value=14,
                                                              min=6),
                                                 numericInput(inputId='cells_graphs.plot.title_size',
                                                              'Title Text Size',
                                                              value=14,
                                                              min=6)
                                          ),
                                          column(6,
                                                 colourInput(inputId='cells_graphs.plot.color1',
                                                             label='Select Plot Color 1:',
                                                             value='#FFD15A',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='cells_graphs.plot.color2',
                                                             label='Select Plot Color 2:',
                                                             value='#2B99BC',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='cells_graphs.plot.color3',
                                                             label='Select Plot Color 3:',
                                                             value='#E17B8D',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='cells_graphs.plot.data_label_color',
                                                             label='Select Data Label Colour:',
                                                             value='white',
                                                             returnName = T,
                                                             closeOnClick = T)
                                          )
                                          
                                        )
                                      )
                                    ),
                                    # Render Plot
                                    plotlyOutput("cells_graphs.plot"),
                                    br(),
                                    # Generate Plot Button
                                    actionButton(inputId='cells_graphs.generate',label='Draw Graph'),
                                    br(),
                                    br(),
                                    # Plot Download Dimensions
                                    fluidRow(
                                      column(width=2,
                                             numericInput(inputId='cells_graphs.plot.plot_width',
                                                          'Saved Plot Width',
                                                          value=8,
                                                          min=5)
                                      ),
                                      column(width=2,
                                             numericInput(inputId='cells_graphs.plot.plot_height',
                                                          'Saved Plot Height',
                                                          value=5,
                                                          min=5)
                                      )
                                    ),
                                    # Download Plot Button
                                    downloadButton(outputId="downloadCells",label="Save Plot as PNG")
                                ) # Box - Plot Output
                              ) # I/O Fluid Layout
                              
                          ), # Box - Report Graphs
                          
                          ## box_cells_other_manager - placeholder for cells.manager_other dataTable                
                          box(id = "box_cells_other_manager",
                              title = "Self vs. Manager & Other", status = "primary", solidHeader = T, width = 12, 
                              collapsible = T, 
                              DT::dataTableOutput("cells.manager_other")
                          ),
                          
                          ## box_cells_other - placeholder for cells.other_only dataTable
                          box(id = "box_cells_other",
                              title = "Self vs. Other", status = "primary", solidHeader = T, width = 12, 
                              collapsible = T, 
                              DT::dataTableOutput("cells.other_only")
                          ),
                          
                          ## box_cells_manager - placeholder for cells.manager_only dataTable
                          box(id = "box_cells_manager",
                              title = "Self vs. Manager", status = "primary", solidHeader = T, width = 12, 
                              collapsible = T, 
                              DT::dataTableOutput("cells.manager_only")
                          ),
                          
                          ## Measures of Dispersion by Self, Other, and Manager
                          box(id = "box_cells_dispersion",
                              title = "Measures of Dispersion", status="primary", solidHeader = TRUE, width = 12,
                              collapsible = TRUE,
                              selectizeInput(inputId='cells_dispersion.relation_select',
                                             label='Relationship to User',
                                             choices=NULL, 
                                             selected=NULL,
                                             multiple=FALSE
                              ),
                              DT::dataTableOutput("cells.dispersion")
                          )
                          
                          
                 ),
                 
                 # Cells & Questions
                 tabPanel("Cells & Questions",
                          br(),
                          
                          ## Report Graphs
                          box(id = "box_questions_graphs",
                              title = 'Report Graphs', status='primary', solidHeader=T, width = 12,
                              collapsible=T,
                              collapsed=T,
                              # Dropdown providing some information for the user
                              dropdown(
                                p("This box allows you to create bar graphs representing the average score given by reviewer type for each behaviour 
                                in the cell selected in Plot Inputs. Plot customisation is available using the gear icon on the right."),
                                style='unite',icon=icon('circle-question'),
                                animate=animateOptions(enter = "fadeInDown", exit = "fadeOutUp"),
                                size='xs',no_outline=T
                              ),
                              # I/O Fluid Layout
                              fluidRow(
                                # Plot Input Boxes
                                box(width=3, title='Plot Inputs',
                                    tags$div(
                                      style="font-size: 12px;",
                                      # Select Cell Input
                                      selectizeInput(inputId='questions_graphs.cell_select',
                                                     label='Select cells:',
                                                     choices=NULL,
                                                     selected=NULL,
                                                     multiple=F),
                                      # Select Questions to Exclude Input
                                      selectizeInput(inputId='questions_graphs.question_exclude',
                                                     label='Select behaviours to exclude:',
                                                     choices=NULL,
                                                     selected=NULL,
                                                     multiple=T),
                                      # Select Reviewer Types to Show Input
                                      checkboxGroupInput(inputId='questions_graphs.relation_checkbox',
                                                         label='Select Reviewer Types',
                                                         choices=NULL,
                                                         selected=NULL),
                                      # Select Reviewer Types to Hide if <4 of that type Input
                                      checkboxGroupInput(inputId='questions_graphs.filter_checkbox',
                                                         label='Select relationships to filter if it has <4 reviewers: ',
                                                         choices=NULL,
                                                         selected=NULL)
                                      
                                    )
                                ),
                                # Plot Output Box
                                box(title='Plot Output',width=9,
                                    # Sidebar for plot customisation options, change `value` here to change defaults
                                    sidebar=boxSidebar(
                                      id='box_questions_graphs.options',
                                      width=75,
                                      tags$div(
                                        style="font-size: 12px;",
                                        fluidRow(
                                          column(11,
                                                 textInput(inputId='questions_graphs.plot.title',
                                                           'Plot Title'),
                                                 textInput(inputId='questions_graphs.plot.subtitle',
                                                           value='Average Score by Behaviour and Relationship',
                                                           'Plot Subtitle'),
                                                 textInput(inputId='questions_graphs.plot.x_title',
                                                           value='Average % of the time',
                                                           'X-axis Title'),
                                                 textInput(inputId='questions_graphs.plot.y_title',
                                                           'Y-axis Title')
                                          )
                                        ),
                                        fluidRow(
                                          
                                          column(6,
                                                 numericInput(inputId='questions_graphs.plot.data_label',
                                                              'Data Label Text Size',
                                                              value=3.5,
                                                              min=3.5),
                                                 numericInput(inputId='questions_graphs.plot.axis_label',
                                                              'Axis Labels Text Size',
                                                              value=10,
                                                              min=6),
                                                 numericInput(inputId='questions_graphs.plot.axis_title',
                                                              'Axis Title Text Size',
                                                              value=14,
                                                              min=6),
                                                 numericInput(inputId='questions_graphs.plot.title_size',
                                                              'Title Text Size',
                                                              value=14,
                                                              min=6),
                                                 numericInput(inputId='questions_graphs.plot.subtitle_size',
                                                              'Subtitle Text Size',
                                                              value=10,
                                                              min=6)
                                          ),
                                          column(6,
                                                 colourInput(inputId='questions_graphs.plot.color1',
                                                             label='Select Plot Color 1:',
                                                             value='#FFD15A',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='questions_graphs.plot.color2',
                                                             label='Select Plot Color 2:',
                                                             value='#2B99BC',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='questions_graphs.plot.color3',
                                                             label='Select Plot Color 3:',
                                                             value='#E17B8D',
                                                             returnName = T,
                                                             closeOnClick = T),
                                                 colourInput(inputId='questions_graphs.plot.data_label_color',
                                                             label='Select Data Label Colour:',
                                                             value='white',
                                                             returnName = T,
                                                             closeOnClick = T)
                                          )
                                          
                                        )
                                      )
                                    ),
                                    # Render Plot
                                    plotlyOutput("questions_graphs.plot"),
                                    br(),
                                    # Generate Plot Button
                                    actionButton(inputId='questions_graphs.generate',label='Draw Graph'),
                                    br(),
                                    br(),
                                    # Plot Download Dimensions
                                    fluidRow(
                                      column(width=2,
                                             numericInput(inputId='questions_graphs.plot.plot_width',
                                                          'Saved Plot Width',
                                                          value=8,
                                                          min=5)
                                      ),
                                      column(width=2,
                                             numericInput(inputId='questions_graphs.plot.plot_height',
                                                          'Saved Plot Height',
                                                          value=5,
                                                          min=5)
                                      )
                                    ),
                                    # Download Plot Button
                                    downloadButton("downloadQuestions",label="Save Plot as PNG")
                                ) # Box- Plot Output
                              ) # I/O Fluid Layout
                              
                          ), # Box - Report Graphs
                          
                          ## box_questions_cells_other_manager - placeholder for cells.questions_manager_other dataTable         
                          box(id = "box_questions_cells_other_manager",
                              title = "Self vs. Manager & Other", 
                              status = "primary", solidHeader = T, width = 12, collapsible = T, 
                              DT::dataTableOutput("cells.questions_manager_other")
                          ),
                          
                          ## box_questions_cells_other - placeholder for cells.questions_other_only dataTable         
                          box(id = "box_questions_cells_other",
                              title = "Self vs. Other", 
                              status = "primary", solidHeader = T, width = 12, collapsible = T, 
                              DT::dataTableOutput("cells.questions_other_only")
                          ),
                          
                          ## box_questions_cells_manager - placeholder for cells.questions_manager_only dataTable         
                          box(id = "box_questions_cells_manager",
                              title = "Self vs. Manager", 
                              status = "primary", solidHeader = T, width = 12, collapsible = T, 
                              DT::dataTableOutput("cells.questions_manager_only")
                          ),
                          
                          ## Measures of Dispersion by Self, Other, and Manager
                          box(id = "box_questions_cells_dispersion",
                              title = "Measures of Dispersion", status="primary", solidHeader = TRUE, width = 12,
                              collapsible = TRUE,
                              selectizeInput(inputId='cells.questions_dispersion.relation_select',
                                             label='Relationship to User',
                                             choices=NULL, 
                                             selected=NULL,
                                             multiple=FALSE
                              ),
                              DT::dataTableOutput("cells.questions_dispersion")
                          )
                          
                 ),
                 
                 
                 
                 # This is the tab for displaying the cell variability scores using density curves.         
                 tabPanel("Variability Graph by Cell",
                          br(),
                          plotlyOutput("cell.variance", height = 0)
                 ),
                 
                 tabPanel("Variability Graph by Question",
                          br(),
                          wellPanel(
                            selectizeInput(inputId='variability.behaviour_select',
                                           label='Select Behaviours',
                                           choices=NULL, 
                                           selected=NULL,
                                           multiple=TRUE,
                                           options=list(maxItems=6)
                            )
                          ),
                          br(),
                          plotlyOutput("question.variance",height=0)
                          
                 ),
                 
                 
                 # This is the tab for displaying Time Series Graphs       
                 tabPanel("Time Series Overall",
                          br(),
                          plotlyOutput("time.series", height = 0)
                 ),
                 
                 tabPanel("Time Series By Cell",
                          br(),
                          plotlyOutput("time.series.by.cell", height = 0)
                 ),
                 
                 tabPanel("Time Series - % Change",
                          br(),
                          box(id = "time.series.overall.change.tbl",
                              title = "Overall % Change", 
                              status = "primary", solidHeader = T, width = 12, collapsible = T,
                              DT::dataTableOutput("time.series.perc.chg.overall")
                          ),
                          box(id = "time.series.cells.change.tbl",
                              title = "% Change by Cell", 
                              status = "primary", solidHeader = T, width = 12, collapsible = T,
                              DT::dataTableOutput("time.series.perc.chg.cells")
                          )
                 ),
                 
                 # Demographic Breakdown Frequency Tab by User and Reviewer
                 tabPanel("Demographics",
                          # Dropdown providing some information for the user
                          dropdown(
                            p("This tab provides a detailed overview of the demographic breakdown of both users and reviewers into 
                              categories of gender, age, sexual orientation, ethnicity, disability, and mental illness. Note, for user 
                              demographics, it is directly related to the data presented in the User Score tabs. So, only shows the 
                              breakdown of demographic categories for all users with 3 or more reviewers."),
                            br(),
                            p("What you can do here:"),
                            tags$div(
                              tags$ul(
                                tags$li(
                                  tags$b('Report Graphs'),
                                  p("Generate a treemap of the categories to visualise the demographic composition. Clicking on draw graphs 
                                    without specifying any groups will use the default categories. You can also specify your own grouping, 
                                    particularly if certain categories are too small to report and need to be collapsed."),
                                  br(),
                                  tags$b("WARNING: Ensure that custom group selections are mutually exclusive or graphs may behave unexpectedly"),
                                  br(),
                                  p("Both missing and 'prefer not to say' values are categorised as 'Missing.' Plot features can be 
                                    customised by clicking on the gear icon on the right."),
                                  br(),
                                ),
                                tags$li(
                                  tags$b('Frequency Table'),
                                  p('This table shows the number of unique users/reviewers for each category and their respective 
                                    percentage of the total. Note, that for user frequencies, it is directly connected to the data in 
                                    the User Score tabs and only reports on users who have received feedback from at least 3 reviewers'),
                                )
                              )
                            ),
                            style='unite',icon=icon('circle-question'),
                            animate=animateOptions(enter = "fadeInDown", exit = "fadeOutUp"),
                            size='xs',no_outline=T
                          ),
                          br(),
                          # Box to hold all reviewer demographic boxes
                          box(
                            id='reviewer_demographics',
                            title='Reviewer Demographics',
                            status='primary',solidHeader = T, width=12, collapsible=T,
                            collapsed=T,
                            # Box for Reviewer Gender Identity
                            box(id = "reviewer_demographics_gender",
                                title = "Reviewer Gender Identity", 
                                status = "primary", solidHeader = T, width = 12, collapsible = T,
                                collapsed=T,
                                fluidRow(
                                  # Box to hold the graph output
                                  box(id='reviewer_demographics_gender.graph.box',
                                      collapsible=T,
                                      collapsed=T,
                                      width=12,
                                      title='Report Graph',
                                      # Sidebar for plot customisation, change `value` here to change defaults
                                      sidebar=boxSidebar(
                                        id='reviewer_demographics_gender.graph.options',
                                        width=50,
                                        tags$div(
                                          style="font-size: 12px;",
                                          fluidRow(
                                            column(11,
                                                   textInput(inputId='reviewer_demographics_gender.graph.plot.title',
                                                             value='Reviewer Gender Demographics',
                                                             'Plot Title')
                                            )
                                          ),
                                          fluidRow(
                                            
                                            column(4,
                                                   numericInput(inputId='reviewer_demographics_gender.graph.plot.data_label',
                                                                'Data Label Text Size',
                                                                value=14,
                                                                min=3),
                                                   numericInput(inputId='reviewer_demographics_gender.graph.plot.title_size',
                                                                'Title Text Size',
                                                                value=14,
                                                                min=6),
                                                   colourInput(inputId='reviewer_demographics_gender.graph.plot.data_label_color',
                                                               label='Select Data Label Colour:',
                                                               value='black',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            ),
                                            column(4,
                                                   colourInput(inputId='reviewer_demographics_gender.graph.plot.color1',
                                                               label='Select Plot Color 1:',
                                                               value='#E69F00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_gender.graph.plot.color2',
                                                               label='Select Plot Color 2:',
                                                               value='#56B4E9',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_gender.graph.plot.color3',
                                                               label='Select Plot Color 3:',
                                                               value='#009E73',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_gender.graph.plot.color4',
                                                               label='Select Plot Color 4:',
                                                               value='#F0E442',
                                                               returnName = T,
                                                               closeOnClick = T)
                                                   
                                            ),
                                            column(4,
                                                   colourInput(inputId='reviewer_demographics_gender.graph.plot.color5',
                                                               label='Select Plot Color 5:',
                                                               value='#D55E00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_gender.graph.plot.color6',
                                                               label='Select Plot Color 6:',
                                                               value='#CC79A7',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_gender.graph.plot.color7',
                                                               label='Select Plot Color 7:',
                                                               value='#FFCCFF',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_gender.graph.plot.color8',
                                                               label='Select Plot Color 8:',
                                                               value='maroon',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            )
                                            
                                          )
                                        )
                                      ),
                                      # I/O Fluid Layout
                                      fluidRow(
                                        # Plot Options
                                        column(3,
                                               # Plot Inputs
                                               fluidRow(
                                                 # Select Custom Grouping Input
                                                 column(width=6,
                                                        selectizeInput(inputId='reviewer_demographics_gender.graph.selectgroup1',
                                                                       label='Select Group 1',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_gender.graph.selectgroup2',
                                                                       label='Select Group 2',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_gender.graph.selectgroup3',
                                                                       label='Select Group 3',choices=NULL,
                                                                       multiple=T)     
                                                 ),
                                                 # Custom Group Labels Input
                                                 column(width=6,
                                                        textInput(inputId='reviewer_demographics_gender.graph.group1label',
                                                                  value='Group 1',
                                                                  'Group 1 Label'),
                                                        textInput(inputId='reviewer_demographics_gender.graph.group2label',
                                                                  value='Group 2',
                                                                  'Group 2 Label'),
                                                        textInput(inputId='reviewer_demographics_gender.graph.group3label',
                                                                  value='Group 3',
                                                                  'Group 3 Label')
                                                 )
                                               ),
                                               
                                               br(),
                                               # Generate Plot Button
                                               actionButton(inputId='reviewer_demographics_gender.graph.plot.generate',label='Draw Graph'),
                                               br(),
                                               # Plot Download Dimensions
                                               fluidRow(
                                                 column(width=6,
                                                        numericInput(inputId='reviewer_demographics_gender.graph.plot_width',
                                                                     'Saved Plot Width',
                                                                     value=8,
                                                                     min=5)
                                                 ),
                                                 column(width=6,
                                                        numericInput(inputId='reviewer_demographics_gender.graph.plot_height',
                                                                     'Saved Plot Height',
                                                                     value=8,
                                                                     min=5)
                                                 )
                                               )
                                        ), # Plot Options Column
                                        # Plot Outputs
                                        column(9,
                                               # Render Plot
                                               plotOutput('reviewer_demographics_gender.graph.plot'),
                                               
                                               br(),
                                               # Download Plot Button
                                               downloadButton("reviewer_demographics_gender.graph.download",label="Save Plot as PNG")
                                               
                                        )
                                      ) # I/O Fluid Layout
                                      
                                  ) # Box-Report Graph
                                ), # Fluid Row Report Graph
                                # Summary Data Table
                                DT::dataTableOutput("reviewer_demo_gender_tbl")
                                
                            ), # Box- Reviewer Gender Identity
                            # Box for Reviewer Age
                            box(id = "reviewer_demographics_age",
                                title = "Reviewer Age",
                                status = "primary", solidHeader = T, width = 12, collapsible = T,
                                collapsed=T,
                                fluidRow(
                                  # Report Graph Box
                                  box(id='reviewer_demographics_age.graph.box',
                                      collapsible=T,
                                      collapsed=T,
                                      width=12,
                                      title='Report Graph',
                                      # Sidebar for plot customisation options, change `value` here to change defaults
                                      sidebar=boxSidebar(
                                        id='reviewer_demographics_age.graph.options',
                                        width=50,
                                        tags$div(
                                          style="font-size: 12px;",
                                          fluidRow(
                                            column(11,
                                                   textInput(inputId='reviewer_demographics_age.graph.plot.title',
                                                             value='Reviewer Age Demographics',
                                                             'Plot Title')
                                            )
                                          ),
                                          fluidRow(
                                            
                                            column(4,
                                                   numericInput(inputId='reviewer_demographics_age.graph.plot.data_label',
                                                                'Data Label Text Size',
                                                                value=14,
                                                                min=3),
                                                   numericInput(inputId='reviewer_demographics_age.graph.plot.title_size',
                                                                'Title Text Size',
                                                                value=14,
                                                                min=6),
                                                   colourInput(inputId='reviewer_demographics_age.graph.plot.data_label_color',
                                                               label='Select Data Label Colour:',
                                                               value='black',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            ),
                                            column(4,
                                                   colourInput(inputId='reviewer_demographics_age.graph.plot.color1',
                                                               label='Select Plot Color 1:',
                                                               value='#E69F00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_age.graph.plot.color2',
                                                               label='Select Plot Color 2:',
                                                               value='#56B4E9',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_age.graph.plot.color3',
                                                               label='Select Plot Color 3:',
                                                               value='#009E73',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_age.graph.plot.color4',
                                                               label='Select Plot Color 4:',
                                                               value='#F0E442',
                                                               returnName = T,
                                                               closeOnClick = T)
                                                   
                                            ),
                                            column(4,
                                                   colourInput(inputId='reviewer_demographics_age.graph.plot.color5',
                                                               label='Select Plot Color 5:',
                                                               value='#D55E00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_age.graph.plot.color6',
                                                               label='Select Plot Color 6:',
                                                               value='#CC79A7',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_age.graph.plot.color7',
                                                               label='Select Plot Color 7:',
                                                               value='#FFCCFF',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_age.graph.plot.color8',
                                                               label='Select Plot Color 8:',
                                                               value='maroon',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            )
                                            
                                          )
                                        )
                                      ),
                                      # I/O Flulid Layout
                                      fluidRow(
                                        # Plot Inputs
                                        column(3,
                                               fluidRow(
                                                 # Select Custom Group Input
                                                 column(width=6,
                                                        selectizeInput(inputId='reviewer_demographics_age.graph.selectgroup1',
                                                                       label='Select Group 1',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_age.graph.selectgroup2',
                                                                       label='Select Group 2',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_age.graph.selectgroup3',
                                                                       label='Select Group 3',choices=NULL,
                                                                       multiple=T)     
                                                 ),
                                                 # Custom Group Label Input
                                                 column(width=6,
                                                        textInput(inputId='reviewer_demographics_age.graph.group1label',
                                                                  value='Group 1',
                                                                  'Group 1 Label'),
                                                        textInput(inputId='reviewer_demographics_age.graph.group2label',
                                                                  value='Group 2',
                                                                  'Group 2 Label'),
                                                        textInput(inputId='reviewer_demographics_age.graph.group3label',
                                                                  value='Group 3',
                                                                  'Group 3 Label')
                                                 )
                                               ),
                                               
                                               br(),
                                               # Generate Plot Button
                                               actionButton(inputId='reviewer_demographics_age.graph.plot.generate',label='Draw Graph'),
                                               br(),
                                               # Plot Download Options
                                               fluidRow(
                                                 column(width=6,
                                                        numericInput(inputId='reviewer_demographics_age.graph.plot_width',
                                                                     'Saved Plot Width',
                                                                     value=8,
                                                                     min=5)
                                                 ),
                                                 column(width=6,
                                                        numericInput(inputId='reviewer_demographics_age.graph.plot_height',
                                                                     'Saved Plot Height',
                                                                     value=8,
                                                                     min=5)
                                                 )
                                               )
                                        ), # Plot Input
                                        # Plot Output
                                        column(9,
                                               plotOutput('reviewer_demographics_age.graph.plot'),
                                               
                                               br(),
                                               downloadButton("reviewer_demographics_age.graph.download",label="Save Plot as PNG")
                                               
                                        ) # Plot Output
                                      ) # I/O Fluid Layout
                                      
                                  ) # Box - Report Graph
                                ),
                                # Summary Data Table
                                DT::dataTableOutput("reviewer_demo_age_tbl")
                            ), # Box - Reviewer Age
                            # Box for Reviewer Sexual Orientation
                            box(id = "reviewer_demographics_sexuality",
                                title = "Reviewer Sexuality", status = "primary", solidHeader = T, width = 12, 
                                collapsible = T,
                                collapsed=T,
                                fluidRow(
                                  # Report Graph Box
                                  box(id='reviewer_demographics_sexuality.graph.box',
                                      collapsible=T,
                                      collapsed=T,
                                      width=12,
                                      title='Report Graph',
                                      # Sidebar for plot customisation options, change `value` here to change defaults
                                      sidebar=boxSidebar(
                                        id='reviewer_demographics_sexuality.graph.options',
                                        width=50,
                                        tags$div(
                                          style="font-size: 12px;",
                                          fluidRow(
                                            column(11,
                                                   textInput(inputId='reviewer_demographics_sexuality.graph.plot.title',
                                                             value='Reviewer Sexual Orientation Demographics',
                                                             'Plot Title')
                                            )
                                          ),
                                          fluidRow(
                                            
                                            column(4,
                                                   numericInput(inputId='reviewer_demographics_sexuality.graph.plot.data_label',
                                                                'Data Label Text Size',
                                                                value=14,
                                                                min=3),
                                                   numericInput(inputId='reviewer_demographics_sexuality.graph.plot.title_size',
                                                                'Title Text Size',
                                                                value=14,
                                                                min=6),
                                                   colourInput(inputId='reviewer_demographics_sexuality.graph.plot.data_label_color',
                                                               label='Select Data Label Colour:',
                                                               value='black',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            ),
                                            column(4,
                                                   colourInput(inputId='reviewer_demographics_sexuality.graph.plot.color1',
                                                               label='Select Plot Color 1:',
                                                               value='#E69F00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_sexuality.graph.plot.color2',
                                                               label='Select Plot Color 2:',
                                                               value='#56B4E9',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_sexuality.graph.plot.color3',
                                                               label='Select Plot Color 3:',
                                                               value='#009E73',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_sexuality.graph.plot.color4',
                                                               label='Select Plot Color 4:',
                                                               value='#F0E442',
                                                               returnName = T,
                                                               closeOnClick = T)
                                                   
                                            ),
                                            column(4,
                                                   colourInput(inputId='reviewer_demographics_sexuality.graph.plot.color5',
                                                               label='Select Plot Color 5:',
                                                               value='#D55E00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_sexuality.graph.plot.color6',
                                                               label='Select Plot Color 6:',
                                                               value='#CC79A7',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_sexuality.graph.plot.color7',
                                                               label='Select Plot Color 7:',
                                                               value='#FFCCFF',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_sexuality.graph.plot.color8',
                                                               label='Select Plot Color 8:',
                                                               value='maroon',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            )
                                            
                                          )
                                        )
                                      ),
                                      # I/O Fluid Layout
                                      fluidRow(
                                        # Plot Inputs
                                        column(3,
                                               fluidRow(
                                                 # Select Custom Groups Input
                                                 column(width=6,
                                                        selectizeInput(inputId='reviewer_demographics_sexuality.graph.selectgroup1',
                                                                       label='Select Group 1',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_sexuality.graph.selectgroup2',
                                                                       label='Select Group 2',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_sexuality.graph.selectgroup3',
                                                                       label='Select Group 3',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_sexuality.graph.selectgroup4',
                                                                       label='Select Group 4',choices=NULL,
                                                                       multiple=T)
                                                 ),
                                                 # Custom Group Labels Input
                                                 column(width=6,
                                                        textInput(inputId='reviewer_demographics_sexuality.graph.group1label',
                                                                  value='Group 1',
                                                                  'Group 1 Label'),
                                                        textInput(inputId='reviewer_demographics_sexuality.graph.group2label',
                                                                  value='Group 2',
                                                                  'Group 2 Label'),
                                                        textInput(inputId='reviewer_demographics_sexuality.graph.group3label',
                                                                  value='Group 3',
                                                                  'Group 3 Label'),
                                                        textInput(inputId='reviewer_demographics_sexuality.graph.group4label',
                                                                  value='Group 4',
                                                                  'Group 4 Label')
                                                 )
                                               ),
                                               
                                               br(),
                                               # Generate Plot Button
                                               actionButton(inputId='reviewer_demographics_sexuality.graph.plot.generate',label='Draw Graph'),
                                               br(),
                                               # Plot Download Dimensions
                                               fluidRow(
                                                 column(width=6,
                                                        numericInput(inputId='reviewer_demographics_sexuality.graph.plot_width',
                                                                     'Saved Plot Width',
                                                                     value=8,
                                                                     min=5)
                                                 ),
                                                 column(width=6,
                                                        numericInput(inputId='reviewer_demographics_sexuality.graph.plot_height',
                                                                     'Saved Plot Height',
                                                                     value=8,
                                                                     min=5)
                                                 )
                                               )
                                        ), # Plot Inputs
                                        # Plot Outputs
                                        column(9,
                                               # Render Plot
                                               plotOutput('reviewer_demographics_sexuality.graph.plot'),
                                               
                                               br(),
                                               # Download Plot Button
                                               downloadButton("reviewer_demographics_sexuality.graph.download",label="Save Plot as PNG")
                                               
                                        ) # Plot Output
                                      ) # I/O Fluid Layout
                                      
                                  ) # Box - Report Graph
                                ),
                                # Summary Data Table
                                DT::dataTableOutput("reviewer_demo_sexuality_tbl")
                            ), # Box - Reviewer Sexual Orientation
                            # Box for Reviewer Ethnicity
                            box(id = "reviewer_demographics_ethnicity",
                                title = "Reviewer Ethnicity", 
                                p("Honeycomb users could select more than one ethnicity, and 
                                 employees who did so are counted once for each selected ethnicity 
                                 in the table below. Users who selected multiple ethnic identities 
                                 are counted once, as multiethnic."),
                                status = "primary", solidHeader = T, width = 12, collapsible = T,
                                collapsed=T,
                                fluidRow(
                                  # Report Graph Box
                                  box(id='reviewer_demographics_ethnicity.graph.box',
                                      collapsible=T,
                                      collapsed=T,
                                      width=12,
                                      title='Report Graph',
                                      # Sidebar for plot customisation options, change `value` here to change defaults
                                      sidebar=boxSidebar(
                                        id='reviewer_demographics_ethnicity.graph.options',
                                        width=50,
                                        tags$div(
                                          style="font-size: 12px;",
                                          fluidRow(
                                            column(11,
                                                   textInput(inputId='reviewer_demographics_ethnicity.graph.plot.title',
                                                             value='Reviewer Ethnicity Demographics',
                                                             'Plot Title')
                                            )
                                          ),
                                          fluidRow(
                                            
                                            column(4,
                                                   numericInput(inputId='reviewer_demographics_ethnicity.graph.plot.data_label',
                                                                'Data Label Text Size',
                                                                value=14,
                                                                min=3),
                                                   numericInput(inputId='reviewer_demographics_ethnicity.graph.plot.title_size',
                                                                'Title Text Size',
                                                                value=14,
                                                                min=6),
                                                   colourInput(inputId='reviewer_demographics_ethnicity.graph.plot.data_label_color',
                                                               label='Select Data Label Colour:',
                                                               value='black',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            ),
                                            column(4,
                                                   colourInput(inputId='reviewer_demographics_ethnicity.graph.plot.color1',
                                                               label='Select Plot Color 1:',
                                                               value='#E69F00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_ethnicity.graph.plot.color2',
                                                               label='Select Plot Color 2:',
                                                               value='#56B4E9',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_ethnicity.graph.plot.color3',
                                                               label='Select Plot Color 3:',
                                                               value='#009E73',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_ethnicity.graph.plot.color4',
                                                               label='Select Plot Color 4:',
                                                               value='#F0E442',
                                                               returnName = T,
                                                               closeOnClick = T)
                                                   
                                            ),
                                            column(4,
                                                   colourInput(inputId='reviewer_demographics_ethnicity.graph.plot.color5',
                                                               label='Select Plot Color 5:',
                                                               value='#D55E00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_ethnicity.graph.plot.color6',
                                                               label='Select Plot Color 6:',
                                                               value='#CC79A7',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_ethnicity.graph.plot.color7',
                                                               label='Select Plot Color 7:',
                                                               value='#FFCCFF',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_ethnicity.graph.plot.color8',
                                                               label='Select Plot Color 8:',
                                                               value='maroon',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            )
                                            
                                          )
                                        )
                                      ),
                                      # I/O Fluid Layout
                                      fluidRow(
                                        # Plot Inputs
                                        column(3,
                                               fluidRow(
                                                 # Select Custom Group Input
                                                 column(width=6,
                                                        selectizeInput(inputId='reviewer_demographics_ethnicity.graph.selectgroup1',
                                                                       label='Select Group 1',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_ethnicity.graph.selectgroup2',
                                                                       label='Select Group 2',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_ethnicity.graph.selectgroup3',
                                                                       label='Select Group 3',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_ethnicity.graph.selectgroup4',
                                                                       label='Select Group 4',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_ethnicity.graph.selectgroup5',
                                                                       label='Select Group 5',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_ethnicity.graph.selectgroup6',
                                                                       label='Select Group 6',choices=NULL,
                                                                       multiple=T)
                                                 ),
                                                 # Custom Group Label Input
                                                 column(width=6,
                                                        textInput(inputId='reviewer_demographics_ethnicity.graph.group1label',
                                                                  value='Group 1',
                                                                  'Group 1 Label'),
                                                        textInput(inputId='reviewer_demographics_ethnicity.graph.group2label',
                                                                  value='Group 2',
                                                                  'Group 2 Label'),
                                                        textInput(inputId='reviewer_demographics_ethnicity.graph.group3label',
                                                                  value='Group 3',
                                                                  'Group 3 Label'),
                                                        textInput(inputId='reviewer_demographics_ethnicity.graph.group4label',
                                                                  value='Group 4',
                                                                  'Group 4 Label'),
                                                        textInput(inputId='reviewer_demographics_ethnicity.graph.group5label',
                                                                  value='Group 5',
                                                                  'Group 5 Label'),
                                                        textInput(inputId='reviewer_demographics_ethnicity.graph.group6label',
                                                                  value='Group 6',
                                                                  'Group 6 Label')
                                                 )
                                               ),
                                               
                                               br(),
                                               # Generate Plot Button
                                               actionButton(inputId='reviewer_demographics_ethnicity.graph.plot.generate',label='Draw Graph'),
                                               br(),
                                               # Plot Download Dimensions
                                               fluidRow(
                                                 column(width=6,
                                                        numericInput(inputId='reviewer_demographics_ethnicity.graph.plot_width',
                                                                     'Saved Plot Width',
                                                                     value=8,
                                                                     min=5)
                                                 ),
                                                 column(width=6,
                                                        numericInput(inputId='reviewer_demographics_ethnicity.graph.plot_height',
                                                                     'Saved Plot Height',
                                                                     value=8,
                                                                     min=5)
                                                 )
                                               )
                                        ), # Plot Input
                                        # Plot Outputs
                                        column(9,
                                               # Render Plot
                                               plotOutput('reviewer_demographics_ethnicity.graph.plot'),
                                               
                                               br(),
                                               # Plot Download Button
                                               downloadButton("reviewer_demographics_ethnicity.graph.download",label="Save Plot as PNG")
                                               
                                        ) # Plot Output
                                      ) # I/O Fluid Layout
                                      
                                  ) # Box - Report Graph
                                ),
                                # Summary Data Table
                                DT::dataTableOutput("reviewer_demo_ethnicity_tbl")
                            ), # Box - Reviewer Ethnicity
                            # Box for Reviewer Ethnicity (Collapsed)
                            box(id = "reviewer_demographics_ethnicity_collapsed",
                                title = "Reviewer Ethnicity - Collapsed", 
                                p("In this table, ethnicity is collapsed to fewer categories, 
                                 and reviewers who selected more than one ethnic category are only 
                                 counted as multiethnic."),
                                status = "primary", solidHeader = T, width = 12, collapsible = T,
                                collapsed=T,
                                fluidRow(
                                  # Report Graph Box
                                  box(id='reviewer_demographics_ethnicity_collapsed.graph.box',
                                      collapsible=T,
                                      collapsed=T,
                                      width=12,
                                      title='Report Graph',
                                      # Sidebar for plot customisation, change `value` here to change defaults
                                      sidebar=boxSidebar(
                                        id='reviewer_demographics_ethnicity_collapsed.graph.options',
                                        width=50,
                                        tags$div(
                                          style="font-size: 12px;",
                                          fluidRow(
                                            column(11,
                                                   textInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.plot.title',
                                                             value='Reviewer Ethnicity (Collapsed) Demographics',
                                                             'Plot Title')
                                            )
                                          ),
                                          fluidRow(
                                            
                                            column(4,
                                                   numericInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.plot.data_label',
                                                                'Data Label Text Size',
                                                                value=14,
                                                                min=3),
                                                   numericInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.plot.title_size',
                                                                'Title Text Size',
                                                                value=14,
                                                                min=6),
                                                   colourInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.plot.data_label_color',
                                                               label='Select Data Label Colour:',
                                                               value='black',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            ),
                                            column(4,
                                                   colourInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.plot.color1',
                                                               label='Select Plot Color 1:',
                                                               value='#E69F00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.plot.color2',
                                                               label='Select Plot Color 2:',
                                                               value='#56B4E9',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.plot.color3',
                                                               label='Select Plot Color 3:',
                                                               value='#009E73',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.plot.color4',
                                                               label='Select Plot Color 4:',
                                                               value='#F0E442',
                                                               returnName = T,
                                                               closeOnClick = T)
                                                   
                                            ),
                                            column(4,
                                                   colourInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.plot.color5',
                                                               label='Select Plot Color 5:',
                                                               value='#D55E00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.plot.color6',
                                                               label='Select Plot Color 6:',
                                                               value='#CC79A7',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.plot.color7',
                                                               label='Select Plot Color 7:',
                                                               value='#FFCCFF',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.plot.color8',
                                                               label='Select Plot Color 8:',
                                                               value='maroon',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            )
                                            
                                          )
                                        )
                                      ),
                                      # I/O Fluid Layout
                                      fluidRow(
                                        # Plot Inputs
                                        column(3,
                                               fluidRow(
                                                 # Select Custom Group Input
                                                 column(width=6,
                                                        selectizeInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.selectgroup1',
                                                                       label='Select Group 1',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.selectgroup2',
                                                                       label='Select Group 2',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.selectgroup3',
                                                                       label='Select Group 3',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.selectgroup4',
                                                                       label='Select Group 4',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.selectgroup5',
                                                                       label='Select Group 5',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.selectgroup6',
                                                                       label='Select Group 6',choices=NULL,
                                                                       multiple=T)
                                                 ),
                                                 # Custom Group Label Input
                                                 column(width=6,
                                                        textInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.group1label',
                                                                  value='Group 1',
                                                                  'Group 1 Label'),
                                                        textInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.group2label',
                                                                  value='Group 2',
                                                                  'Group 2 Label'),
                                                        textInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.group3label',
                                                                  value='Group 3',
                                                                  'Group 3 Label'),
                                                        textInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.group4label',
                                                                  value='Group 4',
                                                                  'Group 4 Label'),
                                                        textInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.group5label',
                                                                  value='Group 5',
                                                                  'Group 5 Label'),
                                                        textInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.group6label',
                                                                  value='Group 6',
                                                                  'Group 6 Label')
                                                 )
                                               ),
                                               
                                               br(),
                                               # Generate Plot Button
                                               actionButton(inputId='reviewer_demographics_ethnicity_collapsed.graph.plot.generate',label='Draw Graph'),
                                               br(),
                                               # Plot Download Dimensions
                                               fluidRow(
                                                 column(width=6,
                                                        numericInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.plot_width',
                                                                     'Saved Plot Width',
                                                                     value=8,
                                                                     min=5)
                                                 ),
                                                 column(width=6,
                                                        numericInput(inputId='reviewer_demographics_ethnicity_collapsed.graph.plot_height',
                                                                     'Saved Plot Height',
                                                                     value=8,
                                                                     min=5)
                                                 )
                                               )
                                        ), # Plot Inputs
                                        # Plot Outputs
                                        column(9,
                                               plotOutput('reviewer_demographics_ethnicity_collapsed.graph.plot'),
                                               
                                               br(),
                                               downloadButton("reviewer_demographics_ethnicity_collapsed.graph.download",label="Save Plot as PNG")
                                               
                                        ) # Plot Outputs
                                      ) #I/O Fluid Layout
                                      
                                  ) # Box - Report Graphs
                                ),
                                # Summary Data Table
                                DT::dataTableOutput("reviewer_demo_ethnicity_collapsed_tbl")
                            ), # Box - Reviewer Ethnicity (Collapsed)
                            # Box for Reviewer Disability
                            box(id = "reviewer_demographics_disability",
                                title = "Reviewer Disability", 
                                status = "primary", solidHeader = T, width = 12, collapsible = T,
                                collapsed=T,
                                fluidRow(
                                  # Report Graph Box
                                  box(id='reviewer_demographics_disability.graph.box',
                                      collapsible=T,
                                      collapsed=T,
                                      width=12,
                                      title='Report Graph',
                                      # Sidebar for plot customisation, change `value` here to change defaults
                                      sidebar=boxSidebar(
                                        id='reviewer_demographics_disability.graph.options',
                                        width=50,
                                        tags$div(
                                          style="font-size: 12px;",
                                          fluidRow(
                                            column(11,
                                                   textInput(inputId='reviewer_demographics_disability.graph.plot.title',
                                                             value='Reviewer Disability Demographics',
                                                             'Plot Title')
                                            )
                                          ),
                                          fluidRow(
                                            
                                            column(4,
                                                   numericInput(inputId='reviewer_demographics_disability.graph.plot.data_label',
                                                                'Data Label Text Size',
                                                                value=14,
                                                                min=3),
                                                   numericInput(inputId='reviewer_demographics_disability.graph.plot.title_size',
                                                                'Title Text Size',
                                                                value=14,
                                                                min=6),
                                                   colourInput(inputId='reviewer_demographics_disability.graph.plot.data_label_color',
                                                               label='Select Data Label Colour:',
                                                               value='black',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            ),
                                            column(4,
                                                   colourInput(inputId='reviewer_demographics_disability.graph.plot.color1',
                                                               label='Select Plot Color 1:',
                                                               value='#E69F00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_disability.graph.plot.color2',
                                                               label='Select Plot Color 2:',
                                                               value='#56B4E9',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_disability.graph.plot.color3',
                                                               label='Select Plot Color 3:',
                                                               value='#009E73',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_disability.graph.plot.color4',
                                                               label='Select Plot Color 4:',
                                                               value='#F0E442',
                                                               returnName = T,
                                                               closeOnClick = T)
                                                   
                                            ),
                                            column(4,
                                                   colourInput(inputId='reviewer_demographics_disability.graph.plot.color5',
                                                               label='Select Plot Color 5:',
                                                               value='#D55E00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_disability.graph.plot.color6',
                                                               label='Select Plot Color 6:',
                                                               value='#CC79A7',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_disability.graph.plot.color7',
                                                               label='Select Plot Color 7:',
                                                               value='#FFCCFF',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_disability.graph.plot.color8',
                                                               label='Select Plot Color 8:',
                                                               value='maroon',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            )
                                            
                                          )
                                        )
                                      ),
                                      # I/O Fluid Layout
                                      fluidRow(
                                        # Plot Inputs
                                        column(3,
                                               fluidRow(
                                                 # Select Custom Group Inputs
                                                 column(width=6,
                                                        selectizeInput(inputId='reviewer_demographics_disability.graph.selectgroup1',
                                                                       label='Select Group 1',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_disability.graph.selectgroup2',
                                                                       label='Select Group 2',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_disability.graph.selectgroup3',
                                                                       label='Select Group 3',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='reviewer_demographics_disability.graph.selectgroup4',
                                                                       label='Select Group 4',choices=NULL,
                                                                       multiple=T)
                                                 ),
                                                 # Custom Group Label Input
                                                 column(width=6,
                                                        textInput(inputId='reviewer_demographics_disability.graph.group1label',
                                                                  value='Group 1',
                                                                  'Group 1 Label'),
                                                        textInput(inputId='reviewer_demographics_disability.graph.group2label',
                                                                  value='Group 2',
                                                                  'Group 2 Label'),
                                                        textInput(inputId='reviewer_demographics_disability.graph.group3label',
                                                                  value='Group 3',
                                                                  'Group 3 Label'),
                                                        textInput(inputId='reviewer_demographics_disability.graph.group4label',
                                                                  value='Group 4',
                                                                  'Group 4 Label')
                                                 )
                                               ),
                                               
                                               br(),
                                               # Generate Plot Button
                                               actionButton(inputId='reviewer_demographics_disability.graph.plot.generate',label='Draw Graph'),
                                               br(),
                                               # Plot Download Dimensions
                                               fluidRow(
                                                 column(width=6,
                                                        numericInput(inputId='reviewer_demographics_disability.graph.plot_width',
                                                                     'Saved Plot Width',
                                                                     value=8,
                                                                     min=5)
                                                 ),
                                                 column(width=6,
                                                        numericInput(inputId='reviewer_demographics_disability.graph.plot_height',
                                                                     'Saved Plot Height',
                                                                     value=8,
                                                                     min=5)
                                                 )
                                               )
                                        ), # Plot Inputs
                                        # Plot Outputs
                                        column(9,
                                               # Render Plot
                                               plotOutput('reviewer_demographics_disability.graph.plot'),
                                               
                                               br(),
                                               # Plot Download Button
                                               downloadButton("reviewer_demographics_disability.graph.download",label="Save Plot as PNG")
                                               
                                        ) # Plot Outputs
                                      ) # I/O Fluid Layout
                                      
                                  ) # Box - Report Graph
                                ), 
                                # Summary Data Table
                                DT::dataTableOutput("reviewer_demo_disability_tbl")
                            ), # Box - Reviewer Disability
                            # Box for Reviewer Mental Illness
                            box(id = "reviewer_demographics_mental_illness",
                                title = "Reviewer Mental Illness",
                                status = "primary", solidHeader = T, width = 12, collapsible = T,
                                collapsed=T,
                                fluidRow(
                                  # Report Graph Box
                                  box(id='reviewer_demographics_mental_illness.graph.box',
                                      collapsible=T,
                                      collapsed=T,
                                      width=12,
                                      title='Report Graph',
                                      # Sidebar for plot customisation, change `value` here to change defaults
                                      sidebar=boxSidebar(
                                        id='reviewer_demographics_mental_illness.graph.options',
                                        width=50,
                                        tags$div(
                                          style="font-size: 12px;",
                                          fluidRow(
                                            column(11,
                                                   textInput(inputId='reviewer_demographics_mental_illness.graph.plot.title',
                                                             value='Reviewer Mental Illness Demographics',
                                                             'Plot Title')
                                            )
                                          ),
                                          fluidRow(
                                            
                                            column(4,
                                                   numericInput(inputId='reviewer_demographics_mental_illness.graph.plot.data_label',
                                                                'Data Label Text Size',
                                                                value=14,
                                                                min=3),
                                                   numericInput(inputId='reviewer_demographics_mental_illness.graph.plot.title_size',
                                                                'Title Text Size',
                                                                value=14,
                                                                min=6),
                                                   colourInput(inputId='reviewer_demographics_mental_illness.graph.plot.data_label_color',
                                                               label='Select Data Label Colour:',
                                                               value='black',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            ),
                                            column(4,
                                                   colourInput(inputId='reviewer_demographics_mental_illness.graph.plot.color1',
                                                               label='Select Plot Color 1:',
                                                               value='#E69F00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_mental_illness.graph.plot.color2',
                                                               label='Select Plot Color 2:',
                                                               value='#56B4E9',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_mental_illness.graph.plot.color3',
                                                               label='Select Plot Color 3:',
                                                               value='#009E73',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_mental_illness.graph.plot.color4',
                                                               label='Select Plot Color 4:',
                                                               value='#F0E442',
                                                               returnName = T,
                                                               closeOnClick = T)
                                                   
                                            ),
                                            column(4,
                                                   colourInput(inputId='reviewer_demographics_mental_illness.graph.plot.color5',
                                                               label='Select Plot Color 5:',
                                                               value='#D55E00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_mental_illness.graph.plot.color6',
                                                               label='Select Plot Color 6:',
                                                               value='#CC79A7',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_mental_illness.graph.plot.color7',
                                                               label='Select Plot Color 7:',
                                                               value='#FFCCFF',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='reviewer_demographics_mental_illness.graph.plot.color8',
                                                               label='Select Plot Color 8:',
                                                               value='maroon',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            )
                                            
                                          )
                                        )
                                      ),
                                      # I/O Fluid Layout
                                      fluidRow(
                                        # Plot Inputs
                                        column(3,
                                               br(),
                                               # Generate Plot Button
                                               actionButton(inputId='reviewer_demographics_mental_illness.graph.plot.generate',label='Draw Graph'),
                                               br(),
                                               # Plot Download Dimensions
                                               fluidRow(
                                                 column(width=6,
                                                        numericInput(inputId='reviewer_demographics_mental_illness.graph.plot_width',
                                                                     'Saved Plot Width',
                                                                     value=8,
                                                                     min=5)
                                                 ),
                                                 column(width=6,
                                                        numericInput(inputId='reviewer_demographics_mental_illness.graph.plot_height',
                                                                     'Saved Plot Height',
                                                                     value=8,
                                                                     min=5)
                                                 )
                                               )
                                        ), # Plot Inputs
                                        # Plot Outputs
                                        column(9,
                                               # Render Plot
                                               plotOutput('reviewer_demographics_mental_illness.graph.plot'),
                                               
                                               br(),
                                               # Download Plot Button
                                               downloadButton("reviewer_demographics_mental_illness.graph.download",label="Save Plot as PNG")
                                               
                                        ) # Plot Outputs
                                      ) # I/O Fluid Layout
                                      
                                  ) # Box - Report Graph
                                ),
                                # Summary Data Table
                                DT::dataTableOutput("reviewer_demo_mental_illness_tbl")
                            ), # Box - Reviewer Mental Illness
                            # Box for Reviewer Relationships
                            box(id = "reviewer_demographics_relationship_employee",
                                title = "Reviewer Relationship to User", 
                                p("A reviewer is counted once for each user 
                                 they provided feedback to."),
                                status = "primary", solidHeader = T, width = 12, collapsible = T,
                                collapsed=T,
                                # Summary Data Table
                                DT::dataTableOutput("reviewer_demo_relationship_employee")
                            ) # Box for Reviewer Relationships
                          ), # Box - Reviewer Demographics
                          # Box to hold all user demographic boxes
                          box(
                            id='user_demographics',
                            title='User Demographics',
                            status='primary',solidHeader = T, width=12, collapsible=T,
                            collapsed=T,
                            # Box for User Gender
                            box(id = "user_demographics_gender",
                                title = "User Gender Identity", 
                                status = "primary", solidHeader = T, width = 12, collapsible = T,
                                collapsed=T,
                                fluidRow(
                                  # Report Graph Box
                                  box(id='user_demographics_gender.graph.box',
                                      collapsible=T,
                                      collapsed=T,
                                      width=12,
                                      title='Report Graph',
                                      # Sidebar for plot customisation, change `value` here to change defaults
                                      sidebar=boxSidebar(
                                        id='user_demographics_gender.graph.options',
                                        width=50,
                                        tags$div(
                                          style="font-size: 12px;",
                                          fluidRow(
                                            column(11,
                                                   textInput(inputId='user_demographics_gender.graph.plot.title',
                                                             value='User Gender Demographics',
                                                             'Plot Title')
                                            )
                                          ),
                                          fluidRow(
                                            
                                            column(4,
                                                   numericInput(inputId='user_demographics_gender.graph.plot.data_label',
                                                                'Data Label Text Size',
                                                                value=14,
                                                                min=3),
                                                   numericInput(inputId='user_demographics_gender.graph.plot.title_size',
                                                                'Title Text Size',
                                                                value=14,
                                                                min=6),
                                                   colourInput(inputId='user_demographics_gender.graph.plot.data_label_color',
                                                               label='Select Data Label Colour:',
                                                               value='black',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            ),
                                            column(4,
                                                   colourInput(inputId='user_demographics_gender.graph.plot.color1',
                                                               label='Select Plot Color 1:',
                                                               value='#E69F00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_gender.graph.plot.color2',
                                                               label='Select Plot Color 2:',
                                                               value='#56B4E9',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_gender.graph.plot.color3',
                                                               label='Select Plot Color 3:',
                                                               value='#009E73',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_gender.graph.plot.color4',
                                                               label='Select Plot Color 4:',
                                                               value='#F0E442',
                                                               returnName = T,
                                                               closeOnClick = T)
                                                   
                                            ),
                                            column(4,
                                                   colourInput(inputId='user_demographics_gender.graph.plot.color5',
                                                               label='Select Plot Color 5:',
                                                               value='#D55E00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_gender.graph.plot.color6',
                                                               label='Select Plot Color 6:',
                                                               value='#CC79A7',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_gender.graph.plot.color7',
                                                               label='Select Plot Color 7:',
                                                               value='#FFCCFF',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_gender.graph.plot.color8',
                                                               label='Select Plot Color 8:',
                                                               value='maroon',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            )
                                            
                                          )
                                        )
                                      ),
                                      # I/O Fluid Layout
                                      fluidRow(
                                        # Plot Inputs
                                        column(3,
                                               fluidRow(
                                                 # Select Custom Group Input
                                                 column(width=6,
                                                        selectizeInput(inputId='user_demographics_gender.graph.selectgroup1',
                                                                       label='Select Group 1',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_gender.graph.selectgroup2',
                                                                       label='Select Group 2',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_gender.graph.selectgroup3',
                                                                       label='Select Group 3',choices=NULL,
                                                                       multiple=T)     
                                                 ),
                                                 # Custom Group Label Input
                                                 column(width=6,
                                                        textInput(inputId='user_demographics_gender.graph.group1label',
                                                                  value='Group 1',
                                                                  'Group 1 Label'),
                                                        textInput(inputId='user_demographics_gender.graph.group2label',
                                                                  value='Group 2',
                                                                  'Group 2 Label'),
                                                        textInput(inputId='user_demographics_gender.graph.group3label',
                                                                  value='Group 3',
                                                                  'Group 3 Label')
                                                 )
                                               ), 
                                               
                                               br(),
                                               # Generate Plot Button
                                               actionButton(inputId='user_demographics_gender.graph.plot.generate',label='Draw Graph'),
                                               br(),
                                               # Plot Download Dimensions
                                               fluidRow(
                                                 column(width=6,
                                                        numericInput(inputId='user_demographics_gender.graph.plot_width',
                                                                     'Saved Plot Width',
                                                                     value=8,
                                                                     min=5)
                                                 ),
                                                 column(width=6,
                                                        numericInput(inputId='user_demographics_gender.graph.plot_height',
                                                                     'Saved Plot Height',
                                                                     value=8,
                                                                     min=5)
                                                 )
                                               )
                                        ), # Plot Inputs
                                        # Plot Outputs
                                        column(9,
                                               # Render Plot
                                               plotOutput('user_demographics_gender.graph.plot'),
                                               
                                               br(),
                                               # Download Plot Button
                                               downloadButton("user_demographics_gender.graph.download",label="Save Plot as PNG")
                                               
                                        ) # Plot Output
                                      ) # I/O Fluid Layout
                                      
                                  ) # Box - Report Graph
                                ),
                                # Summary Data Table
                                DT::dataTableOutput("user_demo_gender_tbl")
                                
                            ), # Box - User Gender
                            # Box for User Age
                            box(id = "user_demographics_age",
                                title = "User Age",
                                status = "primary", solidHeader = T, width = 12, collapsible = T,
                                collapsed=T,
                                fluidRow(
                                  # Report Graph Box
                                  box(id='user_demographics_age.graph.box',
                                      collapsible=T,
                                      collapsed=T,
                                      width=12,
                                      title='Report Graph',
                                      # Sidebar for plot customisation
                                      sidebar=boxSidebar(
                                        id='user_demographics_age.graph.options',
                                        width=50,
                                        tags$div(
                                          style="font-size: 12px;",
                                          fluidRow(
                                            column(11,
                                                   textInput(inputId='user_demographics_age.graph.plot.title',
                                                             value='User Age Demographics',
                                                             'Plot Title')
                                            )
                                          ),
                                          fluidRow(
                                            
                                            column(4,
                                                   numericInput(inputId='user_demographics_age.graph.plot.data_label',
                                                                'Data Label Text Size',
                                                                value=14,
                                                                min=3),
                                                   numericInput(inputId='user_demographics_age.graph.plot.title_size',
                                                                'Title Text Size',
                                                                value=14,
                                                                min=6),
                                                   colourInput(inputId='user_demographics_age.graph.plot.data_label_color',
                                                               label='Select Data Label Colour:',
                                                               value='black',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            ),
                                            column(4,
                                                   colourInput(inputId='user_demographics_age.graph.plot.color1',
                                                               label='Select Plot Color 1:',
                                                               value='#E69F00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_age.graph.plot.color2',
                                                               label='Select Plot Color 2:',
                                                               value='#56B4E9',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_age.graph.plot.color3',
                                                               label='Select Plot Color 3:',
                                                               value='#009E73',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_age.graph.plot.color4',
                                                               label='Select Plot Color 4:',
                                                               value='#F0E442',
                                                               returnName = T,
                                                               closeOnClick = T)
                                                   
                                            ),
                                            column(4,
                                                   colourInput(inputId='user_demographics_age.graph.plot.color5',
                                                               label='Select Plot Color 5:',
                                                               value='#D55E00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_age.graph.plot.color6',
                                                               label='Select Plot Color 6:',
                                                               value='#CC79A7',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_age.graph.plot.color7',
                                                               label='Select Plot Color 7:',
                                                               value='#FFCCFF',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_age.graph.plot.color8',
                                                               label='Select Plot Color 8:',
                                                               value='maroon',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            )
                                            
                                          )
                                        )
                                      ),
                                      # I/O Fluid Layout
                                      fluidRow(
                                        # Plot Inputs
                                        column(3,
                                               fluidRow(
                                                 # Select Custom Groups Input
                                                 column(width=6,
                                                        selectizeInput(inputId='user_demographics_age.graph.selectgroup1',
                                                                       label='Select Group 1',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_age.graph.selectgroup2',
                                                                       label='Select Group 2',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_age.graph.selectgroup3',
                                                                       label='Select Group 3',choices=NULL,
                                                                       multiple=T)     
                                                 ),
                                                 # Custom Group Label Input
                                                 column(width=6,
                                                        textInput(inputId='user_demographics_age.graph.group1label',
                                                                  value='Group 1',
                                                                  'Group 1 Label'),
                                                        textInput(inputId='user_demographics_age.graph.group2label',
                                                                  value='Group 2',
                                                                  'Group 2 Label'),
                                                        textInput(inputId='user_demographics_age.graph.group3label',
                                                                  value='Group 3',
                                                                  'Group 3 Label')
                                                 )
                                               ),
                                               
                                               br(),
                                               # Generate Plot Button
                                               actionButton(inputId='user_demographics_age.graph.plot.generate',label='Draw Graph'),
                                               br(),
                                               # Plot Download Dimensions
                                               fluidRow(
                                                 column(width=6,
                                                        numericInput(inputId='user_demographics_age.graph.plot_width',
                                                                     'Saved Plot Width',
                                                                     value=8,
                                                                     min=5)
                                                 ),
                                                 column(width=6,
                                                        numericInput(inputId='user_demographics_age.graph.plot_height',
                                                                     'Saved Plot Height',
                                                                     value=8,
                                                                     min=5)
                                                 )
                                               )
                                        ), # Plot Input
                                        # Plot Output
                                        column(9,
                                               # Render Plot
                                               plotOutput('user_demographics_age.graph.plot'),
                                               
                                               br(),
                                               # Download Plot Button
                                               downloadButton("user_demographics_age.graph.download",label="Save Plot as PNG")
                                               
                                        ) # Plot Output
                                      ) # I/O Fluid Layout
                                      
                                  ) # Box - Report Graph
                                ),
                                # Summary Data Table
                                DT::dataTableOutput("user_demo_age_tbl")
                            ), # Box - User Age
                            # Box - User Sexual Orientation
                            box(id = "user_demographics_sexuality",
                                title = "User Sexual Orientation", status = "primary", solidHeader = T, width = 12, 
                                collapsible = T,
                                collapsed=T,
                                fluidRow(
                                  # Report Graph Box
                                  box(id='user_demographics_sexuality.graph.box',
                                      collapsible=T,
                                      collapsed=T,
                                      width=12,
                                      title='Report Graph',
                                      # Sidebar for plot customisation, change `value` here to change defaults
                                      sidebar=boxSidebar(
                                        id='user_demographics_sexuality.graph.options',
                                        width=50,
                                        tags$div(
                                          style="font-size: 12px;",
                                          fluidRow(
                                            column(11,
                                                   textInput(inputId='user_demographics_sexuality.graph.plot.title',
                                                             value='User Sexual Orientation Demographics',
                                                             'Plot Title')
                                            )
                                          ),
                                          fluidRow(
                                            
                                            column(4,
                                                   numericInput(inputId='user_demographics_sexuality.graph.plot.data_label',
                                                                'Data Label Text Size',
                                                                value=14,
                                                                min=3),
                                                   numericInput(inputId='user_demographics_sexuality.graph.plot.title_size',
                                                                'Title Text Size',
                                                                value=14,
                                                                min=6),
                                                   colourInput(inputId='user_demographics_sexuality.graph.plot.data_label_color',
                                                               label='Select Data Label Colour:',
                                                               value='black',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            ),
                                            column(4,
                                                   colourInput(inputId='user_demographics_sexuality.graph.plot.color1',
                                                               label='Select Plot Color 1:',
                                                               value='#E69F00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_sexuality.graph.plot.color2',
                                                               label='Select Plot Color 2:',
                                                               value='#56B4E9',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_sexuality.graph.plot.color3',
                                                               label='Select Plot Color 3:',
                                                               value='#009E73',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_sexuality.graph.plot.color4',
                                                               label='Select Plot Color 4:',
                                                               value='#F0E442',
                                                               returnName = T,
                                                               closeOnClick = T)
                                                   
                                            ),
                                            column(4,
                                                   colourInput(inputId='user_demographics_sexuality.graph.plot.color5',
                                                               label='Select Plot Color 5:',
                                                               value='#D55E00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_sexuality.graph.plot.color6',
                                                               label='Select Plot Color 6:',
                                                               value='#CC79A7',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_sexuality.graph.plot.color7',
                                                               label='Select Plot Color 7:',
                                                               value='#FFCCFF',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_sexuality.graph.plot.color8',
                                                               label='Select Plot Color 8:',
                                                               value='maroon',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            )
                                            
                                          )
                                        )
                                      ),
                                      # I/O Fluid Layout
                                      fluidRow(
                                        # Plot Inputs
                                        column(3,
                                               fluidRow(
                                                 # Select Custom Group Input
                                                 column(width=6,
                                                        selectizeInput(inputId='user_demographics_sexuality.graph.selectgroup1',
                                                                       label='Select Group 1',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_sexuality.graph.selectgroup2',
                                                                       label='Select Group 2',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_sexuality.graph.selectgroup3',
                                                                       label='Select Group 3',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_sexuality.graph.selectgroup4',
                                                                       label='Select Group 4',choices=NULL,
                                                                       multiple=T)
                                                 ),
                                                 # Group Label Input
                                                 column(width=6,
                                                        textInput(inputId='user_demographics_sexuality.graph.group1label',
                                                                  value='Group 1',
                                                                  'Group 1 Label'),
                                                        textInput(inputId='user_demographics_sexuality.graph.group2label',
                                                                  value='Group 2',
                                                                  'Group 2 Label'),
                                                        textInput(inputId='user_demographics_sexuality.graph.group3label',
                                                                  value='Group 3',
                                                                  'Group 3 Label'),
                                                        textInput(inputId='user_demographics_sexuality.graph.group4label',
                                                                  value='Group 4',
                                                                  'Group 4 Label')
                                                 )
                                               ),
                                               
                                               br(),
                                               # Generate Plot Button
                                               actionButton(inputId='user_demographics_sexuality.graph.plot.generate',label='Draw Graph'),
                                               br(),
                                               # Plot Download Dimensions
                                               fluidRow(
                                                 column(width=6,
                                                        numericInput(inputId='user_demographics_sexuality.graph.plot_width',
                                                                     'Saved Plot Width',
                                                                     value=8,
                                                                     min=5)
                                                 ),
                                                 column(width=6,
                                                        numericInput(inputId='user_demographics_sexuality.graph.plot_height',
                                                                     'Saved Plot Height',
                                                                     value=8,
                                                                     min=5)
                                                 )
                                               )
                                        ), # Plot Inputs
                                        # Plot Outputs
                                        column(9,
                                               # Render Plot
                                               plotOutput('user_demographics_sexuality.graph.plot'),
                                               
                                               br(),
                                               # Download Plot Button
                                               downloadButton("user_demographics_sexuality.graph.download",label="Save Plot as PNG")
                                               
                                        ) # Plot Output
                                      ) # I/O Fluid Layout
                                      
                                  ) # Box - Report Graph
                                ),
                                # Summary Data Table
                                DT::dataTableOutput("user_demo_sexuality_tbl")
                            ), # Box - User Sexual Orientation
                            # Box for User Ethnicity
                            box(id = "user_demographics_ethnicity",
                                title = "User Ethnicity", 
                                p("Honeycomb users could select more than one ethnicity, and 
                                 employees who did so are counted once for each selected ethnicity 
                                 in the table below. Users who selected multiple ethnic identities 
                                 are counted once, as multiethnic."),
                                status = "primary", solidHeader = T, width = 12, collapsible = T,
                                collapsed=T,
                                fluidRow(
                                  # Box Report Graph
                                  box(id='user_demographics_ethnicity.graph.box',
                                      collapsible=T,
                                      collapsed=T,
                                      width=12,
                                      title='Report Graph',
                                      # Sidebar for plot customisation, change `value` here to change defaults
                                      sidebar=boxSidebar(
                                        id='user_demographics_ethnicity.graph.options',
                                        width=50,
                                        tags$div(
                                          style="font-size: 12px;",
                                          fluidRow(
                                            column(11,
                                                   textInput(inputId='user_demographics_ethnicity.graph.plot.title',
                                                             value='User Ethnicity Demographics',
                                                             'Plot Title')
                                            )
                                          ),
                                          fluidRow(
                                            
                                            column(4,
                                                   numericInput(inputId='user_demographics_ethnicity.graph.plot.data_label',
                                                                'Data Label Text Size',
                                                                value=14,
                                                                min=3),
                                                   numericInput(inputId='user_demographics_ethnicity.graph.plot.title_size',
                                                                'Title Text Size',
                                                                value=14,
                                                                min=6),
                                                   colourInput(inputId='user_demographics_ethnicity.graph.plot.data_label_color',
                                                               label='Select Data Label Colour:',
                                                               value='black',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            ),
                                            column(4,
                                                   colourInput(inputId='user_demographics_ethnicity.graph.plot.color1',
                                                               label='Select Plot Color 1:',
                                                               value='#E69F00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_ethnicity.graph.plot.color2',
                                                               label='Select Plot Color 2:',
                                                               value='#56B4E9',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_ethnicity.graph.plot.color3',
                                                               label='Select Plot Color 3:',
                                                               value='#009E73',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_ethnicity.graph.plot.color4',
                                                               label='Select Plot Color 4:',
                                                               value='#F0E442',
                                                               returnName = T,
                                                               closeOnClick = T)
                                                   
                                            ),
                                            column(4,
                                                   colourInput(inputId='user_demographics_ethnicity.graph.plot.color5',
                                                               label='Select Plot Color 5:',
                                                               value='#D55E00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_ethnicity.graph.plot.color6',
                                                               label='Select Plot Color 6:',
                                                               value='#CC79A7',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_ethnicity.graph.plot.color7',
                                                               label='Select Plot Color 7:',
                                                               value='#FFCCFF',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_ethnicity.graph.plot.color8',
                                                               label='Select Plot Color 8:',
                                                               value='maroon',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            )
                                            
                                          )
                                        )
                                      ),
                                      # I/O Fluid Layout
                                      fluidRow(
                                        # Plot Inputs
                                        column(3,
                                               fluidRow(
                                                 # Select Custom Group Input
                                                 column(width=6,
                                                        selectizeInput(inputId='user_demographics_ethnicity.graph.selectgroup1',
                                                                       label='Select Group 1',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_ethnicity.graph.selectgroup2',
                                                                       label='Select Group 2',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_ethnicity.graph.selectgroup3',
                                                                       label='Select Group 3',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_ethnicity.graph.selectgroup4',
                                                                       label='Select Group 4',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_ethnicity.graph.selectgroup5',
                                                                       label='Select Group 5',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_ethnicity.graph.selectgroup6',
                                                                       label='Select Group 6',choices=NULL,
                                                                       multiple=T)
                                                 ),
                                                 # Custom Group Label Input
                                                 column(width=6,
                                                        textInput(inputId='user_demographics_ethnicity.graph.group1label',
                                                                  value='Group 1',
                                                                  'Group 1 Label'),
                                                        textInput(inputId='user_demographics_ethnicity.graph.group2label',
                                                                  value='Group 2',
                                                                  'Group 2 Label'),
                                                        textInput(inputId='user_demographics_ethnicity.graph.group3label',
                                                                  value='Group 3',
                                                                  'Group 3 Label'),
                                                        textInput(inputId='user_demographics_ethnicity.graph.group4label',
                                                                  value='Group 4',
                                                                  'Group 4 Label'),
                                                        textInput(inputId='user_demographics_ethnicity.graph.group5label',
                                                                  value='Group 5',
                                                                  'Group 5 Label'),
                                                        textInput(inputId='user_demographics_ethnicity.graph.group6label',
                                                                  value='Group 6',
                                                                  'Group 6 Label')
                                                 )
                                               ),
                                               
                                               br(),
                                               # Generate Plot Button
                                               actionButton(inputId='user_demographics_ethnicity.graph.plot.generate',label='Draw Graph'),
                                               br(),
                                               # Plot Downoad Dimensions
                                               fluidRow(
                                                 column(width=6,
                                                        numericInput(inputId='user_demographics_ethnicity.graph.plot_width',
                                                                     'Saved Plot Width',
                                                                     value=8,
                                                                     min=5)
                                                 ),
                                                 column(width=6,
                                                        numericInput(inputId='user_demographics_ethnicity.graph.plot_height',
                                                                     'Saved Plot Height',
                                                                     value=8,
                                                                     min=5)
                                                 )
                                               )
                                        ), # Plot Inputs
                                        # Plot Output
                                        column(9,
                                               # Render Plot
                                               plotOutput('user_demographics_ethnicity.graph.plot'),
                                               
                                               br(),
                                               # Download Plot Button
                                               downloadButton("user_demographics_ethnicity.graph.download",label="Save Plot as PNG")
                                               
                                        ) # Plot Output
                                      ) # I/O Fluid Layout
                                      
                                  ) # Box - Report Graph
                                ),
                                # Summary Data Table
                                DT::dataTableOutput("user_demo_ethnicity_tbl")
                            ), # Box - User Ethnicity
                            # Box for User Ethnicity (Collapsed)
                            box(id = "user_demographics_ethnicity_collapsed",
                                title = "User Ethnicity - Collapsed", 
                                p("In this table, ethnicity is collapsed to fewer categories, 
                                 and users who selected more than one ethnic category are only 
                                 counted as multiethnic."),
                                status = "primary", solidHeader = T, width = 12, collapsible = T,
                                collapsed=T,
                                fluidRow(
                                  # Report Graph Box
                                  box(id='user_demographics_ethnicity_collapsed.graph.box',
                                      collapsible=T,
                                      collapsed=T,
                                      width=12,
                                      title='Report Graph',
                                      # Sidebar for plot customisation, change `value` here to change defaults
                                      sidebar=boxSidebar(
                                        id='user_demographics_ethnicity_collapsed.graph.options',
                                        width=50,
                                        tags$div(
                                          style="font-size: 12px;",
                                          fluidRow(
                                            column(11,
                                                   textInput(inputId='user_demographics_ethnicity_collapsed.graph.plot.title',
                                                             value='User Ethnicity (Collapsed) Demographics',
                                                             'Plot Title')
                                            )
                                          ),
                                          fluidRow(
                                            
                                            column(4,
                                                   numericInput(inputId='user_demographics_ethnicity_collapsed.graph.plot.data_label',
                                                                'Data Label Text Size',
                                                                value=14,
                                                                min=3),
                                                   numericInput(inputId='user_demographics_ethnicity_collapsed.graph.plot.title_size',
                                                                'Title Text Size',
                                                                value=14,
                                                                min=6),
                                                   colourInput(inputId='user_demographics_ethnicity_collapsed.graph.plot.data_label_color',
                                                               label='Select Data Label Colour:',
                                                               value='black',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            ),
                                            column(4,
                                                   colourInput(inputId='user_demographics_ethnicity_collapsed.graph.plot.color1',
                                                               label='Select Plot Color 1:',
                                                               value='#E69F00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_ethnicity_collapsed.graph.plot.color2',
                                                               label='Select Plot Color 2:',
                                                               value='#56B4E9',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_ethnicity_collapsed.graph.plot.color3',
                                                               label='Select Plot Color 3:',
                                                               value='#009E73',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_ethnicity_collapsed.graph.plot.color4',
                                                               label='Select Plot Color 4:',
                                                               value='#F0E442',
                                                               returnName = T,
                                                               closeOnClick = T)
                                                   
                                            ),
                                            column(4,
                                                   colourInput(inputId='user_demographics_ethnicity_collapsed.graph.plot.color5',
                                                               label='Select Plot Color 5:',
                                                               value='#D55E00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_ethnicity_collapsed.graph.plot.color6',
                                                               label='Select Plot Color 6:',
                                                               value='#CC79A7',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_ethnicity_collapsed.graph.plot.color7',
                                                               label='Select Plot Color 7:',
                                                               value='#FFCCFF',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_ethnicity_collapsed.graph.plot.color8',
                                                               label='Select Plot Color 8:',
                                                               value='maroon',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            )
                                            
                                          )
                                        )
                                      ),
                                      # I/O Fluid Layout
                                      fluidRow(
                                        # Plot Inputs
                                        column(3,
                                               fluidRow(
                                                 # Select Custom Groups Input
                                                 column(width=6,
                                                        selectizeInput(inputId='user_demographics_ethnicity_collapsed.graph.selectgroup1',
                                                                       label='Select Group 1',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_ethnicity_collapsed.graph.selectgroup2',
                                                                       label='Select Group 2',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_ethnicity_collapsed.graph.selectgroup3',
                                                                       label='Select Group 3',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_ethnicity_collapsed.graph.selectgroup4',
                                                                       label='Select Group 4',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_ethnicity_collapsed.graph.selectgroup5',
                                                                       label='Select Group 5',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_ethnicity_collapsed.graph.selectgroup6',
                                                                       label='Select Group 6',choices=NULL,
                                                                       multiple=T)
                                                 ),
                                                 # Custom Group Label Input
                                                 column(width=6,
                                                        textInput(inputId='user_demographics_ethnicity_collapsed.graph.group1label',
                                                                  value='Group 1',
                                                                  'Group 1 Label'),
                                                        textInput(inputId='user_demographics_ethnicity_collapsed.graph.group2label',
                                                                  value='Group 2',
                                                                  'Group 2 Label'),
                                                        textInput(inputId='user_demographics_ethnicity_collapsed.graph.group3label',
                                                                  value='Group 3',
                                                                  'Group 3 Label'),
                                                        textInput(inputId='user_demographics_ethnicity_collapsed.graph.group4label',
                                                                  value='Group 4',
                                                                  'Group 4 Label'),
                                                        textInput(inputId='user_demographics_ethnicity_collapsed.graph.group5label',
                                                                  value='Group 5',
                                                                  'Group 5 Label'),
                                                        textInput(inputId='user_demographics_ethnicity_collapsed.graph.group6label',
                                                                  value='Group 6',
                                                                  'Group 6 Label')
                                                 )
                                               ),
                                               
                                               br(),
                                               # Generate Plot Button
                                               actionButton(inputId='user_demographics_ethnicity_collapsed.graph.plot.generate',label='Draw Graph'),
                                               br(),
                                               # Plot Download Dimensions
                                               fluidRow(
                                                 column(width=6,
                                                        numericInput(inputId='user_demographics_ethnicity_collapsed.graph.plot_width',
                                                                     'Saved Plot Width',
                                                                     value=8,
                                                                     min=5)
                                                 ),
                                                 column(width=6,
                                                        numericInput(inputId='user_demographics_ethnicity_collapsed.graph.plot_height',
                                                                     'Saved Plot Height',
                                                                     value=8,
                                                                     min=5)
                                                 )
                                               )
                                        ), # Plot Inputs
                                        # Plot Output
                                        column(9,
                                               # Render Plot
                                               plotOutput('user_demographics_ethnicity_collapsed.graph.plot'),
                                               
                                               br(),
                                               # Download Plot Button
                                               downloadButton("user_demographics_ethnicity_collapsed.graph.download",label="Save Plot as PNG")
                                               
                                        ) # Plot Output
                                      ) # I/O Fluid Layout
                                      
                                  ) # Box - Report Graphs
                                ),
                                # Summary Data Table
                                DT::dataTableOutput("user_demo_ethnicity_collapsed_tbl")
                            ), # Box - User Ethnicity (Collapsed)
                            # Box for User Disability
                            box(id = "user_demographics_disability",
                                title = "User Disability", 
                                status = "primary", solidHeader = T, width = 12, collapsible = T,
                                collapsed=T,
                                fluidRow(
                                  # Report Graph Box
                                  box(id='user_demographics_disability.graph.box',
                                      collapsible=T,
                                      collapsed=T,
                                      width=12,
                                      title='Report Graph',
                                      # Sidebar for Plot Customisation
                                      sidebar=boxSidebar(
                                        id='user_demographics_disability.graph.options',
                                        width=50,
                                        tags$div(
                                          style="font-size: 12px;",
                                          fluidRow(
                                            column(11,
                                                   textInput(inputId='user_demographics_disability.graph.plot.title',
                                                             value='User Disability Demographics',
                                                             'Plot Title')
                                            )
                                          ),
                                          fluidRow(
                                            
                                            column(4,
                                                   numericInput(inputId='user_demographics_disability.graph.plot.data_label',
                                                                'Data Label Text Size',
                                                                value=14,
                                                                min=3),
                                                   numericInput(inputId='user_demographics_disability.graph.plot.title_size',
                                                                'Title Text Size',
                                                                value=14,
                                                                min=6),
                                                   colourInput(inputId='user_demographics_disability.graph.plot.data_label_color',
                                                               label='Select Data Label Colour:',
                                                               value='black',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            ),
                                            column(4,
                                                   colourInput(inputId='user_demographics_disability.graph.plot.color1',
                                                               label='Select Plot Color 1:',
                                                               value='#E69F00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_disability.graph.plot.color2',
                                                               label='Select Plot Color 2:',
                                                               value='#56B4E9',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_disability.graph.plot.color3',
                                                               label='Select Plot Color 3:',
                                                               value='#009E73',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_disability.graph.plot.color4',
                                                               label='Select Plot Color 4:',
                                                               value='#F0E442',
                                                               returnName = T,
                                                               closeOnClick = T)
                                                   
                                            ),
                                            column(4,
                                                   colourInput(inputId='user_demographics_disability.graph.plot.color5',
                                                               label='Select Plot Color 5:',
                                                               value='#D55E00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_disability.graph.plot.color6',
                                                               label='Select Plot Color 6:',
                                                               value='#CC79A7',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_disability.graph.plot.color7',
                                                               label='Select Plot Color 7:',
                                                               value='#FFCCFF',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_disability.graph.plot.color8',
                                                               label='Select Plot Color 8:',
                                                               value='maroon',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            )
                                            
                                          )
                                        )
                                      ),
                                      # I/O Fluid Layout
                                      fluidRow(
                                        # Plot Input
                                        column(3,
                                               fluidRow(
                                                 # Select Custom Group Input
                                                 column(width=6,
                                                        selectizeInput(inputId='user_demographics_disability.graph.selectgroup1',
                                                                       label='Select Group 1',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_disability.graph.selectgroup2',
                                                                       label='Select Group 2',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_disability.graph.selectgroup3',
                                                                       label='Select Group 3',choices=NULL,
                                                                       multiple=T),
                                                        selectizeInput(inputId='user_demographics_disability.graph.selectgroup4',
                                                                       label='Select Group 4',choices=NULL,
                                                                       multiple=T)
                                                 ),
                                                 # Custom Group Label Input
                                                 column(width=6,
                                                        textInput(inputId='user_demographics_disability.graph.group1label',
                                                                  value='Group 1',
                                                                  'Group 1 Label'),
                                                        textInput(inputId='user_demographics_disability.graph.group2label',
                                                                  value='Group 2',
                                                                  'Group 2 Label'),
                                                        textInput(inputId='user_demographics_disability.graph.group3label',
                                                                  value='Group 3',
                                                                  'Group 3 Label'),
                                                        textInput(inputId='user_demographics_disability.graph.group4label',
                                                                  value='Group 4',
                                                                  'Group 4 Label')
                                                 )
                                               ),
                                               
                                               br(),
                                               # Generate Plot Button
                                               actionButton(inputId='user_demographics_disability.graph.plot.generate',label='Draw Graph'),
                                               br(),
                                               # Plot Download Dimensions
                                               fluidRow(
                                                 column(width=6,
                                                        numericInput(inputId='user_demographics_disability.graph.plot_width',
                                                                     'Saved Plot Width',
                                                                     value=8,
                                                                     min=5)
                                                 ),
                                                 column(width=6,
                                                        numericInput(inputId='user_demographics_disability.graph.plot_height',
                                                                     'Saved Plot Height',
                                                                     value=8,
                                                                     min=5)
                                                 )
                                               )
                                        ), # Plot Input
                                        # Plot Output
                                        column(9,
                                               # Render Plot
                                               plotOutput('user_demographics_disability.graph.plot'),
                                               
                                               br(),
                                               # Download Plot Button
                                               downloadButton("user_demographics_disability.graph.download",label="Save Plot as PNG")
                                               
                                        ) # Plot Output
                                      ) # I/O Fluid Layout
                                      
                                  ) # Box - Report Graph
                                ),
                                # Summary Data Table
                                DT::dataTableOutput("user_demo_disability_tbl")
                            ), # Box - User Disability
                            # Box for User Mental Illness
                            box(id = "user_demographics_mental_illness",
                                title = "User Mental Illness",
                                status = "primary", solidHeader = T, width = 12, collapsible = T,
                                collapsed=T,
                                fluidRow(
                                  # Report Graph Box
                                  box(id='user_demographics_mental_illness.graph.box',
                                      collapsible=T,
                                      collapsed=T,
                                      width=12,
                                      title='Report Graph',
                                      # Sidebar for plot customisation, change `value` here to change defaults
                                      sidebar=boxSidebar(
                                        id='user_demographics_mental_illness.graph.options',
                                        width=50,
                                        tags$div(
                                          style="font-size: 12px;",
                                          fluidRow(
                                            column(11,
                                                   textInput(inputId='user_demographics_mental_illness.graph.plot.title',
                                                             value='User Mental Illness Demographics',
                                                             'Plot Title')
                                            )
                                          ),
                                          fluidRow(
                                            
                                            column(4,
                                                   numericInput(inputId='user_demographics_mental_illness.graph.plot.data_label',
                                                                'Data Label Text Size',
                                                                value=14,
                                                                min=3),
                                                   numericInput(inputId='user_demographics_mental_illness.graph.plot.title_size',
                                                                'Title Text Size',
                                                                value=14,
                                                                min=6),
                                                   colourInput(inputId='user_demographics_mental_illness.graph.plot.data_label_color',
                                                               label='Select Data Label Colour:',
                                                               value='black',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            ),
                                            column(4,
                                                   colourInput(inputId='user_demographics_mental_illness.graph.plot.color1',
                                                               label='Select Plot Color 1:',
                                                               value='#E69F00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_mental_illness.graph.plot.color2',
                                                               label='Select Plot Color 2:',
                                                               value='#56B4E9',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_mental_illness.graph.plot.color3',
                                                               label='Select Plot Color 3:',
                                                               value='#009E73',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_mental_illness.graph.plot.color4',
                                                               label='Select Plot Color 4:',
                                                               value='#F0E442',
                                                               returnName = T,
                                                               closeOnClick = T)
                                                   
                                            ),
                                            column(4,
                                                   colourInput(inputId='user_demographics_mental_illness.graph.plot.color5',
                                                               label='Select Plot Color 5:',
                                                               value='#D55E00',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_mental_illness.graph.plot.color6',
                                                               label='Select Plot Color 6:',
                                                               value='#CC79A7',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_mental_illness.graph.plot.color7',
                                                               label='Select Plot Color 7:',
                                                               value='#FFCCFF',
                                                               returnName = T,
                                                               closeOnClick = T),
                                                   colourInput(inputId='user_demographics_mental_illness.graph.plot.color8',
                                                               label='Select Plot Color 8:',
                                                               value='maroon',
                                                               returnName = T,
                                                               closeOnClick = T)
                                            )
                                            
                                          )
                                        )
                                      ),
                                      # I/O Fluid Layout
                                      fluidRow(
                                        # Plot Input
                                        column(3,
                                               br(),
                                               # Generate Plot Button
                                               actionButton(inputId='user_demographics_mental_illness.graph.plot.generate',label='Draw Graph'),
                                               br(),
                                               # Plot Download Dimensions
                                               fluidRow(
                                                 column(width=6,
                                                        numericInput(inputId='user_demographics_mental_illness.graph.plot_width',
                                                                     'Saved Plot Width',
                                                                     value=8,
                                                                     min=5)
                                                 ),
                                                 column(width=6,
                                                        numericInput(inputId='user_demographics_mental_illness.graph.plot_height',
                                                                     'Saved Plot Height',
                                                                     value=8,
                                                                     min=5)
                                                 )
                                               )
                                        ), # Plot Input
                                        # Plot Output
                                        column(9,
                                               # Render Plot
                                               plotOutput('user_demographics_mental_illness.graph.plot'),
                                               
                                               br(),
                                               # Download Plot Button
                                               downloadButton("user_demographics_mental_illness.graph.download",label="Save Plot as PNG")
                                               
                                        ) # Plot Output
                                      ) # I/O Fluid Layout
                                      
                                  ) # Box - Report Graph
                                ),
                                # Summary Data Table
                                DT::dataTableOutput("user_demo_mental_illness_tbl")
                            ) # Box - User Mental Illness
                          ) # Box - User Demographics
                          
                 ), # TabPanel - Demographics
                 
                 # Tab for Demographic Differences Statistical Testing
                 tabPanel("Demographic Differences",
                          br(),
                          # Dropdown to provide some information for the user
                          dropdown(
                            p("This tab is designed to help users explore differences in behaviours 
                            across various demographic groups such as gender, sexual orientation, ethnicity, 
                            age, disability, and mental illness. Using statistical tests, you can uncover whether 
                            specific demographic groups experience or report behaviours differently."),
                            br(),
                            p("What you can do here:"),
                            tags$div(
                              tags$ul(
                                tags$li(
                                  tags$b('Explore Overall Differences (SS Test and Effect Size)'),
                                  p('Use the Kruskal-Wallis test to check if the behaviours differ across all groups in a 
                                  demographic category. For example, does behaviour vary across different age groups or genders?'),
                                  p("The Kruskal-Wallis test is a non-parametric method, making it particularly well-suited for 
                                    data that do not follow a normal distribution or when sample sizes are small. It avoids 
                                    assumptions about data structure, ensuring reliable results across a range of scenarios."),
                                  p('This test not only identifies significant differences but also includes effect sizes to show 
                                    the magnitude of these differences, providing a clearer picture of their practical significance. 
                                    However, the effect size calculations should only be reported if statistically significant differences 
                                    were identified using the Kruskal-Wallis test.')
                                ),
                                tags$li(
                                  tags$b('Pinpoint Specific Differences (Post-Hoc Test)'),
                                  p('If the Kruskal-Wallis test shows significant differences, you can use the post-hoc Dunn 
                                    test. This test compares each pair of demographic groups to identify exactly which groups 
                                    have significant differences. For instance, it can tell you if a specific behaviour is 
                                    significantly different between men and women or between different ethnic groups.'),
                                  tags$em('Adjusted p-values calculated using Bonferroni method'),
                                  br(),
                                  br()
                                ),
                                tags$li(
                                  tags$b('Customize Group Comparisons and Create Graphs (Select/Collapse Groups)'),
                                  p('Sometimes, you might want to simplify the analysis by combining certain groups (e.g., 
                                    merging "non-binary" and "other" into one category for gender). The custom-group Dunn 
                                    test lets you define and collapse groups, making the comparisons more tailored to your needs.'),
                                  tags$b('WARNING: Ensure that groups are mutually exclusive or tests and graphs may behave unexpectedly.'),
                                  br(),
                                  p('In this section, you can also generate comparison plots for the binary comparisons defined in the 
                                    custom groups. Plot customisation is available using the gear icon on the right.')
                                )
                              )
                            ),
                            style='unite',icon=icon('circle-question'),
                            animate=animateOptions(enter = "fadeInDown", exit = "fadeOutUp"),
                            size='xs',no_outline=T
                          ),
                          # Group by Cell/Behaviour Radio button
                          radioButtons(inputId = 'demographicsGrouping', inline = TRUE, 
                                       label = "Show Differences by:",
                                       choiceNames = c("Cell", "Behaviour"),
                                       choiceValues = c("Cell.name", "Question"),
                                       selected = "Cell.name"),
                          br(),
                          
                          # Box for Reviewer Comparisons
                          box(id= "demo.differences_reviewer",
                              title='By Reviewer',
                              status='primary',solidHeader = T, width=12, collapsible=T,
                              br(),
                              
                              # Box for Reviewer Gender
                              box(id="demo.differences_reviewer.gender",
                                  title='By Reviewer Gender',
                                  status='primary', solidHeader=T, width=12, collapsible=T,
                                  collapsed=T,
                                  # Tab Box for Tests and Plot
                                  tabBox(width=12,
                                         tabPanel("SS Test and Effect Size",
                                                  br(),
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.gender.tbl_ss'))),
                                         tabPanel("Post-hoc Tests",
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.gender.tbl_ph'))),
                                         tabPanel("Select/Collapse Groups",
                                                  
                                                  br(),
                                                  # Report Graph
                                                  box(id='demo.differences_reviewer.gender.graph.box',
                                                      collapsible=T,
                                                      collapsed=T,
                                                      width=12,
                                                      title='Report Graph',
                                                      # Sidebar for Plot Customisation, change `value` here to change defaults
                                                      sidebar=boxSidebar(
                                                        id='demo.differences_reviewer.gender.graph.options',
                                                        width=50,
                                                        tags$div(
                                                          style="font-size: 12px;",
                                                          fluidRow(
                                                            column(11,
                                                                   textInput(inputId='demo.differences_reviewer.gender.graph.plot.title',
                                                                             value='Difference in Scores by Reviewer Gender',
                                                                             'Plot Title'),
                                                                   textInput(inputId='demo.differences_reviewer.gender.graph.plot.subtitle',
                                                                             value='',
                                                                             'Plot Subtitle'),
                                                                   textInput(inputId='demo.differences_reviewer.gender.graph.plot.fill_label',
                                                                             value='Demographic Groups',
                                                                             'Legend Caption'),
                                                                   textInput(inputId='demo.differences_reviewer.gender.graph.plot.x_title',
                                                                             value='% of the time',
                                                                             'X-axis Title'),
                                                                   textInput(inputId='demo.differences_reviewer.gender.graph.plot.y_title',
                                                                             value='',
                                                                             'Y-axis Title')
                                                            )
                                                          ),
                                                          fluidRow(
                                                            
                                                            column(4,
                                                                   numericInput(inputId='demo.differences_reviewer.gender.graph.plot.data_label',
                                                                                'Data Label Text Size',
                                                                                value=3,
                                                                                min=1),
                                                                   numericInput(inputId='demo.differences_reviewer.gender.graph.plot.axis_label',
                                                                                'Axis Labels Text Size',
                                                                                value=10,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_reviewer.gender.graph.plot.axis_title',
                                                                                'Axis Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_reviewer.gender.graph.plot.title_size',
                                                                                'Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_reviewer.gender.graph.plot.subtitle_size',
                                                                                'Subtitle Text Size',
                                                                                value=10,
                                                                                min=6)
                                                            ),
                                                            column(4,
                                                                   colourInput(inputId='demo.differences_reviewer.gender.graph.plot.color1',
                                                                               label='Select Plot Color 1:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_reviewer.gender.graph.plot.color2',
                                                                               label='Select Plot Color 2:',
                                                                               value='#BD3B6F',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_reviewer.gender.graph.plot.data_label_color',
                                                                               label='Select Data Label Colour:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T)
                                                            )
                                                            
                                                          )
                                                        )
                                                      ),
                                                      # I/O Fluid Layout
                                                      fluidRow(
                                                        # Plot Input
                                                        column(3,
                                                               # Select Cell/Behaviour Input
                                                               selectizeInput(inputId='demo.differences_reviewer.gender.graph.select',
                                                                              label='Select Cells/Behaviours',choices=NULL,
                                                                              multiple=T,
                                                                              options = list(maxItems=5)),
                                                               br(),
                                                               # Generate Plot Button
                                                               actionButton(inputId='demo.differences_reviewer.gender.graph.plot.generate',label='Draw Graph'),
                                                               br(),
                                                               # Plot Download Dimensions
                                                               fluidRow(
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_reviewer.gender.graph.plot_width',
                                                                                     'Saved Plot Width',
                                                                                     value=8,
                                                                                     min=5)
                                                                 ),
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_reviewer.gender.graph.plot_height',
                                                                                     'Saved Plot Height',
                                                                                     value=5,
                                                                                     min=5)
                                                                 )
                                                               )
                                                        ), # Plot Inputs
                                                        # Plot Output
                                                        column(9,
                                                               # Render plot
                                                               plotOutput('demo.differences_reviewer.gender.graph.plot'),
                                                               
                                                               br(),
                                                               # Download Plot Button
                                                               downloadButton("demo.differences_reviewer.gender.graph.download",label="Save Plot as PNG")
                                                               
                                                        ) # Plot Output
                                                      ) # I/O Fluid Layout
                                                      
                                                  ), # Box - Report Graph
                                                  # Select Group 1 Cats
                                                  selectizeInput(inputId='demo.differences_reviewer.gender.tbl_custom.select1',
                                                                 label='Select Group 1 Demographic(s)',choices=NULL,multiple=T),
                                                  # Select Group 2 Cats
                                                  selectizeInput(inputId='demo.differences_reviewer.gender.tbl_custom.select2',
                                                                 label='Select Group 2 Demographic(s)',choices=NULL,multiple=T),
                                                  # Scrollable Data Table
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.gender.tbl_custom'))
                                         ) # Select/Collapse Groups
                                         
                                         
                                  )), # Box - Reviewer Gender
                              # Box for Reviewer Age
                              box(id="demo.differences_reviewer.age",
                                  title='By Reviewer Age',
                                  status='primary', solidHeader=T, width=12, collapsible=T,
                                  collapsed=T,
                                  # Tabs for Tests and Plots
                                  tabBox(width=12,
                                         tabPanel("SS Test and Effect Size",
                                                  br(),
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.age.tbl_ss'))),
                                         tabPanel("Post-hoc Tests",
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.age.tbl_ph'))),
                                         tabPanel("Select/Collapse Groups",
                                                  br(),
                                                  # Report Graphs
                                                  box(id='demo.differences_reviewer.age.graph.box',
                                                      collapsible=T,
                                                      collapsed=T,
                                                      width=12,
                                                      title='Report Graph',
                                                      # Sidebar for plot customisation, change `value` here to change defaults
                                                      sidebar=boxSidebar(
                                                        id='demo.differences_reviewer.age.graph.options',
                                                        width=50,
                                                        tags$div(
                                                          style="font-size: 12px;",
                                                          fluidRow(
                                                            column(11,
                                                                   textInput(inputId='demo.differences_reviewer.age.graph.plot.title',
                                                                             value='Difference in Scores by Reviewer Age',
                                                                             'Plot Title'),
                                                                   textInput(inputId='demo.differences_reviewer.age.graph.plot.subtitle',
                                                                             value='',
                                                                             'Plot Subtitle'),
                                                                   textInput(inputId='demo.differences_reviewer.age.graph.plot.fill_label',
                                                                             value='Demographic Groups',
                                                                             'Legend Caption'),
                                                                   textInput(inputId='demo.differences_reviewer.age.graph.plot.x_title',
                                                                             value='% of the time',
                                                                             'X-axis Title'),
                                                                   textInput(inputId='demo.differences_reviewer.age.graph.plot.y_title',
                                                                             value='',
                                                                             'Y-axis Title')
                                                            )
                                                          ),
                                                          fluidRow(
                                                            
                                                            column(4,
                                                                   numericInput(inputId='demo.differences_reviewer.age.graph.plot.data_label',
                                                                                'Data Label Text Size',
                                                                                value=3,
                                                                                min=1),
                                                                   numericInput(inputId='demo.differences_reviewer.age.graph.plot.axis_label',
                                                                                'Axis Labels Text Size',
                                                                                value=10,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_reviewer.age.graph.plot.axis_title',
                                                                                'Axis Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_reviewer.age.graph.plot.title_size',
                                                                                'Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_reviewer.age.graph.plot.subtitle_size',
                                                                                'Subtitle Text Size',
                                                                                value=10,
                                                                                min=6)
                                                            ),
                                                            column(4,
                                                                   colourInput(inputId='demo.differences_reviewer.age.graph.plot.color1',
                                                                               label='Select Plot Color 1:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_reviewer.age.graph.plot.color2',
                                                                               label='Select Plot Color 2:',
                                                                               value='#BD3B6F',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_reviewer.age.graph.plot.data_label_color',
                                                                               label='Select Data Label Colour:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T)
                                                            )
                                                            
                                                          )
                                                        )
                                                      ),
                                                      # I/O Fluid Layout
                                                      fluidRow(
                                                        # Plot Input
                                                        column(3,
                                                               selectizeInput(inputId='demo.differences_reviewer.age.graph.select',
                                                                              label='Select Cells/Behaviours',choices=NULL,
                                                                              multiple=T,
                                                                              options = list(maxItems=5)),
                                                               br(),
                                                               actionButton(inputId='demo.differences_reviewer.age.graph.plot.generate',label='Draw Graph'),
                                                               br(),
                                                               fluidRow(
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_reviewer.age.graph.plot_width',
                                                                                     'Saved Plot Width',
                                                                                     value=8,
                                                                                     min=5)
                                                                 ),
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_reviewer.age.graph.plot_height',
                                                                                     'Saved Plot Height',
                                                                                     value=5,
                                                                                     min=5)
                                                                 )
                                                               )
                                                        ), # Plot Input
                                                        # Plot Output
                                                        column(9,
                                                               plotOutput('demo.differences_reviewer.age.graph.plot'),
                                                               
                                                               br(),
                                                               downloadButton("demo.differences_reviewer.age.graph.download",label="Save Plot as PNG")
                                                               
                                                        ) # Plot Output
                                                      ) # I/O Fluid Layout
                                                      
                                                  ), # Box - Report Graph
                                                  # Select Group 1 and Group 2
                                                  selectizeInput(inputId='demo.differences_reviewer.age.tbl_custom.select1',
                                                                 label='Select Group 1 Demographic(s)',choices=NULL,multiple=T),
                                                  selectizeInput(inputId='demo.differences_reviewer.age.tbl_custom.select2',
                                                                 label='Select Group 2 Demographic(s)',choices=NULL,multiple=T),
                                                  # Scrollable Data Table
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.age.tbl_custom'))
                                         ) # select/Collapse Groups
                                  )), # Box - Reviewer Age
                              # Box for Reviewer Sexual Orientation
                              box(id="demo.differences_reviewer.sexuality",
                                  title='By Reviewer Sexual Orientation',
                                  status='primary', solidHeader=T, width=12, collapsible=T,
                                  collapsed=T,
                                  # Box for Tests and Plots
                                  tabBox(width=12,
                                         tabPanel("SS Test and Effect Size",
                                                  br(),
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.sexuality.tbl_ss'))),
                                         tabPanel("Post-hoc Tests",
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.sexuality.tbl_ph'))),
                                         # Tab Select Groups and Create Plots
                                         tabPanel("Select/Collapse Groups",
                                                  br(),
                                                  # Report Graph Box
                                                  box(id='demo.differences_reviewer.sexuality.graph.box',
                                                      collapsible=T,
                                                      collapsed=T,
                                                      width=12,
                                                      title='Report Graph',
                                                      # Sidebar for plot customisation, change `value` here to change defaults
                                                      sidebar=boxSidebar(
                                                        id='demo.differences_reviewer.sexuality.graph.options',
                                                        width=50,
                                                        tags$div(
                                                          style="font-size: 12px;",
                                                          fluidRow(
                                                            column(11,
                                                                   textInput(inputId='demo.differences_reviewer.sexuality.graph.plot.title',
                                                                             value='Difference in Scores by Reviewer Sexual Orientation',
                                                                             'Plot Title'),
                                                                   textInput(inputId='demo.differences_reviewer.sexuality.graph.plot.subtitle',
                                                                             value='',
                                                                             'Plot Subtitle'),
                                                                   textInput(inputId='demo.differences_reviewer.sexuality.graph.plot.fill_label',
                                                                             value='Demographic Groups',
                                                                             'Legend Caption'),
                                                                   textInput(inputId='demo.differences_reviewer.sexuality.graph.plot.x_title',
                                                                             value='% of the time',
                                                                             'X-axis Title'),
                                                                   textInput(inputId='demo.differences_reviewer.sexuality.graph.plot.y_title',
                                                                             value='',
                                                                             'Y-axis Title')
                                                            )
                                                          ),
                                                          fluidRow(
                                                            
                                                            column(4,
                                                                   numericInput(inputId='demo.differences_reviewer.sexuality.graph.plot.data_label',
                                                                                'Data Label Text Size',
                                                                                value=3,
                                                                                min=1),
                                                                   numericInput(inputId='demo.differences_reviewer.sexuality.graph.plot.axis_label',
                                                                                'Axis Labels Text Size',
                                                                                value=10,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_reviewer.sexuality.graph.plot.axis_title',
                                                                                'Axis Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_reviewer.sexuality.graph.plot.title_size',
                                                                                'Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_reviewer.sexuality.graph.plot.subtitle_size',
                                                                                'Subtitle Text Size',
                                                                                value=10,
                                                                                min=6)
                                                            ),
                                                            column(4,
                                                                   colourInput(inputId='demo.differences_reviewer.sexuality.graph.plot.color1',
                                                                               label='Select Plot Color 1:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_reviewer.sexuality.graph.plot.color2',
                                                                               label='Select Plot Color 2:',
                                                                               value='#BD3B6F',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_reviewer.sexuality.graph.plot.data_label_color',
                                                                               label='Select Data Label Colour:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T)
                                                            )
                                                            
                                                          )
                                                        )
                                                      ),
                                                      # I/O Fluid Layout
                                                      fluidRow(
                                                        # Plot Input
                                                        column(3,
                                                               selectizeInput(inputId='demo.differences_reviewer.sexuality.graph.select',
                                                                              label='Select Cells/Behaviours',choices=NULL,
                                                                              multiple=T,
                                                                              options = list(maxItems=5)),
                                                               br(),
                                                               actionButton(inputId='demo.differences_reviewer.sexuality.graph.plot.generate',label='Draw Graph'),
                                                               br(),
                                                               fluidRow(
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_reviewer.sexuality.graph.plot_width',
                                                                                     'Saved Plot Width',
                                                                                     value=8,
                                                                                     min=5)
                                                                 ),
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_reviewer.sexuality.graph.plot_height',
                                                                                     'Saved Plot Height',
                                                                                     value=5,
                                                                                     min=5)
                                                                 )
                                                               )
                                                        ), # Plot Input
                                                        # Plot Output
                                                        column(9,
                                                               plotOutput('demo.differences_reviewer.sexuality.graph.plot'),
                                                               
                                                               br(),
                                                               downloadButton("demo.differences_reviewer.sexuality.graph.download",label="Save Plot as PNG")
                                                               
                                                        ) # Plot Output
                                                      ) # I/O Fluid Layout
                                                      
                                                  ), # Box - Report Graphs
                                                  # Select Group 1 and Group 2
                                                  selectizeInput(inputId='demo.differences_reviewer.sexuality.tbl_custom.select1',
                                                                 label='Select Group 1 Demographic(s)',choices=NULL,multiple=T),
                                                  selectizeInput(inputId='demo.differences_reviewer.sexuality.tbl_custom.select2',
                                                                 label='Select Group 2 Demographic(s)',choices=NULL,multiple=T),
                                                  # Scrollable Data Table
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.sexuality.tbl_custom'))
                                         ) # Select/Collapse Group Tab
                                         
                                         
                                  )), # Box - Reviewer Sexual Orientation
                              # Box for Reviewer Sexual Orientation (Collapsed)
                              box(id="demo.differences_reviewer.sexuality_col",
                                  title='By Reviewer Sexual Orientation (Collapsed)',
                                  status='primary', solidHeader=T, width=12, collapsible=T,
                                  collapsed=T,
                                  # Tab Box for Tests
                                  tabBox(width=12,
                                         tabPanel("SS Test and Effect Size",
                                                  br(),
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.sexuality_col.tbl_ss'))),
                                         tabPanel("Post-hoc Tests",
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.sexuality_col.tbl_ph'))))
                                  ), # Box - Reviewer Sexual Orientation (Collapsed)
                              # Box for Reviewer Ethnicity Collapsed
                              box(id="demo.differences_reviewer.ethnicity_col",
                                  title='By Reviewer Ethnicity (Collapsed)',
                                  status='primary', solidHeader=T, width=12, collapsible=T,
                                  collapsed=T,
                                  # Tab Box for SS Tests and Plots
                                  tabBox(width=12,
                                         tabPanel("SS Test and Effect Size",
                                                  br(),
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.ethnicity_col.tbl_ss'))),
                                         tabPanel("Post-hoc Tests",
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.ethnicity_col.tbl_ph'))),
                                         # Tab for Selecting Groups and Plots
                                         tabPanel("Select/Collapse Groups",
                                                  br(),
                                                  # Report Graph Box
                                                  box(id='demo.differences_reviewer.ethnicity_col.graph.box',
                                                      collapsible=T,
                                                      collapsed=T,
                                                      width=12,
                                                      title='Report Graph',
                                                      # Sidebar for plot customisation, change `value` here to change defaults
                                                      sidebar=boxSidebar(
                                                        id='demo.differences_reviewer.ethnicity_col.graph.options',
                                                        width=50,
                                                        tags$div(
                                                          style="font-size: 12px;",
                                                          fluidRow(
                                                            column(11,
                                                                   textInput(inputId='demo.differences_reviewer.ethnicity_col.graph.plot.title',
                                                                             value='Difference in Scores by Reviewer Ethnicity (Collapsed)',
                                                                             'Plot Title'),
                                                                   textInput(inputId='demo.differences_reviewer.ethnicity_col.graph.plot.subtitle',
                                                                             value='',
                                                                             'Plot Subtitle'),
                                                                   textInput(inputId='demo.differences_reviewer.ethnicity_col.graph.plot.fill_label',
                                                                             value='Demographic Groups',
                                                                             'Legend Caption'),
                                                                   textInput(inputId='demo.differences_reviewer.ethnicity_col.graph.plot.x_title',
                                                                             value='% of the time',
                                                                             'X-axis Title'),
                                                                   textInput(inputId='demo.differences_reviewer.ethnicity_col.graph.plot.y_title',
                                                                             value='',
                                                                             'Y-axis Title')
                                                            )
                                                          ),
                                                          fluidRow(
                                                            
                                                            column(4,
                                                                   numericInput(inputId='demo.differences_reviewer.ethnicity_col.graph.plot.data_label',
                                                                                'Data Label Text Size',
                                                                                value=3,
                                                                                min=1),
                                                                   numericInput(inputId='demo.differences_reviewer.ethnicity_col.graph.plot.axis_label',
                                                                                'Axis Labels Text Size',
                                                                                value=10,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_reviewer.ethnicity_col.graph.plot.axis_title',
                                                                                'Axis Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_reviewer.ethnicity_col.graph.plot.title_size',
                                                                                'Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_reviewer.ethnicity_col.graph.plot.subtitle_size',
                                                                                'Subtitle Text Size',
                                                                                value=10,
                                                                                min=6)
                                                            ),
                                                            column(4,
                                                                   colourInput(inputId='demo.differences_reviewer.ethnicity_col.graph.plot.color1',
                                                                               label='Select Plot Color 1:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_reviewer.ethnicity_col.graph.plot.color2',
                                                                               label='Select Plot Color 2:',
                                                                               value='#BD3B6F',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_reviewer.ethnicity_col.graph.plot.data_label_color',
                                                                               label='Select Data Label Colour:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T)
                                                            )
                                                            
                                                          )
                                                        )
                                                      ),
                                                      # I/O Fluid Layout
                                                      fluidRow(
                                                        # Plot Input
                                                        column(3,
                                                               selectizeInput(inputId='demo.differences_reviewer.ethnicity_col.graph.select',
                                                                              label='Select Cells/Behaviours',choices=NULL,
                                                                              multiple=T,
                                                                              options = list(maxItems=5)),
                                                               br(),
                                                               actionButton(inputId='demo.differences_reviewer.ethnicity_col.graph.plot.generate',label='Draw Graph'),
                                                               br(),
                                                               fluidRow(
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_reviewer.ethnicity_col.graph.plot_width',
                                                                                     'Saved Plot Width',
                                                                                     value=8,
                                                                                     min=5)
                                                                 ),
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_reviewer.ethnicity_col.graph.plot_height',
                                                                                     'Saved Plot Height',
                                                                                     value=5,
                                                                                     min=5)
                                                                 )
                                                               )
                                                        ), # Plot Input
                                                        # Plot Output
                                                        column(9,
                                                               plotOutput('demo.differences_reviewer.ethnicity_col.graph.plot'),
                                                               
                                                               br(),
                                                               downloadButton("demo.differences_reviewer.ethnicity_col.graph.download",label="Save Plot as PNG")
                                                               
                                                        ) # Plot Output
                                                      ) # I/O Fluid Layout
                                                      
                                                  ), # Box - Report Graph
                                                  # Select Group 1 and Group 2
                                                  selectizeInput(inputId='demo.differences_reviewer.ethnicity_col.tbl_custom.select1',
                                                                 label='Select Group 1 Demographic(s)',choices=NULL,multiple=T),
                                                  selectizeInput(inputId='demo.differences_reviewer.ethnicity_col.tbl_custom.select2',
                                                                 label='Select Group 2 Demographic(s)',choices=NULL,multiple=T),
                                                  # Scrollable Data Table
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.ethnicity_col.tbl_custom'))
                                         ) # Select/Collapse Group Tab
                                         
                                         
                                  )), # Box - Reviewer Ethnicity (Collapsed)
                              # Box for Reviewer Disability
                              box(id="demo.differences_reviewer.disability",
                                  title='By Reviewer Disability',
                                  status='primary', solidHeader=T, width=12, collapsible=T,
                                  collapsed=T,
                                  # Tab Box for SS Tests and Plots
                                  tabBox(width=12,
                                         tabPanel("SS Test and Effect Size",
                                                  br(),
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.disability.tbl_ss'))),
                                         tabPanel("Post-hoc Tests",
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.disability.tbl_ph'))),
                                         # Select Groups and Create Plot
                                         tabPanel("Select/Collapse Groups",
                                                  br(),
                                                  # Report Graph Box
                                                  box(id='demo.differences_reviewer.disability.graph.box',
                                                      collapsible=T,
                                                      collapsed=T,
                                                      width=12,
                                                      title='Report Graph',
                                                      # Sidebar for plot customisation, change `value` here to change defaults
                                                      sidebar=boxSidebar(
                                                        id='demo.differences_reviewer.disability.graph.options',
                                                        width=50,
                                                        tags$div(
                                                          style="font-size: 12px;",
                                                          fluidRow(
                                                            column(11,
                                                                   textInput(inputId='demo.differences_reviewer.disability.graph.plot.title',
                                                                             value='Difference in Scores by Reviewer Disability',
                                                                             'Plot Title'),
                                                                   textInput(inputId='demo.differences_reviewer.disability.graph.plot.subtitle',
                                                                             value='',
                                                                             'Plot Subtitle'),
                                                                   textInput(inputId='demo.differences_reviewer.disability.graph.plot.fill_label',
                                                                             value='Demographic Groups',
                                                                             'Legend Caption'),
                                                                   textInput(inputId='demo.differences_reviewer.disability.graph.plot.x_title',
                                                                             value='% of the time',
                                                                             'X-axis Title'),
                                                                   textInput(inputId='demo.differences_reviewer.disability.graph.plot.y_title',
                                                                             value='',
                                                                             'Y-axis Title')
                                                            )
                                                          ),
                                                          fluidRow(
                                                            
                                                            column(4,
                                                                   numericInput(inputId='demo.differences_reviewer.disability.graph.plot.data_label',
                                                                                'Data Label Text Size',
                                                                                value=3,
                                                                                min=1),
                                                                   numericInput(inputId='demo.differences_reviewer.disability.graph.plot.axis_label',
                                                                                'Axis Labels Text Size',
                                                                                value=10,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_reviewer.disability.graph.plot.axis_title',
                                                                                'Axis Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_reviewer.disability.graph.plot.title_size',
                                                                                'Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_reviewer.disability.graph.plot.subtitle_size',
                                                                                'Subtitle Text Size',
                                                                                value=10,
                                                                                min=6)
                                                            ),
                                                            column(4,
                                                                   colourInput(inputId='demo.differences_reviewer.disability.graph.plot.color1',
                                                                               label='Select Plot Color 1:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_reviewer.disability.graph.plot.color2',
                                                                               label='Select Plot Color 2:',
                                                                               value='#BD3B6F',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_reviewer.disability.graph.plot.data_label_color',
                                                                               label='Select Data Label Colour:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T)
                                                            )
                                                            
                                                          )
                                                        )
                                                      ),
                                                      # I/O Fluid Layout
                                                      fluidRow(
                                                        # Plot Input
                                                        column(3,
                                                               selectizeInput(inputId='demo.differences_reviewer.disability.graph.select',
                                                                              label='Select Cells/Behaviours',choices=NULL,
                                                                              multiple=T,
                                                                              options = list(maxItems=5)),
                                                               br(),
                                                               actionButton(inputId='demo.differences_reviewer.disability.graph.plot.generate',label='Draw Graph'),
                                                               br(),
                                                               fluidRow(
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_reviewer.disability.graph.plot_width',
                                                                                     'Saved Plot Width',
                                                                                     value=8,
                                                                                     min=5)
                                                                 ),
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_reviewer.disability.graph.plot_height',
                                                                                     'Saved Plot Height',
                                                                                     value=5,
                                                                                     min=5)
                                                                 )
                                                               )
                                                        ), # Plot Input
                                                        # Plot Output
                                                        column(9,
                                                               plotOutput('demo.differences_reviewer.disability.graph.plot'),
                                                               
                                                               br(),
                                                               downloadButton("demo.differences_reviewer.disability.graph.download",label="Save Plot as PNG")
                                                               
                                                        ) # Plot Output
                                                      ) # I/O Fluid Layout
                                                      
                                                  ), # Box - Report Graph
                                                  # Select Group 1 and Group 2 
                                                  selectizeInput(inputId='demo.differences_reviewer.disability.tbl_custom.select1',
                                                                 label='Select Group 1 Demographic(s)',choices=NULL,multiple=T),
                                                  selectizeInput(inputId='demo.differences_reviewer.disability.tbl_custom.select2',
                                                                 label='Select Group 2 Demographic(s)',choices=NULL,multiple=T),
                                                  # Scrollable Data Table
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.disability.tbl_custom'))
                                         ) # Select/Collapse Group Tab
                                         
                                         
                                  )), # Box - Reviewer Disability
                              
                              # Box for Reviewer Disability (Collapsed)
                              box(id="demo.differences_reviewer.disability_col",
                                  title='By Reviewer Disability (Collapsed)',
                                  status='primary', solidHeader=T, width=12, collapsible=T,
                                  collapsed=T,
                                  # Tab Box for SS Tests
                                  tabBox(width=12,
                                         tabPanel("SS Test and Effect Size",
                                                  br(),
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.disability_col.tbl_ss'))),
                                         tabPanel("Post-hoc Tests",
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.disability_col.tbl_ph'))))
                                  ), # Box - Reviewer Disability (Collapsed)
                              # Box for Reviewer Mental Illness
                              box(id="demo.differences_reviewer.mental_illness",
                                  title='By Reviewer Mental Illness',
                                  status='primary', solidHeader=T, width=12, collapsible=T,
                                  collapsed=T,
                                  # Tab Box for SS Tests and Plot
                                  tabBox(width=12,
                                         tabPanel("SS Test and Effect Size",
                                                  br(),
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.mental_illness.tbl_ss'))),
                                         tabPanel("Post-hoc Tests", # Binary response to demographic question so no need for custom groups
                                                  br(),
                                                  fluidRow(
                                                    # Report Graph
                                                    box(id='demo.differences_reviewer.mental_illness.graph.box',
                                                        collapsible=T,
                                                        collapsed=T,
                                                        width=12,
                                                        title='Report Graph',
                                                        # Sidebar for plot customisation, change `value` here to change defaults
                                                        sidebar=boxSidebar(
                                                          id='demo.differences_reviewer.mental_illness.graph.options',
                                                          width=50,
                                                          tags$div(
                                                            style="font-size: 12px;",
                                                            fluidRow(
                                                              column(11,
                                                                     textInput(inputId='demo.differences_reviewer.mental_illness.graph.plot.title',
                                                                               value='Difference in Scores by Reviewer Mental Illness',
                                                                               'Plot Title'),
                                                                     textInput(inputId='demo.differences_reviewer.mental_illness.graph.plot.subtitle',
                                                                               value='',
                                                                               'Plot Subtitle'),
                                                                     textInput(inputId='demo.differences_reviewer.mental_illness.graph.plot.fill_label',
                                                                               value='Demographic Groups',
                                                                               'Legend Caption'),
                                                                     textInput(inputId='demo.differences_reviewer.mental_illness.graph.plot.x_title',
                                                                               value='% of the time',
                                                                               'X-axis Title'),
                                                                     textInput(inputId='demo.differences_reviewer.mental_illness.graph.plot.y_title',
                                                                               value='',
                                                                               'Y-axis Title')
                                                              )
                                                            ),
                                                            fluidRow(
                                                              
                                                              column(4,
                                                                     numericInput(inputId='demo.differences_reviewer.mental_illness.graph.plot.data_label',
                                                                                  'Data Label Text Size',
                                                                                  value=3,
                                                                                  min=1),
                                                                     numericInput(inputId='demo.differences_reviewer.mental_illness.graph.plot.axis_label',
                                                                                  'Axis Labels Text Size',
                                                                                  value=10,
                                                                                  min=6),
                                                                     numericInput(inputId='demo.differences_reviewer.mental_illness.graph.plot.axis_title',
                                                                                  'Axis Title Text Size',
                                                                                  value=12,
                                                                                  min=6),
                                                                     numericInput(inputId='demo.differences_reviewer.mental_illness.graph.plot.title_size',
                                                                                  'Title Text Size',
                                                                                  value=12,
                                                                                  min=6),
                                                                     numericInput(inputId='demo.differences_reviewer.mental_illness.graph.plot.subtitle_size',
                                                                                  'Subtitle Text Size',
                                                                                  value=10,
                                                                                  min=6)
                                                              ),
                                                              column(4,
                                                                     colourInput(inputId='demo.differences_reviewer.mental_illness.graph.plot.color1',
                                                                                 label='Select Plot Color 1:',
                                                                                 value='grey30',
                                                                                 returnName = T,
                                                                                 closeOnClick = T),
                                                                     colourInput(inputId='demo.differences_reviewer.mental_illness.graph.plot.color2',
                                                                                 label='Select Plot Color 2:',
                                                                                 value='#BD3B6F',
                                                                                 returnName = T,
                                                                                 closeOnClick = T),
                                                                     colourInput(inputId='demo.differences_reviewer.mental_illness.graph.plot.data_label_color',
                                                                                 label='Select Data Label Colour:',
                                                                                 value='grey30',
                                                                                 returnName = T,
                                                                                 closeOnClick = T)
                                                              )
                                                              
                                                            )
                                                          )
                                                        ),
                                                        # I/O Fluid Layout
                                                        fluidRow(
                                                          # Plot Input
                                                          column(3,
                                                                 selectizeInput(inputId='demo.differences_reviewer.mental_illness.graph.select',
                                                                                label='Select Cells/Behaviours',choices=NULL,
                                                                                multiple=T,
                                                                                options = list(maxItems=5)),
                                                                 br(),
                                                                 actionButton(inputId='demo.differences_reviewer.mental_illness.graph.plot.generate',label='Draw Graph'),
                                                                 br(),
                                                                 fluidRow(
                                                                   column(width=6,
                                                                          numericInput(inputId='demo.differences_reviewer.mental_illness.graph.plot_width',
                                                                                       'Saved Plot Width',
                                                                                       value=8,
                                                                                       min=5)
                                                                   ),
                                                                   column(width=6,
                                                                          numericInput(inputId='demo.differences_reviewer.mental_illness.graph.plot_height',
                                                                                       'Saved Plot Height',
                                                                                       value=5,
                                                                                       min=5)
                                                                   )
                                                                 )
                                                          ), # Plot Input
                                                          # Plot Output
                                                          column(9,
                                                                 plotOutput('demo.differences_reviewer.mental_illness.graph.plot'),
                                                                 
                                                                 br(),
                                                                 downloadButton("demo.differences_reviewer.mental_illness.graph.download",label="Save Plot as PNG")
                                                                 
                                                          ) # Plot Output
                                                        ) # I/O Fluid Layout
                                                        
                                                    ) # Box - Report Graph
                                                  ),
                                                  fluidRow(
                                                    div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_reviewer.mental_illness.tbl_ph'))
                                                  )))
                                  ) # Box - Reviewer Mental Illness
                              
                          ), # Box - Reviewer Demographic Differences
                          
                          # Box - User Demographic Differences
                          box(id= "demo.differences_user",
                              title='By User',
                              status='primary',solidHeader = T, width=12, collapsible=T,
                              br(),
                              
                              # Box for User Gender
                              box(id="demo.differences_user.gender",
                                  title='By User Gender',
                                  status='primary', solidHeader=T, width=12, collapsible=T,
                                  collapsed=T,
                                  # Tab Box for SS Test and Plots
                                  tabBox(width=12,
                                         tabPanel("SS Test and Effect Size",
                                                  br(),
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.gender.tbl_ss'))),
                                         tabPanel("Post-hoc Tests",
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.gender.tbl_ph'))),
                                         tabPanel("Select/Collapse Groups",
                                                  br(),
                                                  # Report Graph
                                                  box(id='demo.differences_user.gender.graph.box',
                                                      collapsible=T,
                                                      collapsed=T,
                                                      width=12,
                                                      title='Report Graph',
                                                      # Sidebar for plot customisation, change `value` here to change defaults
                                                      sidebar=boxSidebar(
                                                        id='demo.differences_user.gender.graph.options',
                                                        width=50,
                                                        tags$div(
                                                          style="font-size: 12px;",
                                                          fluidRow(
                                                            column(11,
                                                                   textInput(inputId='demo.differences_user.gender.graph.plot.title',
                                                                             value='Difference in Scores by User Gender',
                                                                             'Plot Title'),
                                                                   textInput(inputId='demo.differences_user.gender.graph.plot.subtitle',
                                                                             value='',
                                                                             'Plot Subtitle'),
                                                                   textInput(inputId='demo.differences_user.gender.graph.plot.fill_label',
                                                                             value='Demographic Groups',
                                                                             'Legend Caption'),
                                                                   textInput(inputId='demo.differences_user.gender.graph.plot.x_title',
                                                                             value='% of the time',
                                                                             'X-axis Title'),
                                                                   textInput(inputId='demo.differences_user.gender.graph.plot.y_title',
                                                                             value='',
                                                                             'Y-axis Title')
                                                            )
                                                          ),
                                                          fluidRow(
                                                            
                                                            column(4,
                                                                   numericInput(inputId='demo.differences_user.gender.graph.plot.data_label',
                                                                                'Data Label Text Size',
                                                                                value=3,
                                                                                min=1),
                                                                   numericInput(inputId='demo.differences_user.gender.graph.plot.axis_label',
                                                                                'Axis Labels Text Size',
                                                                                value=10,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_user.gender.graph.plot.axis_title',
                                                                                'Axis Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_user.gender.graph.plot.title_size',
                                                                                'Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_user.gender.graph.plot.subtitle_size',
                                                                                'Subtitle Text Size',
                                                                                value=10,
                                                                                min=6)
                                                            ),
                                                            column(4,
                                                                   colourInput(inputId='demo.differences_user.gender.graph.plot.color1',
                                                                               label='Select Plot Color 1:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_user.gender.graph.plot.color2',
                                                                               label='Select Plot Color 2:',
                                                                               value='#F1B436',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_user.gender.graph.plot.data_label_color',
                                                                               label='Select Data Label Colour:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T)
                                                            )
                                                            
                                                          )
                                                        )
                                                      ),
                                                      # I/O Fluid Layout
                                                      fluidRow(
                                                        # Plot Input
                                                        column(3,
                                                               selectizeInput(inputId='demo.differences_user.gender.graph.select',
                                                                              label='Select Cells/Behaviours',choices=NULL,
                                                                              multiple=T,
                                                                              options = list(maxItems=5)),
                                                               br(),
                                                               actionButton(inputId='demo.differences_user.gender.graph.plot.generate',label='Draw Graph'),
                                                               br(),
                                                               fluidRow(
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_user.gender.graph.plot_width',
                                                                                     'Saved Plot Width',
                                                                                     value=8,
                                                                                     min=5)
                                                                 ),
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_user.gender.graph.plot_height',
                                                                                     'Saved Plot Height',
                                                                                     value=5,
                                                                                     min=5)
                                                                 )
                                                               )
                                                        ), # Plot Input
                                                        # Plot Output
                                                        column(9,
                                                               plotOutput('demo.differences_user.gender.graph.plot'),
                                                               
                                                               br(),
                                                               downloadButton("demo.differences_user.gender.graph.download",label="Save Plot as PNG")
                                                               
                                                        ) # Plot Output
                                                      ) # I/O Fluid Layout
                                                      
                                                  ), # Box - Report Graph
                                                  # Select Group 1 and Group 2
                                                  selectizeInput(inputId='demo.differences_user.gender.tbl_custom.select1',
                                                                 label='Select Group 1 Demographic(s)',choices=NULL,multiple=T),
                                                  selectizeInput(inputId='demo.differences_user.gender.tbl_custom.select2',
                                                                 label='Select Group 2 Demographic(s)',choices=NULL,multiple=T),
                                                  # Scrollable Data Table
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.gender.tbl_custom'))
                                         ) # Tab - Select/COllapse Groups
                                         
                                  )), # Box - User Gender
                              
                              # Box for User Age
                              box(id="demo.differences_user.age",
                                  title='By User Age',
                                  status='primary', solidHeader=T, width=12, collapsible=T,
                                  collapsed=T,
                                  #Tab box for SS Test and Plots
                                  tabBox(width=12,
                                         tabPanel("SS Test and Effect Size",
                                                  br(),
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.age.tbl_ss'))),
                                         tabPanel("Post-hoc Tests",
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.age.tbl_ph'))),
                                         tabPanel("Select/Collapse Groups",
                                                  br(),
                                                  # Report Graph Box
                                                  box(id='demo.differences_user.age.graph.box',
                                                      collapsible=T,
                                                      collapsed=T,
                                                      width=12,
                                                      title='Report Graph',
                                                      # Sidebar for plot customisation, change `value` here to change defaults
                                                      sidebar=boxSidebar(
                                                        id='demo.differences_user.age.graph.options',
                                                        width=50,
                                                        tags$div(
                                                          style="font-size: 12px;",
                                                          fluidRow(
                                                            column(11,
                                                                   textInput(inputId='demo.differences_user.age.graph.plot.title',
                                                                             value='Difference in Scores by User Age',
                                                                             'Plot Title'),
                                                                   textInput(inputId='demo.differences_user.age.graph.plot.subtitle',
                                                                             value='',
                                                                             'Plot Subtitle'),
                                                                   textInput(inputId='demo.differences_user.age.graph.plot.fill_label',
                                                                             value='Demographic Groups',
                                                                             'Legend Caption'),
                                                                   textInput(inputId='demo.differences_user.age.graph.plot.x_title',
                                                                             value='% of the time',
                                                                             'X-axis Title'),
                                                                   textInput(inputId='demo.differences_user.age.graph.plot.y_title',
                                                                             value='',
                                                                             'Y-axis Title')
                                                            )
                                                          ),
                                                          fluidRow(
                                                            
                                                            column(4,
                                                                   numericInput(inputId='demo.differences_user.age.graph.plot.data_label',
                                                                                'Data Label Text Size',
                                                                                value=3,
                                                                                min=1),
                                                                   numericInput(inputId='demo.differences_user.age.graph.plot.axis_label',
                                                                                'Axis Labels Text Size',
                                                                                value=10,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_user.age.graph.plot.axis_title',
                                                                                'Axis Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_user.age.graph.plot.title_size',
                                                                                'Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_user.age.graph.plot.subtitle_size',
                                                                                'Subtitle Text Size',
                                                                                value=10,
                                                                                min=6)
                                                            ),
                                                            column(4,
                                                                   colourInput(inputId='demo.differences_user.age.graph.plot.color1',
                                                                               label='Select Plot Color 1:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_user.age.graph.plot.color2',
                                                                               label='Select Plot Color 2:',
                                                                               value='#F1B436',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_user.age.graph.plot.data_label_color',
                                                                               label='Select Data Label Colour:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T)
                                                            )
                                                            
                                                          )
                                                        )
                                                      ),
                                                      # I/O Fluid Layout
                                                      fluidRow(
                                                        # Plot Input
                                                        column(3,
                                                               selectizeInput(inputId='demo.differences_user.age.graph.select',
                                                                              label='Select Cells/Behaviours',choices=NULL,
                                                                              multiple=T,
                                                                              options = list(maxItems=5)),
                                                               br(),
                                                               actionButton(inputId='demo.differences_user.age.graph.plot.generate',label='Draw Graph'),
                                                               br(),
                                                               fluidRow(
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_user.age.graph.plot_width',
                                                                                     'Saved Plot Width',
                                                                                     value=8,
                                                                                     min=5)
                                                                 ),
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_user.age.graph.plot_height',
                                                                                     'Saved Plot Height',
                                                                                     value=5,
                                                                                     min=5)
                                                                 )
                                                               )
                                                        ), # Plot Input
                                                        # Plot Output
                                                        column(9,
                                                               plotOutput('demo.differences_user.age.graph.plot'),
                                                               
                                                               br(),
                                                               downloadButton("demo.differences_user.age.graph.download",label="Save Plot as PNG")
                                                               
                                                        ) # Plot Output
                                                      ) # I/O Fluid Layout
                                                      
                                                  ), # Box - Report Graphs
                                                  selectizeInput(inputId='demo.differences_user.age.tbl_custom.select1',
                                                                 label='Select Group 1 Demographic(s)',choices=NULL,multiple=T),
                                                  selectizeInput(inputId='demo.differences_user.age.tbl_custom.select2',
                                                                 label='Select Group 2 Demographic(s)',choices=NULL,multiple=T),
                                                  # Scrollable Data Table
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.age.tbl_custom'))
                                         ) # Tab Select/Collapse Groups
                                  )), # Box - User Age
                              
                              # Box for User Sexual Orientation
                              box(id="demo.differences_user.sexuality",
                                  title='By User Sexual Orientation',
                                  status='primary', solidHeader=T, width=12, collapsible=T,
                                  collapsed=T,
                                  # Tab Box for SS Test and Plots
                                  tabBox(width=12,
                                         tabPanel("SS Test and Effect Size",
                                                  br(),
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.sexuality.tbl_ss'))),
                                         tabPanel("Post-hoc Tests",
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.sexuality.tbl_ph'))),
                                         tabPanel("Select/Collapse Groups",
                                                  br(),
                                                  # Box Report Graphs
                                                  box(id='demo.differences_user.sexuality.graph.box',
                                                      collapsible=T,
                                                      collapsed=T,
                                                      width=12,
                                                      title='Report Graph',
                                                      # Sidebar for plot customisation, change `value` here to change defaults
                                                      sidebar=boxSidebar(
                                                        id='demo.differences_user.sexuality.graph.options',
                                                        width=50,
                                                        tags$div(
                                                          style="font-size: 12px;",
                                                          fluidRow(
                                                            column(11,
                                                                   textInput(inputId='demo.differences_user.sexuality.graph.plot.title',
                                                                             value='Difference in Scores by User Sexual Orientation',
                                                                             'Plot Title'),
                                                                   textInput(inputId='demo.differences_user.sexuality.graph.plot.subtitle',
                                                                             value='',
                                                                             'Plot Subtitle'),
                                                                   textInput(inputId='demo.differences_user.sexuality.graph.plot.fill_label',
                                                                             value='Demographic Groups',
                                                                             'Legend Caption'),
                                                                   textInput(inputId='demo.differences_user.sexuality.graph.plot.x_title',
                                                                             value='% of the time',
                                                                             'X-axis Title'),
                                                                   textInput(inputId='demo.differences_user.sexuality.graph.plot.y_title',
                                                                             value='',
                                                                             'Y-axis Title')
                                                            )
                                                          ),
                                                          fluidRow(
                                                            
                                                            column(4,
                                                                   numericInput(inputId='demo.differences_user.sexuality.graph.plot.data_label',
                                                                                'Data Label Text Size',
                                                                                value=3,
                                                                                min=1),
                                                                   numericInput(inputId='demo.differences_user.sexuality.graph.plot.axis_label',
                                                                                'Axis Labels Text Size',
                                                                                value=10,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_user.sexuality.graph.plot.axis_title',
                                                                                'Axis Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_user.sexuality.graph.plot.title_size',
                                                                                'Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_user.sexuality.graph.plot.subtitle_size',
                                                                                'Subtitle Text Size',
                                                                                value=10,
                                                                                min=6)
                                                            ),
                                                            column(4,
                                                                   colourInput(inputId='demo.differences_user.sexuality.graph.plot.color1',
                                                                               label='Select Plot Color 1:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_user.sexuality.graph.plot.color2',
                                                                               label='Select Plot Color 2:',
                                                                               value='#F1B436',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_user.sexuality.graph.plot.data_label_color',
                                                                               label='Select Data Label Colour:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T)
                                                            )
                                                            
                                                          )
                                                        )
                                                      ),
                                                      # I/O Fluid Layout
                                                      fluidRow(
                                                        # Plot Input
                                                        column(3,
                                                               selectizeInput(inputId='demo.differences_user.sexuality.graph.select',
                                                                              label='Select Cells/Behaviours',choices=NULL,
                                                                              multiple=T,
                                                                              options = list(maxItems=5)),
                                                               br(),
                                                               actionButton(inputId='demo.differences_user.sexuality.graph.plot.generate',label='Draw Graph'),
                                                               br(),
                                                               fluidRow(
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_user.sexuality.graph.plot_width',
                                                                                     'Saved Plot Width',
                                                                                     value=8,
                                                                                     min=5)
                                                                 ),
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_user.sexuality.graph.plot_height',
                                                                                     'Saved Plot Height',
                                                                                     value=5,
                                                                                     min=5)
                                                                 )
                                                               )
                                                        ), # Plot Input
                                                        # Plot Output
                                                        column(9,
                                                               plotOutput('demo.differences_user.sexuality.graph.plot'),
                                                               
                                                               br(),
                                                               downloadButton("demo.differences_user.sexuality.graph.download",label="Save Plot as PNG")
                                                               
                                                        ) # Plot Output
                                                      ) # I/O Fluid Layout
                                                      
                                                  ), # Box - Report Graphs
                                                  # Select Group 1 & 2
                                                  selectizeInput(inputId='demo.differences_user.sexuality.tbl_custom.select1',
                                                                 label='Select Group 1 Demographic(s)',choices=NULL,multiple=T),
                                                  selectizeInput(inputId='demo.differences_user.sexuality.tbl_custom.select2',
                                                                 label='Select Group 2 Demographic(s)',choices=NULL,multiple=T),
                                                  # Scrollable Data Table
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.sexuality.tbl_custom'))
                                         ) # Tab Select/Collapse Groups
                                         
                                  )), # Box - User Sexual Orientation
                              
                              # Box for User Sexual Orientation (Collapsed)
                              box(id="demo.differences_user.sexuality_col",
                                  title='By User Sexual Orientation (Collapsed)',
                                  status='primary', solidHeader=T, width=12, collapsible=T,
                                  collapsed=T,
                                  # Tab Box for SS Tests
                                  tabBox(width=12,
                                         tabPanel("SS Test and Effect Size",
                                                  br(),
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.sexuality_col.tbl_ss'))),
                                         tabPanel("Post-hoc Tests",
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.sexuality_col.tbl_ph'))))
                                  ), # Box - User Sexual Orientation (Collapsed)
                              
                              # Box for User Ethnicity (Collapsed)
                              box(id="demo.differences_user.ethnicity_col",
                                  title='By User Ethnicity (Collapsed)',
                                  status='primary', solidHeader=T, width=12, collapsible=T,
                                  collapsed=T,
                                  # Tab Box for SS Tests and Plot
                                  tabBox(width=12,
                                         tabPanel("SS Test and Effect Size",
                                                  br(),
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.ethnicity_col.tbl_ss'))),
                                         tabPanel("Post-hoc Tests",
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.ethnicity_col.tbl_ph'))),
                                         tabPanel("Select/Collapse Groups",
                                                  br(),
                                                  # Report Graph Box
                                                  box(id='demo.differences_user.ethnicity_col.graph.box',
                                                      collapsible=T,
                                                      collapsed=T,
                                                      width=12,
                                                      title='Report Graph',
                                                      # Sidebar for plot customisation, change `value` here to change defaults
                                                      sidebar=boxSidebar(
                                                        id='demo.differences_user.ethnicity_col.graph.options',
                                                        width=50,
                                                        tags$div(
                                                          style="font-size: 12px;",
                                                          fluidRow(
                                                            column(11,
                                                                   textInput(inputId='demo.differences_user.ethnicity_col.graph.plot.title',
                                                                             value='Difference in Scores by User Ethnicity (Collapsed)',
                                                                             'Plot Title'),
                                                                   textInput(inputId='demo.differences_user.ethnicity_col.graph.plot.subtitle',
                                                                             value='',
                                                                             'Plot Subtitle'),
                                                                   textInput(inputId='demo.differences_user.ethnicity_col.graph.plot.fill_label',
                                                                             value='Demographic Groups',
                                                                             'Legend Caption'),
                                                                   textInput(inputId='demo.differences_user.ethnicity_col.graph.plot.x_title',
                                                                             value='% of the time',
                                                                             'X-axis Title'),
                                                                   textInput(inputId='demo.differences_user.ethnicity_col.graph.plot.y_title',
                                                                             value='',
                                                                             'Y-axis Title')
                                                            )
                                                          ),
                                                          fluidRow(
                                                            
                                                            column(4,
                                                                   numericInput(inputId='demo.differences_user.ethnicity_col.graph.plot.data_label',
                                                                                'Data Label Text Size',
                                                                                value=3,
                                                                                min=1),
                                                                   numericInput(inputId='demo.differences_user.ethnicity_col.graph.plot.axis_label',
                                                                                'Axis Labels Text Size',
                                                                                value=10,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_user.ethnicity_col.graph.plot.axis_title',
                                                                                'Axis Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_user.ethnicity_col.graph.plot.title_size',
                                                                                'Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_user.ethnicity_col.graph.plot.subtitle_size',
                                                                                'Subtitle Text Size',
                                                                                value=10,
                                                                                min=6)
                                                            ),
                                                            column(4,
                                                                   colourInput(inputId='demo.differences_user.ethnicity_col.graph.plot.color1',
                                                                               label='Select Plot Color 1:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_user.ethnicity_col.graph.plot.color2',
                                                                               label='Select Plot Color 2:',
                                                                               value='#F1B436',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_user.ethnicity_col.graph.plot.data_label_color',
                                                                               label='Select Data Label Colour:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T)
                                                            )
                                                            
                                                          )
                                                        )
                                                      ),
                                                      # I/O Fluid Layout
                                                      fluidRow(
                                                        # Plot Input
                                                        column(3,
                                                               selectizeInput(inputId='demo.differences_user.ethnicity_col.graph.select',
                                                                              label='Select Cells/Behaviours',choices=NULL,
                                                                              multiple=T,
                                                                              options = list(maxItems=5)),
                                                               br(),
                                                               actionButton(inputId='demo.differences_user.ethnicity_col.graph.plot.generate',label='Draw Graph'),
                                                               br(),
                                                               fluidRow(
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_user.ethnicity_col.graph.plot_width',
                                                                                     'Saved Plot Width',
                                                                                     value=8,
                                                                                     min=5)
                                                                 ),
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_user.ethnicity_col.graph.plot_height',
                                                                                     'Saved Plot Height',
                                                                                     value=5,
                                                                                     min=5)
                                                                 )
                                                               )
                                                        ), # Plot Input
                                                        # Plot Output
                                                        column(9,
                                                               plotOutput('demo.differences_user.ethnicity_col.graph.plot'),
                                                               
                                                               br(),
                                                               downloadButton("demo.differences_user.ethnicity_col.graph.download",label="Save Plot as PNG")
                                                               
                                                        ) # Plot Output
                                                      ) # I/O Fluid Layout
                                                      
                                                  ), # Box - Report Graphs
                                                  # Select Group 1 & 2
                                                  selectizeInput(inputId='demo.differences_user.ethnicity_col.tbl_custom.select1',
                                                                 label='Select Group 1 Demographic(s)',choices=NULL,multiple=T),
                                                  selectizeInput(inputId='demo.differences_user.ethnicity_col.tbl_custom.select2',
                                                                 label='Select Group 2 Demographic(s)',choices=NULL,multiple=T),
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.ethnicity_col.tbl_custom'))
                                         ) # Tab Select/Collapse Group
                                         
                                  )), # Box - User Ethnicity (Collapsed)
                              
                              # Box User Disability
                              box(id="demo.differences_user.disability",
                                  title='By User Disability',
                                  status='primary', solidHeader=T, width=12, collapsible=T,
                                  collapsed=T,
                                  # Tab Box for SS Test and Plots
                                  tabBox(width=12,
                                         tabPanel("SS Test and Effect Size",
                                                  br(),
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.disability.tbl_ss'))),
                                         tabPanel("Post-hoc Tests",
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.disability.tbl_ph'))),
                                         tabPanel("Select/Collapse Groups",
                                                  br(),
                                                  # Report Graph Box
                                                  box(id='demo.differences_user.disability.graph.box',
                                                      collapsible=T,
                                                      collapsed=T,
                                                      width=12,
                                                      title='Report Graph',
                                                      # Sidebar for plot customisation, change `value` here to change defaults
                                                      sidebar=boxSidebar(
                                                        id='demo.differences_user.disability.graph.options',
                                                        width=50,
                                                        tags$div(
                                                          style="font-size: 12px;",
                                                          fluidRow(
                                                            column(11,
                                                                   textInput(inputId='demo.differences_user.disability.graph.plot.title',
                                                                             value='Difference in Scores by User Disability',
                                                                             'Plot Title'),
                                                                   textInput(inputId='demo.differences_user.disability.graph.plot.subtitle',
                                                                             value='',
                                                                             'Plot Subtitle'),
                                                                   textInput(inputId='demo.differences_user.disability.graph.plot.fill_label',
                                                                             value='Demographic Groups',
                                                                             'Legend Caption'),
                                                                   textInput(inputId='demo.differences_user.disability.graph.plot.x_title',
                                                                             value='% of the time',
                                                                             'X-axis Title'),
                                                                   textInput(inputId='demo.differences_user.disability.graph.plot.y_title',
                                                                             value='',
                                                                             'Y-axis Title')
                                                            )
                                                          ),
                                                          fluidRow(
                                                            
                                                            column(4,
                                                                   numericInput(inputId='demo.differences_user.disability.graph.plot.data_label',
                                                                                'Data Label Text Size',
                                                                                value=3,
                                                                                min=1),
                                                                   numericInput(inputId='demo.differences_user.disability.graph.plot.axis_label',
                                                                                'Axis Labels Text Size',
                                                                                value=10,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_user.disability.graph.plot.axis_title',
                                                                                'Axis Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_user.disability.graph.plot.title_size',
                                                                                'Title Text Size',
                                                                                value=12,
                                                                                min=6),
                                                                   numericInput(inputId='demo.differences_user.disability.graph.plot.subtitle_size',
                                                                                'Subtitle Text Size',
                                                                                value=10,
                                                                                min=6)
                                                            ),
                                                            column(4,
                                                                   colourInput(inputId='demo.differences_user.disability.graph.plot.color1',
                                                                               label='Select Plot Color 1:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_user.disability.graph.plot.color2',
                                                                               label='Select Plot Color 2:',
                                                                               value='#F1B436',
                                                                               returnName = T,
                                                                               closeOnClick = T),
                                                                   colourInput(inputId='demo.differences_user.disability.graph.plot.data_label_color',
                                                                               label='Select Data Label Colour:',
                                                                               value='grey30',
                                                                               returnName = T,
                                                                               closeOnClick = T)
                                                            )
                                                            
                                                          )
                                                        )
                                                      ),
                                                      # I/O Fluid Layout
                                                      fluidRow(
                                                        # Plot Inputs
                                                        column(3,
                                                               selectizeInput(inputId='demo.differences_user.disability.graph.select',
                                                                              label='Select Cells/Behaviours',choices=NULL,
                                                                              multiple=T,
                                                                              options = list(maxItems=5)),
                                                               br(),
                                                               actionButton(inputId='demo.differences_user.disability.graph.plot.generate',label='Draw Graph'),
                                                               br(),
                                                               fluidRow(
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_user.disability.graph.plot_width',
                                                                                     'Saved Plot Width',
                                                                                     value=8,
                                                                                     min=5)
                                                                 ),
                                                                 column(width=6,
                                                                        numericInput(inputId='demo.differences_user.disability.graph.plot_height',
                                                                                     'Saved Plot Height',
                                                                                     value=5,
                                                                                     min=5)
                                                                 )
                                                               )
                                                        ), # Plot Input
                                                        # Plot Outputs
                                                        column(9,
                                                               plotOutput('demo.differences_user.disability.graph.plot'),
                                                               
                                                               br(),
                                                               downloadButton("demo.differences_user.disability.graph.download",label="Save Plot as PNG")
                                                               
                                                        ) # Plot Output
                                                      ) # I/O Fluid Layout
                                                      
                                                  ), # Box - Report Graph
                                                  # Select Group 1 & 2 
                                                  selectizeInput(inputId='demo.differences_user.disability.tbl_custom.select1',
                                                                 label='Select Group 1 Demographic(s)',choices=NULL,multiple=T),
                                                  selectizeInput(inputId='demo.differences_user.disability.tbl_custom.select2',
                                                                 label='Select Group 2 Demographic(s)',choices=NULL,multiple=T),
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.disability.tbl_custom'))
                                         ) # Tab Select/Collapse Group
                                         
                                  )), # Box - User Disability
                              
                              # Box User Disability (Collapsed)
                              box(id="demo.differences_user.disability_col",
                                  title='By User Disability (Collapsed)',
                                  status='primary', solidHeader=T, width=12, collapsible=T,
                                  collapsed=T,
                                  # Tab Box for SS Tests
                                  tabBox(width=12,
                                         tabPanel("SS Test and Effect Size",
                                                  br(),
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.disability_col.tbl_ss'))),
                                         tabPanel("Post-hoc Tests",
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.disability_col.tbl_ph'))))
                                  ), # Box User Disability (Collapsed)
                              
                              # Box User Mental Illness
                              box(id="demo.differences_user.mental_illness",
                                  title='By User Mental Illness',
                                  status='primary', solidHeader=T, width=12, collapsible=T,
                                  collapsed=T,
                                  # Tab Box for SS Test and Plot
                                  tabBox(width=12,
                                         tabPanel("SS Test and Effect Size",
                                                  br(),
                                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.mental_illness.tbl_ss'))),
                                         tabPanel("Post-hoc Tests", # No Custom Grouping becuase only binary response to question
                                                  br(),
                                                  fluidRow(
                                                    # Report Graph
                                                    box(id='demo.differences_user.mental_illness.graph.box',
                                                        collapsible=T,
                                                        collapsed=T,
                                                        width=12,
                                                        title='Report Graph',
                                                        # Sidebar for plot customisation, change `value` here to change defaults
                                                        sidebar=boxSidebar(
                                                          id='demo.differences_user.mental_illness.graph.options',
                                                          width=50,
                                                          tags$div(
                                                            style="font-size: 12px;",
                                                            fluidRow(
                                                              column(11,
                                                                     textInput(inputId='demo.differences_user.mental_illness.graph.plot.title',
                                                                               value='Difference in Scores by User Mental Illness',
                                                                               'Plot Title'),
                                                                     textInput(inputId='demo.differences_user.mental_illness.graph.plot.subtitle',
                                                                               value='',
                                                                               'Plot Subtitle'),
                                                                     textInput(inputId='demo.differences_user.mental_illness.graph.plot.fill_label',
                                                                               value='Demographic Groups',
                                                                               'Legend Caption'),
                                                                     textInput(inputId='demo.differences_user.mental_illness.graph.plot.x_title',
                                                                               value='% of the time',
                                                                               'X-axis Title'),
                                                                     textInput(inputId='demo.differences_user.mental_illness.graph.plot.y_title',
                                                                               value='',
                                                                               'Y-axis Title')
                                                              )
                                                            ),
                                                            fluidRow(
                                                              
                                                              column(4,
                                                                     numericInput(inputId='demo.differences_user.mental_illness.graph.plot.data_label',
                                                                                  'Data Label Text Size',
                                                                                  value=3,
                                                                                  min=1),
                                                                     numericInput(inputId='demo.differences_user.mental_illness.graph.plot.axis_label',
                                                                                  'Axis Labels Text Size',
                                                                                  value=10,
                                                                                  min=6),
                                                                     numericInput(inputId='demo.differences_user.mental_illness.graph.plot.axis_title',
                                                                                  'Axis Title Text Size',
                                                                                  value=12,
                                                                                  min=6),
                                                                     numericInput(inputId='demo.differences_user.mental_illness.graph.plot.title_size',
                                                                                  'Title Text Size',
                                                                                  value=12,
                                                                                  min=6),
                                                                     numericInput(inputId='demo.differences_user.mental_illness.graph.plot.subtitle_size',
                                                                                  'Subtitle Text Size',
                                                                                  value=10,
                                                                                  min=6)
                                                              ),
                                                              column(4,
                                                                     colourInput(inputId='demo.differences_user.mental_illness.graph.plot.color1',
                                                                                 label='Select Plot Color 1:',
                                                                                 value='grey30',
                                                                                 returnName = T,
                                                                                 closeOnClick = T),
                                                                     colourInput(inputId='demo.differences_user.mental_illness.graph.plot.color2',
                                                                                 label='Select Plot Color 2:',
                                                                                 value='#F1B436',
                                                                                 returnName = T,
                                                                                 closeOnClick = T),
                                                                     colourInput(inputId='demo.differences_user.mental_illness.graph.plot.data_label_color',
                                                                                 label='Select Data Label Colour:',
                                                                                 value='grey30',
                                                                                 returnName = T,
                                                                                 closeOnClick = T)
                                                              )
                                                              
                                                            )
                                                          )
                                                        ),
                                                        # I/O Fluid Layout
                                                        fluidRow(
                                                          # Plot Input
                                                          column(3,
                                                                 selectizeInput(inputId='demo.differences_user.mental_illness.graph.select',
                                                                                label='Select Cells/Behaviours',choices=NULL,
                                                                                multiple=T,
                                                                                options = list(maxItems=5)),
                                                                 br(),
                                                                 actionButton(inputId='demo.differences_user.mental_illness.graph.plot.generate',label='Draw Graph'),
                                                                 br(),
                                                                 fluidRow(
                                                                   column(width=6,
                                                                          numericInput(inputId='demo.differences_user.mental_illness.graph.plot_width',
                                                                                       'Saved Plot Width',
                                                                                       value=8,
                                                                                       min=5)
                                                                   ),
                                                                   column(width=6,
                                                                          numericInput(inputId='demo.differences_user.mental_illness.graph.plot_height',
                                                                                       'Saved Plot Height',
                                                                                       value=5,
                                                                                       min=5)
                                                                   )
                                                                 )
                                                          ), # Plot Input
                                                          # Plot Output
                                                          column(9,
                                                                 plotOutput('demo.differences_user.mental_illness.graph.plot'),
                                                                 
                                                                 br(),
                                                                 downloadButton("demo.differences_user.mental_illness.graph.download",label="Save Plot as PNG")
                                                                 
                                                          ) # Plot Output
                                                        ) # I/O Fluid Layout
                                                        
                                                    ) # Box - Report Graph
                                                  ), # Tab - Post-hoc Test
                                                  
                                                  fluidRow(
                                                    div(style = 'overflow-x: scroll',DT::dataTableOutput('demo.differences_user.mental_illness.tbl_ph'))
                                                  )))
                                  ) # Box - User Mental Illnness
                              
                          ) # Box - User Demographic Differences
                 ), # Tab - Demographic Differences
                 
                 
                 
          ) # tabBox
        ) # div
      ) # shinyjs::hidden
    ) # dashboardBody
  ) # dashboardPage
) # shinyUI

