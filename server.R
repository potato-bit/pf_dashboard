# Load the neccessary libraries
# Checks if required packages are installed. If not, installs them. 
for (requirement in 
     c("jsonlite", "jtools", "shinydashboard", "shinydashboardPlus", "shinyjs", 
       "shinyWidgets", "shinydisconnect", "shinyFiles", "shinybusy", "zoo",
       "lubridate", "viridis", "DBI", "RPostgres", "psych", "DT", "plotly",
       "ggplot2", "ggthemes","ggrepel", "ggpubr", "treemapify", "rstatix",
       "tidyr", "dplyr","extrafont","forcats")) {  
  if (!require(requirement, character.only = TRUE)) {
    install.packages(requirement, repo="https://cran.rstudio.com")
    library(requirement, character.only = TRUE)
  }
}

#-------------------------------------------------------------------------------
# Server side script
#-------------------------------------------------------------------------------

shinyServer <- function(input, output, session) {
  
  # load supporting functions and required fonts
  source("datadashboard-functions.R")
  loadfonts(device='all')
  
  # load dataset
  ## The production version of this dashboard uses a database to gather and 
  ## update the data in real-time. For the purpopses of this demo, a synthetic 
  ## dataset is used instead
  
  data <- read.csv('data/sample.csv')
  
  
  
  ## for raw data download
  volumes <- c(wd = fs::path_home(), getVolumes()())
  shinyDirChoose(input,'downloadRaw', roots=volumes, defaultRoot='wd', hidden=FALSE)
  observeEvent(input$downloadRaw, {
    outdir <- parseDirPath(volumes, input$downloadRaw)
    cat(outdir)
    if (length(outdir) > 0) {
      filename <- 'Raw-Data'
      honeyc.df <- apply_filters_and_clean(cells.sqlOutput(), input$surveys, input$user_ids, input$date, na_action='keep')
      tbl <- honeyc.df
      timestamp <- format(Sys.time(), "_%Y.%m.%d_%H.%M.%S")
      save_to_csv(tbl, outdir, filename, timestamp)
      #comment_anonymiser(paste0(outdir,'/',filename,'_',timestamp,'.csv'))
    }
    
  })
  
  # initialize object to track the currently selected pop(s) and the 
  # dataframe containing the associated reviews returned by cells.sqlOutput. 
  uiState <- reactiveValues()
  # updated in pop.list()
  uiState$current_org <- NULL
  # updated in cells.sqlOutput
  uiState$current_pops <- integer(0)
  # only fetches reviews is only run if input$pops != uiState$current_pops
  uiState$reviews <- data.frame()
  
  
  # Org IDs and PopIDs would be collected as a reactive element based on what is 
  # collected from the database. Here since we're using a sample dataset, they're 
  # 'hardcoded' instead
  dbState <- data %>% distinct(OrgID,PopID) %>% 
    rename(orgs=OrgID,pops=PopID)
  
  # Populate the organization dropdown with all organizations in the database
  updateSelectizeInput(session, "orgs", server = TRUE,
                       choices = (dbState$orgs), selected = NULL)
  updateSelectizeInput(session, "pops", server = TRUE,
                       choices = sort(dbState$pops), selected = NULL)
  
  shinyjs::show(id = "hiddensidebar")
  
  # When createReport button is pressed show the entire main panel
  observeEvent(input$createReport, {
    shinyjs::disable(id = "downloadRaw")
    shinyjs::show(id = "hiddenbox")
  })
  
  # The next set of observeEvents governs the logic behind populating the
  # organization, population, survey, date range and user_id dropdown filters
  
  
  # Reactive value pop.list gets list of populations to be displayed
  # based on the current state and possibly new selections in the 
  # org/pops dropdowns.
  pop.list <- reactive({
    
    # if no org is selected...  
    if (is.null(input$orgs)) {
      
      # if no pops are selected, return all pops
      if (is.null(input$pops)) {
        pops <- dbState$pops
      } else {
        # otherwise, return the selected pops
        pops <- input$pops        
      }
      
      # update current_org
      uiState$current_org <- NULL
      
      # if the org has changed or no pops are selected, return all pops for the selected org
    } else if (!identical(sort(input$orgs), sort(uiState$current_org)) | is.null(input$pops)) {
      
      # get all pops for this org
      pops <- dbState$pops[dbState$orgs %in% input$orgs]
      
      # update current_org
      uiState$current_org <- input$orgs
      
      # otherwise, the org has not changed, so return the selected pops
    } else {
      
      pops <- input$pops
      
    }
    return(sort(pops))
    
  })
  
  # When an organization is selected from dropdown "orgs", the
  # "pops" dropdown is updated with list of populations in that organization
  # using initial list of orgs & pops, rather than pop.list(), which would
  # update uiState, which we don't want to happen just yet. 
  # All other UI updates happen in the observeEvent for input$orgs and 
  # input$pops, below. It is activated twice when the org changes.
  # The first time it is activated, input$orgs contains the new org selection, 
  # but input$pops still contains the old pops selection. It is called a second 
  # time because updateSelectizeInput in this function changes the selected pop to NULL.
  observeEvent(input$orgs, {
    
    # Whenever the org selection changes, update the pop list
    if (is.null(input$orgs)) {
      pops <- dbState$pops
    } else {
      pops <- dbState$pops[dbState$orgs %in% input$orgs]
    }
    updateSelectizeInput(session, "pops", server = TRUE, selected = NULL,
                         choices = sort(pops))
    
  }, ignoreInit = T, ignoreNULL = F)
  
  # When an organization or population selection changes, 
  # (0) disable the filters if neither has a selection
  # (1) the dropdown "surveys" is updated with a list of surveys associated with that population
  # (2) the dropdown "user_ids" is updated with all user_ids present in that population.
  # (3) the bucket list "other_categories_bucket" is updated with "other" subcategories 
  # (e.g. colleague, peer) in the Relationships tab.
  observeEvent(c(input$orgs, input$pops), {
    
    # disable download when any of the filters change
    shinyjs::disable(id = "downloadRaw")
    
    if (is.null(input$orgs) & is.null(input$pops)) {
      
      # disable the filters and Generate Report button
      shinyjs::disable(id = "surveys")
      shinyjs::disable(id = "user_ids")
      shinyjs::disable(id = "date")
      shinyjs::disable(id = "createReport")      
      
    } else {
      
      # get the population level data and filter it by other filters
      honeyc.df <- apply_filters_and_clean(cells.sqlOutput(), input$surveys, input$user_ids, input$date)
      demo.df <- demo.differences()
      user_demo.df <- user_scores.demo.tbl()
      
      # Populate the "surveys" dropdown in the sidebar with all surveys associated with the 
      # population selected from the "pops" dropdown
      updateSelectizeInput(session, "surveys",
                           choices = sort(unique(honeyc.df$survey_id)))
      
      # Populate the "user_ids" dropdown in the sidebar with all user ids associated with the 
      # population selected from the "pops" dropdown
      updateSelectizeInput(session, "user_ids", server = TRUE, 
                           choices = sort(unique(honeyc.df$user_id)))
      
      
      
      # Update selections for measures of dispersion in Cells
      unique.relationships <- str_to_title(unique(honeyc.df$Relationshiptouser))
      
      updateSelectizeInput(session,"cells_dispersion.relation_select",
                           choices=sort(unique.relationships))
      
      # Update selections for measures of dispersion in Cells & Questions
      unique.relationships <- str_to_title(unique(honeyc.df$Relationshiptouser))
      
      updateSelectizeInput(session,"cells.questions_dispersion.relation_select",
                           choices=sort(unique.relationships))
      
      # Update selections for relationship in Cell Graphs
      updateCheckboxGroupInput(session,"cells_graphs.relation_checkbox",
                               choices=sort(unique.relationships),
                               selected=unique.relationships)
      
      
      # Update selections for relationship in Question Graphs
      updateCheckboxGroupInput(session,"questions_graphs.relation_checkbox",
                               choices=sort(unique.relationships),
                               selected=unique.relationships)
      
      # Update selections for questions in Variability Graphs
      unique.questions <- unique(honeyc.df$Question)
      
      updateSelectizeInput(session,'variability.behaviour_select',
                           choices=sort(unique.questions))
      
      # Update selections for cells in Cell Report Graphs
      unique.cells <- unique(honeyc.df$Cell.name)
      updateSelectizeInput(session,'cells_graphs.cell_select',
                           choices=sort(unique.cells))
      
      # Update selections for cells in Question Report Graphs
      updateSelectizeInput(session,'questions_graphs.cell_select',
                           choices=sort(unique.cells))
      
      # Update Question Graph Behaviour Filters based on Cell selected
      observeEvent(input$questions_graphs.cell_select,{
        unique.graph_questions <- unique(filter(honeyc.df,Cell.name%in%input$questions_graphs.cell_select)$Question)
        updateSelectizeInput(session,"questions_graphs.question_exclude",
                             choices=sort(unique.graph_questions))
      }, ignoreInit = T, ignoreNULL=T)
      
      
      # Update selections for Demo Differences Reviewer Gender
      unique.r_gender <- unique(demo.df$r.gender)
      unique.r_gender <- unique.r_gender[!unique.r_gender%in%c('Prefer not to say','No response','Missing')]
      updateSelectizeInput(session,'demo.differences_reviewer.gender.tbl_custom.select1',
                           choices=sort(unique.r_gender))
      updateSelectizeInput(session,'demo.differences_reviewer.gender.tbl_custom.select2',
                           choices=sort(unique.r_gender))
      
      # Update selections for Demo Differences Reviewer Age
      unique.r_age <- unique(demo.df$r.age)
      unique.r_age <- unique.r_age[unique.r_age!='Missing'&!is.na(unique.r_age)]
      updateSelectizeInput(session,'demo.differences_reviewer.age.tbl_custom.select1',
                           choices=sort(unique.r_age))
      updateSelectizeInput(session,'demo.differences_reviewer.age.tbl_custom.select2',
                           choices=sort(unique.r_age))
      
      # Update selections for Demo Differences Reviewer Sexual Orientation
      unique.r_sexuality <- unique(demo.df$r.sexuality)
      unique.r_sexuality <- unique.r_sexuality[!unique.r_sexuality%in%c('Prefer not to say','No response','Missing')]
      updateSelectizeInput(session,'demo.differences_reviewer.sexuality.tbl_custom.select1',
                           choices=sort(unique.r_sexuality))
      updateSelectizeInput(session,'demo.differences_reviewer.sexuality.tbl_custom.select2',
                           choices=sort(unique.r_sexuality))
      
      # Update selections for Demo Differences Reviewer Ethnicity
      unique.r_ethnicity_col <- unique(demo.df$r.ethnicity_collapsed)
      unique.r_ethnicity_col <- unique.r_ethnicity_col[!unique.r_ethnicity_col%in%c('No response','Missing','Prefer not to say',
                                                                                    'No match')]
      updateSelectizeInput(session,'demo.differences_reviewer.ethnicity_col.tbl_custom.select1',
                           choices=sort(unique.r_ethnicity_col))
      updateSelectizeInput(session,'demo.differences_reviewer.ethnicity_col.tbl_custom.select2',
                           choices=sort(unique.r_ethnicity_col))
      
      # Update selections for Demo Differences Reviewer Disability
      unique.r_disability <- unique(demo.df$r.disability)
      unique.r_disability <- unique.r_disability[!unique.r_disability%in%c('Prefer not to say','No response','Missing')]
      updateSelectizeInput(session,'demo.differences_reviewer.disability.tbl_custom.select1',
                           choices=sort(unique.r_disability))
      updateSelectizeInput(session,'demo.differences_reviewer.disability.tbl_custom.select2',
                           choices=sort(unique.r_disability))
      
      # Update selections for Demo Differences User Gender
      unique.u_gender <- unique(user_demo.df$u.gender)
      unique.u_gender <- unique.u_gender[!unique.u_gender%in%c('Prefer not to say','No response','Missing')]
      updateSelectizeInput(session,'demo.differences_user.gender.tbl_custom.select1',
                           choices=sort(unique.u_gender))
      updateSelectizeInput(session,'demo.differences_user.gender.tbl_custom.select2',
                           choices=sort(unique.u_gender))
      
      # Update selections for Demo Differences User Age
      unique.u_age <- unique(user_demo.df$u.age)
      unique.u_age <- unique.u_age[unique.u_age!='Missing'&!is.na(unique.u_age)]
      updateSelectizeInput(session,'demo.differences_user.age.tbl_custom.select1',
                           choices=sort(unique.u_age))
      updateSelectizeInput(session,'demo.differences_user.age.tbl_custom.select2',
                           choices=sort(unique.u_age))
      
      # Update selections for Demo Differences User Sexual Orientation
      unique.u_sexuality <- unique(user_demo.df$u.sexuality)
      unique.u_sexuality <- unique.u_sexuality[!unique.u_sexuality%in%c('Prefer not to say','No response','Missing')]
      updateSelectizeInput(session,'demo.differences_user.sexuality.tbl_custom.select1',
                           choices=sort(unique.u_sexuality))
      updateSelectizeInput(session,'demo.differences_user.sexuality.tbl_custom.select2',
                           choices=sort(unique.u_sexuality))
      
      # Update selections for Demo Differences User Ethnicity
      unique.u_ethnicity_col <- unique(user_demo.df$u.ethnicity_collapsed)
      unique.u_ethnicity_col <- unique.u_ethnicity_col[!unique.u_ethnicity_col%in%c('No response','Missing','Prefer not to say',
                                                                                    'No match')]
      updateSelectizeInput(session,'demo.differences_user.ethnicity_col.tbl_custom.select1',
                           choices=sort(unique.u_ethnicity_col))
      updateSelectizeInput(session,'demo.differences_user.ethnicity_col.tbl_custom.select2',
                           choices=sort(unique.u_ethnicity_col))
      
      # Update selections for Demo Differences User Disability
      unique.u_disability <- unique(user_demo.df$u.disability)
      unique.u_disability <- unique.u_disability[!unique.u_disability%in%c('Prefer not to say','No response','Missing')]
      updateSelectizeInput(session,'demo.differences_user.disability.tbl_custom.select1',
                           choices=sort(unique.u_disability))
      updateSelectizeInput(session,'demo.differences_user.disability.tbl_custom.select2',
                           choices=sort(unique.u_disability))
      
      
      
      
      
      
      
      
      # enable the filters and Generate Report button
      shinyjs::enable(id = "surveys")
      shinyjs::enable(id = "user_ids")
      shinyjs::enable(id = "date")
      shinyjs::enable(id = "createReport")
    }
    
  }, ignoreInit = T, ignoreNULL = F)
  
  
  # This step is necessary to update user_ids and other categories in the relationships tab 
  # if a survey(s) is/are chosen.
  # Basically if a survey_id is selected from a list the user_id dropdown will be populated
  # only with the user_ids present in that survey. If survey_ids are selected and then deleted 
  # this segment will return the list of user_ids for the entire population
  observeEvent(input$surveys, {
    
    shinyjs::disable(id = "downloadRaw")
    
    honeyc.df.surveys <- apply_filters_and_clean(cells.sqlOutput(), input$surveys, input$user_ids, input$date)
    demo.df <- demo.differences()
    user_demo.df <- user_scores.demo.tbl()
    
    # Update the list of user_ids using the unique user_ids associated with the selected survey(s)
    updateSelectizeInput(session, "user_ids", server = T, 
                         choices = sort(unique(honeyc.df.surveys$user_id)))
    
    # Update selections for measures of dispersion
    unique.relationships <- str_to_title(unique(honeyc.df.surveys$Relationshiptouser))
    
    updateSelectizeInput(session,"cells_dispersion.relation_select",
                         choices=sort(unique.relationships))
    
    # Update selections for measures of dispersion in Cells & Questions
    unique.relationships <- str_to_title(unique(honeyc.df.surveys$Relationshiptouser))
    
    updateSelectizeInput(session,"cells.questions_dispersion.relation_select",
                         choices=sort(unique.relationships))
    
    # Update selections for relationship in Cell Graphs
    updateCheckboxGroupInput(session,"cells_graphs.relation_checkbox",
                             choices=sort(unique.relationships),
                             selected=unique.relationships)
    
    
    # Update selections for relationship in Question Graphs
    updateCheckboxGroupInput(session,"questions_graphs.relation_checkbox",
                             choices=sort(unique.relationships),
                             selected=unique.relationships)
    
    # Update selections for questions in Variability Graphs
    unique.questions <- unique(honeyc.df.surveys$Question)
    
    updateSelectizeInput(session,'variability.behaviour_select',
                         choices=sort(unique.questions))
    # Update selections for cells in Cell Report Graphs
    unique.cells <- unique(honeyc.df.surveys$Cell.name)
    updateSelectizeInput(session,'cells_graphs.cell_select',
                         choices=sort(unique.cells))
    
    # Update selections for cells in Question Report Graphs
    updateSelectizeInput(session,'questions_graphs.cell_select',
                         choices=sort(unique.cells))
    
    # Update Question Graph Behaviour Filters based on Cell selected
    observeEvent(input$questions_graphs.cell_select,{
      unique.graph_questions <- unique(filter(honeyc.df.surveys,Cell.name%in%input$questions_graphs.cell_select)$Question)
      updateSelectizeInput(session,"questions_graphs.question_exclude",
                           choices=sort(unique.graph_questions))
    }, ignoreInit = T, ignoreNULL=T)
    
    # Update selections for Demo Differences Reviewer Gender
    unique.r_gender <- unique(demo.df$r.gender)
    unique.r_gender <- unique.r_gender[!unique.r_gender%in%c('Prefer not to say','No response','Missing')]
    updateSelectizeInput(session,'demo.differences_reviewer.gender.tbl_custom.select1',
                         choices=sort(unique.r_gender))
    updateSelectizeInput(session,'demo.differences_reviewer.gender.tbl_custom.select2',
                         choices=sort(unique.r_gender))
    
    # Update selections for Demo Differences Reviewer Age
    unique.r_age <- unique(demo.df$r.age)
    unique.r_age <- unique.r_age[unique.r_age!='Missing'&!is.na(unique.r_age)]
    updateSelectizeInput(session,'demo.differences_reviewer.age.tbl_custom.select1',
                         choices=sort(unique.r_age))
    updateSelectizeInput(session,'demo.differences_reviewer.age.tbl_custom.select2',
                         choices=sort(unique.r_age))
    
    # Update selections for Demo Differences Reviewer Sexual Orientation
    unique.r_sexuality <- unique(demo.df$r.sexuality)
    unique.r_sexuality <- unique.r_sexuality[!unique.r_sexuality%in%c('Prefer not to say','No response','Missing')]
    updateSelectizeInput(session,'demo.differences_reviewer.sexuality.tbl_custom.select1',
                         choices=sort(unique.r_sexuality))
    updateSelectizeInput(session,'demo.differences_reviewer.sexuality.tbl_custom.select2',
                         choices=sort(unique.r_sexuality))
    
    # Update selections for Demo Differences Reviewer Ethnicity
    unique.r_ethnicity_col <- unique(demo.df$r.ethnicity_collapsed)
    unique.r_ethnicity_col <- unique.r_ethnicity_col[!unique.r_ethnicity_col%in%c('No response','Missing','Prefer not to say',
                                                                                  'No match')]
    updateSelectizeInput(session,'demo.differences_reviewer.ethnicity_col.tbl_custom.select1',
                         choices=sort(unique.r_ethnicity_col))
    updateSelectizeInput(session,'demo.differences_reviewer.ethnicity_col.tbl_custom.select2',
                         choices=sort(unique.r_ethnicity_col))
    
    # Update selections for Demo Differences Reviewer Disability
    unique.r_disability <- unique(demo.df$r.disability)
    unique.r_disability <- unique.r_disability[!unique.r_disability%in%c('Prefer not to say','No response','Missing')]
    updateSelectizeInput(session,'demo.differences_reviewer.disability.tbl_custom.select1',
                         choices=sort(unique.r_disability))
    updateSelectizeInput(session,'demo.differences_reviewer.disability.tbl_custom.select2',
                         choices=sort(unique.r_disability))
    
    # Update selections for Demo Differences User Gender
    unique.u_gender <- unique(user_demo.df$u.gender)
    unique.u_gender <- unique.u_gender[!unique.u_gender%in%c('Prefer not to say','No response','Missing')]
    updateSelectizeInput(session,'demo.differences_user.gender.tbl_custom.select1',
                         choices=sort(unique.u_gender))
    updateSelectizeInput(session,'demo.differences_user.gender.tbl_custom.select2',
                         choices=sort(unique.u_gender))
    
    # Update selections for Demo Differences User Age
    unique.u_age <- unique(user_demo.df$u.age)
    unique.u_age <- unique.u_age[unique.u_age!='Missing'&!is.na(unique.u_age)]
    updateSelectizeInput(session,'demo.differences_user.age.tbl_custom.select1',
                         choices=sort(unique.u_age))
    updateSelectizeInput(session,'demo.differences_user.age.tbl_custom.select2',
                         choices=sort(unique.u_age))
    
    # Update selections for Demo Differences User Sexual Orientation
    unique.u_sexuality <- unique(user_demo.df$u.sexuality)
    unique.u_sexuality <- unique.u_sexuality[!unique.u_sexuality%in%c('Prefer not to say','No response','Missing')]
    updateSelectizeInput(session,'demo.differences_user.sexuality.tbl_custom.select1',
                         choices=sort(unique.u_sexuality))
    updateSelectizeInput(session,'demo.differences_user.sexuality.tbl_custom.select2',
                         choices=sort(unique.u_sexuality))
    
    # Update selections for Demo Differences User Ethnicity
    unique.u_ethnicity_col <- unique(user_demo.df$u.ethnicity_collapsed)
    unique.u_ethnicity_col <- unique.u_ethnicity_col[!unique.u_ethnicity_col%in%c('No response','Missing','Prefer not to say',
                                                                                  'No match')]
    updateSelectizeInput(session,'demo.differences_user.ethnicity_col.tbl_custom.select1',
                         choices=sort(unique.u_ethnicity_col))
    updateSelectizeInput(session,'demo.differences_user.ethnicity_col.tbl_custom.select2',
                         choices=sort(unique.u_ethnicity_col))
    
    # Update selections for Demo Differences User Disability
    unique.u_disability <- unique(user_demo.df$u.disability)
    unique.u_disability <- unique.u_disability[!unique.u_disability%in%c('Prefer not to say','No response','Missing')]
    updateSelectizeInput(session,'demo.differences_user.disability.tbl_custom.select1',
                         choices=sort(unique.u_disability))
    updateSelectizeInput(session,'demo.differences_user.disability.tbl_custom.select2',
                         choices=sort(unique.u_disability))
    
    
    
    
    
  }, ignoreInit = T, ignoreNULL = F)
  
  
  # The next segment updates surveys, user_ids and bucket list in the Relationships tab
  # if a date range has been selected from the "date" selector
  # This allows the end user to select a date and then only have access to surveys 
  # (surveys dropdown) that were filled out within that time frame and only users 
  # (user_ids dropdown) who did those surveys.
  # Finally the bucket list in the relationships tab is populated with other 
  # categories only found in those surveys.
  # Note: the dropdowns are updated only if both dates are non-empty
  observeEvent(c(input$date[1], input$date[2]), {
    
    shinyjs::disable(id = "downloadRaw")
    
    # Pull population level data and apply all filters except date
    honeyc.df <- apply_filters_and_clean(cells.sqlOutput(), input$surveys, input$user_ids)
    demo.df <- demo.differences()
    user_demo.df <- user_scores.demo.tbl()
    
    # Date filter 
    ## If both dates are present -> subset the data using those dates
    ## Otherwise -> don't subset by date
    if(!is.na(input$date[1]) & !is.na(input$date[2])){
      
      honeyc.df <- subset(honeyc.df, created_at >= as.Date(input$date[1]) & 
                            created_at <= as.Date(input$date[2]))
    }
    
    # Update surveys dropdown using the subsetted dataset by date
    updateSelectizeInput(session, "surveys", server = TRUE,
                         choices = unique(honeyc.df$survey_id))
    
    # Update user_ids dropdown using the subsetted dataset by date
    updateSelectizeInput(session, "user_ids", server = TRUE, 
                         choices = unique(honeyc.df$user_id))
    
    # Identify unique other categories for a particular date range
    unique.other.cats <- honeyc.df %>%
      filter(Relationshiptouser %in% "other") %>%
      summarise(other_cats = unique(name))   
    
    # Populate bucket list in the relationships tab with other categories in response 
    # a change in the selected date range
    updateMultiInput(session, "other_categories_bucket", 
                     choices = unique.other.cats$other_cats)
    
    # Update selections for measures of dispersion
    unique.relationships <- str_to_title(unique(honeyc.df$Relationshiptouser))
    
    updateSelectizeInput(session,"cells_dispersion.relation_select",
                         choices=sort(unique.relationships))
    
    # Update selections for measures of dispersion in Cells & Questions
    unique.relationships <- str_to_title(unique(honeyc.df$Relationshiptouser))
    
    updateSelectizeInput(session,"cells.questions_dispersion.relation_select",
                         choices=sort(unique.relationships))
    
    # Update selections for relationship in Cell Graphs
    updateCheckboxGroupInput(session,"cells_graphs.relation_checkbox",
                             choices=sort(unique.relationships),
                             selected=unique.relationships)
    
    # Update selections for relationship in Question Graphs
    updateCheckboxGroupInput(session,"questions_graphs.relation_checkbox",
                             choices=sort(unique.relationships),
                             selected=unique.relationships)
    
    # Update selections for questions in Variability Graphs
    unique.questions <- unique(honeyc.df$Question)
    
    updateSelectizeInput(session,'variability.behaviour_select',
                         choices=sort(unique.questions))
    # Update selections for cells in Cell Report Graphs
    unique.cells <- unique(honeyc.df$Cell.name)
    updateSelectizeInput(session,'cells_graphs.cell_select',
                         choices=sort(unique.cells))
    
    # Update selections for cells in Question Report Graphs
    updateSelectizeInput(session,'questions_graphs.cell_select',
                         choices=sort(unique.cells))
    # Update Question Graph Behaviour Filters based on Cell selected
    observeEvent(input$questions_graphs.cell_select,{
      unique.graph_questions <- unique(filter(honeyc.df,Cell.name%in%input$questions_graphs.cell_select)$Question)
      updateSelectizeInput(session,"questions_graphs.question_exclude",
                           choices=sort(unique.graph_questions))
    }, ignoreInit = T, ignoreNULL=T)
    
    # Update selections for Demo Differences Reviewer Gender
    unique.r_gender <- unique(demo.df$r.gender)
    unique.r_gender <- unique.r_gender[!unique.r_gender%in%c('Prefer not to say','No response','Missing')]
    updateSelectizeInput(session,'demo.differences_reviewer.gender.tbl_custom.select1',
                         choices=sort(unique.r_gender))
    updateSelectizeInput(session,'demo.differences_reviewer.gender.tbl_custom.select2',
                         choices=sort(unique.r_gender))
    
    # Update selections for Demo Differences Reviewer Age
    unique.r_age <- unique(demo.df$r.age)
    unique.r_age <- unique.r_age[unique.r_age!='Missing'&!is.na(unique.r_age)]
    updateSelectizeInput(session,'demo.differences_reviewer.age.tbl_custom.select1',
                         choices=sort(unique.r_age))
    updateSelectizeInput(session,'demo.differences_reviewer.age.tbl_custom.select2',
                         choices=sort(unique.r_age))
    
    # Update selections for Demo Differences Reviewer Sexual Orientation
    unique.r_sexuality <- unique(demo.df$r.sexuality)
    unique.r_sexuality <- unique.r_sexuality[!unique.r_sexuality%in%c('Prefer not to say','No response','Missing')]
    updateSelectizeInput(session,'demo.differences_reviewer.sexuality.tbl_custom.select1',
                         choices=sort(unique.r_sexuality))
    updateSelectizeInput(session,'demo.differences_reviewer.sexuality.tbl_custom.select2',
                         choices=sort(unique.r_sexuality))
    
    # Update selections for Demo Differences Reviewer Ethnicity
    unique.r_ethnicity_col <- unique(demo.df$r.ethnicity_collapsed)
    unique.r_ethnicity_col <- unique.r_ethnicity_col[!unique.r_ethnicity_col%in%c('No response','Missing','Prefer not to say',
                                                                                  'No match')]
    updateSelectizeInput(session,'demo.differences_reviewer.ethnicity_col.tbl_custom.select1',
                         choices=sort(unique.r_ethnicity_col))
    updateSelectizeInput(session,'demo.differences_reviewer.ethnicity_col.tbl_custom.select2',
                         choices=sort(unique.r_ethnicity_col))
    
    # Update selections for Demo Differences Reviewer Disability
    unique.r_disability <- unique(demo.df$r.disability)
    unique.r_disability <- unique.r_disability[!unique.r_disability%in%c('Prefer not to say','No response','Missing')]
    updateSelectizeInput(session,'demo.differences_reviewer.disability.tbl_custom.select1',
                         choices=sort(unique.r_disability))
    updateSelectizeInput(session,'demo.differences_reviewer.disability.tbl_custom.select2',
                         choices=sort(unique.r_disability))
    
    # Update selections for Demo Differences User Gender
    unique.u_gender <- unique(user_demo.df$u.gender)
    unique.u_gender <- unique.u_gender[!unique.u_gender%in%c('Prefer not to say','No response','Missing')]
    updateSelectizeInput(session,'demo.differences_user.gender.tbl_custom.select1',
                         choices=sort(unique.u_gender))
    updateSelectizeInput(session,'demo.differences_user.gender.tbl_custom.select2',
                         choices=sort(unique.u_gender))
    
    # Update selections for Demo Differences User Age
    unique.u_age <- unique(user_demo.df$u.age)
    unique.u_age <- unique.u_age[unique.u_age!='Missing'&!is.na(unique.u_age)]
    updateSelectizeInput(session,'demo.differences_user.age.tbl_custom.select1',
                         choices=sort(unique.u_age))
    updateSelectizeInput(session,'demo.differences_user.age.tbl_custom.select2',
                         choices=sort(unique.u_age))
    
    # Update selections for Demo Differences User Sexual Orientation
    unique.u_sexuality <- unique(user_demo.df$u.sexuality)
    unique.u_sexuality <- unique.u_sexuality[!unique.u_sexuality%in%c('Prefer not to say','No response','Missing')]
    updateSelectizeInput(session,'demo.differences_user.sexuality.tbl_custom.select1',
                         choices=sort(unique.u_sexuality))
    updateSelectizeInput(session,'demo.differences_user.sexuality.tbl_custom.select2',
                         choices=sort(unique.u_sexuality))
    
    # Update selections for Demo Differences User Ethnicity
    unique.u_ethnicity_col <- unique(user_demo.df$u.ethnicity_collapsed)
    unique.u_ethnicity_col <- unique.u_ethnicity_col[!unique.u_ethnicity_col%in%c('No response','Missing','Prefer not to say',
                                                                                  'No match')]
    updateSelectizeInput(session,'demo.differences_user.ethnicity_col.tbl_custom.select1',
                         choices=sort(unique.u_ethnicity_col))
    updateSelectizeInput(session,'demo.differences_user.ethnicity_col.tbl_custom.select2',
                         choices=sort(unique.u_ethnicity_col))
    
    # Update selections for Demo Differences User Disability
    unique.u_disability <- unique(user_demo.df$u.disability)
    unique.u_disability <- unique.u_disability[!unique.u_disability%in%c('Prefer not to say','No response','Missing')]
    updateSelectizeInput(session,'demo.differences_user.disability.tbl_custom.select1',
                         choices=sort(unique.u_disability))
    updateSelectizeInput(session,'demo.differences_user.disability.tbl_custom.select2',
                         choices=sort(unique.u_disability))
    
    
    
    
    
  }, ignoreInit = T, ignoreNULL = T)
  
  # Update Cell Graph Filters based on Relationships selected
  observeEvent(input$cells_graphs.relation_checkbox,{
    
    # Update selections for filters in Cell Graphs
    updateCheckboxGroupInput(session,"cells_graphs.filter_checkbox",
                             choices=input$cells_graphs.relation_checkbox)
    
  }, ignoreInit = T, ignoreNULL=T)
  
  # Update Question Graph Filters based on Relationship selected
  observeEvent(input$questions_graphs.relation_checkbox,{
    
    # Update selections for filters in Question Graphs
    updateCheckboxGroupInput(session,"questions_graphs.filter_checkbox",
                             choices=input$questions_graphs.relation_checkbox)
  }, ignoreInit=T, ignoreNULL=T)
  
  # This section runs a SQL query to fetch all the data in response to changes in
  # pop.list(). If the query finds no matches, it will return an empty dataframe.
  # The results of this query are used to update the other filter dropdowns. 
  cells.sqlOutput <- reactive({
    
    pops <- pop.list()
    if (is.empty(pops)) return(data.frame())
    
    # if the list of pops hasn't changed, return the cached results
    if (identical(sort(pops), sort(uiState$current_pops))) {
      return(uiState$reviews)
    }
    
    
    # Fetch the data
    honeyc.df <- data %>% filter(PopID %in% pops)
    
    
    honeyc.df$Relationshiptouser <- factor(honeyc.df$Relationshiptouser)
    
    # Make survey_id numeric
    honeyc.df$survey_id <- as.numeric(honeyc.df$survey_id)
    
    # Update state
    uiState$current_pops <- pops
    uiState$reviews <- honeyc.df
    
    return(honeyc.df)
    
  })
  
  #-------ENGAGEMENT CALCULATION-------#
  ## insert code for engagement calculations here
  #------------------------------------#
  
  # User Scores Dataframe
  user_scores.tbl <- reactive({
    df <- cells.sqlOutput()
    
    if(nrow(df) == 0) {
      
      # enable the Download All Reports button
      shinyjs::enable(id = 'downloadRaw')
      
      return(NULL)  
    }
    
    df <- apply_filters_and_clean(df, input$surveys, input$user_ids, input$date)
    
    # Creating a list of users who have feedback from at least 3 reviewers
    user_list <- df %>% filter(Relationshiptouser!='self') %>% group_by(user_id) %>% 
      summarise(n=n_distinct(reviewer_id)) %>% 
      filter(n>=3)
    
    df <- df %>% filter(user_id %in% user_list$user_id)
    
    # Adding user demographics as new columns to DF
    users_demo <- df %>% filter(Relationshiptouser=='self') %>% 
      select(user_id,reviewer_gender,reviewer_sexuality,reviewer_ethnicity,
             reviewer_age,reviewer_disability,reviewer_mental_illness) %>% 
      distinct() 
    colnames(users_demo) <- c('user_id','user_gender','user_sexuality',
                              'user_ethnicity','user_age','user_disability',
                              'user_mental_illness')
    
    df <- merge(df,users_demo,by='user_id')
    
    # Aggregating by reviewer score per user
    users <- df %>% filter(Relationshiptouser!='self') %>% group_by(user_id,PopID,Cell.name,Question) %>% 
      summarise(mean=mean(original_value,na.rm=T),
                n=n_distinct(reviewer_id)) %>% 
      filter(n>=3)
    
    users <- merge(users,users_demo,by='user_id')
    
    cols.to.change <- c('user_gender','user_sexuality',
                        'user_ethnicity','user_disability','user_mental_illness')
    users[,cols.to.change] <- lapply(users[, cols.to.change], function(x) {
      x <- gsub('[\\"\\}\\{]', "", x)
      x <- as.character(ifelse(is.na(x), "Missing", x))
      x <- as.character(ifelse(x == "", "No response", x))
      x <- as.character(ifelse(grepl('Prefer not to say',x,fixed=T),'Prefer not to say', x))
    })
    
    colnames(users) <- ifelse(grepl("id$",colnames(users)),
                              colnames(users),
                              sub("^user_","u.",colnames(users)))
    
    users <- recode_ethnicity(users,'u.ethnicity')
    
    
    users <- users %>% mutate(u.age=case_when(
      u.age == 'Prefer not to say' ~ 'Missing',
      .default=u.age
    ))
    
    users <- users %>% mutate(
      u.sexuality_col=case_when(
        u.sexuality %in% c('No response','Missing','Prefer not to say') ~ 'Missing',
        u.sexuality == 'Heterosexual or straight' ~ 'Heterosexual or straight',
        .default = 'Not heterosexual or straight'
      )
    )
    
    users <- users %>% mutate(
      u.disability_col=case_when(
        u.disability %in% c('No response','Missing','Prefer not to say') ~ 'Missing',
        u.disability == 'No disability' ~ 'No disability',
        .default = 'Has disability'
      ))
    
    # enable the Download All Reports button
    shinyjs::enable(id = 'downloadRaw')
    
    return(users)
    
  })
  
  # User Scores Table Construction
  
  
  user_scores.sum.tbl <- eventReactive(input$createReport,{
    df <- user_scores.tbl() 
    df_cells <- data.frame()
    df_questions <- data.frame()
    if (nrow(df) == 0) {
      df_cells <- NULL
      df_questions <- NULL
    } else {
      # For the cells, we first need to summarise scores by Cell per user, and then summarise that value
      # again grouped by Cell
      df_cells <- df %>% group_by(user_id,Cell.name) %>% summarise(avg=mean(mean,na.rm=T)) %>% 
        group_by(Cell.name) %>% 
        get_summary_stats(avg,show=c('mean','sd','min','q1','median','q3','max','iqr')) %>% 
        select(!variable) %>% 
        rename(Cell=Cell.name) %>% arrange(mean)
      
      df_questions <- df %>% group_by(Cell.name,Question) %>% 
        get_summary_stats(mean,show=c('mean','sd','min','q1','median','q3','max','iqr')) %>% 
        select(!variable) %>% 
        rename(Cell=Cell.name) %>% arrange(mean)
      
      
    } 
    
    # Updating the inputs for the graphs after the datatable is constructed
    updateSelectizeInput(session,'box_userscores_questions.graphs.cell_select',
                         choices=sort(unique(df_cells$Cell)))
    updateSelectizeInput(session,'box_userscores_questions.dotplot.question_select',
                         choices=unique(df_questions$Question),
                         selected = unique(df_questions$Question)[1:10])
    updateSelectizeInput(session,'box_userscores_cells.graphs.cell_select',
                         choices=sort(unique(df_cells$Cell)))
    
    
    
    
    return(list(df_cells,df_questions))
  })
  
  # Observes which hcell has been selected for the User Question Report Graphs and
  # updates the inputs for the list of questions available to select to exclude
  observeEvent(input$box_userscores_questions.graphs.cell_select,{
    df <- user_scores.sum.tbl()[[2]]
    df <- df %>% filter(Cell%in%input$box_userscores_questions.graphs.cell_select)
    updateSelectizeInput(session,'box_userscores_questions.graphs.question_exclude',
                         choices=sort(unique(df$Question)))
  })
  
  
  
  
  
  
  # User Scores Cell Table Output
  output$userscores_cells.sum <- DT::renderDataTable({
    df <- user_scores.sum.tbl()[[1]]
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    df
    
  })
  
  
  # User Scores Cell Report Graph Output
  
  graph.userscores_cells <- eventReactive(input$box_userscores_cells.graphs.generate, {
    
    if (length(input$box_userscores_cells.graphs.cell_select)<=1) {
      showNotification("Please select 2 or more cells", type = "warning", duration = 3)}
    
    req(length(input$box_userscores_cells.graphs.cell_select)>1)
    
    # Need to get a distribution of user scores by cell so cannot use the table constructed for the 
    # data table output
    users <- user_scores.tbl() %>% filter(Cell.name %in% input$box_userscores_cells.graphs.cell_select)
    graph_df <- users %>% 
      group_by(user_id,Cell.name) %>% summarise(avg=mean(mean,na.rm=T)) %>%
      mutate(Cell.name=fct_reorder(as_factor(Cell.name),avg,.fun=mean))
    reordered_levels <- users %>% 
      group_by(user_id,Cell.name) %>% summarise(avg=mean(mean,na.rm=T)) %>% 
      group_by(Cell.name) %>% summarise(avg=mean(avg,na.rm=T)) %>% 
      arrange(avg) %>% mutate(Cell.name=as_factor(Cell.name)) %>% 
      pull(Cell.name) %>% 
      levels()
    
    
    # Warning message to inform that cell has no reviews
    if (nrow(filter(graph_df,is.na(avg)))!=0) {
      empty_cells <- graph_df %>% filter(is.na(avg)) %>% 
        mutate(message=paste0(Cell.name,' has no reviews'))
      
      for (x in empty_cells$message) {
        showNotification(paste(x), type = "warning", duration = 5)
      }
      
      graph_df <- graph_df %>% filter(!is.na(avg))
    }
    
    
    # Creating a vector of colors and mapping it to the factor levels of the dataframe
    # in order of their mean value
    graph.colours <- c(input$box_userscores_cells.graphs.plot.color1,input$box_userscores_cells.graphs.plot.color2,
                       input$box_userscores_cells.graphs.plot.color3,input$box_userscores_cells.graphs.plot.color4,
                       input$box_userscores_cells.graphs.plot.color5,input$box_userscores_cells.graphs.plot.color6)
    color_mapping <- setNames(graph.colours[seq_along(rev(reordered_levels))], rev(reordered_levels))
    
    # Creating the plot
    p <- graph_df %>% ggplot(aes(x=avg/10,y=reorder(Cell.name,avg),fill=Cell.name)) + 
      geom_boxplot(outliers=F,color='grey35',staplewidth = 0.2) + 
      geom_point(data=summarise(group_by(summarise(group_by(users,user_id,Cell.name),avg=mean(mean)),Cell.name),
                                avg=mean(avg,na.rm=T)),
                 aes(x=avg/10,y=Cell.name),size=4,
                 color=input$box_userscores_cells.graphs.plot.data_label_color) +
      geom_text(data=summarise(group_by(summarise(group_by(users,user_id,Cell.name),avg=mean(mean)),Cell.name),
                               avg=mean(avg,na.rm=T)),
                aes(x=avg/10,y=Cell.name,label=paste0(round(avg*10),'%')),family='Work Sans',
                size=input$box_userscores_cells.graphs.plot.data_label,
                vjust=-1.8,
                color=input$box_userscores_cells.graphs.plot.data_label_color) +
      geom_text(data=summarise(group_by(summarise(group_by(users,user_id,Cell.name),avg=mean(mean)),Cell.name),
                               avg=quantile(avg,0.75,na.rm=T),n=n_distinct(user_id)),
                aes(label=paste0(n,' users')),size=input$box_userscores_cells.graphs.plot.data_auxlabel,
                hjust=-.2,vjust=-1,
                family='Work Sans', color=graph.colors()) +
      theme_pubr() +
      theme(text=element_text(family='Work Sans',color=graph.colors()),
            title = element_text(face='bold',size=input$box_userscores_cells.graphs.plot.title_size,color=graph.colors()),
            axis.title = element_text(face='bold',size=input$box_userscores_cells.graphs.plot.axis_title,color=graph.colors()),
            plot.subtitle = element_text(face='italic',size=input$box_userscores_cells.graphs.plot.subtitle_size,color=graph.colors()),
            axis.text = element_text(color=graph.colors(),size=input$box_userscores_cells.graphs.plot.axis_label),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_),
            panel.background = element_rect(fill = "transparent",
                                            colour=NA_character_),
            #panel.grid.major.y = element_line(color='grey35'),
            axis.line = element_line(color='grey35'),
            legend.background = element_rect(fill='transparent',
                                             colour=NA_character_),
            legend.position='none') +
      xlab(input$box_userscores_cells.graphs.plot.x_title) +
      ylab(input$box_userscores_cells.graphs.plot.y_title) +
      labs(title=str_wrap(input$box_userscores_cells.graphs.plot.title,50),
           subtitle=str_wrap(input$box_userscores_cells.graphs.plot.subtitle,50)) +
      scale_fill_manual(values=color_mapping) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
      scale_x_continuous(labels=scales::percent)
    
    
    
    return(p)
    
  })
  
  
  
  output$box_userscores_cells.graphs.plot <- renderPlot({
    
    bg=ifelse(input$graphMode=='light','white','#1b2634')
    graph.userscores_cells() + theme(
      plot.background = element_rect(fill=bg),
      panel.background = element_rect(fill=bg)
    )
    
  })
  
  # Need this line to instantiate the plot on first run or plot won't be drawn in response to the button
  # being clicked
  outputOptions(output,'box_userscores_cells.graphs.plot',suspendWhenHidden=F)
  
  
  output$downloaduserscores_cells <- downloadHandler(
    filename = function(file) {
      "userscores_cells_graph.png"
    },
    content = function(file) {
      ggsave(file,graph.userscores_cells(),bg='transparent',width=input$box_userscores_cells.graphs.plot.plot_width,
             height=input$box_userscores_cells.graphs.plot.plot_height,dpi=600,device='png')
    }
  )
  # Need this line to instantiate the plot on first run or plot won't be drawn in response to the button
  # being clicked
  outputOptions(output,'downloaduserscores_cells',suspendWhenHidden=F)
  
  
  
  
  # User Scores Question Table Output
  output$userscores_questions.sum <- DT::renderDataTable({
    df <- user_scores.sum.tbl()[[2]]
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    df
    
  })
  
  # User Scores Questions Dotplot Outpuut
  
  dotplot.userscores_questions <- eventReactive(input$box_userscores_questions.dotplot.generate,{
    req(!is.null(input$box_userscores_questions.dotplot.question_select))
    
    reordered_levels <- user_scores.tbl() %>% 
      group_by(Cell.name) %>% summarise(avg=mean(mean,na.rm=T)) %>% 
      arrange(avg) %>% 
      mutate(Cell.name=as_factor(Cell.name)) %>% 
      pull(Cell.name) %>% 
      levels()
    
    # Creating a palette of colors from inputs and mapping to factor levels in order of
    # mean score
    graph.palette <- c(input$box_userscores_questions.dotplot.plot.color1, input$box_userscores_questions.dotplot.plot.color2,
                       input$box_userscores_questions.dotplot.plot.color3, input$box_userscores_questions.dotplot.plot.color4,
                       input$box_userscores_questions.dotplot.plot.color5, input$box_userscores_questions.dotplot.plot.color6,
                       input$box_userscores_questions.dotplot.plot.color7, input$box_userscores_questions.dotplot.plot.color8,
                       input$box_userscores_questions.dotplot.plot.color9, input$box_userscores_questions.dotplot.plot.color10,
                       input$box_userscores_questions.dotplot.plot.color11, input$box_userscores_questions.dotplot.plot.color12)
    color_mapping <- setNames(graph.palette[seq_along(rev(reordered_levels))], rev(reordered_levels))
    
    
    users <- user_scores.tbl() %>% filter(Question %in% input$box_userscores_questions.dotplot.question_select) %>% 
      mutate(Question = str_replace_all(Question,"\\([^()]*\\)",""),
             mean=mean/10) %>% 
      rename(avg=mean) %>%  
      group_by(Question,Cell.name) %>% 
      get_summary_stats(avg,show=c('mean','se')) %>% 
      arrange(-mean)
    
    graph_df <- users %>% 
      ggdotchart(x='Question',y='mean',dot.size=4,add='segments',
                 color='Cell.name',
                 add.params=list(color='grey35')) +
      geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0,position=position_dodge(.05),
                    color=input$box_userscores_questions.dotplot.plot.cicolor) +
      geom_text(aes(label=paste0(round(mean*100),'%')),size=input$box_userscores_questions.dotplot.plot.data_label,
                family='Work Sans',nudge_x=0.25,color=graph.colors()) +
      geom_text(aes(y=0,label=str_wrap(Cell.name,30)),family='Work Sans',fontface='italic',
                size=input$box_userscores_questions.dotplot.plot.data_auxlabel,hjust=0, nudge_x=.25,color=graph.colors()) +
      theme_pubr() +
      theme(text=element_text(family='Work Sans',color=graph.colors()),
            title = element_text(face='bold',size=input$box_userscores_questions.dotplot.plot.title_size,color=graph.colors()),
            axis.title = element_text(face='bold',size=input$box_userscores_questions.dotplot.plot.axis_title,color=graph.colors()),
            plot.subtitle = element_text(face='italic',size=input$box_userscores_questions.dotplot.plot.subtitle_size,color=graph.colors()),
            axis.text = element_text(color=graph.colors(),size=input$box_userscores_questions.dotplot.plot.axis_label),
            axis.text.y = element_text(hjust=0),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_),
            panel.background = element_rect(fill = "transparent",
                                            colour=NA_character_),
            panel.grid.major.y = element_line(color='grey35'),
            axis.line = element_line(color='grey35'),
            legend.background = element_rect(fill='transparent',
                                             colour=NA_character_),
            legend.position = 'none') +
      xlab(input$box_userscores_questions.dotplot.plot.y_title) +
      ylab(input$box_userscores_questions.dotplot.plot.x_title) +
      labs(title=input$box_userscores_questions.dotplot.plot.title,
           subtitle=input$box_userscores_questions.dotplot.plot.subtitle,
           color='Cell') +
      scale_color_manual(values=color_mapping) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
      scale_y_continuous(labels=scales::percent,limits=c(0,1.1)) + coord_flip()
    graph_df
    
    
    
    
  })
  
  output$box_userscores_questions.dotplot.plot <- renderPlot({
    
    bg=ifelse(input$graphMode=='light','white','#1b2634')
    dotplot.userscores_questions() + theme(
      plot.background = element_rect(fill=bg),
      panel.background = element_rect(fill=bg),
      legend.position = 'right',
      legend.direction = 'vertical'
    )
    
  })
  
  # Need this line to instantiate the plot on first run or plot won't be drawn in response to the button
  # being clicked
  outputOptions(output,'box_userscores_questions.dotplot.plot',suspendWhenHidden=F)
  
  
  output$downloaduserscores_dotplot <- downloadHandler(
    filename = function(file) {
      "userscores_questions_dotplot.png"
    },
    content = function(file) {
      ggsave(file,dotplot.userscores_questions(),bg='transparent',width=input$box_userscores_questions.dotplot.plot.plot_width,
             height=input$box_userscores_questions.dotplot.plot.plot_height,dpi=600,device='png')
    }
  )
  
  # Need this line to instantiate the plot on first run or plot won't be drawn in response to the button
  # being clicked
  outputOptions(output,'downloaduserscores_dotplot',suspendWhenHidden=F)
  
  # User Scores Question Graph Output
  
  graph.userscores_questions <- eventReactive(input$box_userscores_questions.graphs.generate, {
    
    
    req(!is.null(input$box_userscores_questions.graphs.cell_select))
    
    # Need a new reference dataframe because we need distribution data not summary data from the 
    # datatable output
    users <- user_scores.tbl() %>% filter(Cell.name %in% input$box_userscores_questions.graphs.cell_select,
                                          !Question %in% input$box_userscores_questions.graphs.question_exclude) %>% 
      mutate(Question = str_replace_all(Question,"\\([^()]*\\)","")) %>% 
      rename(avg=mean)
    
    graph_df <- users %>% mutate(Question=fct_reorder(as_factor(Question),avg,.fun=mean))
    reordered_levels <- users %>% 
      group_by(Question) %>% summarise(avg=mean(avg,na.rm=T)) %>% 
      arrange(avg) %>% 
      mutate(Question=as_factor(Question)) %>% 
      pull(Question) %>% 
      levels()
    
    
    # Warning message to inform that cell has no reviews
    if (nrow(filter(graph_df,is.na(avg)))!=0) {
      empty_cells <- graph_df %>% filter(is.na(avg)) %>% 
        mutate(message=paste0(Question,' has no reviews'))
      
      for (x in empty_cells$message) {
        showNotification(paste(x), type = "warning", duration = 5)
      }
      
      graph_df <- graph_df %>% filter(!is.na(avg))
    }
    
    
    
    graph.palette <- c(input$box_userscores_questions.graphs.plot.color1,input$box_userscores_questions.graphs.plot.color2,
                       input$box_userscores_questions.graphs.plot.color3,input$box_userscores_questions.graphs.plot.color4)
    color_mapping <- setNames(graph.palette[seq_along(rev(reordered_levels))], rev(reordered_levels))
    
    graph_df %>% ggplot(aes(x=avg/10,y=reorder(Question,avg),fill=Question)) + geom_boxplot(outliers=F,
                                                                                            staplewidth = 0.2,color='grey35') + 
      geom_point(data=summarise(group_by(users, Question), 
                                avg=mean(avg,na.rm=T)),
                 aes(x=avg/10),size=4) +
      geom_text(data=summarise(group_by(users, Question), 
                               avg=mean(avg,na.rm=T)),
                aes(x=avg/10,label=paste0(round(avg*10),'%')),family='Work Sans',
                size=input$box_userscores_questions.graphs.plot.data_label,
                color=input$box_userscores_questions.graphs.plot.data_label_color,
                vjust=-1.8) +
      theme_pubr() +
      theme(text=element_text(family='Work Sans',color=graph.colors()),
            title = element_text(face='bold',size=input$box_userscores_questions.graphs.plot.title_size,color=graph.colors()),
            axis.title = element_text(face='bold',size=input$box_userscores_questions.graphs.plot.axis_title,color=graph.colors()),
            plot.subtitle = element_text(face='italic',size=input$box_userscores_questions.graphs.plot.subtitle_size,color=graph.colors()),
            axis.text = element_text(color=graph.colors(),size=input$box_userscores_questions.graphs.plot.axis_label),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_),
            panel.background = element_rect(fill = "transparent",
                                            colour=NA_character_),
            #panel.grid.major.y = element_line(color='grey35'),
            axis.line = element_line(color='grey35'),
            legend.background = element_rect(fill='transparent',
                                             colour=NA_character_),
            legend.position='none') +
      xlab(input$box_userscores_questions.graphs.plot.x_title) +
      ylab(input$box_userscores_questions.graphs.plot.y_title) +
      labs(title=str_wrap(input$box_userscores_questions.graphs.plot.title,50),
           subtitle=str_wrap(input$box_userscores_questions.graphs.plot.subtitle,50),
           fill='Relationship') +
      scale_fill_manual(values=color_mapping) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
      scale_x_continuous(labels=scales::percent)
    
    
  })
  
  
  
  output$box_userscores_questions.graphs.plot <- renderPlot({
    
    bg=ifelse(input$graphMode=='light','white','#1b2634')
    graph.userscores_questions() + theme(
      plot.background = element_rect(fill=bg),
      panel.background = element_rect(fill=bg)
    )
    
  })
  
  # Need this line to instantiate the plot on first run or plot won't be drawn in response to the button
  # being clicked
  outputOptions(output,'box_userscores_questions.graphs.plot',suspendWhenHidden=F)
  
  
  output$downloaduserscores_questions <- downloadHandler(
    filename = function(file) {
      "userscores_questions_graph.png"
    },
    content = function(file) {
      ggsave(file,graph.userscores_questions(),bg='transparent',width=input$box_userscores_questions.graphs.plot.plot_width,
             height=input$box_userscores_questions.graphs.plot.plot_height,dpi=600,device='png')
    }
  )
  
  # Need this line to instantiate the plot on first run or plot won't be downloaded in response to the button
  # being clicked
  outputOptions(output,'downloaduserscores_questions',suspendWhenHidden=F)
  
  
  
  
  
  
  # Cells only - measures of dispersion
  cells.dispersion.tbl <- eventReactive(input$createReport, {
    honeyc.df <- cells.sqlOutput()
    if(nrow(honeyc.df) == 0) {
      
      # enable the Download All Reports button
      shinyjs::enable(id = 'downloadRaw')
      
      return(NULL)  
    }
    
    honeyc.df.clean <- apply_filters_and_clean(honeyc.df, input$surveys, input$user_ids, input$date)
    
    # Creating the dataset of measures of dispersion
    honeyc.df.clean.dispersion <- honeyc.df.clean %>% 
      group_by(Cell.name, Relationshiptouser) %>%
      summarise(mean=round(mean(original_value,na.rm=TRUE),2),
                min=min(original_value,na.rm=TRUE),
                q1=quantile(original_value,0.25,na.rm=TRUE,names=FALSE),
                median=quantile(original_value,0.5,na.rm=TRUE,names=FALSE),
                q3=quantile(original_value,0.75,na.rm=TRUE,names=FALSE),
                max=max(original_value,na.rm=TRUE),
                sd=round(sd(original_value,na.rm=TRUE),3),
                variance=round(sd^2,3), .groups="drop")
    
    
    # Covnerting to wide format
    honeyc.df.clean.dispersion.wide <- honeyc.df.clean.dispersion %>% 
      pivot_wider(names_from='Relationshiptouser',values_from=c('mean','min','q1','median','q3','max','sd','variance'))
    
    return(honeyc.df.clean.dispersion.wide)
    
  })
  
  
  # Cells only
  cells.sqlOutput.tbl <- eventReactive(input$createReport, {
    
    honeyc.df <- cells.sqlOutput()  
    
    if (nrow(honeyc.df) == 0) {
      
      # enable the Download All Reports button
      shinyjs::enable(id = 'downloadRaw')
      
      return(NULL)  
    }
    
    honeyc.df.clean <- apply_filters_and_clean(honeyc.df, input$surveys, input$user_ids, input$date)
    
    # Average feedback scores by cell and relationship type
    honeyc.df.clean.avg <- honeyc.df.clean %>% 
      group_by(Cell.name, Relationshiptouser) %>%
      summarise(original_valueMN = mean(original_value, na.rm = T), .groups = "drop")
    
    # Average feedback scores by cell for manager and other combined
    honeyc.df.clean.all.avg <- honeyc.df.clean %>% 
      filter(Relationshiptouser %in% c("manager", "other")) %>%
      group_by(Cell.name) %>%
      summarise(original_valueMN = mean(original_value, na.rm = T), .groups = "drop") %>%
      mutate(Relationshiptouser = "manager_other")
    
    # Combined data.frame so that it contains  manager, other, self, manager_other. 
    honeyc.df.clean.avg.combined <- bind_rows(honeyc.df.clean.avg, honeyc.df.clean.all.avg)
    
    # Restructure average dataset into wide format, compute difference scores and then restructure back into long format.
    honeyc.df.clean.avg.combined.wide <- honeyc.df.clean.avg.combined %>% 
      spread(Relationshiptouser, original_valueMN) 
    
    
    
    if("self" %in% names(honeyc.df.clean.avg.combined.wide)) {
      
    } else {
      
      honeyc.df.clean.avg.combined.wide$self <- NA 
      honeyc.df.clean.avg.combined.wide$manager <- NA  
      honeyc.df.clean.avg.combined.wide$other <- NA  
      honeyc.df.clean.avg.combined.wide$manager_other <- NA  
      
      shinyjs::hide(id = "box_cells_other_manager")
      shinyjs::hide(id = "box_cells_manager")
      shinyjs::hide(id = "box_cells_other_manager")
    }
    
    if("manager" %in% names(honeyc.df.clean.avg.combined.wide)) {
      
      shinyjs::show(id = "box_cells_other_manager")
      shinyjs::show(id = "box_cells_manager")
      
    } else { 
      
      honeyc.df.clean.avg.combined.wide$manager <- NA  
      
      shinyjs::hide(id = "box_cells_manager")
      shinyjs::hide(id = "box_cells_other_manager")
      
    }
    
    
    # if no other person has done a review create an empty variable
    if("other" %in% names(honeyc.df.clean.avg.combined.wide)) {
      
    } else { 
      
      honeyc.df.clean.avg.combined.wide$other <- NA 
    }
    
    # if no other person has done a review create an empty variable
    if("manager_other" %in% names(honeyc.df.clean.avg.combined.wide)) {
      
    } else { 
      
      honeyc.df.clean.avg.combined.wide$manager_other <- NA  
      shinyjs::hide(id = "box_cells_other_manager")
      
    }
    
    
    honeyc.df.clean.avg.combined.final <- honeyc.df.clean.avg.combined.wide %>%
      mutate(diff_self_manager = manager - self,
             diff_self_other = other - self,
             diff_self_other.manager = manager_other - self) %>%
      gather(key = Relationshiptouser, value = original_valueMN, manager:diff_self_other.manager)
    
    
    
    # Count number of reviewers by cell and relationship categories
    honeyc.df.clean.n.reviewers <- honeyc.df.clean %>% 
      group_by(Cell.name, Relationshiptouser) %>% 
      summarise(n.reviewers = length(unique(reviewer_id)), .groups = "drop") 
    
    # Count number of reviewers by user and cell across manager and other
    honeyc.df.clean.all.n.reviewers <- honeyc.df.clean %>% 
      filter(Relationshiptouser %in% c("manager", "other")) %>%
      group_by(Cell.name) %>% 
      summarise(n.reviewers = length(unique(reviewer_id)), .groups = "drop") %>%
      mutate(Relationshiptouser = "manager_other")
    
    # Combined data.frame so that it contains  manager, other, self, manager_other.
    honeyc.df.clean.n.reviewers.combined <- bind_rows(honeyc.df.clean.n.reviewers, honeyc.df.clean.all.n.reviewers) 
    
    
    
    
    ## Count the total number of reviews for each cell by relationship type
    cell.org.total.reviews <- honeyc.df.clean %>% 
      filter(Relationshiptouser != "self") %>%
      group_by(Cell.name, Relationshiptouser) %>% 
      summarise(n = n(), .groups = "drop")
    
    ## Count the total number of reviews for each cell for manager and other combined
    cell.org.all.total.reviews <- honeyc.df.clean %>% 
      filter(Relationshiptouser != "self") %>%
      group_by(Cell.name) %>% 
      summarise(n = n(), .groups = "drop") %>%
      mutate(Relationshiptouser = "manager_other")
    
    cell.org.total.reviews.combined <- bind_rows(cell.org.total.reviews, cell.org.all.total.reviews) 
    
    
    # Create wide format of the behaviour means dataset
    cell.behaviour.means <- honeyc.df.clean.avg.combined.final %>%
      spread(Relationshiptouser, value = original_valueMN)
    
    # Add a suffix to the column names so that the variables have unique names
    colnames(cell.behaviour.means)[-1] <- paste(colnames(cell.behaviour.means)[-1], "_MN", sep = "")
    
    # Create wide format of the reviewers dataset
    cell.reviewers <- honeyc.df.clean.n.reviewers.combined %>% 
      spread(Relationshiptouser, value = n.reviewers)
    
    # Add a suffix to the column names so that the variables have unique names
    colnames(cell.reviewers)[-1] <- paste(colnames(cell.reviewers)[-1], "_reviewers", sep = "")
    
    # Create wide format of the reviews dataset
    cell.reviews <- cell.org.total.reviews.combined %>% 
      spread(Relationshiptouser, value = n)
    
    # Add a suffix to the column names so that the variables have unique names
    colnames(cell.reviews)[-1] <- paste(colnames(cell.reviews)[-1], "_reviews", sep = "")
    
    
    # Compute total self reviews
    cell.total.self.reviews <- honeyc.df.clean %>% 
      filter(Relationshiptouser == "self") %>%
      group_by(Cell.name,  created_at, user_id) %>% 
      dplyr::summarise(n.reviewers = length(unique(user_id)), .groups = "drop") %>%
      group_by(Cell.name, user_id) %>%
      dplyr::summarise(reviewers.sum = sum(n.reviewers), .groups = "drop") %>% 
      group_by(Cell.name) %>%
      dplyr::summarise(total_self_reviews = sum(reviewers.sum), .groups = "drop")
    
    
    # Combine the three datasets into one
    cells.final.combined <- cell.behaviour.means %>%
      left_join(cell.reviewers) %>%
      left_join(cell.reviews) %>%
      left_join(cell.total.self.reviews)
    
    
    # Divide all behaviour means by 10 so that they could be translated into percentages later.
    cells.final.combined[, grep("_MN", colnames(cells.final.combined), value = T)] <- 
      cells.final.combined[, grep("_MN", colnames(cells.final.combined), value = T)] / 10
    
    # Remove trailing spaces from cell.name categories.
    cells.final.combined$Cell.name <- trimws(cells.final.combined$Cell.name, which = "both")
    
    # Identify duplicated rows
    cells.duplicated <- duplicated(cells.final.combined)
    
    # Remove duplicate rows
    cells.final.combined <- cells.final.combined[!cells.duplicated,]
    
    
    # Manager and other reviews - this code checks to see if these variables exist in the dataset and if not 
    # an empty version is created so that it doesn't cause a problem further down the line.
    if(any(c("manager_other_reviewers", "manager_other_reviews") %in% names(cells.final.combined))){ 
      # Do nothing      
    } else {
      # create new variables
      cells.final.combined$manager_other_reviewers <- NA
      cells.final.combined$manager_other_reviews <- NA
      
    }
    
    # Other reviews
    if(any(c("other_reviewers", "other_reviews") %in% names(cells.final.combined))){ 
      # Do nothing      
    } else {
      # create new variables
      cells.final.combined$other_reviewers <- NA
      cells.final.combined$other_reviews <- NA
      
    }
    
    # Manager 
    if(any(c("manager_reviewers", "manager_reviews") %in% names(cells.final.combined))){ 
      # Do nothing    
    } else {
      # create new variables
      cells.final.combined$manager_reviewers <- NA
      cells.final.combined$manager_reviews <- NA
      
    }
    
    # Self 
    if("self_reviewers" %in% names(cells.final.combined)){ 
      # Do nothing    
    } else {
      # create new variables
      cells.final.combined$self_reviewers <- NA
      cells.final.combined$total_self_reviews <- NA
      
    }
    
    if(nrow(cells.final.combined) == 0) {
      cells.final.combined <- NULL
    }
    
    # enable the Download All Reports button
    shinyjs::enable(id = 'downloadRaw')
    
    # Output the dataset
    return(cells.final.combined)
    
  })    
  
  
  # Cells & Questions - measures of dispersion
  cells.questions_dispersion.tbl <- eventReactive(input$createReport, {
    honeyc.df <- cells.sqlOutput()
    if(nrow(honeyc.df) == 0) {
      
      # enable the Download All Reports button
      shinyjs::enable(id = 'downloadRaw')
      
      return(NULL)  
    }
    
    honeyc.df.clean <- apply_filters_and_clean(honeyc.df, input$surveys, input$user_ids, input$date)
    
    # Creating the dataset of measures of dispersion
    honeyc.df.clean.dispersion <- honeyc.df.clean %>% 
      group_by(Cell.name, Question, Relationshiptouser) %>%
      summarise(mean=round(mean(original_value,na.rm=TRUE),2),
                min=min(original_value,na.rm=TRUE),
                q1=quantile(original_value,0.25,na.rm=TRUE,names=FALSE),
                median=quantile(original_value,0.5,na.rm=TRUE,names=FALSE),
                q3=quantile(original_value,0.75,na.rm=TRUE,names=FALSE),
                max=max(original_value,na.rm=TRUE),
                sd=round(sd(original_value,na.rm=TRUE),3),
                variance=round(sd^2,3), .groups="drop")
    
    
    # Covnerting to wide format
    honeyc.df.clean.dispersion.wide <- honeyc.df.clean.dispersion %>% 
      pivot_wider(names_from='Relationshiptouser',values_from=c('mean','min','q1','median','q3','max','sd','variance'))
    
    return(honeyc.df.clean.dispersion.wide)
    
  })
  
  
  
  
  # Cells and Questions
  cells.questions <- eventReactive(input$createReport, {
    
    honeyc.df <- cells.sqlOutput()  
    
    if (nrow(honeyc.df) == 0) {
      
      # enable the Download All Reports button
      shinyjs::enable(id = 'downloadRaw')
      
      return(NULL)
    }
    
    honeyc.df.clean <- apply_filters_and_clean(honeyc.df, input$surveys, input$user_ids, input$date)
    
    # Average feedback scores by cell and relationship type
    honeyc.df.clean.avg <- honeyc.df.clean %>% 
      group_by(Cell.name, Question, Relationshiptouser) %>%
      summarise(original_valueMN = mean(original_value, na.rm = T), .groups = "drop")
    
    # Average feedback scores by cell for manager and other combined
    honeyc.df.clean.all.avg <- honeyc.df.clean %>% 
      filter(Relationshiptouser %in% c("manager", "other")) %>%
      group_by(Cell.name, Question) %>%
      summarise(original_valueMN = mean(original_value, na.rm = T), .groups = "drop") %>%
      mutate(Relationshiptouser = "manager_other")
    
    # Combined data.frame so that it contains  manager, other, self, manager_other. 
    honeyc.df.clean.avg.combined <- bind_rows(honeyc.df.clean.avg, honeyc.df.clean.all.avg)
    
    # Restructure dataset into wide format, compute difference scores and then restructure back into long format.
    honeyc.df.clean.avg.combined.wide <- honeyc.df.clean.avg.combined %>% 
      spread(Relationshiptouser, original_valueMN) 
    
    
    if("self" %in% names(honeyc.df.clean.avg.combined.wide)) {
      
    } else {
      
      honeyc.df.clean.avg.combined.wide$self <- NA 
      honeyc.df.clean.avg.combined.wide$manager <- NA  
      honeyc.df.clean.avg.combined.wide$other <- NA  
      honeyc.df.clean.avg.combined.wide$manager_other <- NA  
      
      shinyjs::hide(id = "box_questions_cells_other_manager")
      shinyjs::hide(id = "box_questions_cells_manager")
      shinyjs::hide(id = "box_questions_cells_other_manager")
    }
    
    if("manager" %in% names(honeyc.df.clean.avg.combined.wide)) {
      
      shinyjs::show(id = "box_questions_cells_other_manager")
      shinyjs::show(id = "box_questions_cells_manager")
      
    } else { 
      
      honeyc.df.clean.avg.combined.wide$manager <- NA  
      
      shinyjs::hide(id = "box_questions_cells_manager")
      shinyjs::hide(id = "box_questions_cells_other_manager")
      
    }
    
    
    # if no other person has done a review create an empty variable
    if("other" %in% names(honeyc.df.clean.avg.combined.wide)) {
      
    } else { 
      
      honeyc.df.clean.avg.combined.wide$other <- NA 
    }
    
    # if no other person has done a review create an empty variable
    if("manager_other" %in% names(honeyc.df.clean.avg.combined.wide)) {
      
    } else { 
      
      honeyc.df.clean.avg.combined.wide$manager_other <- NA  
      shinyjs::hide(id = "box_cells_other_manager")
      
    }
    
    
    
    # Restructure dataset into wide format, compute difference scores and then restructure back into long format.
    honeyc.df.clean.avg.combined.final <- honeyc.df.clean.avg.combined.wide %>%
      mutate(diff_self_manager = manager - self,
             diff_self_other = other - self,
             diff_self_other.manager = manager_other - self) %>%
      gather(key = Relationshiptouser, value = original_valueMN, manager:diff_self_other.manager)
    
    
    # Count number of reviewers by cell and relationship categories
    honeyc.df.clean.n.reviewers <- honeyc.df.clean %>% 
      group_by(Cell.name, Question, Relationshiptouser) %>% 
      summarise(n.reviewers = length(unique(reviewer_id)), .groups = "drop") 
    
    # Count number of reviewers by user and cell across manager and other
    honeyc.df.clean.all.n.reviewers <- honeyc.df.clean %>% 
      filter(Relationshiptouser %in% c("manager", "other")) %>%
      group_by(Cell.name, Question) %>% 
      summarise(n.reviewers = length(unique(reviewer_id)), .groups = "drop") %>%
      mutate(Relationshiptouser = "manager_other")
    
    # Combined data.frame so that it contains  manager, other, self, manager_other.
    honeyc.df.clean.n.reviewers.combined <- bind_rows(honeyc.df.clean.n.reviewers, honeyc.df.clean.all.n.reviewers) 
    
    
    ## Count the total number of reviews for each cell by relationship type
    cell.org.total.reviews <- honeyc.df.clean %>% 
      filter(Relationshiptouser != "self") %>%
      group_by(Cell.name, Question, Relationshiptouser) %>% 
      summarise(n = n(), .groups = "drop")
    
    ## Count the total number of reviews for each cell for manager and other combined
    cell.org.all.total.reviews <- honeyc.df.clean %>% 
      filter(Relationshiptouser != "self") %>%
      group_by(Cell.name, Question) %>% 
      summarise(n = n(), .groups = "drop") %>%
      mutate(Relationshiptouser = "manager_other")
    
    cell.org.total.reviews.combined <- bind_rows(cell.org.total.reviews, cell.org.all.total.reviews) 
    
    
    # Create wide format of the behaviour means dataset
    cell.behaviour.means <- honeyc.df.clean.avg.combined.final %>%
      spread(Relationshiptouser, value = original_valueMN)
    
    # Add a suffix to the column names so that the variables have unique names
    colnames(cell.behaviour.means)[-c(1,2)] <- paste(colnames(cell.behaviour.means)[-c(1,2)], "_MN", sep = "")
    
    # Create wide format of the reviewers dataset
    cell.reviewers <- honeyc.df.clean.n.reviewers.combined %>% 
      spread(Relationshiptouser, value = n.reviewers)
    
    # Add a suffix to the column names so that the variables have unique names
    colnames(cell.reviewers)[-c(1,2)] <- paste(colnames(cell.reviewers)[-c(1,2)], "_reviewers", sep = "")
    
    # Create wide format of the reviews dataset
    cell.reviews <- cell.org.total.reviews.combined %>% 
      spread(Relationshiptouser, value = n)
    
    # Add a suffix to the column names so that the variables have unique names
    colnames(cell.reviews)[-c(1,2)] <- paste(colnames(cell.reviews)[-c(1,2)], "_reviews", sep = "")
    
    
    # Count number of total self reviews by cell and question - some self-reviews were done more than once across time.
    cell.total.self.reviews <- honeyc.df.clean %>% 
      filter(Relationshiptouser == "self") %>%
      group_by(Cell.name, Question, created_at, user_id) %>% 
      dplyr::summarise(n.reviewers = length(unique(user_id)), .groups = "drop") %>%
      group_by(Cell.name, Question, user_id) %>%
      dplyr::summarise(reviewers.sum = sum(n.reviewers), .groups = "drop") %>% 
      group_by(Cell.name, Question) %>%
      dplyr::summarise(total_self_reviews = sum(reviewers.sum), .groups = "drop")
    
    
    # Combine the three datasets into one
    cells.final.combined <- cell.behaviour.means %>%
      left_join(cell.reviewers) %>%
      left_join(cell.reviews) %>%
      left_join(cell.total.self.reviews)
    
    
    # Manager and other reviews - this code checks to see if these variables exist in the dataset and if not 
    # an empty version is created so that it doesn't cause a problem further down the line.
    if(any(c("manager_other_reviewers", "manager_other_reviews") %in% names(cells.final.combined))){ 
      # Do nothing      
    } else {
      # create new variables
      cells.final.combined$manager_other_reviewers <- NA
      cells.final.combined$manager_other_reviews <- NA
      
    }
    
    # Other reviews
    if(any(c("other_reviewers", "other_reviews") %in% names(cells.final.combined))){ 
      # Do nothing      
    } else {
      # create new variables
      cells.final.combined$other_reviewers <- NA
      cells.final.combined$other_reviews <- NA
      
    }
    
    # Manager 
    if(any(c("manager_reviewers", "manager_reviews") %in% names(cells.final.combined))){ 
      # Do nothing    
    } else {
      # create new variables
      cells.final.combined$manager_reviewers <- NA
      cells.final.combined$manager_reviews <- NA
      
    }
    
    # Self 
    if("self_reviewers" %in% names(cells.final.combined)){ 
      # Do nothing    
    } else {
      # create new variables
      cells.final.combined$self_reviewers <- NA
      cells.final.combined$total_self_reviews <- NA
      
    }
    
    # Divide all behaviour means by 10 so that they could be translated into percentages later.
    cells.final.combined[, grep("_MN", colnames(cells.final.combined), value = T)] <- 
      cells.final.combined[, grep("_MN", colnames(cells.final.combined), value = T)] / 10
    
    
    # Remove trailing spaces from cell.name categories.
    cells.final.combined$Cell.name <- trimws(cells.final.combined$Cell.name, which = "both")
    
    # Identify duplicated rows
    cells.duplicated <- duplicated(cells.final.combined)
    
    # Remove duplicate rows
    cells.final.combined <- cells.final.combined[!cells.duplicated,]
    
    if(nrow(cells.final.combined) == 0) {
      
      cells.final.combined <- NULL
      
    }
    
    
    # enable the Download All Reports button
    shinyjs::enable(id = 'downloadRaw')
    
    # Output the dataset
    return(cells.final.combined)
    
  })  
  
  # Variability Graph
  cells.graphs <- eventReactive(input$createReport, {
    
    honeyc.df <- cells.sqlOutput()
    
    if (nrow(honeyc.df) == 0) return(NULL)
    
    honeyc.df.clean <- apply_filters_and_clean(honeyc.df, input$surveys, input$user_ids, input$date)
    
    # enable the Download All Reports button
    shinyjs::enable(id = 'downloadRaw')
    
    return(honeyc.df.clean)
    
  })
  
  # Time Series Percentage change
  time.series.change <- eventReactive(input$createReport, {
    
    honeyc.df <- cells.sqlOutput()
    
    if (nrow(honeyc.df) == 0) {
      
      time.series.output <- NULL
      
    } else {
      
      honeyc.df.clean <- apply_filters_and_clean(honeyc.df, input$surveys, input$user_ids, input$date)
      
      # Average behaviour score by user, date created, relationship type and cell name
      tbl.mean.score <- honeyc.df.clean %>% 
        group_by(user_id, reviewer_id, created_at, Relationshiptouser, Cell.name) %>%
        dplyr::summarise(Mean_Score  = mean(original_value, na.rm = T), .groups = "drop") 
      
      # Number of reviews by user, date created, relationship type and cell name
      tbl.reviews <- honeyc.df.clean %>% 
        group_by(user_id, reviewer_id, created_at, Relationshiptouser, Cell.name) %>%
        dplyr::summarise(Reviews = n(), .groups = "drop") 
      
      combined.time.series.tbl <- left_join(tbl.mean.score, tbl.reviews)
      
      combined.time.series.tbl$created_at <- as.Date(combined.time.series.tbl$created_at)
      
      names(combined.time.series.tbl)[4] <- c("Relationship")
      
      if (nrow(combined.time.series.tbl) == 0) {
        return(NULL)
      }
      
      
      # Pull out unique cells from the dataset
      cells <- unique(combined.time.series.tbl$Cell.name)
      # Pull out relationships from the dataset
      relationships <- unique(combined.time.series.tbl$Relationship)
      
      
      # Create an empty list for storing the output from the regression model
      reg.model <- list()
      # Create empty date for storing starting date
      min.value <- Date()
      # Create empty date variable for ending date
      max.value <- Date()
      # Create empty vector for storing predicted values from a regression model
      predicted.vals <- numeric()
      # Create empty percentage change numeric vector
      perc.change <- numeric()
      # Create empty list for storing the data.frames for each relationship type
      time.series.list <- list()
      
      
      for (j in 1: length(relationships)) {
        
        dat = combined.time.series.tbl[combined.time.series.tbl$Relationship %in% relationships[j],]
        
        for (i in 1:length(cells)){
          
          dat.cells = dat[dat$Cell.name %in% cells[i],]
          
          if (nrow(dat.cells) > 0) {
            
            reg.model[[i]] <- lm(Mean_Score ~ created_at, data = dat.cells)
            
            min.value[i] <- min(dat.cells$created_at, na.rm = T)
            max.value[i] <- max(dat.cells$created_at, na.rm = T)
            
            predicted.vals <- predict(reg.model[[i]], data.frame(created_at = c(min.value[i], max.value[i])))
            perc.change[i] <- (predicted.vals[2] - predicted.vals[1]) / predicted.vals[1]
            
          } else {
            
            min.value[i] <- NA
            max.value[i] <- NA
            perc.change[i] <- NA
            
          }
          
        }
        
        time.series.list[[j]] <- data.frame(Cell_name = cells, Relationships = relationships[j], 
                                            Start = min.value, End = max.value, Percentage_change = perc.change)
      }
      
      # Create data.frame from the list
      time.series.df <- do.call(rbind, time.series.list )
      
      
      # Overall 
      # Create an empty list for storing the output from the regression model
      reg.model <- list()
      # Create empty date for storing starting date
      min.value <- Date()
      # Create empty date variable for ending date
      max.value <- Date()
      # Create empty vector for storing predicted values from a regression model
      predicted.vals <- numeric()
      # Create empty percentage change numeric vector
      perc.change <- numeric()
      # Create empty list for storing the data.frames for each relationship type
      time.series.list <- list()
      
      
      for (i in 1:length(relationships)){
        
        dat = combined.time.series.tbl[combined.time.series.tbl$Relationship == relationships[i],]
        
        reg.model[[i]] <- lm(Mean_Score ~ created_at, data = dat)
        
        min.value[i] <- min(dat$created_at, na.rm = T)
        max.value[i] <- max(dat$created_at, na.rm = T)
        
        predicted.vals <- predict(reg.model[[i]], data.frame(created_at = c(min.value[i], max.value[i])))
        perc.change[i] <- (predicted.vals[2] - predicted.vals[1]) / predicted.vals[1]
        
        time.series.list[[i]] <- data.frame(Relationship = relationships[i], 
                                            Start = min.value[i], End = max.value[i], Percentage_change = perc.change[i])
      }
      
      # Create data.frame from the list
      time.series.overall.df <- do.call(rbind, time.series.list)
      
      # Change column name
      colnames(time.series.overall.df)[4] <- "% Change"
      
      # Change column names
      colnames(time.series.df) <- c("Cell Name", "Relationship", "Start", "End", "% Change")
      
      time.series.output <- list(time.series.overall.df = time.series.overall.df,
                                 time.series.cell.df = time.series.df)
      
    }
    
    # enable the Download All Reports button
    shinyjs::enable(id = 'downloadRaw')
    
    return(time.series.output)
    
  })
  
  # Demographic 
  demographics <- eventReactive(input$createReport, {
    
    reviews.df <- cells.sqlOutput()
    
    if (nrow(reviews.df) == 0) {
      
      users.df <- NULL
      honeyc.df <- NULL
      
    } else {
      
      reviews.df <- apply_filters_and_clean(reviews.df, input$surveys, 
                                            input$user_ids, input$date, 
                                            na_action = "keep")  # keep NAs so we can count skipped behaviours
      id.list <- unique(c(reviews.df$user_id, reviews.df$reviewer_id)) 
      users.qry <- dbSendQuery(dbState$con, 
                               paste0("SELECT id, gender_identity, sexuality, ethnicity 
                                      FROM report_users WHERE id IN (",
                                      paste0(id.list, collapse=","), ")")
      )
      
      users.df <- dbFetch(users.qry)
      dbClearResult(users.qry)
      
      cols.to.change <- c("sexuality", "gender_identity", "ethnicity")
      users.df[, cols.to.change] <- lapply(users.df[, cols.to.change], function(x) {
        x <- gsub('[\\"\\}\\{]', "", x)
        x <- as.character(ifelse(is.na(x), "Missing", x))
        x <- as.character(ifelse(x == "", "No response", x))
      }
      )
      
      users.df <- recode_ethnicity(users.df, "ethnicity")
      
      # recode race/ethnicity as white/non-white [excludes prefer not to say, no answer]
      users.df$binary_white <- case_when(
        users.df$ethnicity_collapsed == "Prefer not to say" ~ NA_character_,
        users.df$ethnicity_collapsed == "No response" ~ NA_character_,
        users.df$ethnicity_collapsed == "Missing" ~ NA_character_,
        is.na(users.df$ethnicity_collapsed) ~ NA_character_,
        users.df$ethnicity_collapsed == "White" ~ "white",
        TRUE ~ "non_white"
      )
      
      # recode sexuality
      users.df$binary_sexuality <- case_when(
        grepl("heterosexual", users.df$sexuality, ignore.case = TRUE) ~ "heterosexual",
        grepl("bisexual|gay|pansexual|queer", users.df$sexuality, ignore.case = TRUE) ~ "LGBTQ",
        TRUE ~ NA_character_
      )  
      
      # recode gender
      users.df$binary_gender <- case_when(
        grepl("man", users.df$gender_identity, ignore.case = T) ~ "man",
        grepl("woman", users.df$gender_identity, ignore.case = T) ~ "woman",
        TRUE ~ NA_character_
      )
      
      # woman must come before man in these case_when statement
      users.df$binary_gender <- case_when(
        grepl("woman", users.df$gender_identity, ignore.case = T) ~ "woman",
        grepl("man", users.df$gender_identity, ignore.case = T) ~ "man",
        TRUE ~ NA_character_
      )
      
      rename_list <- function(x, prefix) {
        names(x) <- paste0(paste0(prefix, '.'), x)
        return(x)
      }
      
      demo.cols <- c("gender_identity", "sexuality", "ethnicity", "ethnicity_collapsed", 
                     "binary_gender", "binary_sexuality", "binary_white")
      
      demo_vars_df <- users.df %>%
        dplyr::select(all_of(c("id", demo.cols)))
      
      # merge user & reviewer demographics into the review data
      # for any users or reviewers not present in users.df, set the demo vars to "Missing"
      honeyc.df <- reviews.df %>%
        left_join(demo_vars_df, by = c("user_id" = "id")) %>%
        mutate(across(all_of(demo.cols), ~ replace_na(., "Missing"))) %>%
        rename(rename_list(demo.cols, "u")) %>%
        left_join(demo_vars_df, by = c("reviewer_id" = "id")) %>%
        mutate(across(all_of(demo.cols), ~ replace_na(., "Missing"))) %>%
        rename(rename_list(demo.cols, "r")) 
      
    } 
    
    df.list <- list(users = users.df,
                    reviews = honeyc.df)
    
    # enable the Download All Reports button
    shinyjs::enable(id = 'downloadRaw')
    
    return(df.list)
    
  })
  
  # Output 
  
  
  ## Engagement Statistics
  
  
  
  ## Cells only
  ### Measures of Dispersion by Self, Manager, and Other
  df.dispersion <- reactive({
    if (nrow(cells.dispersion.tbl())==0) {
      return(NULL)
    }
    df <- cells.dispersion.tbl() %>% select(c('Cell.name',ends_with(input$cells_dispersion.relation_select)))
    names(df) <- c('Cell Name','Mean','Min','Q1','Median','Q3','Max','Standard Deviation','Variance')
    return(df)
  })
  
  
  output$cells.dispersion <- DT::renderDataTable({
    if (is.null(df.dispersion())) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    datatable(df.dispersion(), rownames=FALSE, extensions = "Buttons",
              filter="none", options=list(order = list(1, "desc"), dom = 'Bfrtip',
                                          pageLength = min(MAX_PAGE_LEN, nrow(df.dispersion())),
                                          buttons = c('copy', 'csv', 'excel', 'pdf'),
                                          searching = T, scrollX = TRUE)) %>%
      formatRound(columns = c('Mean','Min','Q1','Median','Q3','Max','Standard Deviation','Variance'), digits = 3)
  })
  
  
  
  ### Manager & Other
  getTable_cells.manager_other <- function(df) {
    
    if (is.null(df)) return(NULL)
    
    manager_other_df <- df[,c("Cell.name", "self_MN", "manager_other_MN", 
                              "diff_self_other.manager_MN", 
                              "self_reviewers", "total_self_reviews",
                              "manager_other_reviewers", "manager_other_reviews")]
    
    colnames(manager_other_df) <- c("Cell", "Self", "Combined", "Difference", 
                                    "Unique Self Reviews", "Total Self Reviews",  
                                    "Reviewers", "Total Reviews")
    return(manager_other_df)
  }
  
  output$cells.manager_other <- DT::renderDataTable({
    
    manager_other_df <- getTable_cells.manager_other(cells.sqlOutput.tbl())
    
    if (is.null(manager_other_df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    datatable(manager_other_df, rownames=FALSE, extensions = "Buttons",
              filter="none", options=list(order = list(1, "desc"), dom = 'Bfrtip',
                                          pageLength = min(MAX_PAGE_LEN, nrow(manager_other_df)),
                                          buttons = c('copy', 'csv', 'excel', 'pdf'),
                                          searching = T, scrollX = TRUE)) %>%
      formatRound(columns = c("Self", "Combined", "Difference"), digits = 3)  %>%
      formatStyle('Difference',
                  backgroundColor = styleInterval(c(-1, 0), c('white', '#f5ae51', '#94ffa4'))) %>%
      formatStyle('Unique Self Reviews',
                  backgroundColor = styleInterval(c(3, 9), c('white', '#f5ae51', '#94ffa4'))) %>%
      formatPercentage(columns = c("Self", "Combined", "Difference"), 1) 
    
  })
  
  ### Other only
  getTable_cells.other <- function(df) {
    
    if (is.null(df)) return(NULL)
    
    other_df <- df[,c("Cell.name", "self_MN", 
                      "other_MN", "diff_self_other_MN", 
                      "self_reviewers", "total_self_reviews",
                      "other_reviewers", "other_reviews")]
    
    colnames(other_df) <- c("Cell", "Self", "Other", "Difference", 
                            "Unique Self Reviews", "Total Self Reviews", 
                            "Reviewers", "Total Reviews")
    return(other_df)
  }    
  
  output$cells.other_only <- DT::renderDataTable({
    other_df <- getTable_cells.other(cells.sqlOutput.tbl())
    
    if (is.null(other_df)) return(NULL)
    
    datatable(other_df, rownames=FALSE, extensions = "Buttons",
              filter="none", options=list(order = list(1, "desc"), dom = 'Bfrtip',
                                          buttons = c('copy', 'csv', 'excel', 'pdf'),
                                          pageLength = min(MAX_PAGE_LEN, nrow(other_df)),
                                          searching = T, scrollX = TRUE)) %>%
      formatRound(columns = c("Self", "Other", "Difference"), digits = 3)  %>%
      formatStyle('Difference',
                  backgroundColor = styleInterval(c(-1, 0), c('white', '#f5ae51', '#94ffa4'))) %>%
      formatStyle("Unique Self Reviews",
                  backgroundColor = styleInterval(c(3, 9), c('white', '#f5ae51', '#94ffa4'))) %>%
      formatPercentage(columns = c("Self", "Other", "Difference"), 1) 
  })
  
  ### Manager only
  getTable_cells.manager <- function(df) {
    
    if (is.null(df)) return(NULL)
    
    manager_df <- df[,c("Cell.name", "self_MN", 
                        "manager_MN", "diff_self_manager_MN", 
                        "self_reviewers", "total_self_reviews",
                        "manager_reviewers", "manager_reviews")]
    
    colnames(manager_df) <- c("Cell", "Self", "Manager", "Difference", "Unique Self Reviews",
                              "Total Self Reviews", "Reviewers", "Total Reviews")
    return(manager_df)
  }        
  
  output$cells.manager_only <- DT::renderDataTable({
    
    manager_df <- getTable_cells.manager(cells.sqlOutput.tbl())
    
    if (is.null(manager_df)) return(NULL)
    
    datatable(manager_df, rownames=FALSE, extensions = "Buttons", 
              filter="none", options=list(order = list(1, "desc"), dom = 'Bfrtip',
                                          pageLength = min(MAX_PAGE_LEN, nrow(manager_df)),
                                          buttons = c('copy', 'csv', 'excel', 'pdf'),
                                          searching = T, scrollX = TRUE)) %>%
      formatRound(columns = c("Self", "Manager", "Difference"), digits = 3)  %>%
      formatStyle('Difference',
                  backgroundColor = styleInterval(c(-1, 0), c('white', '#f5ae51', '#94ffa4'))) %>%
      formatStyle('Unique Self Reviews',
                  backgroundColor = styleInterval(c(3, 9), c('white', '#f5ae51', '#94ffa4'))) %>%
      formatPercentage(columns = c("Self", "Manager", "Difference"), 1) })
  
  ### Cell Report Graphs
  getTable_cells.cells_graphs <- function(df) {
    if (is.null(df)) return(NULL)
    
    cells_graphs_df <- df[,c("Cell.name", "self_MN", "other_MN",
                             "manager_MN","self_reviewers","other_reviewers",
                             "manager_reviewers")]
    cells_graphs_df <- cells_graphs_df %>% pivot_longer(cols=c(starts_with('self_'),starts_with('other_'),
                                                               starts_with('manager_')),
                                                        names_to=c('Relationship','statistic'),names_sep='_')
    cells_graphs_df <- cells_graphs_df %>% pivot_wider(id_cols=c('Cell.name','Relationship'),
                                                       names_from='statistic',values_from='value')
    
    colnames(cells_graphs_df) <- c("Cell", "Relationship", "Mean", "Reviewers")
    cells_graphs_df <- cells_graphs_df %>% mutate(pct=round(Mean*100,1))
    
    return(cells_graphs_df)
  }        
  
  cells_graphs_df <- reactive({
    df <- getTable_cells.cells_graphs(cells.sqlOutput.tbl()) %>% filter(Cell %in% input$cells_graphs.cell_select,
                                                                        Relationship %in% tolower(input$cells_graphs.relation_checkbox))
    skipped_cells <- df
    df <- df %>% filter(!(Reviewers < 4 & Relationship %in% tolower(input$cells_graphs.filter_checkbox)))
    skipped_cells <- skipped_cells %>% filter(Reviewers < 4 & !Relationship %in% tolower(input$cells_graphs.filter_checkbox))
    return(list(df,skipped_cells))
  })
  
  
  graph.colors <- reactive({
    ifelse(input$graphMode=='light','black','white')
  })
  
  
  
  graph.cells_graph <- eventReactive(input$cells_graphs.generate, {
    
    if (length(input$cells_graphs.cell_select)<=1) {
      showNotification("Please select 2 or more cells", type = "warning", duration = 3)}
    
    req(length(input$cells_graphs.cell_select)>1)
    req(!is.null(input$cells_graphs.relation_checkbox))
    
    graph_df <- cells_graphs_df()[[1]]
    skipped_cells <- cells_graphs_df()[[2]]
    
    # Warning message if relationship is not being filtered and has too few reviews
    if (nrow(skipped_cells)!=0) {
      skipped_cells <- skipped_cells %>% mutate(message=paste0(Cell,' does not have enough ',
                                                               Relationship,' reviewers to maintain anonymity'))
      for (x in skipped_cells$message) {
        showNotification(paste(x), type = "warning", duration = 5)
      }
    }
    
    # Warning message to inform that cell has no reviews
    if (nrow(filter(graph_df,is.na(Mean)))!=0) {
      empty_cells <- graph_df %>% filter(is.na(Mean)) %>% 
        mutate(message=paste0(Cell,' has no ',Relationship,' reviews'))
      
      for (x in empty_cells$message) {
        showNotification(paste(x), type = "warning", duration = 5)
      }
      
      graph_df <- graph_df %>% filter(!is.na(Mean))
    }
    
    
    
    cells_graph.colours <- c(input$cells_graphs.plot.color1,input$cells_graphs.plot.color2,
                             input$cells_graphs.plot.color3)
    
    p <- ggplot(graph_df,aes(x=Cell,y=Mean,fill=Relationship)) + 
      geom_col(aes(x=fct_reorder2(Cell,Relationship!='self',Mean)),position = 'dodge') + 
      geom_text(aes(label = paste0(pct,'% ')),hjust=1.2,position=position_dodge(.9),
                family='Work Sans', color=input$cells_graphs.plot.data_label_color,
                size=input$cells_graphs.plot.data_label) +
      theme_hc() +
      theme(text=element_text(family='Work Sans',color=graph.colors()),
            title = element_text(face='bold',size=input$cells_graphs.plot.title_size,color=graph.colors()),
            axis.title = element_text(face='bold',size=input$cells_graphs.plot.axis_title,
                                      color=graph.colors()),
            axis.text = element_text(color=graph.colors(),size=input$cells_graphs.plot.axis_label),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_),
            panel.grid.major.y = element_line(color='grey35'),
            legend.background = element_rect(fill='transparent',
                                             colour=NA_character_)) +
      xlab(input$cells_graphs.plot.y_title) +
      ylab(input$cells_graphs.plot.x_title) +
      labs(fill='Relationship to user') +
      ggtitle(str_wrap(input$cells_graphs.plot.title,50)) +
      scale_fill_manual(values=cells_graph.colours) +
      scale_x_discrete(limits=rev,labels = function(x) str_wrap(x, width = 25)) +
      scale_y_continuous(labels=scales::percent) + coord_flip()
    return(p)
    
  })
  
  
  
  output$cells_graphs.plot <- renderPlotly({
    
    p <- graph.cells_graph()
    ggplotly(
      p = p,
      tooltip = NULL
    ) %>% 
      layout(margin = list(b = 50),
             paper_bgcolor=ifelse(input$graphMode=='light','white','#1b2634'),
             plot_bgcolor=ifelse(input$graphMode=='light','white','#1b2634')) %>%
      style(textposition='left')
    
  })
  
  outputOptions(output,'cells_graphs.plot',suspendWhenHidden=F)
  
  #### Download Graph Button
  output$downloadCells <- downloadHandler(
    filename = function(file) {
      "cells_graph.png"
    },
    content = function(file) {
      ggsave(file,graph.cells_graph(),bg='transparent',width=input$cells_graphs.plot.plot_width,
             height=input$cells_graphs.plot.plot_height,dpi=600,device='png')
    }
  )
  
  outputOptions(output,'downloadCells',suspendWhenHidden=F)
  
  
  ## Cells & Questions
  
  ### Measures of Dispersion by Self, Manager, and Other
  df.questions_dispersion <- reactive({
    if (nrow(cells.questions_dispersion.tbl())==0) {
      return(NULL)
    }
    df <- cells.questions_dispersion.tbl() %>% select(c('Cell.name','Question',ends_with(input$cells.questions_dispersion.relation_select)))
    names(df) <- c('Cell Name','Question','Mean','Min','Q1','Median','Q3','Max','Standard Deviation','Variance')
    return(df)
  })
  
  
  output$cells.questions_dispersion <- DT::renderDataTable({
    if (is.null(df.questions_dispersion())) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    datatable(df.questions_dispersion(), rownames=FALSE, extensions = "Buttons",
              filter="none", options=list(order = list(1, "desc"), dom = 'Bfrtip',
                                          pageLength = min(MAX_PAGE_LEN, nrow(df.dispersion())),
                                          buttons = c('copy', 'csv', 'excel', 'pdf'),
                                          searching = T, scrollX = TRUE)) %>%
      formatRound(columns = c('Mean','Min','Q1','Median','Q3','Max','Standard Deviation','Variance'), digits = 3)
  })
  
  
  
  ### Manager & Other
  getTable_cells.questions_manager_other <- function(df) {
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    question.manager_other_df <- df[,c("Cell.name", "Question", 
                                       "self_MN", "manager_other_MN", 
                                       "diff_self_other.manager_MN", 
                                       "self_reviewers", "total_self_reviews",
                                       "manager_other_reviewers", "manager_other_reviews")]
    
    colnames(question.manager_other_df) <- c("Cell", "Question", "Self", 
                                             "Combined", "Difference", 
                                             "Unique Self Reviews", "Total Self Reviews", 
                                             "Reviewers", "Total Reviews")
    return(question.manager_other_df)
  }
  
  output$cells.questions_manager_other <- DT::renderDataTable({
    
    question.manager_other_df <- getTable_cells.questions_manager_other(cells.questions())
    
    if (is.null(question.manager_other_df)) {
      return(NULL)
    }
    
    datatable(question.manager_other_df, rownames=FALSE, extensions = "Buttons",
              filter="none", options=list(order = list(2, "desc"), dom = 'Bfrtip', 
                                          pageLength = min(MAX_PAGE_LEN, nrow(question.manager_other_df)),
                                          buttons = c('copy', 'csv', 'excel', 'pdf'),
                                          searching = T, scrollX = TRUE)) %>%
      formatRound(columns = c("Self", "Combined", "Difference"), digits = 3)  %>%
      formatStyle('Difference',
                  backgroundColor = styleInterval(c(-1, 0), c('white', '#f5ae51', '#94ffa4'))) %>%
      formatStyle('Unique Self Reviews',
                  backgroundColor = styleInterval(c(3, 9), c('white', '#f5ae51', '#94ffa4'))) %>%
      formatPercentage(columns = c("Self", "Combined", "Difference"), 1) })
  
  ### Other only
  getTable_cells.questions_other <- function(df) {
    
    if (is.null(df)) return(NULL)
    
    question.other_df <- df[,c("Cell.name", "Question","self_MN", 
                               "other_MN", "diff_self_other_MN", 
                               "self_reviewers", "total_self_reviews", 
                               "other_reviewers", "other_reviews")]
    
    colnames(question.other_df) <- c("Cell", "Question", "Self", "Other", "Difference", 
                                     "Unique Self Reviews", "Total Self Reviews",   
                                     "Reviewers", "Total Reviews")
    return(question.other_df)
  }
  
  output$cells.questions_other_only <- DT::renderDataTable({
    
    question.other_df <- getTable_cells.questions_other(cells.questions())
    
    if (is.null(question.other_df)) return(NULL)
    
    datatable(question.other_df, rownames=FALSE, extensions = "Buttons",
              filter="none", options=list(order = list(2, "desc"), dom = 'Bfrtip', 
                                          pageLength = min(MAX_PAGE_LEN, nrow(question.other_df)),
                                          buttons = c('copy', 'csv', 'excel', 'pdf'),
                                          searching = T, scrollX = TRUE)) %>%
      formatRound(columns = c("Self", "Other", "Difference"), digits = 3)  %>%
      formatStyle('Difference',
                  backgroundColor = styleInterval(c(-1, 0), c('white', '#f5ae51', '#94ffa4'))) %>%
      formatStyle('Unique Self Reviews',
                  backgroundColor = styleInterval(c(3, 9), c('white', '#f5ae51', '#94ffa4'))) %>%
      formatPercentage(columns = c("Self", "Other", "Difference"), 1) })
  
  ### Manager only 
  getTable_cells.questions_manager <- function(df) {
    
    if (is.null(df)) return(NULL)
    
    question.manager_df <- df[,c("Cell.name", "Question", "self_MN", 
                                 "manager_MN", "diff_self_manager_MN", 
                                 "self_reviewers", "total_self_reviews",
                                 "manager_reviewers", "manager_reviews")]
    
    colnames(question.manager_df) <- c("Cell", "Question", "Self", "Manager", 
                                       "Difference", "Unique Self Reviews", "Total Self Reviews",   
                                       "Reviewers", "Total Reviews")
    
    return(question.manager_df)
  }    
  
  output$cells.questions_manager_only <- DT::renderDataTable({
    
    question.manager_df <- getTable_cells.questions_manager(cells.questions())
    
    if (is.null(question.manager_df)) return(NULL)
    
    datatable(question.manager_df, rownames=FALSE, extensions = "Buttons",
              filter="none", options=list(order = list(2, "desc"), dom = 'Bfrtip', 
                                          pageLength = min(MAX_PAGE_LEN, nrow(question.manager_df)),
                                          buttons = c('copy', 'csv', 'excel', 'pdf'),
                                          searching = T, scrollX = TRUE)) %>%
      formatRound(columns = c("Self", "Manager", "Difference"), digits = 3)  %>%
      formatStyle('Difference',
                  backgroundColor = styleInterval(c(-1, 0), c('white', '#f5ae51', '#94ffa4'))) %>%
      formatStyle('Unique Self Reviews',
                  backgroundColor = styleInterval(c(3, 9), c('white', '#f5ae51', '#94ffa4'))) %>%
      formatPercentage(columns = c("Self", "Manager", "Difference"), 1) 
  })
  
  ### Question Report Graphs
  getTable_questions.questions_graphs <- function(df) {
    if (is.null(df)) return(NULL)
    
    graphs_df <- df[,c("Cell.name", "Question", "self_MN", "other_MN",
                       "manager_MN","self_reviewers","other_reviewers",
                       "manager_reviewers")]
    graphs_df <- graphs_df %>% pivot_longer(cols=c(starts_with('self_'),starts_with('other_'),
                                                   starts_with('manager_')),
                                            names_to=c('Relationship','statistic'),names_sep='_')
    graphs_df <- graphs_df %>% pivot_wider(id_cols=c('Cell.name','Question','Relationship'),
                                           names_from='statistic',values_from='value')
    
    colnames(graphs_df) <- c("Cell", "Question", "Relationship", "Mean", "Reviewers")
    
    graphs_df <- graphs_df %>% mutate(pct=round(Mean*100,1))
    graphs_df <- graphs_df %>% mutate(Question = str_replace_all(Question,"\\([^()]*\\)",""))
    
    
    return(graphs_df)
  }        
  
  questions_graphs_df <- reactive({
    questions_exclude <- str_replace_all(input$questions_graphs.question_exclude,"\\([^()]*\\)","")
    df <- getTable_questions.questions_graphs(cells.questions()) %>% 
      filter(Cell %in% input$questions_graphs.cell_select,
             !Question %in% questions_exclude,
             Relationship %in% tolower(input$questions_graphs.relation_checkbox))
    
    skipped_questions <- df
    df <- df %>% filter(!(Reviewers < 4 & Relationship %in% tolower(input$questions_graphs.filter_checkbox)))
    skipped_questions <- skipped_questions %>% filter(Reviewers < 4 & !Relationship %in% tolower(input$questions_graphs.filter_checkbox))
    return(list(df,skipped_questions))
    
  })
  
  observeEvent(input$questions_graphs.cell_select,{
    updateTextInput(session,'questions_graphs.plot.title',
                    value=input$questions_graphs.cell_select)
  })
  
  graph.questions_graph <- eventReactive(input$questions_graphs.generate,{
    
    
    req(!is.null(input$questions_graphs.cell_select))
    req(!is.null(input$questions_graphs.relation_checkbox))
    
    graph_df <- questions_graphs_df()[[1]]
    skipped_questions <- questions_graphs_df()[[2]]
    
    if (nrow(skipped_questions)!=0) {
      skipped_questions <- skipped_questions %>% mutate(message=paste0(str_trunc(Question,50),' does not have enough ',
                                                                       Relationship,' reviewers to maintain anonymity'))
      for (x in skipped_questions$message) {
        showNotification(paste(x), type = "warning", duration = 10)
      }
    }
    
    if (nrow(filter(graph_df,is.na(Mean)))!=0) {
      empty_questions <- graph_df %>% filter(is.na(Mean)) %>% 
        mutate(message=paste0(str_trunc(Question,50),' has no ',Relationship,' reviews'))
      
      for (x in empty_questions$message) {
        showNotification(paste(x), type = "warning", duration = 10)
      }
      
      graph_df <- graph_df %>% filter(!is.na(Mean))
    }
    
    questions_graph.colours <- c(input$questions_graphs.plot.color1,input$questions_graphs.plot.color2,
                                 input$questions_graphs.plot.color3)
    
    p <- ggplot(graph_df,aes(x=Question,y=Mean,fill=Relationship)) + 
      geom_col(aes(x=fct_reorder2(Question,Relationship!='self',Mean)),position = 'dodge') + 
      geom_text(aes(label = paste0(pct,'% ')),hjust=1.2,position=position_dodge(.9),
                family='Work Sans', color=input$questions_graphs.plot.data_label_color,
                size=input$questions_graphs.plot.data_label) +
      theme_hc() +
      theme(text=element_text(family='Work Sans',color=graph.colors()),
            title = element_text(face='bold',size=input$questions_graphs.plot.title_size,color=graph.colors()),
            axis.title = element_text(face='bold',size=input$questions_graphs.plot.axis_title,color=graph.colors()),
            plot.subtitle = element_text(face='italic',size=input$questions_graphs.plot.subtitle_size,color=graph.colors()),
            axis.text = element_text(color=graph.colors(),size=input$questions_graphs.plot.axis_label),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_),
            panel.grid.major.y = element_line(color='grey35'),
            legend.background = element_rect(fill='transparent',
                                             colour=NA_character_)) +
      xlab(input$questions_graphs.plot.y_title) +
      ylab(input$questions_graphs.plot.x_title) +
      labs(fill='Relationship to user',
           title=str_wrap(trimws(input$questions_graphs.plot.title),50),
           subtitle=str_wrap(input$questions_graphs.plot.subtitle,50)) +
      scale_fill_manual(values=questions_graph.colours) +
      scale_x_discrete(limits=rev,labels = function(x) str_wrap(str_trunc(x,90), width = 30)) +
      scale_y_continuous(labels=scales::percent) + coord_flip()
    
    return(p)
  })
  
  
  
  
  output$questions_graphs.plot <- renderPlotly({
    
    
    ggplotly(
      p = graph.questions_graph(),
      tooltip = NULL
    ) %>% 
      layout(margin = list(b = 50),
             paper_bgcolor=ifelse(input$graphMode=='light','white','#1b2634'),
             plot_bgcolor=ifelse(input$graphMode=='light','white','#1b2634')) %>%
      style(textposition='left')
    
  })
  
  outputOptions(output,'questions_graphs.plot',suspendWhenHidden=F)
  
  #### Download Graph Button
  output$downloadQuestions <- downloadHandler(
    filename = function(file) {
      paste0(input$questions_graphs.cell_select,'-questions_graph.png')
    },
    content = function(file) {
      ggsave(file,graph.questions_graph(),bg='transparent',width=9,height=5,dpi=600,device='png')
    }
  )
  
  outputOptions(output,'downloadQuestions',suspendWhenHidden=F)
  
  ## Relationships
  ### By Cell
  getTable_relationships_cells <- function(df) {
    
    if (is.null(df)) return(NULL)
    
    other_df <- df[,c("Cell.name", "self_MN", "other_MN", "diff_self_other_MN", 
                      "self_reviewers", "total_self_reviews",
                      "other_reviewers", "other_reviews")]
    
    colnames(other_df) <- c("Cell", "Self", "Other", "Difference", "Unique Self Reviews", 
                            "Total Self Reviews", "Reviewers", "Total Reviews")
    return(other_df)
  }
  
  output$cells.relationships <- DT::renderDataTable({
    
    other_df <- getTable_relationships_cells(cells.relationships.tbl())
    
    if (is.null(other_df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    shinyjs::enable(id = 'downloadRaw')
    
    datatable(other_df, rownames=FALSE, extensions = "Buttons",
              filter="none", options=list(order = list(1, "desc"), dom = 'Bfrtip',
                                          buttons = c('copy', 'csv', 'excel', 'pdf'),
                                          pageLength = min(MAX_PAGE_LEN, nrow(other_df)),
                                          searching = T, scrollX = TRUE)) %>%
      formatRound(columns = c("Self", "Other", "Difference"), digits = 3)  %>%
      formatStyle('Difference',
                  backgroundColor = styleInterval(c(-1, 0), c('white', '#f5ae51', '#94ffa4'))) %>%
      formatStyle('Unique Self Reviews',
                  backgroundColor = styleInterval(c(3, 9), c('white', '#f5ae51', '#94ffa4'))) %>%
      formatPercentage(columns = c("Self", "Other", "Difference"), 1) 
  })
  
  ### By Cell and Question
  getTable_relationships_cells.questions <- function(df) {
    
    if (is.null(df)) return(NULL)
    
    question.other_df <- df[,c("Cell.name", "Question", "self_MN", 
                               "other_MN", "diff_self_other_MN", 
                               "self_reviewers", "total_self_reviews",
                               "other_reviewers", "other_reviews")]
    
    colnames(question.other_df) <- c("Cell", "Question", "Self", "Other", "Difference", 
                                     "Unique Self Reviews", "Total Self Reviews",  
                                     "Reviewers", "Total Reviews")
    return(question.other_df)
  }
  
  output$cells.questions.relationships <- DT::renderDataTable({
    
    question.other_df <- getTable_relationships_cells.questions(cells.questions.relationships.tbl())
    
    if (is.null(question.other_df)) return(NULL)
    
    shinyjs::enable(id = 'downloadRaw')
    
    datatable(question.other_df, rownames=FALSE, extensions = "Buttons",
              filter="none", options=list(order = list(2, "desc"), dom = 'Bfrtip', 
                                          pageLength = min(MAX_PAGE_LEN, nrow(question.other_df)),
                                          buttons = c('copy', 'csv', 'excel', 'pdf'),
                                          searching = T, scrollX = TRUE)) %>%
      formatRound(columns = c("Self", "Other", "Difference"), digits = 3)  %>%
      formatStyle('Difference',
                  backgroundColor = styleInterval(c(-1, 0), c('white', '#f5ae51', '#94ffa4'))) %>%
      formatStyle('Unique Self Reviews',
                  backgroundColor = styleInterval(c(3, 9), c('white', '#f5ae51', '#94ffa4'))) %>%
      formatPercentage(columns = c("Self", "Other", "Difference"), 1) 
  })
  
  ## Cells - Variability
  output$question.variance <- renderPlotly({
    req(length(input$variability.behaviour_select)>2)
    
    graph.dat <- cells.graphs() %>% filter(Question %in% input$variability.behaviour_select)
    if (is.null(graph.dat)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    graph.dat <- graph.dat %>% mutate(Question_wrapped=str_wrap(Question,width=20))
    
    fig <- graph.dat %>% plot_ly(
      type = 'violin',
      spanmode='hard'
    ) 
    
    
    fig <- fig %>% add_trace(
      x = ~Question_wrapped[graph.dat$Relationshiptouser == 'self'],
      y = ~original_value[graph.dat$Relationshiptouser == 'self'],
      legendgroup = 'self',
      scalegroup = 'self',
      name = 'Self',
      side = 'negative',
      box = list(visible=T),
      meanline = list(visible=T),
      color = '#A7C7E7',
      hoverinfo = 'y'
    )
    
    fig <- fig %>% add_trace(
      x = ~Question_wrapped[graph.dat$Relationshiptouser == 'other'],
      y = ~original_value[graph.dat$Relationshiptouser == 'other'],
      legendgroup = 'other',
      scalegroup = 'other',
      name = 'Other',
      side = 'positive',
      box = list(visible=T),
      meanline = list(visible=T),
      color = '#77DD77',
      hoverinfo = 'y'
    )
    
    
    fig <- fig %>% add_trace(
      x = ~Question_wrapped[graph.dat$Relationshiptouser == 'manager'],
      y = ~original_value[graph.dat$Relationshiptouser == 'manager'],
      legendgroup = 'manager',
      scalegroup = 'manager',
      name = 'Manager',
      side = 'positive',
      box = list(visible=T),
      meanline = list(visible=T),
      color = '#FF6961',
      hoverinfo = 'y'
    )
    
    fig <- fig %>%
      layout(
        xaxis = list(title = "Question",range = list(-0.5,length(input$variability.behaviour_select)-0.5)),
        yaxis = list(title = "Score",
                     zeroline = F
        ),
        violingap = 0,
        violingroupgap = 0,
        violinmode = 'overlay'
      )
    
    ggplotly(
      p = fig,
      tooltip = NULL
    ) %>% layout(margin = list(b = 50))
    
  })
  
  
  
  output$cell.variance <- renderPlotly({
    
    graph.dat <- cells.graphs()
    if (is.null(graph.dat)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    # Compute function for dynamically adjusting graph size depending on the number of cells to be presented
    
    ## Compute number of rows
    n.rows <- ceiling(length(unique(graph.dat$Cell.name)) / 2)
    
    graph.spread <- ggplot(graph.dat, 
                           aes(x = original_value, 
                               color = Relationshiptouser, 
                               fill = Relationshiptouser)) + 
      geom_density(alpha = 0.5,adjust = 2) + 
      facet_wrap(. ~ Cell.name, ncol = 2, scales = "free") +
      theme_minimal() + 
      scale_x_continuous(breaks = seq(0,10,1), labels = function(x) paste0(x*10, "%")) + 
      coord_cartesian(xlim = c(0,10)) + 
      labs(x = "Behaviour Performed (%)", y = "", color = "Relationship", fill = "Relationship") + 
      theme(legend.position = "bottom", 
            axis.text.y = element_blank(),
            axis.text.x = element_text(size=6),
            strip.text = element_text(size = 10)) + 
      scale_fill_viridis_d(direction = 1) + 
      scale_color_viridis_d(direction = 1) 
    
    ggplotly(
      p = graph.spread,
      tooltip = NULL,
      height = n.rows * 200
    ) %>% 
      layout(margin = list(b = 50))
    
  }) 
  
  ## Time Series Graphs
  output$time.series <- renderPlotly({
    
    graph.dat <- cells.graphs()
    if (is.null(graph.dat)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    # Compute average behaviour score by user, date created, relationship type and cell name
    tbl.mean.score <- graph.dat %>% 
      group_by(user_id, reviewer_id, created_at, Relationshiptouser, Cell.name) %>%
      dplyr::summarise(Mean_Score  = mean(original_value, na.rm = T), .groups = "drop") 
    
    # Compute number of reviews by user, date created, relationship type and cell name
    tbl.reviews <- graph.dat %>% 
      group_by(user_id, reviewer_id, created_at, Relationshiptouser, Cell.name) %>%
      dplyr::summarise(Reviews = n(), .groups = "drop") 
    
    # Combine the two tables
    combined.time.series.tbl <- left_join(tbl.mean.score, tbl.reviews)
    
    # Recode created_at into a date
    combined.time.series.tbl$created_at <- as.Date(combined.time.series.tbl$created_at)
    
    # Rename relationships variable
    names(combined.time.series.tbl)[4] <- c("Relationship")
    
    if (nrow(combined.time.series.tbl) == 0) {
      return(NULL)
    }
    
    x <- combined.time.series.tbl %>%
      ggplot(aes(x = created_at, y = Mean_Score, color = Relationship, fill = Relationship)) +
      theme_tufte() + 
      geom_point(alpha = 0.4, show.legend = F, 
                 aes(size = Reviews,
                     text = paste('Created on: ', created_at,
                                  '<br>User id:', user_id,
                                  '<br>Reviewer id: ', reviewer_id,
                                  '<br>Behaviour score: ', paste0(round(Mean_Score*10,0), "%"),
                                  '<br>Reviews: ', Reviews
                     ))) +
      geom_smooth(method = "lm", se = F, size = 1, show.legend = T) +
      theme(axis.title.x = element_blank(), 
            axis.text.x = element_text(angle = 90, vjust = 0.2),
            axis.line = element_line(color = "black")) + 
      scale_y_continuous(breaks = seq(0,10,2), labels = function(x) paste0(x*10, "%")) + 
      coord_cartesian(ylim = c(0,10)) + 
      labs(y = "Behaviour Performed (%)") + 
      scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m", 
                   limits = c(floor_date(as.Date(min(combined.time.series.tbl$created_at)), "month"), 
                              ceiling_date(as.Date(max(combined.time.series.tbl$created_at)), "month")), 
                   expand = c(0.03, 0)) + 
      scale_fill_viridis_d(direction = 1) + 
      scale_color_viridis_d(direction = 1) + 
      scale_size(guide="none", range = c(1,2), breaks = seq(1, 2, 0.2))
    
    ggplotly(x = x,
             tooltip = c("text"), 
             height = 600)
    
    
  })
  
  ## Time Series Graphs by Cell
  output$time.series.by.cell <- renderPlotly({
    
    graph.dat <- cells.graphs()
    if (is.null(graph.dat)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    # Compute average behaviour score by user, date created, relationship type and cell name
    tbl.mean.score <- graph.dat %>% 
      group_by(user_id, reviewer_id, created_at, Relationshiptouser, Cell.name) %>%
      dplyr::summarise(Mean_Score  = mean(original_value, na.rm = T), .groups = "drop") 
    
    # Compute number of reviews by user, date created, relationship type and cell name
    tbl.reviews <- graph.dat %>% 
      group_by(user_id, reviewer_id, created_at, Relationshiptouser, Cell.name) %>%
      dplyr::summarise(Reviews = n(), .groups = "drop") 
    
    # Combine the two tables
    combined.time.series.tbl <- left_join(tbl.mean.score, tbl.reviews)
    
    # Recode created_at into a date
    combined.time.series.tbl$created_at <- as.Date(combined.time.series.tbl$created_at)
    
    # Rename relationships variable
    names(combined.time.series.tbl)[4] <- c("Relationship")
    
    x <- combined.time.series.tbl %>%
      ggplot(aes(x = created_at, y = Mean_Score, color = Relationship, fill = Relationship)) +
      theme_tufte() + 
      geom_point(alpha = 0.4, show.legend = F, 
                 aes(size = Reviews,
                     text = paste('Created on: ', created_at,
                                  '<br>User id:', user_id,
                                  '<br>Reviewer id: ', reviewer_id,
                                  '<br>Behaviour score: ', paste0(round(Mean_Score*10,0), "%"),
                                  '<br>Reviews: ', Reviews
                     ))) +
      geom_smooth(method = "lm", se = F, size = 1, show.legend = T) + 
      facet_wrap(. ~ Cell.name, ncol = 2, scales = "free") + 
      theme(axis.title.x = element_blank(), 
            axis.text.x = element_text(angle = 90, vjust = 0.2),
            axis.line = element_line(color = "black"), 
            strip.text = element_text(size = 12)) + 
      scale_y_continuous(breaks = seq(0,10,2), labels = function(x) paste0(x*10, "%")) + 
      coord_cartesian(ylim = c(0,10)) + 
      labs(y = "Behaviour Performed (%)") + 
      scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m", 
                   limits = c(floor_date(as.Date(min(combined.time.series.tbl$created_at)), "month"), 
                              ceiling_date(as.Date(max(combined.time.series.tbl$created_at)), "month")), 
                   expand = c(0.03, 0)) + 
      scale_fill_viridis_d(direction = 1) + 
      scale_color_viridis_d(direction = 1) + 
      scale_size(guide="none", range = c(1,2), breaks = seq(1, 2, 0.2))
    
    ## Compute number of rows for the graph
    n.rows <- ceiling(length(unique(graph.dat$Cell.name)) / 2)
    
    ggplotly(x = x,
             tooltip = c( "text"), 
             height = n.rows * 350)
    
  })
  
  ## Time Series % Change Overall Table
  output$time.series.perc.chg.overall <- DT::renderDataTable({
    
    time.series.tbl <- time.series.change()$time.series.overall.df
    if (is.null(time.series.tbl)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    datatable(time.series.tbl, rownames=FALSE, extensions = "Buttons",
              filter="none", options=list(order = list(1, "asc"), dom = 'Bfrtip', 
                                          pageLength = min(MAX_PAGE_LEN, nrow(time.series.tbl)),
                                          buttons = c('copy', 'csv', 'excel', 'pdf'),
                                          searching = T)) %>%
      formatRound(columns = c("% Change"), digits = 3)  %>%
      formatStyle('% Change',
                  backgroundColor = styleInterval(cuts = 0, values = c('#f5ae51', '#94ffa4'))) %>%
      formatPercentage(columns = c("% Change"), 1) 
  })
  
  # Time Series Percentage Change By Cell Table
  output$time.series.perc.chg.cells <- DT::renderDataTable({
    
    time.series.tbl <- time.series.change()$time.series.cell.df
    if (is.null(time.series.tbl)) {
      return(NULL)
    }
    
    datatable(time.series.tbl, rownames=FALSE, extensions = "Buttons",
              filter="none", options=list(order = list(0, "asc"), dom = 'Bfrtip', 
                                          pageLength = min(MAX_PAGE_LEN, nrow(time.series.tbl)),
                                          buttons = c('copy', 'csv', 'excel', 'pdf'),
                                          searching = T)) %>%
      formatRound(columns = c("% Change"), digits = 3)  %>%
      formatStyle('% Change',
                  backgroundColor = styleInterval(cuts = 0, values = c('#f5ae51', '#94ffa4'))) %>%
      formatPercentage(columns = c("% Change"), 1) 
  })
  
  
  ## Demographics
  # In this section, for each category, we calculate the summary frequency table to show
  # the number of users/reviewers from each group and their relative percentages. We then
  # update the group selection inputs for the plots based on the summary dataframe labels.
  # If any groups are selected, the labels are automatically updated to show a list of the selected
  # labels, and the plot will group by those selections. If no groups are selected, the plot will
  # be generated using the default groups
  
  
  ### Reviewer Demographics - Gender Identity Table
  output$reviewer_demo_gender_tbl <- DT::renderDataTable({
    
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    # Get a dataframe containing the frequency distribution 
    table_df <- demo.tbler(df,'r.gender')
    group_choices <- table_df %>% filter(!Group%in%c('Missing','Prefer not to say','No response')) %>% pull(Group)
    
    updateSelectizeInput(session,'reviewer_demographics_gender.graph.selectgroup1',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_gender.graph.selectgroup2',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_gender.graph.selectgroup3',
                         choices=group_choices)
    
    table_df
    
  })  
  
  ### Reviewer Demographics - Gender Identity Graph
  
  # Updates the text inputs to show a list of labels from the groups selected
  observe(updateTextInput(session,'reviewer_demographics_gender.graph.group1label',
                          value=paste(input$reviewer_demographics_gender.graph.selectgroup1,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_gender.graph.group2label',
                          value=paste(input$reviewer_demographics_gender.graph.selectgroup2,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_gender.graph.group3label',
                          value=paste(input$reviewer_demographics_gender.graph.selectgroup3,collapse=', ')))
  
  reviewer_demographics_gender_graph <- eventReactive(input$reviewer_demographics_gender.graph.plot.generate,{
    df <- demo.differences()
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    if (is.null(input$reviewer_demographics_gender.graph.selectgroup1) & 
        is.null(input$reviewer_demographics_gender.graph.selectgroup2) &
        is.null(input$reviewer_demographics_gender.graph.selectgroup3)) {
      showNotification(paste("No groups selected. Using default groups"), type = "warning", duration = 5)   
      df <- df %>% mutate(graph_group=r.gender)
      df <- df %>% 
        mutate(graph_group=ifelse(graph_group%in%c('Missing','Prefer not to say','No response'),NA,graph_group))
    } else {
      df <- df %>% mutate(graph_group=case_when(
        r.gender%in%input$reviewer_demographics_gender.graph.selectgroup1 ~ input$reviewer_demographics_gender.graph.group1label,
        r.gender%in%input$reviewer_demographics_gender.graph.selectgroup2 ~ input$reviewer_demographics_gender.graph.group2label,
        r.gender%in%input$reviewer_demographics_gender.graph.selectgroup3 ~ input$reviewer_demographics_gender.graph.group3label,
        .default = NA
      ))
    }
    
    # Mapping the palette to the factor levels in order of relative size
    reordered_levels <- df %>% filter(!is.na(original_value),Relationshiptouser!='self',!is.na(graph_group)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(reviewer_id)) %>% 
      arrange(n) %>% 
      mutate(graph_group=as_factor(graph_group)) %>% 
      pull(graph_group) %>% levels()
    
    graph.colours <- c(input$reviewer_demographics_gender.graph.plot.color1,input$reviewer_demographics_gender.graph.plot.color2,
                       input$reviewer_demographics_gender.graph.plot.color3,input$reviewer_demographics_gender.graph.plot.color4,
                       input$reviewer_demographics_gender.graph.plot.color5,input$reviewer_demographics_gender.graph.plot.color6,
                       input$reviewer_demographics_gender.graph.plot.color7,input$reviewer_demographics_gender.graph.plot.color8)
    color_mapping <- setNames(graph.colours[rep(seq_along(graph.colours), length.out = length(reordered_levels))], rev(reordered_levels))
    
    p <- df %>% filter(!is.na(original_value),Relationshiptouser!='self') %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(reviewer_id)) %>% 
      mutate(pct=paste0(round(n*100/sum(n),1),'%'),
             index=paste(ifelse(!is.na(graph_group),graph_group,'Missing'),pct,sep='\n')) %>% 
      ggplot(aes(area=n,fill=graph_group,label=index)) + 
      geom_treemap() + 
      geom_treemap_text(size=input$reviewer_demographics_gender.graph.plot.data_label,
                        grow=F,reflow=T,family='Work Sans') + 
      theme(legend.position = 'none',
            text=element_text(family='Work Sans'),
            title = element_text(color=graph.colors(),face='bold',
                                 size=input$reviewer_demographics_gender.graph.plot.title_size),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_)) +
      scale_fill_manual(values=color_mapping,na.value='grey65') +
      ggtitle(input$reviewer_demographics_gender.graph.plot.title)
    
    
  })
  
  output$reviewer_demographics_gender.graph.plot <- renderPlot({
    p <- reviewer_demographics_gender_graph()
    
    if (is.null(p)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    p
    
  })
  
  output$reviewer_demographics_gender.graph.download <- downloadHandler(
    filename = function(file) {
      "reviewer_genderdemo.png"
    },
    content = function(file) {
      ggsave(file,reviewer_demographics_gender_graph(),bg='transparent',
             width=input$reviewer_demographics_gender.graph.plot_width,
             height=input$reviewer_demographics_gender.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### Reviewer Demographics - Age
  output$reviewer_demo_age_tbl <- DT::renderDataTable({
    
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    # Get a dataframe containing the frequency distribution 
    table_df <- demo.tbler(df,'r.age') 
    group_choices <- table_df %>% filter(!Group%in%c('Missing','Prefer not to say','No response')) %>% pull(Group)
    
    # Update Group Selection Inputs with labels available from the summary table
    updateSelectizeInput(session,'reviewer_demographics_age.graph.selectgroup1',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_age.graph.selectgroup2',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_age.graph.selectgroup3',
                         choices=group_choices)
    table_df
    
  })
  
  ### Reviewer Demographics - Age Graph
  
  # Updates the text inputs to show a list of labels from the groups selected
  observe(updateTextInput(session,'reviewer_demographics_age.graph.group1label',
                          value=paste(input$reviewer_demographics_age.graph.selectgroup1,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_age.graph.group2label',
                          value=paste(input$reviewer_demographics_age.graph.selectgroup2,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_age.graph.group3label',
                          value=paste(input$reviewer_demographics_age.graph.selectgroup3,collapse=', ')))
  
  reviewer_demographics_age_graph <- eventReactive(input$reviewer_demographics_age.graph.plot.generate,{
    df <- demo.differences()
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    if (is.null(input$reviewer_demographics_age.graph.selectgroup1) & 
        is.null(input$reviewer_demographics_age.graph.selectgroup2) &
        is.null(input$reviewer_demographics_age.graph.selectgroup3)) {
      showNotification(paste("No groups selected. Using default groups"), type = "warning", duration = 5)   
      df <- df %>% mutate(graph_group=r.age)
      df <- df %>% 
        mutate(graph_group=ifelse(graph_group%in%c('Missing','Prefer not to say','No response'),NA,graph_group))
    } else {
      df <- df %>% mutate(graph_group=case_when(
        r.age%in%input$reviewer_demographics_age.graph.selectgroup1 ~ input$reviewer_demographics_age.graph.group1label,
        r.age%in%input$reviewer_demographics_age.graph.selectgroup2 ~ input$reviewer_demographics_age.graph.group2label,
        r.age%in%input$reviewer_demographics_age.graph.selectgroup3 ~ input$reviewer_demographics_age.graph.group3label,
        .default = NA
      ))
    }
    
    reordered_levels <- df %>% filter(!is.na(original_value),Relationshiptouser!='self',!is.na(graph_group)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(reviewer_id)) %>% 
      arrange(n) %>% 
      mutate(graph_group=as_factor(graph_group)) %>% 
      pull(graph_group) %>% levels()
    
    graph.colours <- c(input$reviewer_demographics_age.graph.plot.color1,input$reviewer_demographics_age.graph.plot.color2,
                       input$reviewer_demographics_age.graph.plot.color3,input$reviewer_demographics_age.graph.plot.color4,
                       input$reviewer_demographics_age.graph.plot.color5,input$reviewer_demographics_age.graph.plot.color6,
                       input$reviewer_demographics_age.graph.plot.color7,input$reviewer_demographics_age.graph.plot.color8)
    color_mapping <- setNames(graph.colours[rep(seq_along(graph.colours), length.out = length(reordered_levels))], rev(reordered_levels))
    
    p <- df %>% filter(!is.na(original_value),Relationshiptouser!='self') %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(reviewer_id)) %>% 
      mutate(pct=paste0(round(n*100/sum(n),1),'%'),
             index=paste(ifelse(!is.na(graph_group),graph_group,'Missing'),pct,sep='\n')) %>% 
      ggplot(aes(area=n,fill=graph_group,label=index)) + 
      geom_treemap() + 
      geom_treemap_text(size=input$reviewer_demographics_age.graph.plot.data_label,
                        grow=F,reflow=T,family='Work Sans') + 
      theme(legend.position = 'none',
            text=element_text(family='Work Sans'),
            title = element_text(color=graph.colors(),face='bold',
                                 size=input$reviewer_demographics_age.graph.plot.title_size),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_)) +
      scale_fill_manual(values=color_mapping,na.value='grey65') +
      ggtitle(input$reviewer_demographics_age.graph.plot.title)
    
    
  })
  
  output$reviewer_demographics_age.graph.plot <- renderPlot({
    p <- reviewer_demographics_age_graph()
    
    if (is.null(p)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    p
    
  })
  
  output$reviewer_demographics_age.graph.download <- downloadHandler(
    filename = function(file) {
      "reviewer_agedemo.png"
    },
    content = function(file) {
      ggsave(file,reviewer_demographics_age_graph(),bg='transparent',
             width=input$reviewer_demographics_age.graph.plot_width,
             height=input$reviewer_demographics_age.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### Reviewer Demographics - Sexuality
  output$reviewer_demo_sexuality_tbl <- DT::renderDataTable({
    
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    # Get a dataframe containing the frequency distribution 
    table_df <- demo.tbler(df,'r.sexuality') 
    group_choices <- table_df %>% filter(!Group%in%c('Missing','Prefer not to say','No response')) %>% pull(Group)
    
    updateSelectizeInput(session,'reviewer_demographics_sexuality.graph.selectgroup1',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_sexuality.graph.selectgroup2',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_sexuality.graph.selectgroup3',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_sexuality.graph.selectgroup4',
                         choices=group_choices)
    table_df
    
  })  
  
  ### Reviewer Demographics - Sexuality Graph
  
  observe(updateTextInput(session,'reviewer_demographics_sexuality.graph.group1label',
                          value=paste(input$reviewer_demographics_sexuality.graph.selectgroup1,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_sexuality.graph.group2label',
                          value=paste(input$reviewer_demographics_sexuality.graph.selectgroup2,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_sexuality.graph.group3label',
                          value=paste(input$reviewer_demographics_sexuality.graph.selectgroup3,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_sexuality.graph.group4label',
                          value=paste(input$reviewer_demographics_sexuality.graph.selectgroup4,collapse=', ')))
  
  reviewer_demographics_sexuality_graph <- eventReactive(input$reviewer_demographics_sexuality.graph.plot.generate,{
    df <- demo.differences()
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    if (is.null(input$reviewer_demographics_sexuality.graph.selectgroup1) & 
        is.null(input$reviewer_demographics_sexuality.graph.selectgroup2) &
        is.null(input$reviewer_demographics_sexuality.graph.selectgroup3) &
        is.null(input$reviewer_demographics_sexuality.graph.selectgroup4)) {
      showNotification(paste("No groups selected. Using default groups"), type = "warning", duration = 5)   
      df <- df %>% mutate(graph_group=r.sexuality)
      df <- df %>% 
        mutate(graph_group=ifelse(graph_group%in%c('Missing','Prefer not to say','No response'),NA,graph_group))
    } else {
      df <- df %>% mutate(graph_group=case_when(
        r.sexuality%in%input$reviewer_demographics_sexuality.graph.selectgroup1 ~ input$reviewer_demographics_sexuality.graph.group1label,
        r.sexuality%in%input$reviewer_demographics_sexuality.graph.selectgroup2 ~ input$reviewer_demographics_sexuality.graph.group2label,
        r.sexuality%in%input$reviewer_demographics_sexuality.graph.selectgroup3 ~ input$reviewer_demographics_sexuality.graph.group3label,
        r.sexuality%in%input$reviewer_demographics_sexuality.graph.selectgroup4 ~ input$reviewer_demographics_sexuality.graph.group4label,
        .default = NA
      ))
    }
    
    reordered_levels <- df %>% filter(!is.na(original_value),Relationshiptouser!='self',!is.na(graph_group)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(reviewer_id)) %>% 
      arrange(n) %>% 
      mutate(graph_group=as_factor(graph_group)) %>% 
      pull(graph_group) %>% levels()
    
    graph.colours <- c(input$reviewer_demographics_sexuality.graph.plot.color1,input$reviewer_demographics_sexuality.graph.plot.color2,
                       input$reviewer_demographics_sexuality.graph.plot.color3,input$reviewer_demographics_sexuality.graph.plot.color4,
                       input$reviewer_demographics_sexuality.graph.plot.color5,input$reviewer_demographics_sexuality.graph.plot.color6,
                       input$reviewer_demographics_sexuality.graph.plot.color7,input$reviewer_demographics_sexuality.graph.plot.color8)
    color_mapping <- setNames(graph.colours[rep(seq_along(graph.colours), length.out = length(reordered_levels))], rev(reordered_levels))
    
    p <- df %>% filter(!is.na(original_value),Relationshiptouser!='self') %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(reviewer_id)) %>% 
      mutate(pct=paste0(round(n*100/sum(n),1),'%'),
             index=paste(ifelse(!is.na(graph_group),graph_group,'Missing'),pct,sep='\n')) %>% 
      ggplot(aes(area=n,fill=graph_group,label=index)) + 
      geom_treemap() + 
      geom_treemap_text(size=input$reviewer_demographics_sexuality.graph.plot.data_label,
                        grow=F,reflow=T,family='Work Sans') + 
      theme(legend.position = 'none',
            text=element_text(family='Work Sans'),
            title = element_text(color=graph.colors(),face='bold',
                                 size=input$reviewer_demographics_sexuality.graph.plot.title_size),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_)) +
      scale_fill_manual(values=color_mapping,na.value='grey65') +
      ggtitle(input$reviewer_demographics_sexuality.graph.plot.title)
    
    
  })
  
  output$reviewer_demographics_sexuality.graph.plot <- renderPlot({
    p <- reviewer_demographics_sexuality_graph()
    
    if (is.null(p)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    p
    
  })
  
  output$reviewer_demographics_sexuality.graph.download <- downloadHandler(
    filename = function(file) {
      "reviewer_sexualitydemo.png"
    },
    content = function(file) {
      ggsave(file,reviewer_demographics_sexuality_graph(),bg='transparent',
             width=input$reviewer_demographics_sexuality.graph.plot_width,
             height=input$reviewer_demographics_sexuality.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### Reviewer Demographics - Ethnicity
  output$reviewer_demo_ethnicity_tbl <- DT::renderDataTable({
    
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    # Get a dataframe containing the frequency distribution 
    table_df <- demo.tbler(df,'r.ethnicity')
    
    group_choices <- table_df %>% filter(!Group%in%c('Missing','Prefer not to say','No response')) %>% pull(Group)
    
    updateSelectizeInput(session,'reviewer_demographics_ethnicity.graph.selectgroup1',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_ethnicity.graph.selectgroup2',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_ethnicity.graph.selectgroup3',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_ethnicity.graph.selectgroup4',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_ethnicity.graph.selectgroup5',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_ethnicity.graph.selectgroup6',
                         choices=group_choices)
    table_df
    
  })
  
  ### Reviewer Demographics - Ethnicity Graph
  
  observe(updateTextInput(session,'reviewer_demographics_ethnicity.graph.group1label',
                          value=paste(input$reviewer_demographics_ethnicity.graph.selectgroup1,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_ethnicity.graph.group2label',
                          value=paste(input$reviewer_demographics_ethnicity.graph.selectgroup2,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_ethnicity.graph.group3label',
                          value=paste(input$reviewer_demographics_ethnicity.graph.selectgroup3,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_ethnicity.graph.group4label',
                          value=paste(input$reviewer_demographics_ethnicity.graph.selectgroup4,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_ethnicity.graph.group5label',
                          value=paste(input$reviewer_demographics_ethnicity.graph.selectgroup5,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_ethnicity.graph.group6label',
                          value=paste(input$reviewer_demographics_ethnicity.graph.selectgroup6,collapse=', ')))
  
  reviewer_demographics_ethnicity_graph <- eventReactive(input$reviewer_demographics_ethnicity.graph.plot.generate,{
    df <- demo.differences()
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    if (is.null(input$reviewer_demographics_ethnicity.graph.selectgroup1) & 
        is.null(input$reviewer_demographics_ethnicity.graph.selectgroup2) &
        is.null(input$reviewer_demographics_ethnicity.graph.selectgroup3) &
        is.null(input$reviewer_demographics_ethnicity.graph.selectgroup4) & 
        is.null(input$reviewer_demographics_ethnicity.graph.selectgroup5) &
        is.null(input$reviewer_demographics_ethnicity.graph.selectgroup6)) {
      showNotification(paste("No groups selected. Using default groups"), type = "warning", duration = 5)   
      df <- df %>% mutate(graph_group=r.ethnicity)
      df <- df %>% 
        mutate(graph_group=ifelse(graph_group%in%c('Missing','Prefer not to say','No response'),NA,graph_group))
    } else {
      df <- df %>% mutate(graph_group=case_when(
        r.ethnicity%in%input$reviewer_demographics_ethnicity.graph.selectgroup1 ~ input$reviewer_demographics_ethnicity.graph.group1label,
        r.ethnicity%in%input$reviewer_demographics_ethnicity.graph.selectgroup2 ~ input$reviewer_demographics_ethnicity.graph.group2label,
        r.ethnicity%in%input$reviewer_demographics_ethnicity.graph.selectgroup3 ~ input$reviewer_demographics_ethnicity.graph.group3label,
        r.ethnicity%in%input$reviewer_demographics_ethnicity.graph.selectgroup4 ~ input$reviewer_demographics_ethnicity.graph.group4label,
        r.ethnicity%in%input$reviewer_demographics_ethnicity.graph.selectgroup5 ~ input$reviewer_demographics_ethnicity.graph.group5label,
        r.ethnicity%in%input$reviewer_demographics_ethnicity.graph.selectgroup6 ~ input$reviewer_demographics_ethnicity.graph.group6label,
        .default = NA
      ))
    }
    
    reordered_levels <- df %>% filter(!is.na(original_value),Relationshiptouser!='self',!is.na(graph_group)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(reviewer_id)) %>% 
      arrange(n) %>% 
      mutate(graph_group=as_factor(graph_group)) %>% 
      pull(graph_group) %>% levels()
    
    graph.colours <- c(input$reviewer_demographics_ethnicity.graph.plot.color1,input$reviewer_demographics_ethnicity.graph.plot.color2,
                       input$reviewer_demographics_ethnicity.graph.plot.color3,input$reviewer_demographics_ethnicity.graph.plot.color4,
                       input$reviewer_demographics_ethnicity.graph.plot.color5,input$reviewer_demographics_ethnicity.graph.plot.color6,
                       input$reviewer_demographics_ethnicity.graph.plot.color7,input$reviewer_demographics_ethnicity.graph.plot.color8)
    color_mapping <- setNames(graph.colours[rep(seq_along(graph.colours), length.out = length(reordered_levels))], rev(reordered_levels))
    
    p <- df %>% filter(!is.na(original_value),Relationshiptouser!='self') %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(reviewer_id)) %>% 
      mutate(pct=paste0(round(n*100/sum(n),1),'%'),
             index=paste(ifelse(!is.na(graph_group),graph_group,'Missing'),pct,sep='\n')) %>% 
      ggplot(aes(area=n,fill=graph_group,label=index)) + 
      geom_treemap() + 
      geom_treemap_text(size=input$reviewer_demographics_ethnicity.graph.plot.data_label,
                        grow=F,reflow=T,family='Work Sans') + 
      theme(legend.position = 'none',
            text=element_text(family='Work Sans'),
            title = element_text(color=graph.colors(),face='bold',
                                 size=input$reviewer_demographics_ethnicity.graph.plot.title_size),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_)) +
      scale_fill_manual(values=color_mapping,na.value='grey65') +
      ggtitle(input$reviewer_demographics_ethnicity.graph.plot.title)
    
    
  })
  
  output$reviewer_demographics_ethnicity.graph.plot <- renderPlot({
    p <- reviewer_demographics_ethnicity_graph()
    
    if (is.null(p)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    p
    
  })
  
  output$reviewer_demographics_ethnicity.graph.download <- downloadHandler(
    filename = function(file) {
      "reviewer_ethnicitydemo.png"
    },
    content = function(file) {
      ggsave(file,reviewer_demographics_ethnicity_graph(),bg='transparent',
             width=input$reviewer_demographics_ethnicity.graph.plot_width,
             height=input$reviewer_demographics_ethnicity.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### Reviewer Demographics - Ethnicity Collapsed
  output$reviewer_demo_ethnicity_collapsed_tbl <- DT::renderDataTable({
    
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    # Get a dataframe containing the frequency distribution 
    table_df <- demo.tbler(df,'r.ethnicity_collapsed') 
    
    group_choices <- table_df %>% filter(!Group%in%c('Missing','Prefer not to say','No response')) %>% pull(Group)
    
    updateSelectizeInput(session,'reviewer_demographics_ethnicity_collapsed.graph.selectgroup1',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_ethnicity_collapsed.graph.selectgroup2',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_ethnicity_collapsed.graph.selectgroup3',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_ethnicity_collapsed.graph.selectgroup4',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_ethnicity_collapsed.graph.selectgroup5',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_ethnicity_collapsed.graph.selectgroup6',
                         choices=group_choices)
    table_df
    
  })
  
  ### Reviewer Demographics - Ethnicity Colllapsed Graph
  
  observe(updateTextInput(session,'reviewer_demographics_ethnicity_collapsed.graph.group1label',
                          value=paste(input$reviewer_demographics_ethnicity_collapsed.graph.selectgroup1,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_ethnicity_collapsed.graph.group2label',
                          value=paste(input$reviewer_demographics_ethnicity_collapsed.graph.selectgroup2,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_ethnicity_collapsed.graph.group3label',
                          value=paste(input$reviewer_demographics_ethnicity_collapsed.graph.selectgroup3,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_ethnicity_collapsed.graph.group4label',
                          value=paste(input$reviewer_demographics_ethnicity_collapsed.graph.selectgroup4,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_ethnicity_collapsed.graph.group5label',
                          value=paste(input$reviewer_demographics_ethnicity_collapsed.graph.selectgroup5,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_ethnicity_collapsed.graph.group6label',
                          value=paste(input$reviewer_demographics_ethnicity_collapsed.graph.selectgroup6,collapse=', ')))
  
  reviewer_demographics_ethnicity_collapsed_graph <- eventReactive(input$reviewer_demographics_ethnicity_collapsed.graph.plot.generate,{
    df <- demo.differences()
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    if (is.null(input$reviewer_demographics_ethnicity_collapsed.graph.selectgroup1) & 
        is.null(input$reviewer_demographics_ethnicity_collapsed.graph.selectgroup2) &
        is.null(input$reviewer_demographics_ethnicity_collapsed.graph.selectgroup3) &
        is.null(input$reviewer_demographics_ethnicity_collapsed.graph.selectgroup4) & 
        is.null(input$reviewer_demographics_ethnicity_collapsed.graph.selectgroup5) &
        is.null(input$reviewer_demographics_ethnicity_collapsed.graph.selectgroup6)) {
      showNotification(paste("No groups selected. Using default groups"), type = "warning", duration = 5)   
      df <- df %>% mutate(graph_group=r.ethnicity_collapsed)
      df <- df %>% 
        mutate(graph_group=ifelse(graph_group%in%c('Missing','Prefer not to say','No response'),NA,graph_group))
    } else {
      df <- df %>% mutate(graph_group=case_when(
        r.ethnicity_collapsed%in%input$reviewer_demographics_ethnicity_collapsed.graph.selectgroup1 ~ input$reviewer_demographics_ethnicity_collapsed.graph.group1label,
        r.ethnicity_collapsed%in%input$reviewer_demographics_ethnicity_collapsed.graph.selectgroup2 ~ input$reviewer_demographics_ethnicity_collapsed.graph.group2label,
        r.ethnicity_collapsed%in%input$reviewer_demographics_ethnicity_collapsed.graph.selectgroup3 ~ input$reviewer_demographics_ethnicity_collapsed.graph.group3label,
        r.ethnicity_collapsed%in%input$reviewer_demographics_ethnicity_collapsed.graph.selectgroup4 ~ input$reviewer_demographics_ethnicity_collapsed.graph.group4label,
        r.ethnicity_collapsed%in%input$reviewer_demographics_ethnicity_collapsed.graph.selectgroup5 ~ input$reviewer_demographics_ethnicity_collapsed.graph.group5label,
        r.ethnicity_collapsed%in%input$reviewer_demographics_ethnicity_collapsed.graph.selectgroup6 ~ input$reviewer_demographics_ethnicity_collapsed.graph.group6label,
        .default = NA
      ))
    }
    
    reordered_levels <- df %>% filter(!is.na(original_value),Relationshiptouser!='self',!is.na(graph_group)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(reviewer_id)) %>% 
      arrange(n) %>% 
      mutate(graph_group=as_factor(graph_group)) %>% 
      pull(graph_group) %>% levels()
    
    graph.colours <- c(input$reviewer_demographics_ethnicity_collapsed.graph.plot.color1,input$reviewer_demographics_ethnicity_collapsed.graph.plot.color2,
                       input$reviewer_demographics_ethnicity_collapsed.graph.plot.color3,input$reviewer_demographics_ethnicity_collapsed.graph.plot.color4,
                       input$reviewer_demographics_ethnicity_collapsed.graph.plot.color5,input$reviewer_demographics_ethnicity_collapsed.graph.plot.color6,
                       input$reviewer_demographics_ethnicity_collapsed.graph.plot.color7,input$reviewer_demographics_ethnicity_collapsed.graph.plot.color8)
    color_mapping <- setNames(graph.colours[rep(seq_along(graph.colours), length.out = length(reordered_levels))], rev(reordered_levels))
    
    p <- df %>% filter(!is.na(original_value),Relationshiptouser!='self') %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(reviewer_id)) %>% 
      mutate(pct=paste0(round(n*100/sum(n),1),'%'),
             index=paste(ifelse(!is.na(graph_group),graph_group,'Missing'),pct,sep='\n')) %>% 
      ggplot(aes(area=n,fill=graph_group,label=index)) + 
      geom_treemap() + 
      geom_treemap_text(size=input$reviewer_demographics_ethnicity_collapsed.graph.plot.data_label,
                        grow=F,reflow=T,family='Work Sans') + 
      theme(legend.position = 'none',
            text=element_text(family='Work Sans'),
            title = element_text(color=graph.colors(),face='bold',
                                 size=input$reviewer_demographics_ethnicity_collapsed.graph.plot.title_size),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_)) +
      scale_fill_manual(values=color_mapping,na.value='grey65') +
      ggtitle(input$reviewer_demographics_ethnicity_collapsed.graph.plot.title)
    
    
  })
  
  output$reviewer_demographics_ethnicity_collapsed.graph.plot <- renderPlot({
    p <- reviewer_demographics_ethnicity_collapsed_graph()
    
    if (is.null(p)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    p
    
  })
  
  output$reviewer_demographics_ethnicity_collapsed.graph.download <- downloadHandler(
    filename = function(file) {
      "reviewer_ethnicity_collapseddemo.png"
    },
    content = function(file) {
      ggsave(file,reviewer_demographics_ethnicity_collapsed_graph(),bg='transparent',
             width=input$reviewer_demographics_ethnicity_collapsed.graph.plot_width,
             height=input$reviewer_demographics_ethnicity_collapsed.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### Reviewer Demographics - Disability
  output$reviewer_demo_disability_tbl <- DT::renderDataTable({
    
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    # Get a dataframe containing the frequency distribution 
    table_df <- demo.tbler(df,'r.disability') 
    
    group_choices <- table_df %>% filter(!Group%in%c('Missing','Prefer not to say','No response')) %>% pull(Group)
    
    updateSelectizeInput(session,'reviewer_demographics_disability.graph.selectgroup1',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_disability.graph.selectgroup2',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_disability.graph.selectgroup3',
                         choices=group_choices)
    updateSelectizeInput(session,'reviewer_demographics_disability.graph.selectgroup4',
                         choices=group_choices)
    table_df
    
  })
  
  ### Reviewer Demographics - Disability Graph
  
  observe(updateTextInput(session,'reviewer_demographics_disability.graph.group1label',
                          value=paste(input$reviewer_demographics_disability.graph.selectgroup1,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_disability.graph.group2label',
                          value=paste(input$reviewer_demographics_disability.graph.selectgroup2,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_disability.graph.group3label',
                          value=paste(input$reviewer_demographics_disability.graph.selectgroup3,collapse=', ')))
  observe(updateTextInput(session,'reviewer_demographics_disability.graph.group4label',
                          value=paste(input$reviewer_demographics_disability.graph.selectgroup4,collapse=', ')))
  
  reviewer_demographics_disability_graph <- eventReactive(input$reviewer_demographics_disability.graph.plot.generate,{
    df <- demo.differences()
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    if (is.null(input$reviewer_demographics_disability.graph.selectgroup1) & 
        is.null(input$reviewer_demographics_disability.graph.selectgroup2) &
        is.null(input$reviewer_demographics_disability.graph.selectgroup3) &
        is.null(input$reviewer_demographics_disability.graph.selectgroup4)) {
      showNotification(paste("No groups selected. Using default groups"), type = "warning", duration = 5)   
      df <- df %>% mutate(graph_group=r.disability)
      df <- df %>% 
        mutate(graph_group=ifelse(graph_group%in%c('Missing','Prefer not to say','No response'),NA,graph_group))
    } else {
      df <- df %>% mutate(graph_group=case_when(
        r.disability%in%input$reviewer_demographics_disability.graph.selectgroup1 ~ input$reviewer_demographics_disability.graph.group1label,
        r.disability%in%input$reviewer_demographics_disability.graph.selectgroup2 ~ input$reviewer_demographics_disability.graph.group2label,
        r.disability%in%input$reviewer_demographics_disability.graph.selectgroup3 ~ input$reviewer_demographics_disability.graph.group3label,
        r.disability%in%input$reviewer_demographics_disability.graph.selectgroup4 ~ input$reviewer_demographics_disability.graph.group4label,
        .default = NA
      ))
    }
    
    reordered_levels <- df %>% filter(!is.na(original_value),Relationshiptouser!='self',!is.na(graph_group)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(reviewer_id)) %>% 
      arrange(n) %>% 
      mutate(graph_group=as_factor(graph_group)) %>% 
      pull(graph_group) %>% levels()
    
    graph.colours <- c(input$reviewer_demographics_disability.graph.plot.color1,input$reviewer_demographics_disability.graph.plot.color2,
                       input$reviewer_demographics_disability.graph.plot.color3,input$reviewer_demographics_disability.graph.plot.color4,
                       input$reviewer_demographics_disability.graph.plot.color5,input$reviewer_demographics_disability.graph.plot.color6,
                       input$reviewer_demographics_disability.graph.plot.color7,input$reviewer_demographics_disability.graph.plot.color8)
    color_mapping <- setNames(graph.colours[rep(seq_along(graph.colours), length.out = length(reordered_levels))], rev(reordered_levels))
    
    p <- df %>% filter(!is.na(original_value),Relationshiptouser!='self') %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(reviewer_id)) %>% 
      mutate(pct=paste0(round(n*100/sum(n),1),'%'),
             index=paste(ifelse(!is.na(graph_group),graph_group,'Missing'),pct,sep='\n')) %>% 
      ggplot(aes(area=n,fill=graph_group,label=index)) + 
      geom_treemap() + 
      geom_treemap_text(size=input$reviewer_demographics_disability.graph.plot.data_label,
                        grow=F,reflow=T,family='Work Sans') + 
      theme(legend.position = 'none',
            text=element_text(family='Work Sans'),
            title = element_text(color=graph.colors(),face='bold',
                                 size=input$reviewer_demographics_disability.graph.plot.title_size),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_)) +
      scale_fill_manual(values=color_mapping,na.value='grey65') +
      ggtitle(input$reviewer_demographics_disability.graph.plot.title)
    
    
  })
  
  output$reviewer_demographics_disability.graph.plot <- renderPlot({
    p <- reviewer_demographics_disability_graph()
    
    if (is.null(p)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    p
    
  })
  
  output$reviewer_demographics_disability.graph.download <- downloadHandler(
    filename = function(file) {
      "reviewer_disabilitydemo.png"
    },
    content = function(file) {
      ggsave(file,reviewer_demographics_disability_graph(),bg='transparent',
             width=input$reviewer_demographics_disability.graph.plot_width,
             height=input$reviewer_demographics_disability.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### Reviewer Demographics - Mental Illness
  output$reviewer_demo_mental_illness_tbl <- DT::renderDataTable({
    
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    # Get a dataframe containing the frequency distribution 
    table_df <- demo.tbler(df,'r.mental_illness') 
    
  })
  
  ### Reviewer Demographics - Mental Illness Graph
  
  
  reviewer_demographics_mental_illness_graph <- eventReactive(input$reviewer_demographics_mental_illness.graph.plot.generate,{
    df <- demo.differences()
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    df <- df %>% mutate(graph_group=r.mental_illness)
    df <- df %>% 
      mutate(graph_group=ifelse(graph_group%in%c('Missing','Prefer not to say','No response'),NA,graph_group))
    
    reordered_levels <- df %>% filter(!is.na(original_value),Relationshiptouser!='self',!is.na(graph_group)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(reviewer_id)) %>% 
      arrange(n) %>% 
      mutate(graph_group=as_factor(graph_group)) %>% 
      pull(graph_group) %>% levels()
    
    graph.colours <- c(input$reviewer_demographics_mental_illness.graph.plot.color1,input$reviewer_demographics_mental_illness.graph.plot.color2,
                       input$reviewer_demographics_mental_illness.graph.plot.color3,input$reviewer_demographics_mental_illness.graph.plot.color4,
                       input$reviewer_demographics_mental_illness.graph.plot.color5,input$reviewer_demographics_mental_illness.graph.plot.color6,
                       input$reviewer_demographics_mental_illness.graph.plot.color7,input$reviewer_demographics_mental_illness.graph.plot.color8)
    color_mapping <- setNames(graph.colours[rep(seq_along(graph.colours), length.out = length(reordered_levels))], rev(reordered_levels))
    
    p <- df %>% filter(!is.na(original_value),Relationshiptouser!='self') %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(reviewer_id)) %>% 
      mutate(pct=paste0(round(n*100/sum(n),1),'%'),
             index=paste(ifelse(!is.na(graph_group),graph_group,'Missing'),pct,sep='\n')) %>% 
      ggplot(aes(area=n,fill=graph_group,label=index)) + 
      geom_treemap() + 
      geom_treemap_text(size=input$reviewer_demographics_mental_illness.graph.plot.data_label,
                        grow=F,reflow=T,family='Work Sans') + 
      theme(legend.position = 'none',
            text=element_text(family='Work Sans'),
            title = element_text(color=graph.colors(),face='bold',
                                 size=input$reviewer_demographics_mental_illness.graph.plot.title_size),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_)) +
      scale_fill_manual(values=color_mapping,na.value='grey65') +
      ggtitle(input$reviewer_demographics_mental_illness.graph.plot.title)
    
    
  })
  
  output$reviewer_demographics_mental_illness.graph.plot <- renderPlot({
    p <- reviewer_demographics_mental_illness_graph()
    
    if (is.null(p)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    p
    
  })
  
  output$reviewer_demographics_mental_illness.graph.download <- downloadHandler(
    filename = function(file) {
      "reviewer_mental_illnessdemo.png"
    },
    content = function(file) {
      ggsave(file,reviewer_demographics_mental_illness_graph(),bg='transparent',
             width=input$reviewer_demographics_mental_illness.graph.plot_width,
             height=input$reviewer_demographics_mental_illness.graph.plot_height,dpi=600,device='png')
    }
  )
  
  
  
  ### Reviewer Demographics - Relationship to Employee Being Reviewed
  output$reviewer_demo_relationship_employee <- DT::renderDataTable({
    
    df <- demographics()$reviews
    
    if (is.null(df)) return(NULL)
    
    reviews.df <- df %>%
      filter(!is.na(original_value)) %>%
      mutate(Relationshiptouser = factor(Relationshiptouser, 
                                         levels = c("self", "other", "manager"), 
                                         labels = c("Self", "Other", "Manager"))) 
    
    # Get a dataframe containing the frequency distribution 
    table_df <- oneway_freq_dataframe(reviews.df, 
                                      "reviewer_id", "Relationshiptouser") 
    
    frequency_datatable(table_df) 
    
  })
  
  ### User Demographics - Gender Identity Table
  output$user_demo_gender_tbl <- DT::renderDataTable({
    
    df <- user_scores.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    # Get a dataframe containing the frequency distribution 
    table_df <- demo.tbler(df,'u.gender',user=T)
    group_choices <- table_df %>% filter(!Group%in%c('Missing','Prefer not to say','No response')) %>% pull(Group)
    
    updateSelectizeInput(session,'user_demographics_gender.graph.selectgroup1',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_gender.graph.selectgroup2',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_gender.graph.selectgroup3',
                         choices=group_choices)
    
    table_df
    
  })  
  
  ### User Demographics - Gender Identity Graph
  
  observe(updateTextInput(session,'user_demographics_gender.graph.group1label',
                          value=paste(input$user_demographics_gender.graph.selectgroup1,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_gender.graph.group2label',
                          value=paste(input$user_demographics_gender.graph.selectgroup2,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_gender.graph.group3label',
                          value=paste(input$user_demographics_gender.graph.selectgroup3,collapse=', ')))
  
  user_demographics_gender_graph <- eventReactive(input$user_demographics_gender.graph.plot.generate,{
    df <- user_scores.tbl()
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    if (is.null(input$user_demographics_gender.graph.selectgroup1) & 
        is.null(input$user_demographics_gender.graph.selectgroup2) &
        is.null(input$user_demographics_gender.graph.selectgroup3)) {
      showNotification(paste("No groups selected. Using default groups"), type = "warning", duration = 5)   
      df <- df %>% mutate(graph_group=u.gender)
      df <- df %>% 
        mutate(graph_group=ifelse(graph_group%in%c('Missing','Prefer not to say','No response'),NA,graph_group))
    } else {
      df <- df %>% mutate(graph_group=case_when(
        u.gender%in%input$user_demographics_gender.graph.selectgroup1 ~ input$user_demographics_gender.graph.group1label,
        u.gender%in%input$user_demographics_gender.graph.selectgroup2 ~ input$user_demographics_gender.graph.group2label,
        u.gender%in%input$user_demographics_gender.graph.selectgroup3 ~ input$user_demographics_gender.graph.group3label,
        .default = NA
      ))
    }
    
    reordered_levels <- df %>% filter(!is.na(mean),!is.na(graph_group)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(user_id)) %>% 
      arrange(n) %>% 
      mutate(graph_group=as_factor(graph_group)) %>% 
      pull(graph_group) %>% levels()
    
    graph.colours <- c(input$user_demographics_gender.graph.plot.color1,input$user_demographics_gender.graph.plot.color2,
                       input$user_demographics_gender.graph.plot.color3,input$user_demographics_gender.graph.plot.color4,
                       input$user_demographics_gender.graph.plot.color5,input$user_demographics_gender.graph.plot.color6,
                       input$user_demographics_gender.graph.plot.color7,input$user_demographics_gender.graph.plot.color8)
    color_mapping <- setNames(graph.colours[rep(seq_along(graph.colours), length.out = length(reordered_levels))], rev(reordered_levels))
    
    p <- df %>% filter(!is.na(mean)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(user_id)) %>% 
      mutate(pct=paste0(round(n*100/sum(n),1),'%'),
             index=paste(ifelse(!is.na(graph_group),graph_group,'Missing'),pct,sep='\n')) %>% 
      ggplot(aes(area=n,fill=graph_group,label=index)) + 
      geom_treemap() + 
      geom_treemap_text(size=input$user_demographics_gender.graph.plot.data_label,
                        grow=F,reflow=T,family='Work Sans') + 
      theme(legend.position = 'none',
            text=element_text(family='Work Sans'),
            title = element_text(color=graph.colors(),face='bold',
                                 size=input$user_demographics_gender.graph.plot.title_size),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_)) +
      scale_fill_manual(values=color_mapping,na.value='grey65') +
      ggtitle(input$user_demographics_gender.graph.plot.title)
    
    
  })
  
  output$user_demographics_gender.graph.plot <- renderPlot({
    p <- user_demographics_gender_graph()
    
    if (is.null(p)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    p
    
  })
  
  output$user_demographics_gender.graph.download <- downloadHandler(
    filename = function(file) {
      "user_genderdemo.png"
    },
    content = function(file) {
      ggsave(file,user_demographics_gender_graph(),bg='transparent',
             width=input$user_demographics_gender.graph.plot_width,
             height=input$user_demographics_gender.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### User Demographics - Age
  output$user_demo_age_tbl <- DT::renderDataTable({
    
    df <- user_scores.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    # Get a dataframe containing the frequency distribution 
    table_df <- demo.tbler(df,'u.age',user=T) 
    group_choices <- table_df %>% filter(!Group%in%c('Missing','Prefer not to say','No response')) %>% pull(Group)
    
    updateSelectizeInput(session,'user_demographics_age.graph.selectgroup1',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_age.graph.selectgroup2',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_age.graph.selectgroup3',
                         choices=group_choices)
    table_df
    
  })
  
  ### User Demographics - Age Graph
  
  observe(updateTextInput(session,'user_demographics_age.graph.group1label',
                          value=paste(input$user_demographics_age.graph.selectgroup1,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_age.graph.group2label',
                          value=paste(input$user_demographics_age.graph.selectgroup2,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_age.graph.group3label',
                          value=paste(input$user_demographics_age.graph.selectgroup3,collapse=', ')))
  
  user_demographics_age_graph <- eventReactive(input$user_demographics_age.graph.plot.generate,{
    df <- user_scores.tbl()
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    if (is.null(input$user_demographics_age.graph.selectgroup1) & 
        is.null(input$user_demographics_age.graph.selectgroup2) &
        is.null(input$user_demographics_age.graph.selectgroup3)) {
      showNotification(paste("No groups selected. Using default groups"), type = "warning", duration = 5)   
      df <- df %>% mutate(graph_group=u.age)
      df <- df %>% 
        mutate(graph_group=ifelse(graph_group%in%c('Missing','Prefer not to say','No response'),NA,graph_group))
    } else {
      df <- df %>% mutate(graph_group=case_when(
        u.age%in%input$user_demographics_age.graph.selectgroup1 ~ input$user_demographics_age.graph.group1label,
        u.age%in%input$user_demographics_age.graph.selectgroup2 ~ input$user_demographics_age.graph.group2label,
        u.age%in%input$user_demographics_age.graph.selectgroup3 ~ input$user_demographics_age.graph.group3label,
        .default = NA
      ))
    }
    
    reordered_levels <- df %>% filter(!is.na(mean),!is.na(graph_group)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(user_id)) %>% 
      arrange(n) %>% 
      mutate(graph_group=as_factor(graph_group)) %>% 
      pull(graph_group) %>% levels()
    
    graph.colours <- c(input$user_demographics_age.graph.plot.color1,input$user_demographics_age.graph.plot.color2,
                       input$user_demographics_age.graph.plot.color3,input$user_demographics_age.graph.plot.color4,
                       input$user_demographics_age.graph.plot.color5,input$user_demographics_age.graph.plot.color6,
                       input$user_demographics_age.graph.plot.color7,input$user_demographics_age.graph.plot.color8)
    color_mapping <- setNames(graph.colours[rep(seq_along(graph.colours), length.out = length(reordered_levels))], rev(reordered_levels))
    
    p <- df %>% filter(!is.na(mean)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(user_id)) %>% 
      mutate(pct=paste0(round(n*100/sum(n),1),'%'),
             index=paste(ifelse(!is.na(graph_group),graph_group,'Missing'),pct,sep='\n')) %>% 
      ggplot(aes(area=n,fill=graph_group,label=index)) + 
      geom_treemap() + 
      geom_treemap_text(size=input$user_demographics_age.graph.plot.data_label,
                        grow=F,reflow=T,family='Work Sans') + 
      theme(legend.position = 'none',
            text=element_text(family='Work Sans'),
            title = element_text(color=graph.colors(),face='bold',
                                 size=input$user_demographics_age.graph.plot.title_size),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_)) +
      scale_fill_manual(values=color_mapping,na.value='grey65') +
      ggtitle(input$user_demographics_age.graph.plot.title)
    
    
  })
  
  output$user_demographics_age.graph.plot <- renderPlot({
    p <- user_demographics_age_graph()
    
    if (is.null(p)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    p
    
  })
  
  output$user_demographics_age.graph.download <- downloadHandler(
    filename = function(file) {
      "user_agedemo.png"
    },
    content = function(file) {
      ggsave(file,user_demographics_age_graph(),bg='transparent',
             width=input$user_demographics_age.graph.plot_width,
             height=input$user_demographics_age.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### User Demographics - Sexuality
  output$user_demo_sexuality_tbl <- DT::renderDataTable({
    
    df <- user_scores.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    # Get a dataframe containing the frequency distribution 
    table_df <- demo.tbler(df,'u.sexuality',user=T) 
    group_choices <- table_df %>% filter(!Group%in%c('Missing','Prefer not to say','No response')) %>% pull(Group)
    
    updateSelectizeInput(session,'user_demographics_sexuality.graph.selectgroup1',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_sexuality.graph.selectgroup2',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_sexuality.graph.selectgroup3',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_sexuality.graph.selectgroup4',
                         choices=group_choices)
    table_df
    
  })  
  
  ### User Demographics - Sexuality Graph
  
  observe(updateTextInput(session,'user_demographics_sexuality.graph.group1label',
                          value=paste(input$user_demographics_sexuality.graph.selectgroup1,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_sexuality.graph.group2label',
                          value=paste(input$user_demographics_sexuality.graph.selectgroup2,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_sexuality.graph.group3label',
                          value=paste(input$user_demographics_sexuality.graph.selectgroup3,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_sexuality.graph.group4label',
                          value=paste(input$user_demographics_sexuality.graph.selectgroup4,collapse=', ')))
  
  user_demographics_sexuality_graph <- eventReactive(input$user_demographics_sexuality.graph.plot.generate,{
    df <- user_scores.tbl()
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    if (is.null(input$user_demographics_sexuality.graph.selectgroup1) & 
        is.null(input$user_demographics_sexuality.graph.selectgroup2) &
        is.null(input$user_demographics_sexuality.graph.selectgroup3) &
        is.null(input$user_demographics_sexuality.graph.selectgroup4)) {
      showNotification(paste("No groups selected. Using default groups"), type = "warning", duration = 5)   
      df <- df %>% mutate(graph_group=u.sexuality)
      df <- df %>% 
        mutate(graph_group=ifelse(graph_group%in%c('Missing','Prefer not to say','No response'),NA,graph_group))
    } else {
      df <- df %>% mutate(graph_group=case_when(
        u.sexuality%in%input$user_demographics_sexuality.graph.selectgroup1 ~ input$user_demographics_sexuality.graph.group1label,
        u.sexuality%in%input$user_demographics_sexuality.graph.selectgroup2 ~ input$user_demographics_sexuality.graph.group2label,
        u.sexuality%in%input$user_demographics_sexuality.graph.selectgroup3 ~ input$user_demographics_sexuality.graph.group3label,
        u.sexuality%in%input$user_demographics_sexuality.graph.selectgroup4 ~ input$user_demographics_sexuality.graph.group4label,
        .default = NA
      ))
    }
    
    reordered_levels <- df %>% filter(!is.na(mean),!is.na(graph_group)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(user_id)) %>% 
      arrange(n) %>% 
      mutate(graph_group=as_factor(graph_group)) %>% 
      pull(graph_group) %>% levels()
    
    graph.colours <- c(input$user_demographics_sexuality.graph.plot.color1,input$user_demographics_sexuality.graph.plot.color2,
                       input$user_demographics_sexuality.graph.plot.color3,input$user_demographics_sexuality.graph.plot.color4,
                       input$user_demographics_sexuality.graph.plot.color5,input$user_demographics_sexuality.graph.plot.color6,
                       input$user_demographics_sexuality.graph.plot.color7,input$user_demographics_sexuality.graph.plot.color8)
    color_mapping <- setNames(graph.colours[rep(seq_along(graph.colours), length.out = length(reordered_levels))], rev(reordered_levels))
    
    p <- df %>% filter(!is.na(mean)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(user_id)) %>% 
      mutate(pct=paste0(round(n*100/sum(n),1),'%'),
             index=paste(ifelse(!is.na(graph_group),graph_group,'Missing'),pct,sep='\n')) %>% 
      ggplot(aes(area=n,fill=graph_group,label=index)) + 
      geom_treemap() + 
      geom_treemap_text(size=input$user_demographics_sexuality.graph.plot.data_label,
                        grow=F,reflow=T,family='Work Sans') + 
      theme(legend.position = 'none',
            text=element_text(family='Work Sans'),
            title = element_text(color=graph.colors(),face='bold',
                                 size=input$user_demographics_sexuality.graph.plot.title_size),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_)) +
      scale_fill_manual(values=color_mapping,na.value='grey65') +
      ggtitle(input$user_demographics_sexuality.graph.plot.title)
    
    
  })
  
  output$user_demographics_sexuality.graph.plot <- renderPlot({
    p <- user_demographics_sexuality_graph()
    
    if (is.null(p)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    p
    
  })
  
  output$user_demographics_sexuality.graph.download <- downloadHandler(
    filename = function(file) {
      "user_sexualitydemo.png"
    },
    content = function(file) {
      ggsave(file,user_demographics_sexuality_graph(),bg='transparent',
             width=input$user_demographics_sexuality.graph.plot_width,
             height=input$user_demographics_sexuality.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### User Demographics - Ethnicity
  output$user_demo_ethnicity_tbl <- DT::renderDataTable({
    
    df <- user_scores.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    # Get a dataframe containing the frequency distribution 
    table_df <- demo.tbler(df,'u.ethnicity',user=T)
    
    group_choices <- table_df %>% filter(!Group%in%c('Missing','Prefer not to say','No response')) %>% pull(Group)
    
    updateSelectizeInput(session,'user_demographics_ethnicity.graph.selectgroup1',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_ethnicity.graph.selectgroup2',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_ethnicity.graph.selectgroup3',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_ethnicity.graph.selectgroup4',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_ethnicity.graph.selectgroup5',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_ethnicity.graph.selectgroup6',
                         choices=group_choices)
    table_df
    
  })
  
  ### User Demographics - Ethnicity Graph
  
  observe(updateTextInput(session,'user_demographics_ethnicity.graph.group1label',
                          value=paste(input$user_demographics_ethnicity.graph.selectgroup1,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_ethnicity.graph.group2label',
                          value=paste(input$user_demographics_ethnicity.graph.selectgroup2,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_ethnicity.graph.group3label',
                          value=paste(input$user_demographics_ethnicity.graph.selectgroup3,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_ethnicity.graph.group4label',
                          value=paste(input$user_demographics_ethnicity.graph.selectgroup4,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_ethnicity.graph.group5label',
                          value=paste(input$user_demographics_ethnicity.graph.selectgroup5,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_ethnicity.graph.group6label',
                          value=paste(input$user_demographics_ethnicity.graph.selectgroup6,collapse=', ')))
  
  user_demographics_ethnicity_graph <- eventReactive(input$user_demographics_ethnicity.graph.plot.generate,{
    df <- user_scores.tbl()
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    if (is.null(input$user_demographics_ethnicity.graph.selectgroup1) & 
        is.null(input$user_demographics_ethnicity.graph.selectgroup2) &
        is.null(input$user_demographics_ethnicity.graph.selectgroup3) &
        is.null(input$user_demographics_ethnicity.graph.selectgroup4) & 
        is.null(input$user_demographics_ethnicity.graph.selectgroup5) &
        is.null(input$user_demographics_ethnicity.graph.selectgroup6)) {
      showNotification(paste("No groups selected. Using default groups"), type = "warning", duration = 5)   
      df <- df %>% mutate(graph_group=u.ethnicity)
      df <- df %>% 
        mutate(graph_group=ifelse(graph_group%in%c('Missing','Prefer not to say','No response'),NA,graph_group))
    } else {
      df <- df %>% mutate(graph_group=case_when(
        u.ethnicity%in%input$user_demographics_ethnicity.graph.selectgroup1 ~ input$user_demographics_ethnicity.graph.group1label,
        u.ethnicity%in%input$user_demographics_ethnicity.graph.selectgroup2 ~ input$user_demographics_ethnicity.graph.group2label,
        u.ethnicity%in%input$user_demographics_ethnicity.graph.selectgroup3 ~ input$user_demographics_ethnicity.graph.group3label,
        u.ethnicity%in%input$user_demographics_ethnicity.graph.selectgroup4 ~ input$user_demographics_ethnicity.graph.group4label,
        u.ethnicity%in%input$user_demographics_ethnicity.graph.selectgroup5 ~ input$user_demographics_ethnicity.graph.group5label,
        u.ethnicity%in%input$user_demographics_ethnicity.graph.selectgroup6 ~ input$user_demographics_ethnicity.graph.group6label,
        .default = NA
      ))
    }
    
    reordered_levels <- df %>% filter(!is.na(mean),!is.na(graph_group)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(user_id)) %>% 
      arrange(n) %>% 
      mutate(graph_group=as_factor(graph_group)) %>% 
      pull(graph_group) %>% levels()
    
    graph.colours <- c(input$user_demographics_ethnicity.graph.plot.color1,input$user_demographics_ethnicity.graph.plot.color2,
                       input$user_demographics_ethnicity.graph.plot.color3,input$user_demographics_ethnicity.graph.plot.color4,
                       input$user_demographics_ethnicity.graph.plot.color5,input$user_demographics_ethnicity.graph.plot.color6,
                       input$user_demographics_ethnicity.graph.plot.color7,input$user_demographics_ethnicity.graph.plot.color8)
    color_mapping <- setNames(graph.colours[rep(seq_along(graph.colours), length.out = length(reordered_levels))], rev(reordered_levels))
    
    p <- df %>% filter(!is.na(mean)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(user_id)) %>% 
      mutate(pct=paste0(round(n*100/sum(n),1),'%'),
             index=paste(ifelse(!is.na(graph_group),graph_group,'Missing'),pct,sep='\n')) %>% 
      ggplot(aes(area=n,fill=graph_group,label=index)) + 
      geom_treemap() + 
      geom_treemap_text(size=input$user_demographics_ethnicity.graph.plot.data_label,
                        grow=F,reflow=T,family='Work Sans') + 
      theme(legend.position = 'none',
            text=element_text(family='Work Sans'),
            title = element_text(color=graph.colors(),face='bold',
                                 size=input$user_demographics_ethnicity.graph.plot.title_size),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_)) +
      scale_fill_manual(values=color_mapping,na.value='grey65') +
      ggtitle(input$user_demographics_ethnicity.graph.plot.title)
    
    
  })
  
  output$user_demographics_ethnicity.graph.plot <- renderPlot({
    p <- user_demographics_ethnicity_graph()
    
    if (is.null(p)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    p
    
  })
  
  output$user_demographics_ethnicity.graph.download <- downloadHandler(
    filename = function(file) {
      "user_ethnicitydemo.png"
    },
    content = function(file) {
      ggsave(file,user_demographics_ethnicity_graph(),bg='transparent',
             width=input$user_demographics_ethnicity.graph.plot_width,
             height=input$user_demographics_ethnicity.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### User Demographics - Ethnicity Collapsed
  output$user_demo_ethnicity_collapsed_tbl <- DT::renderDataTable({
    
    df <- user_scores.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    # Get a dataframe containing the frequency distribution 
    table_df <- demo.tbler(df,'u.ethnicity_collapsed',user=T) 
    
    group_choices <- table_df %>% filter(!Group%in%c('Missing','Prefer not to say','No response')) %>% pull(Group)
    
    updateSelectizeInput(session,'user_demographics_ethnicity_collapsed.graph.selectgroup1',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_ethnicity_collapsed.graph.selectgroup2',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_ethnicity_collapsed.graph.selectgroup3',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_ethnicity_collapsed.graph.selectgroup4',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_ethnicity_collapsed.graph.selectgroup5',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_ethnicity_collapsed.graph.selectgroup6',
                         choices=group_choices)
    table_df
    
  })
  
  ### User Demographics - Ethnicity Colllapsed Graph
  
  observe(updateTextInput(session,'user_demographics_ethnicity_collapsed.graph.group1label',
                          value=paste(input$user_demographics_ethnicity_collapsed.graph.selectgroup1,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_ethnicity_collapsed.graph.group2label',
                          value=paste(input$user_demographics_ethnicity_collapsed.graph.selectgroup2,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_ethnicity_collapsed.graph.group3label',
                          value=paste(input$user_demographics_ethnicity_collapsed.graph.selectgroup3,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_ethnicity_collapsed.graph.group4label',
                          value=paste(input$user_demographics_ethnicity_collapsed.graph.selectgroup4,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_ethnicity_collapsed.graph.group5label',
                          value=paste(input$user_demographics_ethnicity_collapsed.graph.selectgroup5,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_ethnicity_collapsed.graph.group6label',
                          value=paste(input$user_demographics_ethnicity_collapsed.graph.selectgroup6,collapse=', ')))
  
  user_demographics_ethnicity_collapsed_graph <- eventReactive(input$user_demographics_ethnicity_collapsed.graph.plot.generate,{
    df <- user_scores.tbl()
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    if (is.null(input$user_demographics_ethnicity_collapsed.graph.selectgroup1) & 
        is.null(input$user_demographics_ethnicity_collapsed.graph.selectgroup2) &
        is.null(input$user_demographics_ethnicity_collapsed.graph.selectgroup3) &
        is.null(input$user_demographics_ethnicity_collapsed.graph.selectgroup4) & 
        is.null(input$user_demographics_ethnicity_collapsed.graph.selectgroup5) &
        is.null(input$user_demographics_ethnicity_collapsed.graph.selectgroup6)) {
      showNotification(paste("No groups selected. Using default groups"), type = "warning", duration = 5)   
      df <- df %>% mutate(graph_group=u.ethnicity_collapsed)
      df <- df %>% 
        mutate(graph_group=ifelse(graph_group%in%c('Missing','Prefer not to say','No response'),NA,graph_group))
    } else {
      df <- df %>% mutate(graph_group=case_when(
        u.ethnicity_collapsed%in%input$user_demographics_ethnicity_collapsed.graph.selectgroup1 ~ input$user_demographics_ethnicity_collapsed.graph.group1label,
        u.ethnicity_collapsed%in%input$user_demographics_ethnicity_collapsed.graph.selectgroup2 ~ input$user_demographics_ethnicity_collapsed.graph.group2label,
        u.ethnicity_collapsed%in%input$user_demographics_ethnicity_collapsed.graph.selectgroup3 ~ input$user_demographics_ethnicity_collapsed.graph.group3label,
        u.ethnicity_collapsed%in%input$user_demographics_ethnicity_collapsed.graph.selectgroup4 ~ input$user_demographics_ethnicity_collapsed.graph.group4label,
        u.ethnicity_collapsed%in%input$user_demographics_ethnicity_collapsed.graph.selectgroup5 ~ input$user_demographics_ethnicity_collapsed.graph.group5label,
        u.ethnicity_collapsed%in%input$user_demographics_ethnicity_collapsed.graph.selectgroup6 ~ input$user_demographics_ethnicity_collapsed.graph.group6label,
        .default = NA
      ))
    }
    
    reordered_levels <- df %>% filter(!is.na(mean),!is.na(graph_group)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(user_id)) %>% 
      arrange(n) %>% 
      mutate(graph_group=as_factor(graph_group)) %>% 
      pull(graph_group) %>% levels()
    
    graph.colours <- c(input$user_demographics_ethnicity_collapsed.graph.plot.color1,input$user_demographics_ethnicity_collapsed.graph.plot.color2,
                       input$user_demographics_ethnicity_collapsed.graph.plot.color3,input$user_demographics_ethnicity_collapsed.graph.plot.color4,
                       input$user_demographics_ethnicity_collapsed.graph.plot.color5,input$user_demographics_ethnicity_collapsed.graph.plot.color6,
                       input$user_demographics_ethnicity_collapsed.graph.plot.color7,input$user_demographics_ethnicity_collapsed.graph.plot.color8)
    color_mapping <- setNames(graph.colours[rep(seq_along(graph.colours), length.out = length(reordered_levels))], rev(reordered_levels))
    
    p <- df %>% filter(!is.na(mean)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(user_id)) %>% 
      mutate(pct=paste0(round(n*100/sum(n),1),'%'),
             index=paste(ifelse(!is.na(graph_group),graph_group,'Missing'),pct,sep='\n')) %>% 
      ggplot(aes(area=n,fill=graph_group,label=index)) + 
      geom_treemap() + 
      geom_treemap_text(size=input$user_demographics_ethnicity_collapsed.graph.plot.data_label,
                        grow=F,reflow=T,family='Work Sans') + 
      theme(legend.position = 'none',
            text=element_text(family='Work Sans'),
            title = element_text(color=graph.colors(),face='bold',
                                 size=input$user_demographics_ethnicity_collapsed.graph.plot.title_size),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_)) +
      scale_fill_manual(values=color_mapping,na.value='grey65') +
      ggtitle(input$user_demographics_ethnicity_collapsed.graph.plot.title)
    
    
  })
  
  output$user_demographics_ethnicity_collapsed.graph.plot <- renderPlot({
    p <- user_demographics_ethnicity_collapsed_graph()
    
    if (is.null(p)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    p
    
  })
  
  output$user_demographics_ethnicity_collapsed.graph.download <- downloadHandler(
    filename = function(file) {
      "user_ethnicity_collapseddemo.png"
    },
    content = function(file) {
      ggsave(file,user_demographics_ethnicity_collapsed_graph(),bg='transparent',
             width=input$user_demographics_ethnicity_collapsed.graph.plot_width,
             height=input$user_demographics_ethnicity_collapsed.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### User Demographics - Disability
  output$user_demo_disability_tbl <- DT::renderDataTable({
    
    df <- user_scores.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    # Get a dataframe containing the frequency distribution 
    table_df <- demo.tbler(df,'u.disability',user=T) 
    
    group_choices <- table_df %>% filter(!Group%in%c('Missing','Prefer not to say','No response')) %>% pull(Group)
    
    updateSelectizeInput(session,'user_demographics_disability.graph.selectgroup1',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_disability.graph.selectgroup2',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_disability.graph.selectgroup3',
                         choices=group_choices)
    updateSelectizeInput(session,'user_demographics_disability.graph.selectgroup4',
                         choices=group_choices)
    table_df
    
  })
  
  ### User Demographics - Disability Graph
  
  observe(updateTextInput(session,'user_demographics_disability.graph.group1label',
                          value=paste(input$user_demographics_disability.graph.selectgroup1,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_disability.graph.group2label',
                          value=paste(input$user_demographics_disability.graph.selectgroup2,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_disability.graph.group3label',
                          value=paste(input$user_demographics_disability.graph.selectgroup3,collapse=', ')))
  observe(updateTextInput(session,'user_demographics_disability.graph.group4label',
                          value=paste(input$user_demographics_disability.graph.selectgroup4,collapse=', ')))
  
  user_demographics_disability_graph <- eventReactive(input$user_demographics_disability.graph.plot.generate,{
    df <- user_scores.tbl()
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    if (is.null(input$user_demographics_disability.graph.selectgroup1) & 
        is.null(input$user_demographics_disability.graph.selectgroup2) &
        is.null(input$user_demographics_disability.graph.selectgroup3) &
        is.null(input$user_demographics_disability.graph.selectgroup4)) {
      showNotification(paste("No groups selected. Using default groups"), type = "warning", duration = 5)   
      df <- df %>% mutate(graph_group=u.disability)
      df <- df %>% 
        mutate(graph_group=ifelse(graph_group%in%c('Missing','Prefer not to say','No response'),NA,graph_group))
    } else {
      df <- df %>% mutate(graph_group=case_when(
        u.disability%in%input$user_demographics_disability.graph.selectgroup1 ~ input$user_demographics_disability.graph.group1label,
        u.disability%in%input$user_demographics_disability.graph.selectgroup2 ~ input$user_demographics_disability.graph.group2label,
        u.disability%in%input$user_demographics_disability.graph.selectgroup3 ~ input$user_demographics_disability.graph.group3label,
        u.disability%in%input$user_demographics_disability.graph.selectgroup4 ~ input$user_demographics_disability.graph.group4label,
        .default = NA
      ))
    }
    
    reordered_levels <- df %>% filter(!is.na(mean),!is.na(graph_group)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(user_id)) %>% 
      arrange(n) %>% 
      mutate(graph_group=as_factor(graph_group)) %>% 
      pull(graph_group) %>% levels()
    
    graph.colours <- c(input$user_demographics_disability.graph.plot.color1,input$user_demographics_disability.graph.plot.color2,
                       input$user_demographics_disability.graph.plot.color3,input$user_demographics_disability.graph.plot.color4,
                       input$user_demographics_disability.graph.plot.color5,input$user_demographics_disability.graph.plot.color6,
                       input$user_demographics_disability.graph.plot.color7,input$user_demographics_disability.graph.plot.color8)
    color_mapping <- setNames(graph.colours[rep(seq_along(graph.colours), length.out = length(reordered_levels))], rev(reordered_levels))
    
    p <- df %>% filter(!is.na(mean)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(user_id)) %>% 
      mutate(pct=paste0(round(n*100/sum(n),1),'%'),
             index=paste(ifelse(!is.na(graph_group),graph_group,'Missing'),pct,sep='\n')) %>% 
      ggplot(aes(area=n,fill=graph_group,label=index)) + 
      geom_treemap() + 
      geom_treemap_text(size=input$user_demographics_disability.graph.plot.data_label,
                        grow=F,reflow=T,family='Work Sans') + 
      theme(legend.position = 'none',
            text=element_text(family='Work Sans'),
            title = element_text(color=graph.colors(),face='bold',
                                 size=input$user_demographics_disability.graph.plot.title_size),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_)) +
      scale_fill_manual(values=color_mapping,na.value='grey65') +
      ggtitle(input$user_demographics_disability.graph.plot.title)
    
    
  })
  
  output$user_demographics_disability.graph.plot <- renderPlot({
    p <- user_demographics_disability_graph()
    
    if (is.null(p)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    p
    
  })
  
  output$user_demographics_disability.graph.download <- downloadHandler(
    filename = function(file) {
      "user_disabilitydemo.png"
    },
    content = function(file) {
      ggsave(file,user_demographics_disability_graph(),bg='transparent',
             width=input$user_demographics_disability.graph.plot_width,
             height=input$user_demographics_disability.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### User Demographics - Mental Illness
  output$user_demo_mental_illness_tbl <- DT::renderDataTable({
    
    df <- user_scores.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    # Get a dataframe containing the frequency distribution 
    table_df <- demo.tbler(df,'u.mental_illness',user=T) 
    
  })
  
  ### User Demographics - Mental Illness Graph
  
  
  user_demographics_mental_illness_graph <- eventReactive(input$user_demographics_mental_illness.graph.plot.generate,{
    df <- user_scores.tbl()
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    df <- df %>% mutate(graph_group=u.mental_illness)
    df <- df %>% 
      mutate(graph_group=ifelse(graph_group%in%c('Missing','Prefer not to say','No response'),NA,graph_group))
    
    reordered_levels <- df %>% filter(!is.na(mean),!is.na(graph_group)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(user_id)) %>% 
      arrange(n) %>% 
      mutate(graph_group=as_factor(graph_group)) %>% 
      pull(graph_group) %>% levels()
    
    graph.colours <- c(input$user_demographics_mental_illness.graph.plot.color1,input$user_demographics_mental_illness.graph.plot.color2,
                       input$user_demographics_mental_illness.graph.plot.color3,input$user_demographics_mental_illness.graph.plot.color4,
                       input$user_demographics_mental_illness.graph.plot.color5,input$user_demographics_mental_illness.graph.plot.color6,
                       input$user_demographics_mental_illness.graph.plot.color7,input$user_demographics_mental_illness.graph.plot.color8)
    color_mapping <- setNames(graph.colours[rep(seq_along(graph.colours), length.out = length(reordered_levels))], rev(reordered_levels))
    
    p <- df %>% filter(!is.na(mean)) %>% 
      group_by(graph_group) %>% 
      summarise(n=n_distinct(user_id)) %>% 
      mutate(pct=paste0(round(n*100/sum(n),1),'%'),
             index=paste(ifelse(!is.na(graph_group),graph_group,'Missing'),pct,sep='\n')) %>% 
      ggplot(aes(area=n,fill=graph_group,label=index)) + 
      geom_treemap() + 
      geom_treemap_text(size=input$user_demographics_mental_illness.graph.plot.data_label,
                        grow=F,reflow=T,family='Work Sans') + 
      theme(legend.position = 'none',
            text=element_text(family='Work Sans'),
            title = element_text(color=graph.colors(),face='bold',
                                 size=input$user_demographics_mental_illness.graph.plot.title_size),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_)) +
      scale_fill_manual(values=color_mapping,na.value='grey65') +
      ggtitle(input$user_demographics_mental_illness.graph.plot.title)
    
    
  })
  
  output$user_demographics_mental_illness.graph.plot <- renderPlot({
    p <- user_demographics_mental_illness_graph()
    
    if (is.null(p)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    p
    
  })
  
  output$user_demographics_mental_illness.graph.download <- downloadHandler(
    filename = function(file) {
      "user_mental_illnessdemo.png"
    },
    content = function(file) {
      ggsave(file,user_demographics_mental_illness_graph(),bg='transparent',
             width=input$user_demographics_mental_illness.graph.plot_width,
             height=input$user_demographics_mental_illness.graph.plot_height,dpi=600,device='png')
    }
  )
  
  
  
  ## Demographic Differences V2
  # In this section, for each category, we calculate the statistical significance of differences
  # in the distribution using the kruskal_test(), effect sizes are calculated using kruskal_effsize().
  # If SS findings are found, the user can use post-hoc testing to identify which groups are experiencing
  # differences using the dunn_test(). We also allow the user to define custom grouping for post-hoc comparisons.
  # Custom group selections must be selected in order to create the plot as these are the arguments passed to the 
  # eventReactive() that generates theh plot.
  
  # Creates the dataframe on on open after population filtering to include the reviewer demographic columns
  # and allow comparison of their scores
  demo.differences <- reactive({
    demo.df <- cells.sqlOutput()
    if (nrow(demo.df) == 0) return(NULL)
    
    demo.df <- apply_filters_and_clean(demo.df, input$surveys, 
                                       input$user_ids, input$date)
    
    
    
    cols.to.change <- c('reviewer_gender','reviewer_sexuality',
                        'reviewer_ethnicity','reviewer_disability', 'reviewer_mental_illness')
    
    demo.df[,cols.to.change] <- lapply(demo.df[, cols.to.change], function(x) {
      x <- gsub('[\\"\\}\\{]', "", x)
      x <- as.character(ifelse(is.na(x), "Missing", x))
      x <- as.character(ifelse(x == "", "No response", x))
      x <- as.character(ifelse(grepl('Prefer not to say',x,fixed=T),'Prefer not to say', x))
    })
    
    
    colnames(demo.df) <- ifelse(grepl("id$",colnames(demo.df)),
                                colnames(demo.df),
                                sub("^reviewer_","r.",colnames(demo.df)))
    
    demo.df <- recode_ethnicity(demo.df,'r.ethnicity')
    
    
    demo.df <- demo.df %>% mutate(r.age=case_when(
      r.age == 'Prefer not to say' ~ 'Missing',
      .default=r.age
    ))
    
    
    
    demo.df <- demo.df %>% mutate(
      r.sexuality_col=case_when(
        r.sexuality %in% c('No response','Missing','Prefer not to say') ~ 'Missing',
        r.sexuality == 'Heterosexual or straight' ~ 'Heterosexual or straight',
        .default = 'Not heterosexual or straight'
      )
    )
    
    demo.df <- demo.df %>% mutate(
      r.disability_col=case_when(
        r.disability %in% c('No response','Missing','Prefer not to say') ~ 'Missing',
        r.disability == 'No disability' ~ 'No disability',
        .default = 'Has disability'
      )
    )
    
    
    
    # enable the Download All Reports button
    shinyjs::enable(id = 'downloadRaw')
    
    return(demo.df)
  })
  
  ## SS test column names
  ss_column_labels <- reactive({
    column_labels <- vector()
    if (input$demographicsGrouping=='Cell.name') {
      column_labels <- c("Cell", "p-value", "Reviews", "SS", "Unique Reviewers",
                         "Effect Size","Magnitude")
    } else {
      column_labels <- c("Behaviour", "p-value", "Reviews", "SS", "Unique Reviewers",
                         "Effect Size","Magnitude")
    }
    return(column_labels)
  })
  
  
  ## PH test column names
  ph_column_labels <- reactive({
    column_labels <- vector()
    if (input$demographicsGrouping=='Cell.name') {
      column_labels <- c("Cell", "p-value", "adj. p-value", "SS", "G1", "G2", 
                         "G1 Reviews", "G2 Reviews", "G1 Unique Reviewers", "G2 Unique Reviewers",
                         "G1 Mean", "G2 Mean")
    } else {
      column_labels <- c("Behaviour", "p-value", "adj. p-value", "SS", "G1", "G2", 
                         "G1 Reviews", "G2 Reviews", "G1 Unique Reviewers", "G2 Unique Reviewers",
                         "G1 Mean", "G2 Mean")
    }
    return(column_labels)
  })
  
  
  ### Differences by Reviewer Gender
  
  #### SS Test
  output$demo.differences_reviewer.gender.tbl_ss <- DT::renderDataTable({
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        !r.gender%in%c('Prefer not to say','No response','Missing'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),r.gender) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ss_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_test(original_value~r.gender) %>% 
      select(!!as.name(input$demographicsGrouping),p,n) %>% 
      mutate(sig=case_when(
        p<=0.05 & p>0.01 ~ '*',
        p<=0.01 & p>0.001 ~ '**',
        p<=0.001 ~ '***',
        .default=NA
      ),
      p=round(p,4))
    
    ss_test_n <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>%
      summarise(n_unique=n_distinct(reviewer_id))
    
    ss_test <- merge(ss_test,ss_test_n,by.x=input$demographicsGrouping)
    
    ss_size <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_effsize(original_value~r.gender) %>% 
      mutate(effsize=round(effsize,3)) %>% 
      select(!!as.name(input$demographicsGrouping),effsize,magnitude)
    
    ss_test <- merge(ss_test,ss_size,by.x=input$demographicsGrouping) %>% arrange(p)
    
    
    colnames(ss_test) <- ss_column_labels()
    
    
    return(ss_test)
    
  })
  
  #### PH Test
  output$demo.differences_reviewer.gender.tbl_ph <- DT::renderDataTable({
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        !r.gender%in%c('Prefer not to say','No response','Missing'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),r.gender) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~r.gender,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),r.gender) %>%
      summarise(n=n_distinct(reviewer_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(group=r.gender,
             n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Custom Test
  df.differences_reviewer.gender <- reactiveVal()
  output$demo.differences_reviewer.gender.tbl_custom <- DT::renderDataTable({
    req(!is.na(input$demo.differences_reviewer.gender.tbl_custom.select1) &
          !is.na(input$demo.differences_reviewer.gender.tbl_custom.select2))
    
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        !r.gender%in%c('Prefer not to say','No response','Missing'))
    
    df <- df %>% mutate(group=case_when(
      r.gender %in% input$demo.differences_reviewer.gender.tbl_custom.select1 ~ 'group1',
      r.gender %in% input$demo.differences_reviewer.gender.tbl_custom.select2 ~ 'group2',
      .default=NA
    ))
    
    df <- df %>% filter(!is.na(group))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),group) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    
    
    
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~group,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),group) %>%
      summarise(n=n_distinct(reviewer_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    graph_select <- ph_test %>% pull(!!as.name(input$demographicsGrouping))
    
    
    updateSelectizeInput(session,'demo.differences_reviewer.gender.graph.select',
                         choices=graph_select)
    
    df.differences_reviewer.gender(ph_test)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Report Graph
  
  graph_demo.differences_reviewer.gender <- eventReactive(input$demo.differences_reviewer.gender.graph.plot.generate,{
    req(!is.null(input$demo.differences_reviewer.gender.graph.select))
    req(!is.null(input$demo.differences_reviewer.gender.tbl_custom.select1))
    req(!is.null(input$demo.differences_reviewer.gender.tbl_custom.select2))
    
    
    df <- df.differences_reviewer.gender()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    group1_label=paste(input$demo.differences_reviewer.gender.tbl_custom.select1,collapse=', ')
    group2_label=paste(input$demo.differences_reviewer.gender.tbl_custom.select2,collapse=', ')
    
    
    df <- df %>% select(!!as.name(input$demographicsGrouping),group_1,group_2,mean_1,mean_2) %>% 
      pivot_longer(cols= !input$demographicsGrouping,names_to=c('.value','tib'),names_sep='_') %>% 
      mutate(mean=mean/10)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% input$demo.differences_reviewer.gender.graph.select)
    if (input$demographicsGrouping=='Question') {
      df <- df %>% mutate(Question=str_replace_all(Question,"\\([^()]*\\)",""))
    }
    
    df %>% 
      ggplot(aes(x=mean/10,y=reorder(!!as.name(input$demographicsGrouping),mean),
                 fill=group)) + 
      geom_col(position='identity',width=c(0.6,0.4)) + 
      geom_text_repel(aes(label=paste0(round(mean*10),'%')),family='Work Sans',
                      size=input$demo.differences_reviewer.gender.graph.plot.data_label,
                      color=input$demo.differences_reviewer.gender.graph.plot.data_label_color,
                      direction='x', nudge_y=0.5, segment.linetype=3) +
      theme_pubr() +
      theme(text=element_text(family='Work Sans',color=graph.colors()),
            title = element_text(face='bold',size=input$demo.differences_reviewer.gender.graph.plot.title_size,
                                 color=graph.colors()),
            axis.title = element_text(face='bold',size=input$demo.differences_reviewer.gender.graph.plot.axis_title,
                                      color=graph.colors()),
            plot.subtitle = element_text(face='italic',size=input$demo.differences_reviewer.gender.graph.plot.subtitle_size,
                                         color=graph.colors()),
            axis.text = element_text(color=graph.colors(),size=input$demo.differences_reviewer.gender.graph.plot.axis_label),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_),
            panel.background = element_rect(fill = "transparent",
                                            colour=NA_character_),
            #panel.grid.major.y = element_line(color='grey35'),
            axis.line = element_line(color='grey35'),
            legend.background = element_rect(fill='transparent',
                                             colour=NA_character_),
            legend.position='bottom') +
      xlab(input$demo.differences_reviewer.gender.graph.plot.x_title) +
      ylab(input$demo.differences_reviewer.gender.graph.plot.y_title) +
      labs(title=input$demo.differences_reviewer.gender.graph.plot.title,
           subtitle=input$demo.differences_reviewer.gender.graph.plot.subtitle,
           fill=input$demo.differences_reviewer.gender.graph.plot.fill_label) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
      scale_x_continuous(labels=scales::percent) +
      scale_fill_manual(labels=c(str_wrap(group1_label,50),str_wrap(group2_label,50)),
                        values = c(input$demo.differences_reviewer.gender.graph.plot.color1,
                                   input$demo.differences_reviewer.gender.graph.plot.color2)) 
  })
  
  
  output$demo.differences_reviewer.gender.graph.plot <- renderPlot({
    bg=ifelse(input$graphMode=='light','white','#1b2634')
    graph_demo.differences_reviewer.gender() + theme(
      plot.background = element_rect(fill=bg),
      panel.background = element_rect(fill=bg)
    )
    
  })
  
  
  output$demo.differences_reviewer.gender.graph.download <- downloadHandler(
    filename = function(file) {
      "reviewer_genderdiff.png"
    },
    content = function(file) {
      ggsave(file,graph_demo.differences_reviewer.gender(),bg='transparent',
             width=input$demo.differences_reviewer.gender.graph.plot_width,
             height=input$demo.differences_reviewer.gender.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### Differences by Reviewer Age
  
  #### SS Test
  output$demo.differences_reviewer.age.tbl_ss <- DT::renderDataTable({
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        r.age!='Missing',
                        !is.na(r.age))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),r.age) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ss_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_test(original_value~r.age) %>% 
      select(!!as.name(input$demographicsGrouping),p,n) %>% 
      mutate(sig=case_when(
        p<=0.05 & p>0.01 ~ '*',
        p<=0.01 & p>0.001 ~ '**',
        p<=0.001 ~ '***',
        .default=NA
      ),
      p=round(p,4))
    
    ss_test_n <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>%
      summarise(n_unique=n_distinct(reviewer_id))
    
    ss_test <- merge(ss_test,ss_test_n,by.x=input$demographicsGrouping)
    
    ss_size <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_effsize(original_value~r.age) %>% 
      mutate(effsize=round(effsize,3)) %>% 
      select(!!as.name(input$demographicsGrouping),effsize,magnitude)
    
    ss_test <- merge(ss_test,ss_size,by.x=input$demographicsGrouping) %>% arrange(p)
    
    colnames(ss_test) <- ss_column_labels()
    
    
    return(ss_test)
    
  })
  
  #### PH Test
  output$demo.differences_reviewer.age.tbl_ph <- DT::renderDataTable({
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        r.age!='Missing',
                        !is.na(r.age))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),r.age) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~r.age,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),r.age) %>%
      summarise(n=n_distinct(reviewer_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(group=r.age,
             n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Custom Test
  df.differences_reviewer.age <- reactiveVal()
  output$demo.differences_reviewer.age.tbl_custom <- DT::renderDataTable({
    req(!is.na(input$demo.differences_reviewer.age.tbl_custom.select1) &
          !is.na(input$demo.differences_reviewer.age.tbl_custom.select2))
    
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        r.age!='Missing',
                        !is.na(r.age))
    
    df <- df %>% mutate(group=case_when(
      r.age %in% input$demo.differences_reviewer.age.tbl_custom.select1 ~ 'group1',
      r.age %in% input$demo.differences_reviewer.age.tbl_custom.select2 ~ 'group2',
      .default=NA
    ))
    
    df <- df %>% filter(!is.na(group))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),group) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    
    
    
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~group,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),group) %>%
      summarise(n=n_distinct(reviewer_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    graph_select <- ph_test %>% pull(!!as.name(input$demographicsGrouping))
    
    
    updateSelectizeInput(session,'demo.differences_reviewer.age.graph.select',
                         choices=graph_select)
    
    df.differences_reviewer.age(ph_test)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Report Graph
  
  graph_demo.differences_reviewer.age <- eventReactive(input$demo.differences_reviewer.age.graph.plot.generate,{
    req(!is.null(input$demo.differences_reviewer.age.graph.select))
    req(!is.null(input$demo.differences_reviewer.age.tbl_custom.select1))
    req(!is.null(input$demo.differences_reviewer.age.tbl_custom.select2))
    
    
    df <- df.differences_reviewer.age()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    group1_label=paste(input$demo.differences_reviewer.age.tbl_custom.select1,collapse=', ')
    group2_label=paste(input$demo.differences_reviewer.age.tbl_custom.select2,collapse=', ')
    
    
    df <- df %>% select(!!as.name(input$demographicsGrouping),group_1,group_2,mean_1,mean_2) %>% 
      pivot_longer(cols= !input$demographicsGrouping,names_to=c('.value','tib'),names_sep='_') %>% 
      mutate(mean=mean/10)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% input$demo.differences_reviewer.age.graph.select)
    if (input$demographicsGrouping=='Question') {
      df <- df %>% mutate(Question=str_replace_all(Question,"\\([^()]*\\)",""))
    }
    
    df %>% 
      ggplot(aes(x=mean/10,y=reorder(!!as.name(input$demographicsGrouping),mean),
                 fill=group)) + 
      geom_col(position='identity',width=c(0.6,0.4)) + 
      geom_text_repel(aes(label=paste0(round(mean*10),'%')),family='Work Sans',
                      size=input$demo.differences_reviewer.age.graph.plot.data_label,
                      color=input$demo.differences_reviewer.age.graph.plot.data_label_color,
                      direction='x', nudge_y=0.5, segment.linetype=3) +
      theme_pubr() +
      theme(text=element_text(family='Work Sans',color=graph.colors()),
            title = element_text(face='bold',size=input$demo.differences_reviewer.age.graph.plot.title_size,
                                 color=graph.colors()),
            axis.title = element_text(face='bold',size=input$demo.differences_reviewer.age.graph.plot.axis_title,
                                      color=graph.colors()),
            plot.subtitle = element_text(face='italic',size=input$demo.differences_reviewer.age.graph.plot.subtitle_size,
                                         color=graph.colors()),
            axis.text = element_text(color=graph.colors(),size=input$demo.differences_reviewer.age.graph.plot.axis_label),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_),
            panel.background = element_rect(fill = "transparent",
                                            colour=NA_character_),
            #panel.grid.major.y = element_line(color='grey35'),
            axis.line = element_line(color='grey35'),
            legend.background = element_rect(fill='transparent',
                                             colour=NA_character_),
            legend.position='bottom') +
      xlab(input$demo.differences_reviewer.age.graph.plot.x_title) +
      ylab(input$demo.differences_reviewer.age.graph.plot.y_title) +
      labs(title=input$demo.differences_reviewer.age.graph.plot.title,
           subtitle=input$demo.differences_reviewer.age.graph.plot.subtitle,
           fill=input$demo.differences_reviewer.age.graph.plot.fill_label) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
      scale_x_continuous(labels=scales::percent) +
      scale_fill_manual(labels=c(str_wrap(group1_label,50),str_wrap(group2_label,50)),
                        values = c(input$demo.differences_reviewer.age.graph.plot.color1,
                                   input$demo.differences_reviewer.age.graph.plot.color2)) 
  })
  
  
  output$demo.differences_reviewer.age.graph.plot <- renderPlot({
    bg=ifelse(input$graphMode=='light','white','#1b2634')
    graph_demo.differences_reviewer.age() + theme(
      plot.background = element_rect(fill=bg),
      panel.background = element_rect(fill=bg)
    )
    
  })
  
  
  output$demo.differences_reviewer.age.graph.download <- downloadHandler(
    filename = function(file) {
      "reviewer_agediff.png"
    },
    content = function(file) {
      ggsave(file,graph_demo.differences_reviewer.age(),bg='transparent',
             width=input$demo.differences_reviewer.age.graph.plot_width,
             height=input$demo.differences_reviewer.age.graph.plot_height,dpi=600,device='png')
    }
  )
  
  
  
  
  ### Differences by Reviewer Sexual Identity
  
  #### SS Test
  output$demo.differences_reviewer.sexuality.tbl_ss <- DT::renderDataTable({
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        !r.sexuality %in% c('No response','Missing','Prefer not to say'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),r.sexuality) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ss_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_test(original_value~r.sexuality) %>% 
      select(!!as.name(input$demographicsGrouping),p,n) %>% 
      mutate(sig=case_when(
        p<=0.05 & p>0.01 ~ '*',
        p<=0.01 & p>0.001 ~ '**',
        p<=0.001 ~ '***',
        .default=NA
      ),
      p=round(p,4))
    
    ss_test_n <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>%
      summarise(n_unique=n_distinct(reviewer_id))
    
    ss_test <- merge(ss_test,ss_test_n,by.x=input$demographicsGrouping)
    
    ss_size <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_effsize(original_value~r.sexuality) %>% 
      mutate(effsize=round(effsize,3)) %>% 
      select(!!as.name(input$demographicsGrouping),effsize,magnitude)
    
    ss_test <- merge(ss_test,ss_size,by.x=input$demographicsGrouping) %>% arrange(p)
    
    colnames(ss_test) <- ss_column_labels()
    
    
    return(ss_test)
    
  })
  
  #### PH Test
  output$demo.differences_reviewer.sexuality.tbl_ph <- DT::renderDataTable({
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        !r.sexuality %in% c('No response','Missing','Prefer not to say'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),r.sexuality) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~r.sexuality,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),r.sexuality) %>%
      summarise(n=n_distinct(reviewer_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(group=r.sexuality,
             n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Custom Test
  df.differences_reviewer.sexuality <- reactiveVal()
  output$demo.differences_reviewer.sexuality.tbl_custom <- DT::renderDataTable({
    req(!is.na(input$demo.differences_reviewer.sexuality.tbl_custom.select1) &
          !is.na(input$demo.differences_reviewer.sexuality.tbl_custom.select2))
    
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        !r.sexuality %in% c('No response','Missing','Prefer not to say'))
    
    df <- df %>% mutate(group=case_when(
      r.sexuality %in% input$demo.differences_reviewer.sexuality.tbl_custom.select1 ~ 'group1',
      r.sexuality %in% input$demo.differences_reviewer.sexuality.tbl_custom.select2 ~ 'group2',
      .default=NA
    ))
    
    df <- df %>% filter(!is.na(group))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),group) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    
    
    
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~group,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),group) %>%
      summarise(n=n_distinct(reviewer_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    graph_select <- ph_test %>% pull(!!as.name(input$demographicsGrouping))
    
    
    updateSelectizeInput(session,'demo.differences_reviewer.sexuality.graph.select',
                         choices=graph_select)
    
    df.differences_reviewer.sexuality(ph_test)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Report Graph
  
  graph_demo.differences_reviewer.sexuality <- eventReactive(input$demo.differences_reviewer.sexuality.graph.plot.generate,{
    req(!is.null(input$demo.differences_reviewer.sexuality.graph.select))
    req(!is.null(input$demo.differences_reviewer.sexuality.tbl_custom.select1))
    req(!is.null(input$demo.differences_reviewer.sexuality.tbl_custom.select2))
    
    
    df <- df.differences_reviewer.sexuality()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    group1_label=paste(input$demo.differences_reviewer.sexuality.tbl_custom.select1,collapse=', ')
    group2_label=paste(input$demo.differences_reviewer.sexuality.tbl_custom.select2,collapse=', ')
    
    
    df <- df %>% select(!!as.name(input$demographicsGrouping),group_1,group_2,mean_1,mean_2) %>% 
      pivot_longer(cols= !input$demographicsGrouping,names_to=c('.value','tib'),names_sep='_') %>% 
      mutate(mean=mean/10)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% input$demo.differences_reviewer.sexuality.graph.select)
    if (input$demographicsGrouping=='Question') {
      df <- df %>% mutate(Question=str_replace_all(Question,"\\([^()]*\\)",""))
    }
    
    df %>% 
      ggplot(aes(x=mean/10,y=reorder(!!as.name(input$demographicsGrouping),mean),
                 fill=group)) + 
      geom_col(position='identity',width=c(0.6,0.4)) + 
      geom_text_repel(aes(label=paste0(round(mean*10),'%')),
                      family='Work Sans',
                      size=input$demo.differences_reviewer.sexuality.graph.plot.data_label,
                      color=input$demo.differences_reviewer.sexuality.graph.plot.data_label_color,
                      direction='x', nudge_y=0.5, segment.linetype=3) +
      theme_pubr() +
      theme(text=element_text(family='Work Sans',color=graph.colors()),
            title = element_text(face='bold',size=input$demo.differences_reviewer.sexuality.graph.plot.title_size,
                                 color=graph.colors()),
            axis.title = element_text(face='bold',size=input$demo.differences_reviewer.sexuality.graph.plot.axis_title,
                                      color=graph.colors()),
            plot.subtitle = element_text(face='italic',size=input$demo.differences_reviewer.sexuality.graph.plot.subtitle_size,
                                         color=graph.colors()),
            axis.text = element_text(color=graph.colors(),size=input$demo.differences_reviewer.sexuality.graph.plot.axis_label),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_),
            panel.background = element_rect(fill = "transparent",
                                            colour=NA_character_),
            #panel.grid.major.y = element_line(color='grey35'),
            axis.line = element_line(color='grey35'),
            legend.background = element_rect(fill='transparent',
                                             colour=NA_character_),
            legend.position='bottom') +
      xlab(input$demo.differences_reviewer.sexuality.graph.plot.x_title) +
      ylab(input$demo.differences_reviewer.sexuality.graph.plot.y_title) +
      labs(title=input$demo.differences_reviewer.sexuality.graph.plot.title,
           subtitle=input$demo.differences_reviewer.sexuality.graph.plot.subtitle,
           fill=input$demo.differences_reviewer.sexuality.graph.plot.fill_label) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
      scale_x_continuous(labels=scales::percent) +
      scale_fill_manual(labels=c(str_wrap(group1_label,50),str_wrap(group2_label,50)),
                        values = c(input$demo.differences_reviewer.sexuality.graph.plot.color1,
                                   input$demo.differences_reviewer.sexuality.graph.plot.color2))
  })
  
  
  output$demo.differences_reviewer.sexuality.graph.plot <- renderPlot({
    bg=ifelse(input$graphMode=='light','white','#1b2634')
    graph_demo.differences_reviewer.sexuality() + theme(
      plot.background = element_rect(fill=bg),
      panel.background = element_rect(fill=bg)
    )
    
  })
  
  
  output$demo.differences_reviewer.sexuality.graph.download <- downloadHandler(
    filename = function(file) {
      "reviewer_sexualitydiff.png"
    },
    content = function(file) {
      ggsave(file,graph_demo.differences_reviewer.sexuality(),bg='transparent',
             width=input$demo.differences_reviewer.sexuality.graph.plot_width,
             height=input$demo.differences_reviewer.sexuality.graph.plot_height,dpi=600,device='png')
    }
  )
  
  
  ### Differences by Reviewer Sexual Identity (Collapsed)
  
  #### SS Test
  output$demo.differences_reviewer.sexuality_col.tbl_ss <- DT::renderDataTable({
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        !r.sexuality_col %in% c('No response','Missing','Prefer not to say'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),r.sexuality_col) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ss_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_test(original_value~r.sexuality_col) %>% 
      select(!!as.name(input$demographicsGrouping),p,n) %>% 
      mutate(sig=case_when(
        p<=0.05 & p>0.01 ~ '*',
        p<=0.01 & p>0.001 ~ '**',
        p<=0.001 ~ '***',
        .default=NA
      ),
      p=round(p,4))
    
    ss_test_n <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>%
      summarise(n_unique=n_distinct(reviewer_id))
    
    ss_test <- merge(ss_test,ss_test_n,by.x=input$demographicsGrouping)
    
    ss_size <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_effsize(original_value~r.sexuality_col) %>% 
      mutate(effsize=round(effsize,3)) %>% 
      select(!!as.name(input$demographicsGrouping),effsize,magnitude)
    
    ss_test <- merge(ss_test,ss_size,by.x=input$demographicsGrouping) %>% arrange(p)
    
    colnames(ss_test) <- ss_column_labels()
    
    
    return(ss_test)
    
  })
  
  #### PH Test
  output$demo.differences_reviewer.sexuality_col.tbl_ph <- DT::renderDataTable({
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        !r.sexuality_col %in% c('No response','Missing','Prefer not to say'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),r.sexuality_col) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~r.sexuality_col,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),r.sexuality_col) %>%
      summarise(n=n_distinct(reviewer_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(group=r.sexuality_col,
             n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  
  ### Differences by Reviewer Ethnicity (Collapsed)
  
  #### SS Test
  output$demo.differences_reviewer.ethnicity_col.tbl_ss <- DT::renderDataTable({
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        !r.ethnicity_collapsed %in% c('No response','Missing','Prefer not to say',
                                                      'No match'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),r.ethnicity_collapsed) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ss_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_test(original_value~r.ethnicity_collapsed) %>% 
      select(!!as.name(input$demographicsGrouping),p,n) %>% 
      mutate(sig=case_when(
        p<=0.05 & p>0.01 ~ '*',
        p<=0.01 & p>0.001 ~ '**',
        p<=0.001 ~ '***',
        .default=NA
      ),
      p=round(p,4))
    
    ss_test_n <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>%
      summarise(n_unique=n_distinct(reviewer_id))
    
    ss_test <- merge(ss_test,ss_test_n,by.x=input$demographicsGrouping)
    
    ss_size <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_effsize(original_value~r.ethnicity_collapsed) %>% 
      mutate(effsize=round(effsize,3)) %>% 
      select(!!as.name(input$demographicsGrouping),effsize,magnitude)
    
    ss_test <- merge(ss_test,ss_size,by.x=input$demographicsGrouping) %>% arrange(p)
    
    colnames(ss_test) <- ss_column_labels()
    
    
    return(ss_test)
    
  })
  
  #### PH Test
  output$demo.differences_reviewer.ethnicity_col.tbl_ph <- DT::renderDataTable({
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        !r.ethnicity_collapsed %in% c('No response','Missing','Prefer not to say',
                                                      'No match'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),r.ethnicity_collapsed) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~r.ethnicity_collapsed,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),r.ethnicity_collapsed) %>%
      summarise(n=n_distinct(reviewer_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(group=r.ethnicity_collapsed,
             n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  
  #### Custom Test
  df.differences_reviewer.ethnicity_col <- reactiveVal()
  output$demo.differences_reviewer.ethnicity_col.tbl_custom <- DT::renderDataTable({
    req(!is.na(input$demo.differences_reviewer.ethnicity_col.tbl_custom.select1) &
          !is.na(input$demo.differences_reviewer.ethnicity_col.tbl_custom.select2))
    
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        !r.ethnicity_collapsed %in% c('No response','Missing','Prefer not to say',
                                                      'No match'))
    
    df <- df %>% mutate(group=case_when(
      r.ethnicity_collapsed %in% input$demo.differences_reviewer.ethnicity_col.tbl_custom.select1 ~ 'group1',
      r.ethnicity_collapsed %in% input$demo.differences_reviewer.ethnicity_col.tbl_custom.select2 ~ 'group2',
      .default=NA
    ))
    
    df <- df %>% filter(!is.na(group))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),group) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    
    
    
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~group,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),group) %>%
      summarise(n=n_distinct(reviewer_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    graph_select <- ph_test %>% pull(!!as.name(input$demographicsGrouping))
    
    
    updateSelectizeInput(session,'demo.differences_reviewer.ethnicity_col.graph.select',
                         choices=graph_select)
    
    df.differences_reviewer.ethnicity_col(ph_test)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Report Graph
  
  graph_demo.differences_reviewer.ethnicity_col <- eventReactive(input$demo.differences_reviewer.ethnicity_col.graph.plot.generate,{
    req(!is.null(input$demo.differences_reviewer.ethnicity_col.graph.select))
    req(!is.null(input$demo.differences_reviewer.ethnicity_col.tbl_custom.select1))
    req(!is.null(input$demo.differences_reviewer.ethnicity_col.tbl_custom.select2))
    
    
    df <- df.differences_reviewer.ethnicity_col()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    group1_label=paste(input$demo.differences_reviewer.ethnicity_col.tbl_custom.select1,collapse=', ')
    group2_label=paste(input$demo.differences_reviewer.ethnicity_col.tbl_custom.select2,collapse=', ')
    
    
    df <- df %>% select(!!as.name(input$demographicsGrouping),group_1,group_2,mean_1,mean_2) %>% 
      pivot_longer(cols= !input$demographicsGrouping,names_to=c('.value','tib'),names_sep='_') %>% 
      mutate(mean=mean/10)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% input$demo.differences_reviewer.ethnicity_col.graph.select)
    if (input$demographicsGrouping=='Question') {
      df <- df %>% mutate(Question=str_replace_all(Question,"\\([^()]*\\)",""))
    }
    
    df %>% 
      ggplot(aes(x=mean/10,y=reorder(!!as.name(input$demographicsGrouping),mean),
                 fill=group)) + 
      geom_col(position='identity',width=c(0.6,0.4)) + 
      geom_text_repel(aes(label=paste0(round(mean*10),'%')),
                      family='Work Sans',
                      size=input$demo.differences_reviewer.ethnicity_col.graph.plot.data_label,
                      color=input$demo.differences_reviewer.ethnicity_col.graph.plot.data_label_color,
                      direction='x', nudge_y=0.5, segment.linetype=3) +
      theme_pubr() +
      theme(text=element_text(family='Work Sans',color=graph.colors()),
            title = element_text(face='bold',size=input$demo.differences_reviewer.ethnicity_col.graph.plot.title_size,
                                 color=graph.colors()),
            axis.title = element_text(face='bold',size=input$demo.differences_reviewer.ethnicity_col.graph.plot.axis_title,
                                      color=graph.colors()),
            plot.subtitle = element_text(face='italic',size=input$demo.differences_reviewer.ethnicity_col.graph.plot.subtitle_size,
                                         color=graph.colors()),
            axis.text = element_text(color=graph.colors(),size=input$demo.differences_reviewer.ethnicity_col.graph.plot.axis_label),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_),
            panel.background = element_rect(fill = "transparent",
                                            colour=NA_character_),
            #panel.grid.major.y = element_line(color='grey35'),
            axis.line = element_line(color='grey35'),
            legend.background = element_rect(fill='transparent',
                                             colour=NA_character_),
            legend.position='bottom') +
      xlab(input$demo.differences_reviewer.ethnicity_col.graph.plot.x_title) +
      ylab(input$demo.differences_reviewer.ethnicity_col.graph.plot.y_title) +
      labs(title=input$demo.differences_reviewer.ethnicity_col.graph.plot.title,
           subtitle=input$demo.differences_reviewer.ethnicity_col.graph.plot.subtitle,
           fill=input$demo.differences_reviewer.ethnicity_col.graph.plot.fill_label) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
      scale_x_continuous(labels=scales::percent) +
      scale_fill_manual(labels=c(str_wrap(group1_label,50),str_wrap(group2_label,50)),
                        values = c(input$demo.differences_reviewer.ethnicity_col.graph.plot.color1,
                                   input$demo.differences_reviewer.ethnicity_col.graph.plot.color2))
  })
  
  
  output$demo.differences_reviewer.ethnicity_col.graph.plot <- renderPlot({
    bg=ifelse(input$graphMode=='light','white','#1b2634')
    graph_demo.differences_reviewer.ethnicity_col() + theme(
      plot.background = element_rect(fill=bg),
      panel.background = element_rect(fill=bg)
    )
    
  })
  
  
  output$demo.differences_reviewer.ethnicity_col.graph.download <- downloadHandler(
    filename = function(file) {
      "reviewer_ethnicity_coldiff.png"
    },
    content = function(file) {
      ggsave(file,graph_demo.differences_reviewer.ethnicity_col(),bg='transparent',
             width=input$demo.differences_reviewer.ethnicity_col.graph.plot_width,
             height=input$demo.differences_reviewer.ethnicity_col.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### Differences by Disability
  
  #### SS Test
  output$demo.differences_reviewer.disability.tbl_ss <- DT::renderDataTable({
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        !r.disability %in% c('No response','Missing','Prefer not to say'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),r.disability) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ss_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_test(original_value~r.disability) %>% 
      select(!!as.name(input$demographicsGrouping),p,n) %>% 
      mutate(sig=case_when(
        p<=0.05 & p>0.01 ~ '*',
        p<=0.01 & p>0.001 ~ '**',
        p<=0.001 ~ '***',
        .default=NA
      ),
      p=round(p,4))
    
    ss_test_n <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>%
      summarise(n_unique=n_distinct(reviewer_id))
    
    ss_test <- merge(ss_test,ss_test_n,by.x=input$demographicsGrouping)
    
    ss_size <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_effsize(original_value~r.disability) %>% 
      mutate(effsize=round(effsize,3)) %>% 
      select(!!as.name(input$demographicsGrouping),effsize,magnitude)
    
    ss_test <- merge(ss_test,ss_size,by.x=input$demographicsGrouping) %>% arrange(p)
    
    colnames(ss_test) <- ss_column_labels()
    
    
    return(ss_test)
    
  })
  
  #### PH Test
  output$demo.differences_reviewer.disability.tbl_ph <- DT::renderDataTable({
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        !r.disability %in% c('No response','Missing','Prefer not to say'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),r.disability) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~r.disability,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),r.disability) %>%
      summarise(n=n_distinct(reviewer_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(group=r.disability,
             n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Custom Test
  df.differences_reviewer.disability <- reactiveVal()
  output$demo.differences_reviewer.disability.tbl_custom <- DT::renderDataTable({
    req(!is.na(input$demo.differences_reviewer.disability.tbl_custom.select1) &
          !is.na(input$demo.differences_reviewer.disability.tbl_custom.select2))
    
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        !r.disability %in% c('No response','Missing','Prefer not to say'))
    
    df <- df %>% mutate(group=case_when(
      r.disability %in% input$demo.differences_reviewer.disability.tbl_custom.select1 ~ 'group1',
      r.disability %in% input$demo.differences_reviewer.disability.tbl_custom.select2 ~ 'group2',
      .default=NA
    ))
    
    df <- df %>% filter(!is.na(group))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),group) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    
    
    
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~group,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),group) %>%
      summarise(n=n_distinct(reviewer_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    graph_select <- ph_test %>% pull(!!as.name(input$demographicsGrouping))
    
    
    updateSelectizeInput(session,'demo.differences_reviewer.disability.graph.select',
                         choices=graph_select)
    
    df.differences_reviewer.disability(ph_test)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Report Graphs
  graph_demo.differences_reviewer.disability <- eventReactive(input$demo.differences_reviewer.disability.graph.plot.generate,{
    req(!is.null(input$demo.differences_reviewer.disability.graph.select))
    req(!is.null(input$demo.differences_reviewer.disability.tbl_custom.select1))
    req(!is.null(input$demo.differences_reviewer.disability.tbl_custom.select2))
    
    
    df <- df.differences_reviewer.disability()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    group1_label=paste(input$demo.differences_reviewer.disability.tbl_custom.select1,collapse=', ')
    group2_label=paste(input$demo.differences_reviewer.disability.tbl_custom.select2,collapse=', ')
    
    
    df <- df %>% select(!!as.name(input$demographicsGrouping),group_1,group_2,mean_1,mean_2) %>% 
      pivot_longer(cols= !input$demographicsGrouping,names_to=c('.value','tib'),names_sep='_') %>% 
      mutate(mean=mean/10)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% input$demo.differences_reviewer.disability.graph.select)
    if (input$demographicsGrouping=='Question') {
      df <- df %>% mutate(Question=str_replace_all(Question,"\\([^()]*\\)",""))
    }
    
    df %>% 
      ggplot(aes(x=mean/10,y=reorder(!!as.name(input$demographicsGrouping),mean),
                 fill=group)) + 
      geom_col(position='identity',width=c(0.6,0.4)) + 
      geom_text_repel(aes(label=paste0(round(mean*10),'%')),
                      family='Work Sans',
                      size=input$demo.differences_reviewer.disability.graph.plot.data_label,
                      color=input$demo.differences_reviewer.disability.graph.plot.data_label_color,
                      direction='x', nudge_y=0.5, segment.linetype=3) +
      theme_pubr() +
      theme(text=element_text(family='Work Sans',color=graph.colors()),
            title = element_text(face='bold',size=input$demo.differences_reviewer.disability.graph.plot.title_size,
                                 color=graph.colors()),
            axis.title = element_text(face='bold',size=input$demo.differences_reviewer.disability.graph.plot.axis_title,
                                      color=graph.colors()),
            plot.subtitle = element_text(face='italic',size=input$demo.differences_reviewer.disability.graph.plot.subtitle_size,
                                         color=graph.colors()),
            axis.text = element_text(color=graph.colors(),size=input$demo.differences_reviewer.disability.graph.plot.axis_label),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_),
            panel.background = element_rect(fill = "transparent",
                                            colour=NA_character_),
            #panel.grid.major.y = element_line(color='grey35'),
            axis.line = element_line(color='grey35'),
            legend.background = element_rect(fill='transparent',
                                             colour=NA_character_),
            legend.position='bottom') +
      xlab(input$demo.differences_reviewer.disability.graph.plot.x_title) +
      ylab(input$demo.differences_reviewer.disability.graph.plot.y_title) +
      labs(title=input$demo.differences_reviewer.disability.graph.plot.title,
           subtitle=input$demo.differences_reviewer.disability.graph.plot.subtitle,
           fill=input$demo.differences_reviewer.disability.graph.plot.fill_label) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
      scale_x_continuous(labels=scales::percent) +
      scale_fill_manual(labels=c(str_wrap(group1_label,50),str_wrap(group2_label,50)),
                        values = c(input$demo.differences_reviewer.disability.graph.plot.color1,
                                   input$demo.differences_reviewer.disability.graph.plot.color2))
  })
  
  
  output$demo.differences_reviewer.disability.graph.plot <- renderPlot({
    bg=ifelse(input$graphMode=='light','white','#1b2634')
    graph_demo.differences_reviewer.disability() + theme(
      plot.background = element_rect(fill=bg),
      panel.background = element_rect(fill=bg)
    )
    
  })
  
  
  output$demo.differences_reviewer.disability.graph.download <- downloadHandler(
    filename = function(file) {
      "reviewer_disabilitydiff.png"
    },
    content = function(file) {
      ggsave(file,graph_demo.differences_reviewer.disability(),bg='transparent',
             width=input$demo.differences_reviewer.disability.graph.plot_width,
             height=input$demo.differences_reviewer.disability.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### Differences by Disability (Collapsed)
  
  #### SS Test
  output$demo.differences_reviewer.disability_col.tbl_ss <- DT::renderDataTable({
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        !r.disability_col %in% c('No response','Missing','Prefer not to say'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),r.disability_col) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ss_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_test(original_value~r.disability_col) %>% 
      select(!!as.name(input$demographicsGrouping),p,n) %>% 
      mutate(sig=case_when(
        p<=0.05 & p>0.01 ~ '*',
        p<=0.01 & p>0.001 ~ '**',
        p<=0.001 ~ '***',
        .default=NA
      ),
      p=round(p,4))
    
    ss_test_n <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>%
      summarise(n_unique=n_distinct(reviewer_id))
    
    ss_test <- merge(ss_test,ss_test_n,by.x=input$demographicsGrouping)
    
    ss_size <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_effsize(original_value~r.disability_col) %>% 
      mutate(effsize=round(effsize,3)) %>% 
      select(!!as.name(input$demographicsGrouping),effsize,magnitude)
    
    ss_test <- merge(ss_test,ss_size,by.x=input$demographicsGrouping) %>% arrange(p)
    
    colnames(ss_test) <- ss_column_labels()
    
    
    return(ss_test)
    
  })
  
  #### PH Test
  output$demo.differences_reviewer.disability_col.tbl_ph <- DT::renderDataTable({
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        !r.disability_col %in% c('No response','Missing','Prefer not to say'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),r.disability_col) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~r.disability_col,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),r.disability_col) %>%
      summarise(n=n_distinct(reviewer_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(group=r.disability_col,
             n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  ### Differences by Mental Illness
  
  #### SS Test
  output$demo.differences_reviewer.mental_illness.tbl_ss <- DT::renderDataTable({
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        r.mental_illness %in% c('Yes','No'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),r.mental_illness) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ss_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_test(original_value~r.mental_illness) %>% 
      select(!!as.name(input$demographicsGrouping),p,n) %>% 
      mutate(sig=case_when(
        p<=0.05 & p>0.01 ~ '*',
        p<=0.01 & p>0.001 ~ '**',
        p<=0.001 ~ '***',
        .default=NA
      ),
      p=round(p,4))
    
    ss_test_n <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>%
      summarise(n_unique=n_distinct(reviewer_id))
    
    ss_test <- merge(ss_test,ss_test_n,by.x=input$demographicsGrouping)
    
    ss_size <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_effsize(original_value~r.mental_illness) %>% 
      mutate(effsize=round(effsize,3)) %>% 
      select(!!as.name(input$demographicsGrouping),effsize,magnitude)
    
    ss_test <- merge(ss_test,ss_size,by.x=input$demographicsGrouping) %>% arrange(p)
    
    colnames(ss_test) <- ss_column_labels()
    
    
    return(ss_test)
    
  })
  
  #### PH Test
  df.differences_reviewer.mental_illness <- reactiveVal()
  output$demo.differences_reviewer.mental_illness.tbl_ph <- DT::renderDataTable({
    df <- demo.differences()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(Relationshiptouser!='self',
                        r.mental_illness %in% c('Yes','No'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),r.mental_illness) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~r.mental_illness,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),r.mental_illness) %>%
      summarise(n=n_distinct(reviewer_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(group=r.mental_illness,
             n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    graph_select <- ph_test %>% pull(!!as.name(input$demographicsGrouping))
    
    
    updateSelectizeInput(session,'demo.differences_reviewer.mental_illness.graph.select',
                         choices=graph_select)
    
    df.differences_reviewer.mental_illness(ph_test)
    
    colnames(ph_test) <- ph_column_labels()
    
    ph_test
    
  })
  
  #### Report Graphs
  
  graph_demo.differences_reviewer.mental_illness <- eventReactive(input$demo.differences_reviewer.mental_illness.graph.plot.generate,{
    req(!is.null(input$demo.differences_reviewer.mental_illness.graph.select))
    
    
    df <- df.differences_reviewer.mental_illness()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    
    df <- df %>% select(!!as.name(input$demographicsGrouping),group_1,group_2,mean_1,mean_2) %>% 
      pivot_longer(cols= !input$demographicsGrouping,names_to=c('.value','tib'),names_sep='_') %>% 
      mutate(mean=mean/10)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% input$demo.differences_reviewer.mental_illness.graph.select)
    if (input$demographicsGrouping=='Question') {
      df <- df %>% mutate(Question=str_replace_all(Question,"\\([^()]*\\)",""))
    }
    
    df %>% 
      ggplot(aes(x=mean/10,y=reorder(!!as.name(input$demographicsGrouping),mean),
                 fill=group)) + 
      geom_col(position='identity',width=c(0.6,0.4)) + 
      geom_text_repel(aes(label=paste0(round(mean*10),'%')),
                      family='Work Sans',
                      size=input$demo.differences_reviewer.mental_illness.graph.plot.data_label,
                      color=input$demo.differences_reviewer.mental_illness.graph.plot.data_label_color,
                      direction='x', nudge_y=0.5, segment.linetype=3) +
      theme_pubr() +
      theme(text=element_text(family='Work Sans',color=graph.colors()),
            title = element_text(face='bold',size=input$demo.differences_reviewer.mental_illness.graph.plot.title_size,
                                 color=graph.colors()),
            axis.title = element_text(face='bold',size=input$demo.differences_reviewer.mental_illness.graph.plot.axis_title,
                                      color=graph.colors()),
            plot.subtitle = element_text(face='italic',size=input$demo.differences_reviewer.mental_illness.graph.plot.subtitle_size,
                                         color=graph.colors()),
            axis.text = element_text(color=graph.colors(),size=input$demo.differences_reviewer.mental_illness.graph.plot.axis_label),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_),
            panel.background = element_rect(fill = "transparent",
                                            colour=NA_character_),
            #panel.grid.major.y = element_line(color='grey35'),
            axis.line = element_line(color='grey35'),
            legend.background = element_rect(fill='transparent',
                                             colour=NA_character_),
            legend.position='bottom') +
      xlab(input$demo.differences_reviewer.mental_illness.graph.plot.x_title) +
      ylab(input$demo.differences_reviewer.mental_illness.graph.plot.y_title) +
      labs(title=input$demo.differences_reviewer.mental_illness.graph.plot.title,
           subtitle=input$demo.differences_reviewer.mental_illness.graph.plot.subtitle,
           fill=input$demo.differences_reviewer.mental_illness.graph.plot.fill_label) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
      scale_x_continuous(labels=scales::percent) +
      scale_fill_manual(values = c(input$demo.differences_reviewer.mental_illness.graph.plot.color1,
                                   input$demo.differences_reviewer.mental_illness.graph.plot.color2))
  })
  
  
  output$demo.differences_reviewer.mental_illness.graph.plot <- renderPlot({
    bg=ifelse(input$graphMode=='light','white','#1b2634')
    graph_demo.differences_reviewer.mental_illness() + theme(
      plot.background = element_rect(fill=bg),
      panel.background = element_rect(fill=bg)
    )
    
  })
  
  
  output$demo.differences_reviewer.mental_illness.graph.download <- downloadHandler(
    filename = function(file) {
      "reviewer_mental_illnessdiff.png"
    },
    content = function(file) {
      ggsave(file,graph_demo.differences_reviewer.mental_illness(),bg='transparent',
             width=input$demo.differences_reviewer.mental_illness.graph.plot_width,
             height=input$demo.differences_reviewer.mental_illness.graph.plot_height,dpi=600,device='png')
    }
  )
  
  # Constructing User Demographics Diff Table
  user_scores.demo.tbl <- reactive({
    df <- user_scores.tbl()
    user_demo <- df %>% select(user_id,u.gender,u.sexuality,u.ethnicity_collapsed,
                               u.age,u.disability,u.mental_illness,u.sexuality_col,
                               u.disability_col) %>% distinct()
    
    if (input$demographicsGrouping=='Cell.name') {
      df <- df %>% group_by(user_id,Cell.name) %>% 
        summarise(original_value=mean(mean,na.rm=T))
      df <- merge(df,user_demo,by.x='user_id')
    } else {
      df <- df %>% rename(original_value=mean)
    }
    
    return(df)
    
    
  })
  
  ### Differences by User Gender
  
  #### SS Test
  output$demo.differences_user.gender.tbl_ss <- DT::renderDataTable({
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(!u.gender%in%c('Prefer not to say','No response','Missing'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),u.gender) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ss_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_test(original_value~u.gender) %>% 
      select(!!as.name(input$demographicsGrouping),p,n) %>% 
      mutate(sig=case_when(
        p<=0.05 & p>0.01 ~ '*',
        p<=0.01 & p>0.001 ~ '**',
        p<=0.001 ~ '***',
        .default=NA
      ),
      p=round(p,4))
    
    ss_test_n <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>%
      summarise(n_unique=n_distinct(user_id))
    
    ss_test <- merge(ss_test,ss_test_n,by.x=input$demographicsGrouping)
    
    ss_size <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_effsize(original_value~u.gender) %>% 
      mutate(effsize=round(effsize,3)) %>% 
      select(!!as.name(input$demographicsGrouping),effsize,magnitude)
    
    ss_test <- merge(ss_test,ss_size,by.x=input$demographicsGrouping) %>% arrange(p)
    
    
    colnames(ss_test) <- ss_column_labels()
    
    
    return(ss_test)
    
  })
  
  #### PH Test
  output$demo.differences_user.gender.tbl_ph <- DT::renderDataTable({
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(!u.gender%in%c('Prefer not to say','No response','Missing'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),u.gender) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~u.gender,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),u.gender) %>%
      summarise(n=n_distinct(user_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(group=u.gender,
             n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Custom Test
  df.differences_user.gender <- reactiveVal()
  output$demo.differences_user.gender.tbl_custom <- DT::renderDataTable({
    req(!is.na(input$demo.differences_user.gender.tbl_custom.select1) &
          !is.na(input$demo.differences_user.gender.tbl_custom.select2))
    
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(!u.gender%in%c('Prefer not to say','No response','Missing'))
    
    df <- df %>% mutate(group=case_when(
      u.gender %in% input$demo.differences_user.gender.tbl_custom.select1 ~ 'group1',
      u.gender %in% input$demo.differences_user.gender.tbl_custom.select2 ~ 'group2',
      .default=NA
    ))
    
    df <- df %>% filter(!is.na(group))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),group) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    
    
    
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~group,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),group) %>%
      summarise(n=n_distinct(user_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    graph_select <- ph_test %>% pull(!!as.name(input$demographicsGrouping))
    
    
    updateSelectizeInput(session,'demo.differences_user.gender.graph.select',
                         choices=graph_select)
    
    df.differences_user.gender(ph_test)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Report Graphs
  
  graph_demo.differences_user.gender <- eventReactive(input$demo.differences_user.gender.graph.plot.generate,{
    req(!is.null(input$demo.differences_user.gender.graph.select))
    req(!is.null(input$demo.differences_user.gender.tbl_custom.select1))
    req(!is.null(input$demo.differences_user.gender.tbl_custom.select2))
    
    
    df <- df.differences_user.gender()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    group1_label=paste(input$demo.differences_user.gender.tbl_custom.select1,collapse=', ')
    group2_label=paste(input$demo.differences_user.gender.tbl_custom.select2,collapse=', ')
    
    
    df <- df %>% select(!!as.name(input$demographicsGrouping),group_1,group_2,mean_1,mean_2) %>% 
      pivot_longer(cols= !input$demographicsGrouping,names_to=c('.value','tib'),names_sep='_') %>% 
      mutate(mean=mean/10)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% input$demo.differences_user.gender.graph.select)
    if (input$demographicsGrouping=='Question') {
      df <- df %>% mutate(Question=str_replace_all(Question,"\\([^()]*\\)",""))
    }
    
    df %>% 
      ggplot(aes(x=mean/10,y=reorder(!!as.name(input$demographicsGrouping),mean),
                 fill=group)) + 
      geom_col(position='identity',width=c(0.6,0.4)) + 
      geom_text_repel(aes(label=paste0(round(mean*10),'%')),
                      family='Work Sans',
                      size=input$demo.differences_user.gender.graph.plot.data_label,
                      color=input$demo.differences_user.gender.graph.plot.data_label_color,
                      direction='x', nudge_y=0.5, segment.linetype=3) +
      theme_pubr() +
      theme(text=element_text(family='Work Sans',color=graph.colors()),
            title = element_text(face='bold',size=input$demo.differences_user.gender.graph.plot.title_size,
                                 color=graph.colors()),
            axis.title = element_text(face='bold',size=input$demo.differences_user.gender.graph.plot.axis_title,
                                      color=graph.colors()),
            plot.subtitle = element_text(face='italic',size=input$demo.differences_user.gender.graph.plot.subtitle_size,
                                         color=graph.colors()),
            axis.text = element_text(color=graph.colors(),size=input$demo.differences_user.gender.graph.plot.axis_label),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_),
            panel.background = element_rect(fill = "transparent",
                                            colour=NA_character_),
            #panel.grid.major.y = element_line(color='grey35'),
            axis.line = element_line(color='grey35'),
            legend.background = element_rect(fill='transparent',
                                             colour=NA_character_),
            legend.position='bottom') +
      xlab(input$demo.differences_user.gender.graph.plot.x_title) +
      ylab(input$demo.differences_user.gender.graph.plot.y_title) +
      labs(title=input$demo.differences_user.gender.graph.plot.title,
           subtitle=input$demo.differences_user.gender.graph.plot.subtitle,
           fill=input$demo.differences_user.gender.graph.plot.fill_label) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
      scale_x_continuous(labels=scales::percent) +
      scale_fill_manual(labels=c(str_wrap(group1_label,50),str_wrap(group2_label,50)),
                        values = c(input$demo.differences_user.gender.graph.plot.color1,
                                   input$demo.differences_user.gender.graph.plot.color2))
  })
  
  
  output$demo.differences_user.gender.graph.plot <- renderPlot({
    bg=ifelse(input$graphMode=='light','white','#1b2634')
    graph_demo.differences_user.gender() + theme(
      plot.background = element_rect(fill=bg),
      panel.background = element_rect(fill=bg)
    )
    
  })
  
  
  output$demo.differences_user.gender.graph.download <- downloadHandler(
    filename = function(file) {
      "user_genderdiff.png"
    },
    content = function(file) {
      ggsave(file,graph_demo.differences_user.gender(),bg='transparent',
             width=input$demo.differences_user.gender.graph.plot_width,
             height=input$demo.differences_user.gender.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### Differences by User Age
  
  #### SS Test
  output$demo.differences_user.age.tbl_ss <- DT::renderDataTable({
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(
      u.age!='Missing',
      !is.na(u.age))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),u.age) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ss_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_test(original_value~u.age) %>% 
      select(!!as.name(input$demographicsGrouping),p,n) %>% 
      mutate(sig=case_when(
        p<=0.05 & p>0.01 ~ '*',
        p<=0.01 & p>0.001 ~ '**',
        p<=0.001 ~ '***',
        .default=NA
      ),
      p=round(p,4))
    
    ss_test_n <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>%
      summarise(n_unique=n_distinct(user_id))
    
    ss_test <- merge(ss_test,ss_test_n,by.x=input$demographicsGrouping)
    
    ss_size <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_effsize(original_value~u.age) %>% 
      mutate(effsize=round(effsize,3)) %>% 
      select(!!as.name(input$demographicsGrouping),effsize,magnitude)
    
    ss_test <- merge(ss_test,ss_size,by.x=input$demographicsGrouping) %>% arrange(p)
    
    colnames(ss_test) <- ss_column_labels()
    
    
    return(ss_test)
    
  })
  
  #### PH Test
  output$demo.differences_user.age.tbl_ph <- DT::renderDataTable({
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(u.age!='Missing',
                        !is.na(u.age))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),u.age) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~u.age,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),u.age) %>%
      summarise(n=n_distinct(user_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(group=u.age,
             n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Custom Test
  df.differences_user.age <- reactiveVal()
  output$demo.differences_user.age.tbl_custom <- DT::renderDataTable({
    req(!is.na(input$demo.differences_user.age.tbl_custom.select1) &
          !is.na(input$demo.differences_user.age.tbl_custom.select2))
    
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(u.age!='Missing',
                        !is.na(u.age))
    
    df <- df %>% mutate(group=case_when(
      u.age %in% input$demo.differences_user.age.tbl_custom.select1 ~ 'group1',
      u.age %in% input$demo.differences_user.age.tbl_custom.select2 ~ 'group2',
      .default=NA
    ))
    
    df <- df %>% filter(!is.na(group))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),group) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    
    
    
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~group,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),group) %>%
      summarise(n=n_distinct(user_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    graph_select <- ph_test %>% pull(!!as.name(input$demographicsGrouping))
    
    
    updateSelectizeInput(session,'demo.differences_user.age.graph.select',
                         choices=graph_select)
    
    df.differences_user.age(ph_test)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Report Graphs
  
  graph_demo.differences_user.age <- eventReactive(input$demo.differences_user.age.graph.plot.generate,{
    req(!is.null(input$demo.differences_user.age.graph.select))
    req(!is.null(input$demo.differences_user.age.tbl_custom.select1))
    req(!is.null(input$demo.differences_user.age.tbl_custom.select2))
    
    
    df <- df.differences_user.age()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    group1_label=paste(input$demo.differences_user.age.tbl_custom.select1,collapse=', ')
    group2_label=paste(input$demo.differences_user.age.tbl_custom.select2,collapse=', ')
    
    
    df <- df %>% select(!!as.name(input$demographicsGrouping),group_1,group_2,mean_1,mean_2) %>% 
      pivot_longer(cols= !input$demographicsGrouping,names_to=c('.value','tib'),names_sep='_') %>% 
      mutate(mean=mean/10)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% input$demo.differences_user.age.graph.select)
    if (input$demographicsGrouping=='Question') {
      df <- df %>% mutate(Question=str_replace_all(Question,"\\([^()]*\\)",""))
    }
    
    df %>% 
      ggplot(aes(x=mean/10,y=reorder(!!as.name(input$demographicsGrouping),mean),
                 fill=group)) + 
      geom_col(position='identity',width=c(0.6,0.4)) + 
      geom_text_repel(aes(label=paste0(round(mean*10),'%')),
                      family='Work Sans',
                      size=input$demo.differences_user.age.graph.plot.data_label,
                      color=input$demo.differences_user.age.graph.plot.data_label_color,
                      direction='x', nudge_y=0.5, segment.linetype=3) +
      theme_pubr() +
      theme(text=element_text(family='Work Sans',color=graph.colors()),
            title = element_text(face='bold',size=input$demo.differences_user.age.graph.plot.title_size,
                                 color=graph.colors()),
            axis.title = element_text(face='bold',size=input$demo.differences_user.age.graph.plot.axis_title,
                                      color=graph.colors()),
            plot.subtitle = element_text(face='italic',size=input$demo.differences_user.age.graph.plot.subtitle_size,
                                         color=graph.colors()),
            axis.text = element_text(color=graph.colors(),size=input$demo.differences_user.age.graph.plot.axis_label),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_),
            panel.background = element_rect(fill = "transparent",
                                            colour=NA_character_),
            #panel.grid.major.y = element_line(color='grey35'),
            axis.line = element_line(color='grey35'),
            legend.background = element_rect(fill='transparent',
                                             colour=NA_character_),
            legend.position='bottom') +
      xlab(input$demo.differences_user.age.graph.plot.x_title) +
      ylab(input$demo.differences_user.age.graph.plot.y_title) +
      labs(title=input$demo.differences_user.age.graph.plot.title,
           subtitle=input$demo.differences_user.age.graph.plot.subtitle,
           fill=input$demo.differences_user.age.graph.plot.fill_label) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
      scale_x_continuous(labels=scales::percent) +
      scale_fill_manual(labels=c(str_wrap(group1_label,50),str_wrap(group2_label,50)),
                        values = c(input$demo.differences_user.age.graph.plot.color1,
                                   input$demo.differences_user.age.graph.plot.color2))
  })
  
  
  output$demo.differences_user.age.graph.plot <- renderPlot({
    bg=ifelse(input$graphMode=='light','white','#1b2634')
    graph_demo.differences_user.age() + theme(
      plot.background = element_rect(fill=bg),
      panel.background = element_rect(fill=bg)
    )
    
  })
  
  
  output$demo.differences_user.age.graph.download <- downloadHandler(
    filename = function(file) {
      "user_agediff.png"
    },
    content = function(file) {
      ggsave(file,graph_demo.differences_user.age(),bg='transparent',
             width=input$demo.differences_user.age.graph.plot_width,
             height=input$demo.differences_user.age.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### Differences by User Sexual Identity
  
  #### SS Test
  output$demo.differences_user.sexuality.tbl_ss <- DT::renderDataTable({
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(
      !u.sexuality %in% c('No response','Missing','Prefer not to say'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),u.sexuality) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ss_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_test(original_value~u.sexuality) %>% 
      select(!!as.name(input$demographicsGrouping),p,n) %>% 
      mutate(sig=case_when(
        p<=0.05 & p>0.01 ~ '*',
        p<=0.01 & p>0.001 ~ '**',
        p<=0.001 ~ '***',
        .default=NA
      ),
      p=round(p,4))
    
    ss_test_n <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>%
      summarise(n_unique=n_distinct(user_id))
    
    ss_test <- merge(ss_test,ss_test_n,by.x=input$demographicsGrouping)
    
    ss_size <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_effsize(original_value~u.sexuality) %>% 
      mutate(effsize=round(effsize,3)) %>% 
      select(!!as.name(input$demographicsGrouping),effsize,magnitude)
    
    ss_test <- merge(ss_test,ss_size,by.x=input$demographicsGrouping) %>% arrange(p)
    
    colnames(ss_test) <- ss_column_labels()
    
    
    return(ss_test)
    
  })
  
  #### PH Test
  output$demo.differences_user.sexuality.tbl_ph <- DT::renderDataTable({
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(
      !u.sexuality %in% c('No response','Missing','Prefer not to say'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),u.sexuality) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~u.sexuality,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),u.sexuality) %>%
      summarise(n=n_distinct(user_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(group=u.sexuality,
             n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Custom Test
  df.differences_user.sexuality <- reactiveVal()
  output$demo.differences_user.sexuality.tbl_custom <- DT::renderDataTable({
    req(!is.na(input$demo.differences_user.sexuality.tbl_custom.select1) &
          !is.na(input$demo.differences_user.sexuality.tbl_custom.select2))
    
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(!u.sexuality%in%c('Prefer not to say','No response','Missing'))
    
    df <- df %>% mutate(group=case_when(
      u.sexuality %in% input$demo.differences_user.sexuality.tbl_custom.select1 ~ 'group1',
      u.sexuality %in% input$demo.differences_user.sexuality.tbl_custom.select2 ~ 'group2',
      .default=NA
    ))
    
    df <- df %>% filter(!is.na(group))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),group) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    
    
    
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~group,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),group) %>%
      summarise(n=n_distinct(user_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    graph_select <- ph_test %>% pull(!!as.name(input$demographicsGrouping))
    
    
    updateSelectizeInput(session,'demo.differences_user.sexuality.graph.select',
                         choices=graph_select)
    
    df.differences_user.sexuality(ph_test)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Report Graphs
  
  graph_demo.differences_user.sexuality <- eventReactive(input$demo.differences_user.sexuality.graph.plot.generate,{
    req(!is.null(input$demo.differences_user.sexuality.graph.select))
    req(!is.null(input$demo.differences_user.sexuality.tbl_custom.select1))
    req(!is.null(input$demo.differences_user.sexuality.tbl_custom.select2))
    
    
    df <- df.differences_user.sexuality()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    group1_label=paste(input$demo.differences_user.sexuality.tbl_custom.select1,collapse=', ')
    group2_label=paste(input$demo.differences_user.sexuality.tbl_custom.select2,collapse=', ')
    
    
    df <- df %>% select(!!as.name(input$demographicsGrouping),group_1,group_2,mean_1,mean_2) %>% 
      pivot_longer(cols= !input$demographicsGrouping,names_to=c('.value','tib'),names_sep='_') %>% 
      mutate(mean=mean/10)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% input$demo.differences_user.sexuality.graph.select)
    if (input$demographicsGrouping=='Question') {
      df <- df %>% mutate(Question=str_replace_all(Question,"\\([^()]*\\)",""))
    }
    
    df %>% 
      ggplot(aes(x=mean/10,y=reorder(!!as.name(input$demographicsGrouping),mean),
                 fill=group)) + 
      geom_col(position='identity',width=c(0.6,0.4)) + 
      geom_text_repel(aes(label=paste0(round(mean*10),'%')),
                      family='Work Sans',
                      size=input$demo.differences_user.sexuality.graph.plot.data_label,
                      color=input$demo.differences_user.sexuality.graph.plot.data_label_color,
                      direction='x', nudge_y=0.5, segment.linetype=3) +
      theme_pubr() +
      theme(text=element_text(family='Work Sans',color=graph.colors()),
            title = element_text(face='bold',size=input$demo.differences_user.sexuality.graph.plot.title_size,
                                 color=graph.colors()),
            axis.title = element_text(face='bold',size=input$demo.differences_user.sexuality.graph.plot.axis_title,
                                      color=graph.colors()),
            plot.subtitle = element_text(face='italic',size=input$demo.differences_user.sexuality.graph.plot.subtitle_size,
                                         color=graph.colors()),
            axis.text = element_text(color=graph.colors(),size=input$demo.differences_user.sexuality.graph.plot.axis_label),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_),
            panel.background = element_rect(fill = "transparent",
                                            colour=NA_character_),
            #panel.grid.major.y = element_line(color='grey35'),
            axis.line = element_line(color='grey35'),
            legend.background = element_rect(fill='transparent',
                                             colour=NA_character_),
            legend.position='bottom') +
      xlab(input$demo.differences_user.sexuality.graph.plot.x_title) +
      ylab(input$demo.differences_user.sexuality.graph.plot.y_title) +
      labs(title=input$demo.differences_user.sexuality.graph.plot.title,
           subtitle=input$demo.differences_user.sexuality.graph.plot.subtitle,
           fill=input$demo.differences_user.sexuality.graph.plot.fill_label) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
      scale_x_continuous(labels=scales::percent) +
      scale_fill_manual(labels=c(str_wrap(group1_label,50),str_wrap(group2_label,50)),
                        values = c(input$demo.differences_user.sexuality.graph.plot.color1,
                                   input$demo.differences_user.sexuality.graph.plot.color2))
  })
  
  
  output$demo.differences_user.sexuality.graph.plot <- renderPlot({
    bg=ifelse(input$graphMode=='light','white','#1b2634')
    graph_demo.differences_user.sexuality() + theme(
      plot.background = element_rect(fill=bg),
      panel.background = element_rect(fill=bg)
    )
    
  })
  
  
  output$demo.differences_user.sexuality.graph.download <- downloadHandler(
    filename = function(file) {
      "user_sexualitydiff.png"
    },
    content = function(file) {
      ggsave(file,graph_demo.differences_user.sexuality(),bg='transparent',
             width=input$demo.differences_user.sexuality.graph.plot_width,
             height=input$demo.differences_user.sexuality.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### Differences by User Sexual Identity (Collapsed)
  
  #### SS Test
  output$demo.differences_user.sexuality_col.tbl_ss <- DT::renderDataTable({
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(
      !u.sexuality_col %in% c('No response','Missing','Prefer not to say'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),u.sexuality_col) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ss_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_test(original_value~u.sexuality_col) %>% 
      select(!!as.name(input$demographicsGrouping),p,n) %>% 
      mutate(sig=case_when(
        p<=0.05 & p>0.01 ~ '*',
        p<=0.01 & p>0.001 ~ '**',
        p<=0.001 ~ '***',
        .default=NA
      ),
      p=round(p,4))
    
    ss_test_n <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>%
      summarise(n_unique=n_distinct(user_id))
    
    ss_test <- merge(ss_test,ss_test_n,by.x=input$demographicsGrouping)
    
    ss_size <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_effsize(original_value~u.sexuality_col) %>% 
      mutate(effsize=round(effsize,3)) %>% 
      select(!!as.name(input$demographicsGrouping),effsize,magnitude)
    
    ss_test <- merge(ss_test,ss_size,by.x=input$demographicsGrouping) %>% arrange(p)
    
    colnames(ss_test) <- ss_column_labels()
    
    
    return(ss_test)
    
  })
  
  #### PH Test
  output$demo.differences_user.sexuality_col.tbl_ph <- DT::renderDataTable({
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(
      !u.sexuality_col %in% c('No response','Missing','Prefer not to say'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),u.sexuality_col) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~u.sexuality_col,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),u.sexuality_col) %>%
      summarise(n=n_distinct(user_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(group=u.sexuality_col,
             n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  
  ### Differences by User Ethnicity (Collapsed)
  
  #### SS Test
  output$demo.differences_user.ethnicity_col.tbl_ss <- DT::renderDataTable({
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(
      !u.ethnicity_collapsed %in% c('No response','Missing','Prefer not to say',
                                    'No match'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),u.ethnicity_collapsed) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ss_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_test(original_value~u.ethnicity_collapsed) %>% 
      select(!!as.name(input$demographicsGrouping),p,n) %>% 
      mutate(sig=case_when(
        p<=0.05 & p>0.01 ~ '*',
        p<=0.01 & p>0.001 ~ '**',
        p<=0.001 ~ '***',
        .default=NA
      ),
      p=round(p,4))
    
    ss_test_n <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>%
      summarise(n_unique=n_distinct(user_id))
    
    ss_test <- merge(ss_test,ss_test_n,by.x=input$demographicsGrouping)
    
    ss_size <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_effsize(original_value~u.ethnicity_collapsed) %>% 
      mutate(effsize=round(effsize,3)) %>% 
      select(!!as.name(input$demographicsGrouping),effsize,magnitude)
    
    ss_test <- merge(ss_test,ss_size,by.x=input$demographicsGrouping) %>% arrange(p)
    
    colnames(ss_test) <- ss_column_labels()
    
    
    return(ss_test)
    
  })
  
  #### PH Test
  output$demo.differences_user.ethnicity_col.tbl_ph <- DT::renderDataTable({
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(!u.ethnicity_collapsed %in% c('No response','Missing','Prefer not to say',
                                                      'No match'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),u.ethnicity_collapsed) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~u.ethnicity_collapsed,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),u.ethnicity_collapsed) %>%
      summarise(n=n_distinct(user_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(group=u.ethnicity_collapsed,
             n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Custom Test
  df.differences_user.ethnicity_col <- reactiveVal()
  output$demo.differences_user.ethnicity_col.tbl_custom <- DT::renderDataTable({
    req(!is.na(input$demo.differences_user.ethnicity_col.tbl_custom.select1) &
          !is.na(input$demo.differences_user.ethnicity_col.tbl_custom.select2))
    
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(!u.ethnicity_collapsed %in% c('No response','Missing','Prefer not to say',
                                                      'No match'))
    
    df <- df %>% mutate(group=case_when(
      u.ethnicity_collapsed %in% input$demo.differences_user.ethnicity_col.tbl_custom.select1 ~ 'group1',
      u.ethnicity_collapsed %in% input$demo.differences_user.ethnicity_col.tbl_custom.select2 ~ 'group2',
      .default=NA
    ))
    
    df <- df %>% filter(!is.na(group))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),group) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    
    
    
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~group,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),group) %>%
      summarise(n=n_distinct(user_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    graph_select <- ph_test %>% pull(!!as.name(input$demographicsGrouping))
    
    
    updateSelectizeInput(session,'demo.differences_user.ethnicity_col.graph.select',
                         choices=graph_select)
    
    df.differences_user.ethnicity_col(ph_test)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Report Graphs
  
  graph_demo.differences_user.ethnicity_col <- eventReactive(input$demo.differences_user.ethnicity_col.graph.plot.generate,{
    req(!is.null(input$demo.differences_user.ethnicity_col.graph.select))
    req(!is.null(input$demo.differences_user.ethnicity_col.tbl_custom.select1))
    req(!is.null(input$demo.differences_user.ethnicity_col.tbl_custom.select2))
    
    
    df <- df.differences_user.ethnicity_col()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    group1_label=paste(input$demo.differences_user.ethnicity_col.tbl_custom.select1,collapse=', ')
    group2_label=paste(input$demo.differences_user.ethnicity_col.tbl_custom.select2,collapse=', ')
    
    
    df <- df %>% select(!!as.name(input$demographicsGrouping),group_1,group_2,mean_1,mean_2) %>% 
      pivot_longer(cols= !input$demographicsGrouping,names_to=c('.value','tib'),names_sep='_') %>% 
      mutate(mean=mean/10)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% input$demo.differences_user.ethnicity_col.graph.select)
    if (input$demographicsGrouping=='Question') {
      df <- df %>% mutate(Question=str_replace_all(Question,"\\([^()]*\\)",""))
    }
    
    df %>% 
      ggplot(aes(x=mean/10,y=reorder(!!as.name(input$demographicsGrouping),mean),
                 fill=group)) + 
      geom_col(position='identity',width=c(0.6,0.4)) + 
      geom_text_repel(aes(label=paste0(round(mean*10),'%')),
                      family='Work Sans',
                      size=input$demo.differences_user.ethnicity_col.graph.plot.data_label,
                      color=input$demo.differences_user.ethnicity_col.graph.plot.data_label_color,
                      direction='x', nudge_y=0.5, segment.linetype=3) +
      theme_pubr() +
      theme(text=element_text(family='Work Sans',color=graph.colors()),
            title = element_text(face='bold',size=input$demo.differences_user.ethnicity_col.graph.plot.title_size,
                                 color=graph.colors()),
            axis.title = element_text(face='bold',size=input$demo.differences_user.ethnicity_col.graph.plot.axis_title,
                                      color=graph.colors()),
            plot.subtitle = element_text(face='italic',size=input$demo.differences_user.ethnicity_col.graph.plot.subtitle_size,
                                         color=graph.colors()),
            axis.text = element_text(color=graph.colors(),size=input$demo.differences_user.ethnicity_col.graph.plot.axis_label),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_),
            panel.background = element_rect(fill = "transparent",
                                            colour=NA_character_),
            #panel.grid.major.y = element_line(color='grey35'),
            axis.line = element_line(color='grey35'),
            legend.background = element_rect(fill='transparent',
                                             colour=NA_character_),
            legend.position='bottom') +
      xlab(input$demo.differences_user.ethnicity_col.graph.plot.x_title) +
      ylab(input$demo.differences_user.ethnicity_col.graph.plot.y_title) +
      labs(title=input$demo.differences_user.ethnicity_col.graph.plot.title,
           subtitle=input$demo.differences_user.ethnicity_col.graph.plot.subtitle,
           fill=input$demo.differences_user.ethnicity_col.graph.plot.fill_label) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
      scale_x_continuous(labels=scales::percent) +
      scale_fill_manual(labels=c(str_wrap(group1_label,50),str_wrap(group2_label,50)),
                        values = c(input$demo.differences_user.ethnicity_col.graph.plot.color1,
                                   input$demo.differences_user.ethnicity_col.graph.plot.color2))
  })
  
  
  output$demo.differences_user.ethnicity_col.graph.plot <- renderPlot({
    bg=ifelse(input$graphMode=='light','white','#1b2634')
    graph_demo.differences_user.ethnicity_col() + theme(
      plot.background = element_rect(fill=bg),
      panel.background = element_rect(fill=bg)
    )
    
  })
  
  
  output$demo.differences_user.ethnicity_col.graph.download <- downloadHandler(
    filename = function(file) {
      "user_ethnicity_coldiff.png"
    },
    content = function(file) {
      ggsave(file,graph_demo.differences_user.ethnicity_col(),bg='transparent',
             width=input$demo.differences_user.ethnicity_col.graph.plot_width,
             height=input$demo.differences_user.ethnicity_col.graph.plot_height,dpi=600,device='png')
    }
  )
  
  ### Differences by Disability
  
  #### SS Test
  output$demo.differences_user.disability.tbl_ss <- DT::renderDataTable({
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(
      !u.disability %in% c('No response','Missing','Prefer not to say'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),u.disability) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ss_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_test(original_value~u.disability) %>% 
      select(!!as.name(input$demographicsGrouping),p,n) %>% 
      mutate(sig=case_when(
        p<=0.05 & p>0.01 ~ '*',
        p<=0.01 & p>0.001 ~ '**',
        p<=0.001 ~ '***',
        .default=NA
      ),
      p=round(p,4))
    
    ss_test_n <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>%
      summarise(n_unique=n_distinct(user_id))
    
    ss_test <- merge(ss_test,ss_test_n,by.x=input$demographicsGrouping)
    
    ss_size <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_effsize(original_value~u.disability) %>% 
      mutate(effsize=round(effsize,3)) %>% 
      select(!!as.name(input$demographicsGrouping),effsize,magnitude)
    
    ss_test <- merge(ss_test,ss_size,by.x=input$demographicsGrouping) %>% arrange(p)
    
    colnames(ss_test) <- ss_column_labels()
    
    
    return(ss_test)
    
  })
  
  #### PH Test
  output$demo.differences_user.disability.tbl_ph <- DT::renderDataTable({
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(
      !u.disability %in% c('No response','Missing','Prefer not to say'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),u.disability) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~u.disability,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),u.disability) %>%
      summarise(n=n_distinct(user_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(group=u.disability,
             n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Custom Test
  df.differences_user.disability <- reactiveVal()
  output$demo.differences_user.disability.tbl_custom <- DT::renderDataTable({
    req(!is.na(input$demo.differences_user.disability.tbl_custom.select1) &
          !is.na(input$demo.differences_user.disability.tbl_custom.select2))
    
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(!u.disability%in%c('Prefer not to say','No response','Missing'))
    
    df <- df %>% mutate(group=case_when(
      u.disability %in% input$demo.differences_user.disability.tbl_custom.select1 ~ 'group1',
      u.disability %in% input$demo.differences_user.disability.tbl_custom.select2 ~ 'group2',
      .default=NA
    ))
    
    df <- df %>% filter(!is.na(group))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),group) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    
    
    
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~group,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),group) %>%
      summarise(n=n_distinct(user_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    graph_select <- ph_test %>% pull(!!as.name(input$demographicsGrouping))
    
    
    updateSelectizeInput(session,'demo.differences_user.disability.graph.select',
                         choices=graph_select)
    
    df.differences_user.disability(ph_test)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  
  
  #### Report Graphs
  
  graph_demo.differences_user.disability <- eventReactive(input$demo.differences_user.disability.graph.plot.generate,{
    req(!is.null(input$demo.differences_user.disability.graph.select))
    req(!is.null(input$demo.differences_user.disability.tbl_custom.select1))
    req(!is.null(input$demo.differences_user.disability.tbl_custom.select2))
    
    
    df <- df.differences_user.disability()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    group1_label=paste(input$demo.differences_user.disability.tbl_custom.select1,collapse=', ')
    group2_label=paste(input$demo.differences_user.disability.tbl_custom.select2,collapse=', ')
    
    
    df <- df %>% select(!!as.name(input$demographicsGrouping),group_1,group_2,mean_1,mean_2) %>% 
      pivot_longer(cols= !input$demographicsGrouping,names_to=c('.value','tib'),names_sep='_') %>% 
      mutate(mean=mean/10)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% input$demo.differences_user.disability.graph.select)
    if (input$demographicsGrouping=='Question') {
      df <- df %>% mutate(Question=str_replace_all(Question,"\\([^()]*\\)",""))
    }
    
    df %>% 
      ggplot(aes(x=mean/10,y=reorder(!!as.name(input$demographicsGrouping),mean),
                 fill=group)) + 
      geom_col(position='identity',width=c(0.6,0.4)) + 
      geom_text_repel(aes(label=paste0(round(mean*10),'%')),
                      family='Work Sans',
                      size=input$demo.differences_user.disability.graph.plot.data_label,
                      color=input$demo.differences_user.disability.graph.plot.data_label_color,
                      direction='x', nudge_y=0.5, segment.linetype=3) +
      theme_pubr() +
      theme(text=element_text(family='Work Sans',color=graph.colors()),
            title = element_text(face='bold',size=input$demo.differences_user.disability.graph.plot.title_size,
                                 color=graph.colors()),
            axis.title = element_text(face='bold',size=input$demo.differences_user.disability.graph.plot.axis_title,
                                      color=graph.colors()),
            plot.subtitle = element_text(face='italic',size=input$demo.differences_user.disability.graph.plot.subtitle_size,
                                         color=graph.colors()),
            axis.text = element_text(color=graph.colors(),size=input$demo.differences_user.disability.graph.plot.axis_label),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_),
            panel.background = element_rect(fill = "transparent",
                                            colour=NA_character_),
            #panel.grid.major.y = element_line(color='grey35'),
            axis.line = element_line(color='grey35'),
            legend.background = element_rect(fill='transparent',
                                             colour=NA_character_),
            legend.position='bottom') +
      xlab(input$demo.differences_user.disability.graph.plot.x_title) +
      ylab(input$demo.differences_user.disability.graph.plot.y_title) +
      labs(title=input$demo.differences_user.disability.graph.plot.title,
           subtitle=input$demo.differences_user.disability.graph.plot.subtitle,
           fill=input$demo.differences_user.disability.graph.plot.fill_label) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
      scale_x_continuous(labels=scales::percent) +
      scale_fill_manual(labels=c(str_wrap(group1_label,50),str_wrap(group2_label,50)),
                        values = c(input$demo.differences_user.disability.graph.plot.color1,
                                   input$demo.differences_user.disability.graph.plot.color2))
  })
  
  
  output$demo.differences_user.disability.graph.plot <- renderPlot({
    bg=ifelse(input$graphMode=='light','white','#1b2634')
    graph_demo.differences_user.disability() + theme(
      plot.background = element_rect(fill=bg),
      panel.background = element_rect(fill=bg)
    )
    
  })
  
  
  output$demo.differences_user.disability.graph.download <- downloadHandler(
    filename = function(file) {
      "user_disabilitydiff.png"
    },
    content = function(file) {
      ggsave(file,graph_demo.differences_user.disability(),bg='transparent',
             width=input$demo.differences_user.disability.graph.plot_width,
             height=input$demo.differences_user.disability.graph.plot_height,dpi=600,device='png')
    }
  )
  
  
  ### Differences by Disability (Collapsed)
  
  #### SS Test
  output$demo.differences_user.disability_col.tbl_ss <- DT::renderDataTable({
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(
      !u.disability_col %in% c('No response','Missing','Prefer not to say'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),u.disability_col) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ss_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_test(original_value~u.disability_col) %>% 
      select(!!as.name(input$demographicsGrouping),p,n) %>% 
      mutate(sig=case_when(
        p<=0.05 & p>0.01 ~ '*',
        p<=0.01 & p>0.001 ~ '**',
        p<=0.001 ~ '***',
        .default=NA
      ),
      p=round(p,4))
    
    ss_test_n <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>%
      summarise(n_unique=n_distinct(user_id))
    
    ss_test <- merge(ss_test,ss_test_n,by.x=input$demographicsGrouping)
    
    ss_size <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_effsize(original_value~u.disability_col) %>% 
      mutate(effsize=round(effsize,3)) %>% 
      select(!!as.name(input$demographicsGrouping),effsize,magnitude)
    
    ss_test <- merge(ss_test,ss_size,by.x=input$demographicsGrouping) %>% arrange(p)
    
    colnames(ss_test) <- ss_column_labels()
    
    
    return(ss_test)
    
  })
  
  #### PH Test
  output$demo.differences_user.disability_col.tbl_ph <- DT::renderDataTable({
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(
      !u.disability_col %in% c('No response','Missing','Prefer not to say'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),u.disability_col) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~u.disability_col,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),u.disability_col) %>%
      summarise(n=n_distinct(user_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(group=u.disability_col,
             n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  ### Differences by Mental Illness
  
  #### SS Test
  output$demo.differences_user.mental_illness.tbl_ss <- DT::renderDataTable({
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(
      u.mental_illness %in% c('Yes','No'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),u.mental_illness) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ss_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_test(original_value~u.mental_illness) %>% 
      select(!!as.name(input$demographicsGrouping),p,n) %>% 
      mutate(sig=case_when(
        p<=0.05 & p>0.01 ~ '*',
        p<=0.01 & p>0.001 ~ '**',
        p<=0.001 ~ '***',
        .default=NA
      ),
      p=round(p,4))
    
    ss_test_n <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>%
      summarise(n_unique=n_distinct(user_id))
    
    ss_test <- merge(ss_test,ss_test_n,by.x=input$demographicsGrouping)
    
    ss_size <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      kruskal_effsize(original_value~u.mental_illness) %>% 
      mutate(effsize=round(effsize,3)) %>% 
      select(!!as.name(input$demographicsGrouping),effsize,magnitude)
    
    ss_test <- merge(ss_test,ss_size,by.x=input$demographicsGrouping) %>% arrange(p)
    
    colnames(ss_test) <- ss_column_labels()
    
    
    return(ss_test)
    
  })
  
  #### PH Test
  df.differences_user.mental_illness <- reactiveVal()
  output$demo.differences_user.mental_illness.tbl_ph <- DT::renderDataTable({
    df <- user_scores.demo.tbl()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    if (nrow(df) == 0) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)  
      
    }
    
    df <- df %>% filter(
      u.mental_illness %in% c('Yes','No'))
    
    df_check <- df %>% group_by(!!as.name(input$demographicsGrouping),u.mental_illness) %>% 
      summarise(n=n()) %>% 
      count(!!as.name(input$demographicsGrouping)) %>% 
      filter(n>1)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% pull(df_check,1))
    
    if (nrow(df) == 0) {
      showNotification(paste("Not enough groups to compare."), type = "warning", duration = 5)  
      
    }
    
    ph_test <- df %>% 
      group_by(!!as.name(input$demographicsGrouping)) %>% 
      dunn_test(original_value~u.mental_illness,p.adjust.method = 'bonferroni') %>% 
      rename(group_1=group1,
             group_2=group2,
             n_1=n1,
             n_2=n2) %>% 
      pivot_longer(cols=c('group_1','group_2','n_1','n_2'),names_to=c('.value','tib'),names_sep='_') %>% 
      select(!2:3) %>% 
      mutate(p=round(p,4),
             p.adj=round(p.adj,4))
    
    ph_test_sum <- df %>% 
      group_by(!!as.name(input$demographicsGrouping),u.mental_illness) %>%
      summarise(n=n_distinct(user_id),
                mean=round(mean(original_value,na.rm=T)*10,1)) %>% 
      rename(group=u.mental_illness,
             n_unique=n)
    
    ph_test <- left_join(ph_test,ph_test_sum,by=c(input$demographicsGrouping,'group')) %>% 
      pivot_wider(names_from='tib',values_from=c('group','n','n_unique','mean')) %>% 
      arrange(p.adj)
    
    graph_select <- ph_test %>% pull(!!as.name(input$demographicsGrouping))
    
    
    updateSelectizeInput(session,'demo.differences_user.mental_illness.graph.select',
                         choices=graph_select)
    
    df.differences_user.mental_illness(ph_test)
    
    colnames(ph_test) <- ph_column_labels()
    
    return(ph_test)
    
  })
  
  #### Report Graphs
  
  graph_demo.differences_user.mental_illness <- eventReactive(input$demo.differences_user.mental_illness.graph.plot.generate,{
    req(!is.null(input$demo.differences_user.mental_illness.graph.select))
    
    
    df <- df.differences_user.mental_illness()
    
    if (is.null(df)) {
      showNotification(paste("No results to display."), type = "warning", duration = 5)   
      return(NULL)
    }
    
    
    df <- df %>% select(!!as.name(input$demographicsGrouping),group_1,group_2,mean_1,mean_2) %>% 
      pivot_longer(cols= !input$demographicsGrouping,names_to=c('.value','tib'),names_sep='_') %>% 
      mutate(mean=mean/10)
    
    df <- df %>% filter(!!as.name(input$demographicsGrouping) %in% input$demo.differences_user.mental_illness.graph.select)
    if (input$demographicsGrouping=='Question') {
      df <- df %>% mutate(Question=str_replace_all(Question,"\\([^()]*\\)",""))
    }
    
    df %>% 
      ggplot(aes(x=mean/10,y=reorder(!!as.name(input$demographicsGrouping),mean),
                 fill=group)) + 
      geom_col(position='identity',width=c(0.6,0.4)) + 
      geom_text_repel(aes(label=paste0(round(mean*10),'%')),
                      family='Work Sans',
                      size=input$demo.differences_user.mental_illness.graph.plot.data_label,
                      color=input$demo.differences_user.mental_illness.graph.plot.data_label_color,
                      direction='x', nudge_y=0.5, segment.linetype=3) +
      theme_pubr() +
      theme(text=element_text(family='Work Sans',color=graph.colors()),
            title = element_text(face='bold',size=input$demo.differences_user.mental_illness.graph.plot.title_size,
                                 color=graph.colors()),
            axis.title = element_text(face='bold',size=input$demo.differences_user.mental_illness.graph.plot.axis_title,
                                      color=graph.colors()),
            plot.subtitle = element_text(face='italic',size=input$demo.differences_user.mental_illness.graph.plot.subtitle_size,
                                         color=graph.colors()),
            axis.text = element_text(color=graph.colors(),size=input$demo.differences_user.mental_illness.graph.plot.axis_label),
            plot.background = element_rect(fill = "transparent",
                                           colour=NA_character_),
            panel.background = element_rect(fill = "transparent",
                                            colour=NA_character_),
            #panel.grid.major.y = element_line(color='grey35'),
            axis.line = element_line(color='grey35'),
            legend.background = element_rect(fill='transparent',
                                             colour=NA_character_),
            legend.position='bottom') +
      xlab(input$demo.differences_user.mental_illness.graph.plot.x_title) +
      ylab(input$demo.differences_user.mental_illness.graph.plot.y_title) +
      labs(title=input$demo.differences_user.mental_illness.graph.plot.title,
           subtitle=input$demo.differences_user.mental_illness.graph.plot.subtitle,
           fill=input$demo.differences_user.mental_illness.graph.plot.fill_label) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
      scale_x_continuous(labels=scales::percent) +
      scale_fill_manual(values = c(input$demo.differences_user.mental_illness.graph.plot.color1,
                                   input$demo.differences_user.mental_illness.graph.plot.color2))
  })
  
  
  output$demo.differences_user.mental_illness.graph.plot <- renderPlot({
    bg=ifelse(input$graphMode=='light','white','#1b2634')
    graph_demo.differences_user.mental_illness() + theme(
      plot.background = element_rect(fill=bg),
      panel.background = element_rect(fill=bg)
    )
    
  })
  
  
  output$demo.differences_user.mental_illness.graph.download <- downloadHandler(
    filename = function(file) {
      "user_mental_illnessdiff.png"
    },
    content = function(file) {
      ggsave(file,graph_demo.differences_user.mental_illness(),bg='transparent',
             width=input$demo.differences_user.mental_illness.graph.plot_width,
             height=input$demo.differences_user.mental_illness.graph.plot_height,dpi=600,device='png')
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
  
  
  
  
  
  
  
  
  
  
  
  
  