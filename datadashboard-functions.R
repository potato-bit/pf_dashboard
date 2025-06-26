######################################################################################################
# GLOBAL CONSTANTS
######################################################################################################

# color palettes
CBPALETTE <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#FFCCFF", "#999999")
REPORT_PALETTE <- c('#FFD15A','#2B99BC','#E17B8D')

# standard number of entries to display in tables before paging
DEFAULT_PAGE_LEN <- 10

# maximum number of entries to display in tables before paging
MAX_PAGE_LEN <- 25

# minimum number of reviews in order to display differences
MIN_CELL_SIZE <- 5

# These strings correspond to no response/missing response values
PREFER_NOT_TO_SAY_STRING <- "Prefer not to say"
NO_RESPONSE_STRING       <- "No response"
MISSING_VALUE_STRING     <- "Missing"

# They should always come last in frequency tables
DO_NOT_ALPHABETIZE_STRINGS <- c(PREFER_NOT_TO_SAY_STRING,
                                NO_RESPONSE_STRING,
                                MISSING_VALUE_STRING)

######################################################################################################
# DATADASHBOARD HELPER FUNCTIONS
######################################################################################################

# check if object is empty
is.empty <- function(x, mode = NULL){
  if (is.null(mode)) mode <- class(x)
  identical(vector(mode, 1), c(x, vector(class(x), 1)))
}

rownames_to_col <- function(df, var) {
  df[[var]] <- row.names(df)
  row.names(df) <- NULL
  return(df %>% select(one_of(var), everything()))
}

col_to_rownames <- function(df, var) {
  row.names(df) <- df[[var]]
  df[[var]] <- NULL
  return(df)
}

# apply filters to data containing columns 'survey_id', 'user_id', and date.column
apply_filters <- function(df, surveys = numeric(0), user_ids = numeric(0), 
                          dates = c(NA, NA), date.column = "created_at") {
  
  if (is.null(df)) {
    return(NULL)
  }
  
  # Filter by Survey 
  if(length(surveys) != 0) {
    df <- subset(df, survey_id %in% as.numeric(surveys))
  }
  
  # Filter by Date - only if both start and end date supplied
  if (length(dates) == 2) {
    if (sum(is.na(dates)) == 2) {
      # no filtering in this case
    } else if (sum(is.na(dates)) == 0) {
      if (!is.Date(dates[1]) | !is.Date(dates[2])) {
        stop("Dates supplied to date filter are not Date format")
      }
      if (!date.column %in% names(df)) {
        stop(paste0("Date column ", date.column, " does not exist in the dataframe being filtered"))
      }
      df <- df[df[[date.column]] >= dates[1] & df[[date.column]] <= dates[2], ]
    } else {
      showNotification("Both start and end of date range must be supplied for date filter", 
                       type = "warning", duration = NULL)
    }
  } else {
    stop(paste0("Both start and end of date range must be supplied for date filter"))
  }
  
  # Filter by user_id
  if(length(user_ids) != 0){
    df <- subset(df, !(as.numeric(user_id)  %in% as.numeric(user_ids)))
  }
  
  # Reset row names
  rownames(df) <- NULL
  
  return(df)
}


# apply filters to reviews returned by the main query, and clean review data 
apply_filters_and_clean <- function(reviews_df, surveys = numeric(0), user_ids = numeric(0), 
                                    dates = c(NA, NA), na_action = "drop") {
  
  if (is.null(reviews_df)) {
    return(NULL)
  }
  
  df <- apply_filters(reviews_df, surveys, user_ids, dates, date.column = "created_at")
  
  # Identify and remove rows where the cell name contains the word ARCHIVE
  cell.name.archive <- grep("ARCHIVE", df$Cell.name)
  df_clean <- df[!rownames(df) %in% cell.name.archive, ]
  
  # Remove leading/trailing white space from cell.name categories and questions
  df_clean$Cell.name <- trimws(df_clean$Cell.name, which = "both")
  df_clean$Question <- trimws(df_clean$Question, which = "both")
  
  # Convert Cell.name to sentence case (only first word is capitalized)
  df_clean$Cell.name <- str_to_sentence(df_clean$Cell.name)
    
  if (na_action == "drop") {
    # keep cases that are not missing on original_value
    df_clean <- df_clean %>%
      filter(!is.na(original_value))
  }
    
  return(df_clean)
}

# add SQL to filter on populations in pop.list to query 
add_population_filter_to_query <- function(query, pop.list, field, connector = "AND") {
  
  # if field contains a 'dot', then it includes the column name
  # if not, append ".population_id" to it
  if (!is.null(pop.list)) {
    query <- paste0(query, 
                    " ", connector, " ",
                    ifelse(grepl("\\.", field), 
                           field, 
                           paste0(field, ".population_id")),
                    " IN (", 
                    paste(pop.list, collapse = ", "), ")")
  }
  return(query)
}


# creates a standard datatable for the dataframe in table_df
standard_datatable <- function(table_df, rownames = TRUE, sort = TRUE) {
  
  if (is.null(table_df)) {
    return(NULL)
  }
  
  if (nrow(table_df) < DEFAULT_PAGE_LEN) {
    page_length <- nrow(table_df)
    dom <- "Bfrti"
  } else {
    page_length <- DEFAULT_PAGE_LEN
    dom <- "Bfrtip"
  }
  
  options <- list(dom = dom, 
                  pageLength = page_length,
                  buttons = c('copy', 'csv', 'excel', 'pdf'),
                  searching = T)
  
  # if sorting, add that to the options list
  if (sort == TRUE) {
    options <- append(options, list(order = list(0, "asc")))
  }
  
  table_df %>%
    datatable(rownames = rownames, extensions = "Buttons",
              filter="none", options = options)
}

# converts a dataframe containing a column named 'Freq' into a datatable,
# with column 'Freq' formatted as a percentage
frequency_datatable <- function(table_df) {
  
  if (is.null(table_df))  return(NULL)

  standard_datatable(table_df) %>%
    formatPercentage(columns = c("Percent"), digits = 2)
  
}


# construct a full csv pathname from dir, root and timestamp
get_pathname <- function(dir, root, timestamp) {
  SEP <- "_"
  EXT <- ".csv"
  file.path(dir, paste0(root, SEP, timestamp, EXT))
}


# if table is not empty, save it to csv
save_to_csv <- function(tbl, outdir, filename, timestamp, row.names = FALSE) {
  if (!is.null(tbl)) {
    tryCatch(
      write.csv(tbl, get_pathname(outdir, filename, timestamp), row.names = row.names)
    )
    error=function(cond) {
      showNotification(paste("Error saving ", filename), type = "error", duration = NULL)
    }
  }  
}

######################################################################################################
# DEMOGRAPHIC SECTION CONSTANTS AND FUNCTIONS
######################################################################################################

# labels to use in place of variable names
labels_ethnicity <- c(
  eth_white      = "White",
  eth_namerican  = "North American",
  eth_samerican  = "South American",
  eth_camerican  = "Central American",
  eth_caribbean  = "Caribbean",
  eth_latino     = "Latino/a",
  eth_black      = "Black",
  eth_african    = "African",
  eth_easian     = "East Asian",
  eth_asian      = "Asian",
  eth_sasian     = "South Asian",
  eth_seasian    = "South-East Asian",
  eth_mideast    = "Middle Eastern/Arab",
  eth_westeuro   = "Western Europe",
  eth_easteuro   = "Eastern Europe",
  eth_multi      = "Multiethnic",
  eth_other      = "Other",
  eth_dts        = "Prefer not to say",
  eth_noresponse = "No response",
  eth_missing    = "Missing"
)

recode_ethnicity <- function(df, col_name) {
  
  if (grepl("^r\\.", col_name)) {
    prefix = "r."
  } else if (grepl("^u\\.", col_name)) {
    prefix = "u."
  } else {
    prefix = ""
  }
  
  df$.eth <- df[[col_name]]
  
  # set all ethnicities selected to TRUE
  df[[paste0(prefix, "eth_seasian")]] <- grepl("south-east asian", df$.eth, ignore.case=TRUE)
  df[[paste0(prefix, "eth_sasian")]] <- grepl("south asian", df$.eth, ignore.case=TRUE)
  df[[paste0(prefix, "eth_easian")]] <- grepl("^east asian|, east asian", df$.eth, ignore.case=TRUE)
  df[[paste0(prefix, "eth_asian")]] <- grepl("^asian|, asian", df$.eth, ignore.case=TRUE)
  df[[paste0(prefix, "eth_black")]] <- grepl("black", df$.eth, ignore.case=TRUE)
  df[[paste0(prefix, "eth_african")]] <- grepl("african", df$.eth, ignore.case=TRUE)
  df[[paste0(prefix, "eth_latino")]] <- grepl("latino", df$.eth, ignore.case=TRUE)
  df[[paste0(prefix, "eth_white")]] <- grepl("white", df$.eth, ignore.case=TRUE)
  df[[paste0(prefix, "eth_westeuro")]] <- grepl("western", df$.eth, ignore.case=TRUE)
  df[[paste0(prefix, "eth_easteuro")]] <- grepl("eastern european", df$.eth, ignore.case=TRUE)
  df[[paste0(prefix, "eth_namerican")]] <- grepl("north american", df$.eth, ignore.case=TRUE)
  df[[paste0(prefix, "eth_caribbean")]] <- grepl("caribbean", df$.eth, ignore.case=TRUE)
  df[[paste0(prefix, "eth_mideast")]] <- grepl("arab|turkish", df$.eth, ignore.case=TRUE)
  df[[paste0(prefix, "eth_other")]] <- grepl("other", df$.eth, ignore.case=TRUE)
  
  # count number of ethnicities selected
  df[[paste0(prefix, "eth_count")]] <- rowSums(df[, grep(paste0(prefix, "eth_"), names(df))])
  
  # one of the options is Mixed/Multiple ethnicities. Also count as multiethnic users who
  # selected more than one ethnicity
  df[[paste0(prefix, "eth_multi")]] <- grepl("Multiple", df$.eth, ignore.case=TRUE)
  df[[paste0(prefix, "eth_multi")]] <- ifelse( df[[paste0(prefix, "eth_count")]] > 1, 
                                               TRUE, df[[paste0(prefix, "eth_multi")]])
  
  # mark those who explicitly declined to select an ethnicity
  df[[paste0(prefix, "eth_dts")]] <- grepl("prefer", df$.eth, ignore.case=TRUE)

  # lastly, mark those who declined to provide a response or who have no data
  df[[paste0(prefix, "eth_noresponse")]] <- grepl("response", df$.eth, ignore.case=TRUE)
  df[[paste0(prefix, "eth_missing")]] <- grepl("Missing", df$.eth, ignore.case=TRUE)
  
  
  # create a collapsed version that combines some of the ethnic groups
  df[[paste0(prefix, "ethnicity_collapsed")]] <- case_when(
    df[[paste0(prefix, "eth_multi")]] == 1 ~ "Multiethnic",
    df[[paste0(prefix, "eth_asian")]] == 1 ~ "Asian",
    df[[paste0(prefix, "eth_easian")]] == 1 ~ "Asian",
    df[[paste0(prefix, "eth_sasian")]] == 1 ~ "Asian",
    df[[paste0(prefix, "eth_seasian")]] == 1 ~ "Asian",
    df[[paste0(prefix, "eth_black")]] == 1 ~ "Black",
    df[[paste0(prefix, "eth_african")]] == 1 ~ "Black",
    df[[paste0(prefix, "eth_caribbean")]] == 1 ~ "Latino",
    df[[paste0(prefix, "eth_latino")]] == 1 ~ "Latino",
    df[[paste0(prefix, "eth_mideast")]] == 1 ~ "Middle Eastern/Arab",
    df[[paste0(prefix, "eth_white")]] == 1 ~ "White",
    df[[paste0(prefix, "eth_westeuro")]] == 1 ~ "White",
    df[[paste0(prefix, "eth_easteuro")]] == 1 ~ "White",
    df[[paste0(prefix, "eth_namerican")]] == 1 ~ "White",
    df[[paste0(prefix, "eth_other")]] == 1 ~ "Other ethnic group",
    df[[paste0(prefix, "eth_dts")]] == 1 ~ "Prefer not to say",
    df[[paste0(prefix, "eth_noresponse")]] == 1 ~ "No response",
    df[[paste0(prefix, "eth_missing")]] == 1 ~ "Missing",
    TRUE ~ "No match"  #  
  )
  
  if (any(df$ethnicity_collapsed == "No match")) {
    showNotification("In recode_ethnicity: Unknown ethnicity encountered.", type = "error", duration = NULL)   
  }
  
  # delete these columns, they are no longer needed
  df$.eth <- NULL
  df[[paste0(prefix, "eth_count")]] <- NULL
  
  return(df)
}

# Get a dataframe containing distinct users or reviewers, and construct
# a frequency table for column column.name

demo.tbler <- function(df,demo_cat,user=FALSE) {
  if (is.null(df)) return(NULL)
  
  
  
  id.col <- 'reviewer_id'
  if (user==T) {
    id.col <- 'user_id'
    df <- df %>% filter(!is.na(mean)) %>% 
      group_by(!!as.name(demo_cat)) %>% 
      summarise(N=n_distinct(!!as.name(id.col))) %>%
      mutate(Percent=paste0(round((N/sum(N))*100,2),'%')) %>% 
      arrange(-N)
  } else {
    df <- df %>% filter(!is.na(original_value),Relationshiptouser!='self') %>% 
      group_by(!!as.name(demo_cat)) %>% 
      summarise(N=n_distinct(!!as.name(id.col))) %>% 
      mutate(Percent=paste0(round((N/sum(N))*100,2),'%')) %>% 
      arrange(-N)
  }
  
  df <- df %>% rename(Group=!!as.name(demo_cat))
  df$Group <- df$Group %>% replace_na('No response')
  
  return(df)
}




oneway_freq_dataframe <- function(df, id.col, column.name, alphabetize = TRUE) {
  
  if (is.null(df)) return(NULL)
  
  df <- df %>%
    select(all_of(c(id.col, column.name))) %>%
    distinct()
  
  ftable <- as.matrix(table(df[[column.name]]))
  ptable <- prop.table(ftable)
  table_df <- as.data.frame(cbind(ftable, ptable)) %>%
    rownames_to_col(var = "rowname") %>%
    arrange(desc(V2))
  
  if (alphabetize == TRUE) {
    names <- table_df$rowname
    # sort the names that are not in the list of strings to not alphabetize
    levels <- sort(names[!names %in% DO_NOT_ALPHABETIZE_STRINGS])
    # add the non-alphabetized strings to the end of the list of levels
    levels <- c(levels, DO_NOT_ALPHABETIZE_STRINGS)
    # factorize the rownames, sort by level number, and drop rowname
    table_df <- table_df %>%
      mutate(rowname = factor(rowname, levels = levels, ordered = TRUE)) %>%
      arrange(as.numeric(rowname)) %>%
      col_to_rownames(var = "rowname")
  }
  
  names(table_df) <- c("Freq", "Percent")

  return(table_df)
}

# Construct an ethnicity frequency table using detailed ethnic categories
ethnicity_frequency_table <- function(df, id.col, eth.col, alphabetize = TRUE) {
  
  if (is.null(df)) return(NULL)
  
  df <- df %>%
    select(all_of(c(id.col, eth.col))) %>%
    rename(ethnicity = !!as.name(eth.col)) %>%
    distinct()
  
  df <- recode_ethnicity(df, "ethnicity")
  eth.cols <- grep("eth_", names(df), perl = TRUE, value = TRUE)
  eth_matrix <- t(as.data.frame(lapply(df[, eth.cols], sum, na.rm = TRUE)))
  
  df2 <- as.data.frame(eth_matrix) %>%
    mutate(Ethnicity = gsub("r\\.", "", eth.cols)) %>%
    dplyr::rename(N = V1) %>%
    arrange(desc(N)) %>%
    select(Ethnicity, N) %>%
    filter(N > 0)
  
  row.names(df2) <- labels_ethnicity[df2$Ethnicity]
  df2$Ethnicity <- NULL
  
  ftable <- as.matrix(df2)
  ptable <- prop.table(ftable)
  table_df <- as.data.frame(cbind(ftable, ptable)) %>%
    rownames_to_col(var = "rowname")
  
  if (alphabetize == TRUE) {
    names <- table_df$rowname
    # sort the names that are not in the list of strings to not alphabetize
    levels <- sort(names[!names %in% DO_NOT_ALPHABETIZE_STRINGS])
    # add the non-alphabetized strings to the end of the list of levels
    levels <- c(levels, DO_NOT_ALPHABETIZE_STRINGS)
    # factorize the rownames, sort by level number, and drop rowname
    table_df <- table_df %>%
      mutate(rowname = factor(rowname, levels = levels, ordered = TRUE)) %>%
      arrange(as.numeric(rowname)) %>%
      col_to_rownames(var = "rowname")
  }
  
  names(table_df) <- c("Freq", "Percent")
  return(table_df)
}


count_reviews_by_category <- function(df, var) {
  count_df <- df %>%
    dplyr::filter(!is.na(original_value)) %>%
    dplyr::count(!!as.name(var)) %>%
    dplyr::rename(category = !!as.name(var)) %>%
    dplyr::filter(!is.na(category) & category != "Missing") 
}


# calculate the average difference in scores across question.col by groups
# of var.name
calculate_demographic_gap <- function(input_df, var.name, groups, question.col) {
  
  if (is.null(input_df)) return(NULL)
  if (nrow(input_df) == 0) return(NULL)
  
  avg_scores <- paste0("average.score_", groups)
  group_sizes <- paste0("n_", groups)
  
  input_df <- input_df[input_df[[var.name]] %in% unlist(groups), ]
  
  # summarize the scores by group and question.col, after removing self reviews
  # and reviews that have no score (original_value is missing) and categories of
  # var.name that are not named in groups
  summary_df <- input_df %>%
    filter(!is.na(original_value)) %>%
    filter(Relationshiptouser != "self") %>%
    filter(!!as.name(var.name) %in% groups) %>%
    dplyr::group_by(!!as.name(var.name), !!as.name(question.col)) %>%
    dplyr::summarise(average.score = mean(original_value, na.rm = T), 
                     n = sum(!is.na(original_value)), .groups = "drop") 

  # check that both groups are represented in the data
  levels <- unique(summary_df[[var.name]])
  if (length(levels) != 2) {
    return(NULL)
  }  
    
  # now convert to wide format, keep only rows that contain values for both groups
  # and calculate the gap between the group values
  summary_df <- summary_df %>%
    pivot_wider(names_from = !!as.name(var.name),
                values_from = c(n, average.score)) %>% 
    filter(!is.na(!!as.name(avg_scores[1])) & !is.na(!!as.name(avg_scores[2]))) %>%
    filter(!!as.name(group_sizes[1]) >= MIN_CELL_SIZE & !!as.name(group_sizes[2]) >= MIN_CELL_SIZE) %>%
    dplyr::mutate(Gap = !!as.name(avg_scores[2]) - !!as.name(avg_scores[1])) %>%
    arrange(desc(Gap)) %>%
    dplyr::select(all_of(c(question.col, group_sizes[1], avg_scores[1], 
                           group_sizes[2], avg_scores[2])), Gap)
  
  # the following works for question.cols that are character - numeric values lose their names
  # so there is nothing to merge on. if it is numeric, temporarily convert to character
  if (is.numeric(input_df[[question.col]])) {
    question.col.class <- class(input_df[[question.col]])
    input_df[[question.col]] <- as.character(input_df[[question.col]])
  }
  test_df <- as.data.frame(sapply(unique(input_df[[question.col]]), function(x) {
    
    # keep rows for this question, that have a value and are not missing on var.name
    df <- input_df %>%
      filter(!!as.name(question.col) == x ) %>%
      filter(!is.na(original_value) & !is.na(!!as.name(var.name))) %>%
      filter(!!as.name(var.name) %in% groups)
    
    # count the number of observations in each level of var.name
    counts <- df %>%
      group_by(!!as.name(var.name)) %>% 
      dplyr::summarize(n = n())
    
    # if any of counts are NA or less than 2, return NA, 
    # else return the p.value of the t.test of the difference in means
    if (nrow(counts) != 2 | any(is.na(counts$n)) | any(counts$n < 2)) {
      NA_real_
    } else {
      try(t.test(as.formula(paste0("original_value ~ ", var.name)), data = df, na.action = na.omit)$p.value)
    }
  }, USE.NAMES = TRUE))
  
  names(test_df) <- "p.value"
  
  # https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-019-0754-4
  # They recommend the Hommel method when the distributional assumptions are met: "To control the FWER, the Hommel 
  # method requires that the joint distribution of the overall hypothesis test statistic is known." 
  # "The distributional assumption associated with the Hommel method is not restrictive and is met in many multiplicity problems arising in clinical trials [22]. Even when the data followed a skewed distribution, the Hommel method performed well, showing it may be used to analyse a variety of outcomes, including those with a skewed distribution."
  
  test_df$p.adjusted <- p.adjust(test_df$p.value, method = "hommel")
  
  # convert row.names to a column
  test_df[[question.col]] <- row.names(test_df)
  if (exists("question.col.class")) {
    if (question.col.class == "integer") {
      test_df[[question.col]] <- as.integer(test_df[[question.col]])
    } else if (question.col.class == "double") {
      test_df[[question.col]] <- as.double(test_df[[question.col]])
    } else if (question.col.class == "numeric") {
      test_df[[question.col]] <- as.numeric(test_df[[question.col]])
    } else {
      knit_exit("mean_score_difference: question.col is not character or numeric")
    }       
  } 
  
  output_df <- merge(summary_df, test_df, by = question.col)
  row.names(output_df) <- NULL
  return(output_df)
}

demographic_gap_table <- function(df, name_list, n_df) {
  
  if (is.null(df)) return(NULL)
  
  col_name <- ifelse(any(grepl("Cell", names(df))), "Cell", "Behaviour")
  
  tbl_container = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 3, col_name),
        th(colspan = 2, names(name_list)[1]),
        th(colspan = 2, names(name_list)[2]),
        th(rowspan = 3, 'Gap'),
        th(rowspan = 3, 'p-value'),
        th(rowspan = 3, 'p-value (adj)')
      ),
      tr(
        th(colspan = 2, paste0('Total N = ', n_df$n[n_df$category == name_list[1]])),
        th(colspan = 2, paste0('Total N = ', n_df$n[n_df$category == name_list[2]]))
      ),      
      tr(
        lapply(rep(c('N', 'Score'), 2), th)
      )
    )
  ))
  
  df %>%
    datatable(extensions = c("FixedColumns", "Buttons"),
              rownames = FALSE,
              container = tbl_container,
              options = list(pageLength = min(MAX_PAGE_LEN, nrow(df)),
                             dom = 'Bfrtip',
                             buttons = c("copy", "csv", "excel"),
                             scrollX = TRUE,
                             fixedColumns = list(leftColumns = 1))
    )  %>%
    formatRound(columns = c(paste0("average.score_", name_list), "Gap", "p.value", "p.adjusted"), digits = 3)
}

