# MSc Digital Scholarship 2026
# DDeR 2026-02-26
#
# This Shiny code has been generated iteratively with ChatGPT for
# the class, in order to demonstrate various aspects of the "real data"
# in the Bate spreadsheet.
#
# It provides a minimal Shiny catalogue explorer for the Bate Collection
# CSV with an additional "Data quality" tab for Digital Scholarship
# discussion of missingness, unparsed dates, top values.

# Download the Bate spreadsheet bate_catalogue.csv to the same directory
# as this file app.R
#  
# Load this file into RStudio using New File -> Shiny Web App
# then choose "Run App" and interact through the Web browser.
# 
# NB The Excel version of the catalogue is available from
# https://bate.web.ox.ac.uk/sites/default/files/bate_catalogue.xlsx
# The shared CSV version has been stripped of non-printing characters
# (e.g. ^M, ^]) using the unix command
# tr -cd '\11\12\40-\176'
# which only retains tabs, newlines and printable characters.

# Packages needed:
#
# shiny – the web application framework (UI + reactive server logic)
# DT – interactive tables (sorting, paging, search, row selection)
# dplyr – readable data manipulation (filter, mutate, count)
# stringr – safer, clearer string handling (regex for dates, keyword search)
# readr – robust CSV reading (better than base read.csv() for messy data)
# ggplot2 – plotting
#
# NB The last 4 could also be brought in by install.packages("tidyverse")
#
# Generated code follows:
# 
# app.R — Bate Collection Catalogue Shiny Explorer (minimal + DH "Data quality" tab)
# Includes:
#  - Filters + searchable/interactive table
#  - Details panel (shows rich text + image/audio links where possible)
#  - Summary plot with an override control: Auto / Timeline / Class / Material / Maker / Playable
#  - Data quality tab: missingness, date parsing stats, top values, examples of unparsed dates
#
# Setup:
# 1) Put your CSV next to this app.R and name it "bate_catalogue.csv"
#    OR set an environment variable:
#       Sys.setenv(BATE_CSV_PATH="path/to/bate_catalogue.csv")
# 2) Run:
#       shiny::runApp()

library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)

# ---------- helpers ----------

csv_path <- function() {
  p <- Sys.getenv("BATE_CSV_PATH", unset = "")
  if (nzchar(p)) return(p)
  "bate_catalogue.csv"
}

safe_col <- function(df, col) col %in% names(df)

first_nonempty <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & trimws(x) != ""]
  if (length(x) == 0) return(NA_character_)
  x[1]
}

make_clickable_url <- function(u) {
  u <- trimws(as.character(u))
  if (is.na(u) || u == "") return(NA_character_)
  u1 <- str_extract(u, "https?://\\S+")
  if (is.na(u1) || u1 == "") return(NA_character_)
  sprintf('<a href="%s" target="_blank" rel="noopener">%s</a>', u1, u1)
}

extract_year <- function(date_str) {
  # Minimal “good enough” extraction: first 4-digit year (1500–2099)
  d <- as.character(date_str)
  suppressWarnings(as.integer(str_extract(d, "\\b(1[5-9]\\d{2}|20\\d{2})\\b")))
}

read_bate_csv <- function(path) {
  # readr is more robust than base read.csv for messy CSV exports
  df <- readr::read_csv(
    file = path,
    show_col_types = FALSE,
    guess_max = 5000,
    progress = FALSE
  )
  
  expected <- c(
    "Number","Other Numbers","Name","Class","Description","Longer Description","Material",
    "Dimensions","Acoustic Measure","Provenance","Maker","Date","Location","Source","Acquired",
    "Previous Owners","Mark","Labels","Information","Publication","Image","Sound Recording",
    "Playable","Condition","Recorder"
  )
  for (col in expected) {
    if (!safe_col(df, col)) df[[col]] <- NA_character_
  }
  
  df %>%
    mutate(
      .row_id = row_number(),
      .year = extract_year(.data$Date),
      .has_image = !is.na(Image) & trimws(Image) != "",
      .has_audio = !is.na(`Sound Recording`) & trimws(`Sound Recording`) != ""
    )
}

topN_plot <- function(data, col, title, n = 15) {
  x <- data %>%
    mutate(.x = ifelse(is.na(.data[[col]]) | trimws(.data[[col]]) == "", "(blank)", as.character(.data[[col]]))) %>%
    count(.x, sort = TRUE) %>%
    head(n)
  
  ggplot(x, aes(x = reorder(.x, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(x = NULL, y = "Count", title = title)
}

timeline_plot_if_informative <- function(data) {
  df2 <- data %>%
    mutate(decade = ifelse(is.na(.data$.year), NA_character_,
                           paste0((.data$.year %/% 10) * 10, "s")))
  
  n_known <- sum(!is.na(df2$decade))
  n_decades <- dplyr::n_distinct(df2$decade, na.rm = TRUE)
  
  if (n_known >= 10 && n_decades >= 3) {
    return(
      ggplot(df2 %>% filter(!is.na(decade)), aes(x = decade)) +
        geom_bar() +
        labs(x = "Decade (from Date)", y = "Count",
             title = "Instruments by decade (parsable years only)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  }
  NULL
}

# ---------- UI ----------

ui <- fluidPage(
  titlePanel("Bate Collection Catalogue — Shiny Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Filters update the catalogue and visuals immediately. Click a row to see details."),
      
      selectInput("class", "Class",
                  choices = c("All" = "__ALL__"),
                  selected = "__ALL__"),
      
      selectizeInput("maker", "Maker",
                     choices = c("All" = "__ALL__"),
                     selected = "__ALL__",
                     options = list(placeholder = "Type to search makers…")),
      
      selectInput("playable", "Playable",
                  choices = c("All"="__ALL__", "Yes"="Yes", "No"="No"),
                  selected = "__ALL__"),
      
      selectInput("condition", "Condition",
                  choices = c("All"="__ALL__"),
                  selected = "__ALL__"),
      
      textInput("q", "Keyword (Name/Description/Maker/Material/Provenance)", ""),
      
      checkboxInput("only_with_media", "Only show records with Image or Sound Recording", FALSE),
      
      hr(),
      
      # NEW CONTROLS (plot override)
      checkboxInput("show_year_plot", "Allow timeline in Auto mode (requires parsable year)", TRUE),
      
      selectInput(
        "plot_mode",
        "Summary plot",
        choices = c(
          "Auto (what varies)" = "auto",
          "Timeline (by decade)" = "timeline",
          "Class" = "class",
          "Material" = "material",
          "Maker" = "maker",
          "Playable" = "playable"
        ),
        selected = "auto"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Explore",
          fluidRow(
            column(
              width = 6,
              h4("Summary"),
              plotOutput("plot1", height = 260),
              uiOutput("summary_text")
            ),
            column(
              width = 6,
              h4("Selected record"),
              uiOutput("details")
            )
          ),
          hr(),
          DTOutput("tbl")
        ),
        
        tabPanel(
          "Data quality",
          h4("Data quality overview (for DH discussion)"),
          uiOutput("dq_headline"),
          
          fluidRow(
            column(width = 6,
                   h5("Missingness by column"),
                   DTOutput("dq_missing_tbl")),
            column(width = 6,
                   h5("Date parsing (first 4-digit year)"),
                   DTOutput("dq_date_tbl"))
          ),
          
          hr(),
          
          fluidRow(
            column(width = 6,
                   h5("Most common Classes"),
                   DTOutput("dq_top_class")),
            column(width = 6,
                   h5("Most common Makers"),
                   DTOutput("dq_top_maker"))
          ),
          
          hr(),
          
          h5("Examples of unparsed Date values (sample)"),
          DTOutput("dq_unparsed_examples")
        )
      )
    )
  )
)

# ---------- Server ----------

server <- function(input, output, session) {
  
  df_all <- reactiveVal(NULL)
  
  observeEvent(TRUE, {
    df <- read_bate_csv(csv_path())
    df_all(df)
    
    updateSelectInput(session, "class",
                      choices = c("All"="__ALL__", sort(unique(na.omit(df$Class)))),
                      selected = "__ALL__")
    
    makers <- sort(unique(na.omit(df$Maker)))
    updateSelectizeInput(session, "maker",
                         choices = c("All"="__ALL__", makers),
                         selected = "__ALL__",
                         server = TRUE)
    
    conds <- sort(unique(na.omit(df$Condition)))
    updateSelectInput(session, "condition",
                      choices = c("All"="__ALL__", conds),
                      selected = "__ALL__")
  }, once = TRUE)
  
  df_filtered <- reactive({
    df <- df_all()
    req(df)
    
    out <- df
    
    if (!identical(input$class, "__ALL__")) {
      out <- out %>% filter(.data$Class == input$class)
    }
    if (!identical(input$maker, "__ALL__")) {
      out <- out %>% filter(.data$Maker == input$maker)
    }
    if (!identical(input$playable, "__ALL__")) {
      out <- out %>% filter(.data$Playable == input$playable)
    }
    if (!identical(input$condition, "__ALL__")) {
      out <- out %>% filter(.data$Condition == input$condition)
    }
    if (isTRUE(input$only_with_media)) {
      out <- out %>% filter(.data$.has_image | .data$.has_audio)
    }
    
    q <- trimws(input$q)
    if (nzchar(q)) {
      q_lower <- tolower(q)
      out <- out %>%
        filter(
          str_detect(tolower(coalesce(.data$Name, "")), fixed(q_lower)) |
            str_detect(tolower(coalesce(.data$Description, "")), fixed(q_lower)) |
            str_detect(tolower(coalesce(.data$Maker, "")), fixed(q_lower)) |
            str_detect(tolower(coalesce(.data$Material, "")), fixed(q_lower)) |
            str_detect(tolower(coalesce(.data$Provenance, "")), fixed(q_lower))
        )
    }
    
    out
  })
  
  # --- summary plot (with override control) ---
  output$plot1 <- renderPlot({
    df <- df_filtered()
    req(df)
    
    mode <- if (is.null(input$plot_mode)) "auto" else input$plot_mode
    
    # Explicit user override modes
    if (mode == "timeline") {
      p <- timeline_plot_if_informative(df)
      if (!is.null(p)) return(p)
      return(topN_plot(df, "Material", "Materials (timeline unavailable here)", n = 15))
    }
    if (mode == "class")    return(topN_plot(df, "Class",    "Instrument classes", n = 15))
    if (mode == "material") return(topN_plot(df, "Material", "Materials",          n = 15))
    if (mode == "maker")    return(topN_plot(df, "Maker",    "Makers (top 15)",    n = 15))
    if (mode == "playable") return(topN_plot(df, "Playable", "Playable?",          n = 10))
    
    # Auto mode (what varies)
    if (isTRUE(input$show_year_plot)) {
      p <- timeline_plot_if_informative(df)
      if (!is.null(p)) return(p)
    }
    
    # If Class is pinned, show Material first (more consistent), else Maker, else Playable
    if (!identical(input$class, "__ALL__")) {
      n_mat <- dplyr::n_distinct(df$Material, na.rm = TRUE)
      if (n_mat >= 3) return(topN_plot(df, "Material", "Materials (within current filter)", n = 15))
      
      n_makers <- dplyr::n_distinct(df$Maker, na.rm = TRUE)
      if (n_makers >= 3) return(topN_plot(df, "Maker", "Makers (within current filter)", n = 15))
      
      return(topN_plot(df, "Playable", "Playable? (within current filter)", n = 10))
    }
    
    # Overall summary
    n_classes <- dplyr::n_distinct(df$Class, na.rm = TRUE)
    if (n_classes >= 3) return(topN_plot(df, "Class", "Instrument classes (top 15)", n = 15))
    
    topN_plot(df, "Material", "Materials (top 15)", n = 15)
  })
  
  output$summary_text <- renderUI({
    df <- df_filtered()
    req(df)
    df_all_ <- df_all()
    req(df_all_)
    
    n <- nrow(df)
    n_total <- nrow(df_all_)
    n_year <- sum(!is.na(df$.year))
    n_img <- sum(df$.has_image)
    n_audio <- sum(df$.has_audio)
    
    HTML(sprintf(
      "<p><b>%s</b> records shown (of %s total).<br/>
       Parsable year: %s | Has image: %s | Has audio: %s</p>",
      format(n, big.mark = ","),
      format(n_total, big.mark = ","),
      format(n_year, big.mark = ","),
      format(n_img, big.mark = ","),
      format(n_audio, big.mark = ",")
    ))
  })
  
  # --- table ---
  output$tbl <- renderDT({
    df <- df_filtered()
    req(df)
    
    show_cols <- c("Number","Name","Class","Maker","Date","Location","Playable","Condition","Image","Sound Recording")
    
    df_show <- df %>%
      mutate(
        Image = vapply(Image, make_clickable_url, character(1)),
        `Sound Recording` = vapply(`Sound Recording`, make_clickable_url, character(1))
      ) %>%
      select(any_of(c(".row_id", show_cols)))
    
    datatable(
      df_show,
      escape = FALSE,
      selection = "single",
      options = list(
        pageLength = 12,
        lengthMenu = c(12, 25, 50, 100),
        scrollX = TRUE
      )
    )
  })
  
  selected_row <- reactive({
    df <- df_filtered()
    req(df)
    s <- input$tbl_rows_selected
    if (length(s) != 1) return(NULL)
    df[s, , drop = FALSE]
  })
  
  # --- details panel ---
  output$details <- renderUI({
    row <- selected_row()
    if (is.null(row)) {
      return(HTML("<p><i>Select a row in the table to see details.</i></p>"))
    }
    
    r <- row[1, ]
    img_url <- str_extract(as.character(r$Image), "https?://\\S+")
    aud_url <- str_extract(as.character(r$`Sound Recording`), "https?://\\S+")
    
    pieces <- list(
      tags$div(
        tags$b("Number: "), tags$span(first_nonempty(r$Number)), tags$br(),
        tags$b("Name: "), tags$span(first_nonempty(r$Name)), tags$br(),
        tags$b("Class: "), tags$span(first_nonempty(r$Class)), tags$br(),
        tags$b("Maker: "), tags$span(first_nonempty(r$Maker)), tags$br(),
        tags$b("Date: "), tags$span(first_nonempty(r$Date)), tags$br(),
        tags$b("Location: "), tags$span(first_nonempty(r$Location)), tags$br(),
        tags$b("Playable: "), tags$span(first_nonempty(r$Playable)), tags$br(),
        tags$b("Condition: "), tags$span(first_nonempty(r$Condition))
      ),
      tags$hr(),
      tags$div(
        tags$b("Description"), tags$br(),
        tags$div(style="white-space: pre-wrap;", first_nonempty(r$Description))
      )
    )
    
    if (!is.na(first_nonempty(r$`Longer Description`))) pieces <- c(pieces, list(
      tags$hr(),
      tags$b("Longer Description"), tags$br(),
      tags$div(style="white-space: pre-wrap;", first_nonempty(r$`Longer Description`))
    ))
    if (!is.na(first_nonempty(r$Material))) pieces <- c(pieces, list(
      tags$hr(),
      tags$b("Material"), tags$br(),
      tags$div(style="white-space: pre-wrap;", first_nonempty(r$Material))
    ))
    if (!is.na(first_nonempty(r$Dimensions))) pieces <- c(pieces, list(
      tags$hr(),
      tags$b("Dimensions"), tags$br(),
      tags$div(style="white-space: pre-wrap;", first_nonempty(r$Dimensions))
    ))
    if (!is.na(first_nonempty(r$Provenance))) pieces <- c(pieces, list(
      tags$hr(),
      tags$b("Provenance"), tags$br(),
      tags$div(style="white-space: pre-wrap;", first_nonempty(r$Provenance))
    ))
    
    media <- list()
    if (!is.na(img_url) && nzchar(img_url)) {
      media <- c(media, list(
        tags$hr(),
        tags$b("Image"), tags$br(),
        tags$img(src = img_url, style = "max-width: 100%; height: auto; border-radius: 6px;"),
        tags$br(),
        HTML(make_clickable_url(img_url))
      ))
    } else if (!is.na(first_nonempty(r$Image))) {
      media <- c(media, list(
        tags$hr(),
        tags$b("Image link"), tags$br(),
        HTML(make_clickable_url(first_nonempty(r$Image)))
      ))
    }
    
    if (!is.na(aud_url) && nzchar(aud_url)) {
      media <- c(media, list(
        tags$hr(),
        tags$b("Sound Recording"), tags$br(),
        tags$audio(controls = NA, src = aud_url, style = "width: 100%;"),
        tags$br(),
        HTML(make_clickable_url(aud_url))
      ))
    } else if (!is.na(first_nonempty(r$`Sound Recording`))) {
      media <- c(media, list(
        tags$hr(),
        tags$b("Sound Recording link"), tags$br(),
        HTML(make_clickable_url(first_nonempty(r$`Sound Recording`)))
      ))
    }
    
    do.call(tagList, c(pieces, media))
  })
  
  # ---------- Data quality tab ----------
  
  output$dq_headline <- renderUI({
    df <- df_all()
    req(df)
    
    n <- nrow(df)
    date_present <- !is.na(df$Date) & trimws(df$Date) != ""
    n_year <- sum(date_present & !is.na(df$.year))
    n_unparsed <- sum(date_present & is.na(df$.year))
    
    HTML(sprintf(
      "<p>Total records: <b>%s</b><br/>
         Date present but unparsed (no 4-digit year found): <b>%s</b><br/>
         Parsable year: <b>%s</b></p>",
      format(n, big.mark = ","),
      format(n_unparsed, big.mark = ","),
      format(n_year, big.mark = ",")
    ))
  })
  
  output$dq_missing_tbl <- renderDT({
    df <- df_all()
    req(df)
    
    miss <- data.frame(
      Column = names(df),
      MissingOrBlank = vapply(df, function(x) sum(is.na(x) | trimws(as.character(x)) == ""), integer(1)),
      stringsAsFactors = FALSE
    ) %>%
      mutate(Percent = round(100 * MissingOrBlank / nrow(df), 1)) %>%
      arrange(desc(Percent))
    
    datatable(miss, options = list(pageLength = 12), rownames = FALSE)
  })
  
  output$dq_date_tbl <- renderDT({
    df <- df_all()
    req(df)
    
    date_present <- !is.na(df$Date) & trimws(df$Date) != ""
    parsed <- date_present & !is.na(df$.year)
    unparsed <- date_present & is.na(df$.year)
    empty <- !date_present
    
    tab <- data.frame(
      Category = c("Date empty/blank", "Date parsed (has 4-digit year)", "Date present but unparsed"),
      Count = c(sum(empty), sum(parsed), sum(unparsed)),
      stringsAsFactors = FALSE
    ) %>%
      mutate(Percent = round(100 * Count / nrow(df), 1))
    
    datatable(tab, options = list(dom = "t"), rownames = FALSE)
  })
  
  output$dq_top_class <- renderDT({
    df <- df_all()
    req(df)
    
    top <- df %>%
      mutate(Class = ifelse(is.na(Class) | trimws(Class) == "", "(blank)", as.character(Class))) %>%
      count(Class, sort = TRUE) %>%
      head(20)
    
    datatable(top, options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$dq_top_maker <- renderDT({
    df <- df_all()
    req(df)
    
    top <- df %>%
      mutate(Maker = ifelse(is.na(Maker) | trimws(Maker) == "", "(blank)", as.character(Maker))) %>%
      count(Maker, sort = TRUE) %>%
      head(20)
    
    datatable(top, options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$dq_unparsed_examples <- renderDT({
    df <- df_all()
    req(df)
    
    ex <- df %>%
      filter(!is.na(Date), trimws(Date) != "", is.na(.year)) %>%
      select(Number, Name, Maker, Date) %>%
      distinct() %>%
      head(50)
    
    datatable(ex, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
}

shinyApp(ui, server)

# End of app.R