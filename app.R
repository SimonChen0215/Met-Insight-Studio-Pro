# =============================================================================
# Met Data Processor  (v2)
# Upload met data -> map columns -> QC -> wind roses / temperature / rainfall
# All tables, figures and exports follow the date range selected in the sidebar.
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(openair)
  library(dplyr)
  library(readr)
  library(readxl)
  library(lubridate)
  library(ggplot2)
  library(DT)
})

# Increase file upload size limit (100MB)
options(shiny.maxRequestSize = 100 * 1024^2)

# -----------------------------------------------------------------------------
# Constants (one place for all plot settings -> screen and PDF stay identical)
# -----------------------------------------------------------------------------
WR_COLS   <- c("#E5EBEF", "#BFCDD7", "#7F9BB0", "#3F6889", "#003763")
WR_BREAKS <- c(0.49999, 0.999999, 2.99999, 4.9999, 7.4999)
WR_LABELS <- c("0.5 to 1", "1 to 3", "3 to 5", "5 to 7.5", ">7.5")

WR_SETTINGS <- list(
  default = list(label = "Overall",        type = "default", angle = 22.5, grid.line = 2.5, max.freq = NULL),
  month   = list(label = "By Month",       type = "month",   angle = 22.5, grid.line = 10,  max.freq = NULL),
  year    = list(label = "By Year",        type = "year",    angle = 22.5, grid.line = 10,  max.freq = NULL),
  season  = list(label = "By Season",      type = "season",  angle = 22.5, grid.line = 10,  max.freq = NULL),
  tod     = list(label = "By Time of Day", type = "ToD",     angle = 22.5, grid.line = 10,  max.freq = NULL),
  hour    = list(label = "By Hour",        type = "hour",    angle = 10,   grid.line = 5,   max.freq = NULL)
)
WR_MANUAL_DEFAULTS <- c(default = 40, month = 35, year = 40, season = 35, tod = 35, hour = 20)

COL_DARK  <- "#003763"
COL_MID   <- "#7F9BB0"

TZ_CHOICES <- c("Australia/Hobart", "Australia/Sydney", "Australia/Melbourne",
                "Australia/Brisbane", "Australia/Adelaide", "Australia/Darwin",
                "Australia/Perth",
                "AEST, no daylight saving (Etc/GMT-10)" = "Etc/GMT-10",
                "UTC")

VAR_LABELS <- c(temp = "Temperature", ws = "Wind speed", wd = "Wind direction", mm = "Rainfall")

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------

# Guess a column from its name (used only as a default; user can change it)
guess_col <- function(cols, patterns) {
  for (p in patterns) {
    hit <- grep(p, cols, ignore.case = TRUE, value = TRUE)
    if (length(hit) > 0) return(hit[1])
  }
  ""
}

# Read uploaded file
read_input_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  na_vals <- c("", "NA", "Null", "NULL")
  if (ext %in% c("xlsx", "xls")) {
    as.data.frame(read_excel(path, guess_max = 1e6, na = na_vals))
  } else if (ext == "csv") {
    as.data.frame(read_csv(path, show_col_types = FALSE, guess_max = 1e6, na = na_vals))
  } else if (ext %in% c("dat", "txt")) {
    df <- tryCatch(read.csv(path, comment.char = "#", stringsAsFactors = FALSE, check.names = FALSE),
                   error = function(e) NULL)
    if (is.null(df) || ncol(df) <= 1) {
      df <- read.delim(path, comment.char = "#", stringsAsFactors = FALSE, check.names = FALSE)
    }
    df
  } else {
    stop("Unsupported file type: ", ext)
  }
}

# Parse datetime using ONLY the selected order (no mixing dmy / ymd / mdy)
parse_datetime_col <- function(x, ord, tz) {
  if (inherits(x, "POSIXt")) return(force_tz(as.POSIXct(x), tzone = tz))
  if (inherits(x, "Date"))   return(force_tz(as.POSIXct(x), tzone = tz))
  if (is.numeric(x)) {
    # Excel serial date number -> rounded to the nearest minute
    secs <- round((x - 25569) * 1440) * 60
    return(force_tz(as.POSIXct(secs, origin = "1970-01-01", tz = "UTC"), tzone = tz))
  }
  parse_date_time(trimws(as.character(x)),
                  orders = paste0(ord, c(" HMS", " HM", " H", "")),
                  tz = tz, quiet = TRUE)
}

# Convert to numeric and flag blank / invalid cells
#   blank   = empty cell / whitespace / NA / Null / NaN / N/A
#   invalid = has content but is not a number (e.g. "Calm", "---", "ERR")
qc_numeric <- function(x) {
  raw   <- trimws(as.character(x))
  blank <- is.na(raw) | raw == "" | toupper(raw) %in% c("NA", "NULL", "NAN", "N/A")
  num   <- suppressWarnings(as.numeric(gsub("[^0-9.eE+-]", "", raw)))
  num[blank] <- NA
  list(value = num, blank = blank, invalid = !blank & is.na(num))
}

fmt_period <- function(s, e) paste(format(s, "%d/%m/%Y"), "to", format(e, "%d/%m/%Y"))

# ---- Wind rose (used by screen and PDF) ----
plot_windrose <- function(df, key, title = "", max.freq = NULL) {
  s  <- WR_SETTINGS[[key]]
  wr <- df[, c("date", "ws", "wd")]
  wr <- wr[complete.cases(wr), ]
  if (nrow(wr) == 0) {
    plot.new(); text(0.5, 0.5, "No valid wind data in the selected date range", cex = 1.5)
    return(invisible(NULL))
  }
  if (key == "tod") {
    # [00,06) [06,12) [12,18) [18,24)  -> each block is exactly 6 hours
    wr$ToD <- cut(hour(wr$date), breaks = c(0, 6, 12, 18, 24), right = FALSE,
                  labels = c("00:00-06:00", "06:00-12:00", "12:00-18:00", "18:00-24:00"))
  }
  args <- list(mydata = wr, type = s$type, main = title,
               breaks = WR_BREAKS, dig.lab = 2,
               hemisphere = "southern", annotate = TRUE, angle = s$angle,
               key = list(labels = WR_LABELS), cols = WR_COLS,
               grid.line = s$grid.line, offset = 2, paddle = FALSE,
               border = "black", key.position = "left")
  if (!is.null(max.freq)) args$max.freq <- max.freq
  do.call(windRose, args)
  invisible(NULL)
}

# ---- Temperature monthly statistics ----
#   avg = mean of all records in the calendar month
#   min / max = median of daily minimum / maximum in the calendar month
calc_temp_monthly <- function(df, tz) {
  d <- df[!is.na(df$temp), c("date", "temp")]
  if (nrow(d) == 0) return(NULL)
  d$day <- as.Date(d$date, tz = tz)
  d$m   <- month(d$date)

  avg <- d %>% group_by(m) %>% summarise(avg = mean(temp), .groups = "drop")
  daily <- d %>% group_by(day, m) %>%
    summarise(dmin = min(temp), dmax = max(temp), .groups = "drop")
  mm <- daily %>% group_by(m) %>%
    summarise(min = median(dmin), max = median(dmax), .groups = "drop")

  out <- inner_join(avg, mm, by = "m")
  out$date <- as.POSIXct(sprintf("2020-%02d-01", out$m), tz = "UTC")
  out
}

plot_temperature <- function(df, tz, base_size = 14, txt_size = 4, title = NULL) {
  pd <- calc_temp_monthly(df, tz)
  if (is.null(pd)) {
    plot.new(); text(0.5, 0.5, "No valid temperature data in the selected date range", cex = 1.5)
    return(invisible(NULL))
  }
  ggplot(pd) +
    geom_segment(aes(x = date, xend = date, y = min + 0.1, yend = max - 0.1,
                     colour = "Monthly range"), size = 19) +
    geom_text(aes(x = date, y = min + 1.1, label = sprintf("%.1f", min)),
              colour = "white", size = txt_size) +
    geom_text(aes(x = date, y = max - 1, label = sprintf("%.1f", max)),
              colour = "white", size = txt_size) +
    geom_line(aes(x = date, y = avg, colour = "Monthly average"), size = 1.5) +
    scale_x_datetime(name = "", date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(name = "Temperature (°C)", breaks = seq(-10, 100, 5)) +
    scale_colour_manual("", values = c("Monthly range" = COL_DARK,
                                       "Monthly average" = "dark grey")) +
    labs(title = title) +
    theme_light(base_size = base_size) +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(override.aes = list(size = 5)))
}

# ---- Rainfall monthly statistics ----
#   daily total -> rain day if >= 0.25 mm -> monthly total per year -> mean across years
calc_rain_monthly <- function(df, tz) {
  d <- df[!is.na(df$mm), c("date", "mm")]
  if (nrow(d) == 0) return(NULL)
  d$day <- as.Date(d$date, tz = tz)

  daily <- d %>% group_by(day) %>% summarise(mm = sum(mm), .groups = "drop") %>%
    mutate(rd = as.numeric(mm >= 0.25), y = year(day), m = month(day))

  out <- daily %>% group_by(y, m) %>%
    summarise(mm = sum(mm), rd = sum(rd), .groups = "drop") %>%
    group_by(m) %>%
    summarise(mm = mean(mm), RainDays = mean(rd), .groups = "drop")

  out$date <- as.POSIXct(sprintf("2020-%02d-01", out$m), tz = "UTC")
  out
}

plot_rainfall <- function(df, tz, base_size = 14, txt_size = 4, title = NULL) {
  rd <- calc_rain_monthly(df, tz)
  if (is.null(rd)) {
    plot.new(); text(0.5, 0.5, "No valid rainfall data in the selected date range", cex = 1.5)
    return(invisible(NULL))
  }
  adjust <- if (max(rd$RainDays) == 0) 1 else max(rd$mm) / max(rd$RainDays) + 2

  ggplot(rd) +
    geom_bar(aes(x = date, y = mm, fill = "Rainfall amount"), stat = "identity") +
    geom_text(aes(x = date, y = pmax(mm - 5, 0), label = sprintf("%.0f", mm)),
              colour = "white", size = txt_size, vjust = 0) +
    geom_line(aes(x = date, y = RainDays * adjust, colour = "Days of rainfall", group = 1),
              size = 1.5) +
    geom_text(aes(x = date, y = RainDays * adjust, label = sprintf("%.1f", RainDays)),
              nudge_y = 10, size = txt_size) +
    scale_y_continuous(name = "Monthly rainfall (mm)", breaks = seq(0, 1000, 20),
                       sec.axis = sec_axis(~ . / adjust,
                                           name = "Days with rainfall >0.25mm",
                                           breaks = seq(0, 100, 2))) +
    scale_x_datetime(name = "", date_breaks = "1 month", date_labels = "%b",
                     expand = c(0.01, 0.01)) +
    scale_fill_manual("", values = c("Rainfall amount" = COL_DARK)) +
    scale_colour_manual("", values = c("Days of rainfall" = COL_MID)) +
    labs(title = title) +
    theme_light(base_size = base_size) +
    theme(legend.position = "bottom")
}

# ---- Stability class: cloud data (BoM hourly, LST) ----
#   expects columns: date (YYYY-MM-DD HH:MM), CA (tenths), CH (feet)
read_cloud_file <- function(path) {
  read_csv(path, col_types = cols(.default = col_character())) %>%
    transmute(date = ymd_hm(date, tz = "UTC"),
              CA = suppressWarnings(as.numeric(CA)),
              CH = suppressWarnings(as.numeric(CH))) %>%
    filter(!is.na(date)) %>%
    distinct(date, .keep_all = TRUE)
}

# ---- Stability class: Pasquill-Gifford (Turner method, US EPA) ----
#   df: met data with date (POSIXct, local) and ws (m/s); lat: site latitude (negative = south)
calc_stability <- function(df, cloud, lat) {
  LST_TZ <- "Etc/GMT-10"   # UTC+10, no daylight saving

  # Met data converted to LST, then hourly means (hour-beginning)
  met_hr <- df[, c("date", "ws")] %>%
    filter(!is.na(date)) %>%
    mutate(date = force_tz(with_tz(date, LST_TZ), "UTC")) %>%
    distinct(date, .keep_all = TRUE) %>%
    mutate(date = floor_date(date, "hour")) %>%
    group_by(date) %>%
    summarise(ws = mean(ws, na.rm = TRUE), .groups = "drop") %>%
    mutate(ws = ifelse(is.nan(ws), NA, ws))

  MetData <- inner_join(met_hr, cloud, by = "date") %>%
    filter(!is.na(ws), !is.na(CA))

  if (nrow(MetData) == 0) return(NULL)

  # Sun elevation -> insolation class
  d2r <- pi / 180
  MetData <- MetData %>%
    mutate(
      Day   = pmin(yday(date), 365),
      delta = -23.45 * cos((360 / 365) * (Day + 10) * d2r),
      SAh   = ifelse(hour(date) == 0, 11.5, hour(date) - 12.5),
      SA    = asin(cos(d2r * 15 * SAh) * cos(d2r * delta) * cos(d2r * lat) +
                   sin(d2r * delta) * sin(d2r * lat)) / d2r,
      InsolationClass = case_when(SA <= 0  ~ 0,
                                  SA > 60  ~ 4,
                                  SA >= 35 ~ 3,
                                  SA >= 15 ~ 2,
                                  TRUE     ~ 1),
      DayNight = ifelse(InsolationClass == 0, "Night", "Day")
    )

  # Turner method: NRI (Table 6-6), wind class (Table 6-4), PG class
  MetData <- MetData %>%
    mutate(
      ins = InsolationClass,
      NRI = case_when(
        CA == 10 & CH < 7000                                          ~ 0,
        DayNight == "Night" & CA <= 4                                 ~ -2,
        DayNight == "Night" & CA > 4                                  ~ -1,
        DayNight == "Day" & CA <= 5                                   ~ ins,
        DayNight == "Day" & CA > 5 & CH < 7000 & ins > 2              ~ ins - 2,
        DayNight == "Day" & CA > 5 & CH < 7000                        ~ 1,
        DayNight == "Day" & CA > 5 & between(CH, 7000, 160000) & ins > 1 ~ ins - 1,
        DayNight == "Day" & CA > 5 & between(CH, 7000, 160000)        ~ 1,
        TRUE                                                          ~ ins
      ),
      ws1 = round(ws, 1),
      wsClass = case_when(ws1 <= 0.7 ~ 1, ws1 <= 1.8 ~ 2, ws1 <= 2.8 ~ 3,
                          ws1 <= 3.3 ~ 4, ws1 <= 3.8 ~ 5, ws1 <= 4.8 ~ 6,
                          ws1 <= 5.4 ~ 7, ws1 <= 5.9 ~ 8, TRUE ~ 9),
      PG = case_when(
        NRI == 4  & wsClass <= 3 ~ 1,
        NRI == 4  & wsClass <= 6 ~ 2,
        NRI == 4                 ~ 3,
        NRI == 3  & wsClass == 1 ~ 1,
        NRI == 3  & wsClass <= 5 ~ 2,
        NRI == 3  & wsClass <= 8 ~ 3,
        NRI == 3                 ~ 4,
        NRI == 2  & wsClass <= 2 ~ 2,
        NRI == 2  & wsClass <= 6 ~ 3,
        NRI == 2                 ~ 4,
        NRI == 1  & wsClass <= 2 ~ 3,
        NRI == 1                 ~ 4,
        NRI == 0                 ~ 4,
        NRI == -1 & wsClass <= 2 ~ 6,
        NRI == -1 & wsClass <= 4 ~ 5,
        NRI == -1                ~ 4,
        NRI == -2 & wsClass <= 2 ~ 7,
        NRI == -2 & wsClass <= 4 ~ 6,
        NRI == -2 & wsClass <= 7 ~ 5,
        NRI == -2                ~ 4
      ),
      # G is merged into F (EPA)
      PG_EPA = factor(LETTERS[pmin(PG, 6)], levels = LETTERS[1:6])
    )

  counts <- MetData %>%
    count(class = PG_EPA, .drop = FALSE) %>%
    mutate(percentage = n / sum(n) * 100)

  list(data = MetData, counts = counts, n = nrow(MetData),
       period = paste(format(min(MetData$date), "%d/%m/%Y"), "to",
                      format(max(MetData$date), "%d/%m/%Y")))
}

# ---- Stability class frequency bar chart ----
plot_stability <- function(counts, base_size = 14, title = NULL) {
  ggplot(counts, aes(x = class, y = percentage)) +
    geom_col(fill = COL_DARK) +
    geom_text(aes(label = paste0(sprintf("%0.2g", percentage), "%")), vjust = -0.5) +
    scale_y_continuous(name = "Percentage (%)") +
    scale_x_discrete(name = NULL, drop = FALSE) +
    labs(title = title) +
    theme_light(base_size = base_size)
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Met Data Processor"),

  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("1. Upload Data",   tabName = "upload",  icon = icon("upload")),
      menuItem("2. Check Data",    tabName = "check",   icon = icon("check-circle")),
      menuItem("3. Wind Roses",    tabName = "wind",    icon = icon("wind")),
      menuItem("4. Climate Plots", tabName = "climate", icon = icon("cloud-sun-rain")),
      menuItem("5. Stability",     tabName = "stability", icon = icon("cloud")),
      menuItem("6. Export",        tabName = "export",  icon = icon("file-pdf"))
    ),
    hr(),
    # Date range controls appear only after data are processed
    conditionalPanel(
      condition = "output.data_ready",
      dateRangeInput("date_range", "Date Range:",
                     format = "dd/mm/yyyy", separator = " to ", weekstart = 1),
      div(style = "padding: 0 15px;",
          actionButton("reset_range", "Reset to Full Range",
                       icon = icon("undo"), class = "btn-sm", width = "80%")),
      div(style = "padding: 8px 15px 0 15px; font-size: 12px; color: #ccc;",
          textOutput("range_info")),
      div(style = "padding: 0 15px;",
          checkboxInput("show_period", "Show period on figures", value = FALSE))
    ),
    conditionalPanel(
      condition = "!output.data_ready",
      div(style = "padding: 0 15px; font-size: 12px; color: #aaa;",
          "Upload and process a file to enable the date range filter.")
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .box { border-radius: 4px; }
      pre { font-size: 12px; }
      .small-note { font-size: 12px; color: #777; }
      .table-qc td, .table-qc th { padding: 4px 10px !important; }
    "))),

    tabItems(
      # ---- Upload tab ----
      tabItem(tabName = "upload",
        fluidRow(
          box(width = 6, title = "1. Upload File", status = "primary", solidHeader = TRUE,
              fileInput("file", "Choose File (CSV, XLSX, XLS, DAT, TXT)",
                        accept = c(".csv", ".xlsx", ".xls", ".dat", ".txt")),
              hr(),
              h4("Detected Columns:"),
              verbatimTextOutput("detected_cols")
          ),
          box(width = 6, title = "2. Column Mapping", status = "info", solidHeader = TRUE,
              p("Columns are pre-selected from their names where possible. Please check before processing."),
              selectInput("datetime_col", "DateTime Column *", choices = NULL, width = "100%"),
              selectInput("dt_format", "DateTime Format *",
                          choices = c("Day/Month/Year (AU, e.g. 1/01/2014 11:30)" = "dmy",
                                      "Year-Month-Day (e.g. 2014-01-01 11:30)"   = "ymd",
                                      "Month/Day/Year (US)"                      = "mdy"),
                          selected = "dmy", width = "100%"),
              selectInput("temp_col", "Temperature Column *",       choices = NULL, width = "100%"),
              selectInput("ws_col",   "Wind Speed Column *",        choices = NULL, width = "100%"),
              selectInput("wd_col",   "Wind Direction Column *",    choices = NULL, width = "100%"),
              selectInput("rain_col", "Rainfall Column (Optional)", choices = NULL, width = "100%"),
              selectizeInput("timezone", "Timezone *", choices = TZ_CHOICES,
                             selected = "Australia/Hobart", width = "100%",
                             options = list(create = TRUE)),
              p(class = "small-note",
                "If the logger records in standard time all year (no daylight saving), ",
                "use 'AEST, no daylight saving' to avoid missing/duplicate hours at DST changes."),
              actionButton("process", "Process Data", class = "btn-success btn-lg btn-block",
                           icon = icon("cogs"))
          )
        )
      ),

      # ---- Check tab ----
      tabItem(tabName = "check",
        fluidRow(
          valueBoxOutput("vb_period",   width = 3),
          valueBoxOutput("vb_records",  width = 3),
          valueBoxOutput("vb_interval", width = 3),
          valueBoxOutput("vb_missing",  width = 3)
        ),
        fluidRow(
          box(width = 7, title = "Data Quality (selected range)", status = "warning", solidHeader = TRUE,
              div(class = "table-qc", tableOutput("qc_table")),
              p(class = "small-note",
                strong("Blank"), " = empty / NA / Null cell;  ",
                strong("Invalid"), " = non-numeric text;  ",
                strong("Missing timestamps"), " = time step absent from the file;  ",
                strong("Capture"), " = valid records / expected records.")
          ),
          box(width = 5, title = "Data Summary (selected range)", status = "info", solidHeader = TRUE,
              verbatimTextOutput("data_summary")
          )
        ),
        fluidRow(
          box(width = 12, title = "Data Preview (selected range)", status = "primary", solidHeader = TRUE,
              DTOutput("data_preview")
          )
        )
      ),

      # ---- Wind tab ----
      tabItem(tabName = "wind",
        fluidRow(
          box(width = 12, title = "Wind Rose Settings", status = "primary", solidHeader = TRUE,
              checkboxGroupInput("wind_selected", "Wind Roses:",
                                 choices = setNames(names(WR_SETTINGS),
                                                    sapply(WR_SETTINGS, `[[`, "label")),
                                 selected = names(WR_SETTINGS), inline = TRUE),
              actionButton("wind_all", "Select All", icon = icon("check-square")),
              actionButton("wind_none", "Clear", icon = icon("times")),
              conditionalPanel(
                condition = "input.wind_selected && input.wind_selected.length > 0",
                radioButtons("wind_scale", "Frequency Scale:",
                             choices = c("Auto" = "auto", "Manual" = "manual"),
                             selected = "auto", inline = TRUE),
                conditionalPanel(
                  condition = "input.wind_scale === 'manual'",
                  fluidRow(lapply(names(WR_SETTINGS), function(key) {
                    column(4, conditionalPanel(
                      condition = sprintf("input.wind_selected && input.wind_selected.indexOf('%s') >= 0", key),
                      numericInput(paste0("max_freq_", key),
                                   paste0(WR_SETTINGS[[key]]$label, " - Maximum frequency (%)"),
                                   value = WR_MANUAL_DEFAULTS[[key]], min = 0.1, max = 100, step = 0.5)
                    ))
                  }))
                )
              )
          )
        ),
        fluidRow(
          box(width = 12, title = uiOutput("wind_title", inline = TRUE),
              status = "primary", solidHeader = TRUE,
              selectInput("wind_type", "Wind Rose Type:",
                          choices = setNames(names(WR_SETTINGS),
                                             sapply(WR_SETTINGS, `[[`, "label")),
                          selected = "default"),
              plotOutput("windrose_plot", height = "700px")
          )
        )
      ),

      # ---- Climate tab ----
      tabItem(tabName = "climate",
        fluidRow(
          box(width = 12, title = "Climate Plot Selection", status = "primary", solidHeader = TRUE,
              checkboxGroupInput("climate_selected", "Climate Plots:",
                                 choices = c("Temperature" = "temp", "Rainfall" = "rain"),
                                 selected = c("temp", "rain"), inline = TRUE)
          )
        ),
        conditionalPanel(condition = "input.climate_selected && input.climate_selected.indexOf('temp') >= 0",
        fluidRow(
          box(width = 12, title = uiOutput("temp_title", inline = TRUE),
              status = "danger", solidHeader = TRUE,
              plotOutput("temp_plot", height = "500px")
          )
        )),
        conditionalPanel(condition = "input.climate_selected && input.climate_selected.indexOf('rain') >= 0",
        fluidRow(
          box(width = 12, title = uiOutput("rain_title", inline = TRUE),
              status = "info", solidHeader = TRUE,
              conditionalPanel(
                condition = "output.has_rain",
                plotOutput("rain_plot", height = "500px")
              ),
              conditionalPanel(
                condition = "!output.has_rain",
                h4("No rainfall data in the selected date range",
                   style = "text-align:center; color:#999;")
              )
          )
        ))
      ),

      # ---- Stability tab ----
      tabItem(tabName = "stability",
        fluidRow(
          box(width = 5, title = "Stability Inputs", status = "primary", solidHeader = TRUE,
              fileInput("cloud_file", "Cloud data CSV (date, CA, CH)", accept = ".csv"),
              numericInput("stab_lat", "Site latitude (degrees, negative = south)",
                           value = -42.83, min = -90, max = 90, step = 0.01, width = "100%"),
              checkboxInput("stab_in_pdf", "Include stability plot in PDF report", value = TRUE),
              p(class = "small-note",
                "Uses the date range selected in the sidebar. Met wind speed is converted to LST (UTC+10) before matching to cloud hours.")
          ),
          box(width = 7, title = "Stability Class Frequency", status = "info", solidHeader = TRUE,
              plotOutput("stab_plot", height = "450px")
          )
        ),
        fluidRow(
          box(width = 12, title = "Stability Class Table", status = "info", solidHeader = TRUE,
              tableOutput("stab_table"),
              downloadButton("download_stab_csv", "Download CSV", class = "btn-info")
          )
        )
      ),

      # ---- Export tab ----
      tabItem(tabName = "export",
        fluidRow(
          box(width = 12, title = "Export", status = "success", solidHeader = TRUE,
              h4("Export period: ", textOutput("export_period", inline = TRUE)),
              p(class = "small-note", "All exports use the date range selected in the sidebar."),
              hr(),
              h4("PDF report includes:"),
              uiOutput("export_summary"),
              textInput("export_filename", "PDF Filename Prefix:", value = "met_analysis"),
              textOutput("pdf_filename"),
              uiOutput("pdf_download_ui"),
              hr(),
              h4("Processed data:"),
              downloadButton("download_csv", "Download CSV", class = "btn-info")
          )
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(
    raw_data = NULL,   # file as read
    data     = NULL,   # processed: date, temp, ws, wd, (mm)
    qc       = NULL,   # blank / invalid flags, row-aligned with data
    day      = NULL,   # local date of each row (for fast filtering)
    tz       = NULL    # timezone used at processing time
  )

  # ---- Read file ----
  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      rv$raw_data <- read_input_file(input$file$datapath, input$file$name)
      rv$data <- NULL; rv$qc <- NULL; rv$day <- NULL

      cols <- names(rv$raw_data)
      opt  <- c("-- Select Column --" = "", cols)

      updateSelectInput(session, "datetime_col", choices = opt,
                        selected = guess_col(cols, c("^date", "time", "^dt$")))
      updateSelectInput(session, "temp_col", choices = opt,
                        selected = guess_col(cols, c("^temp", "air.?temp", "^t$")))
      updateSelectInput(session, "ws_col", choices = opt,
                        selected = guess_col(cols, c("^ws$", "wind.?speed", "^ws")))
      updateSelectInput(session, "wd_col", choices = opt,
                        selected = guess_col(cols, c("^wd$", "wind.?dir", "^wd")))
      updateSelectInput(session, "rain_col", choices = c("-- None (Optional) --" = "", cols),
                        selected = guess_col(cols, c("^rain", "^mm$", "precip")))

      showNotification("File uploaded. Check the column mapping, then click 'Process Data'.",
                       type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error", duration = 8)
    })
  })

  output$detected_cols <- renderPrint({
    req(rv$raw_data)
    cols <- names(rv$raw_data)
    cat("Found", length(cols), "columns,", nrow(rv$raw_data), "rows:\n\n")
    cat(sprintf("%2d. %s\n", seq_along(cols), cols), sep = "")
    cat("\n--- First 3 Rows Preview ---\n")
    print(head(rv$raw_data, 3))
  })

  # ---- Process data ----
  observeEvent(input$process, {
    req(rv$raw_data)

    missing <- c("DateTime" = input$datetime_col, "Temperature" = input$temp_col,
                 "Wind speed" = input$ws_col, "Wind direction" = input$wd_col)
    missing <- names(missing)[missing == ""]
    if (length(missing) > 0) {
      showNotification(paste("Please select:", paste(missing, collapse = ", ")),
                       type = "error", duration = 6)
      return()
    }
    tz <- input$timezone
    if (!(tz %in% OlsonNames())) {
      showNotification(paste0("Unknown timezone: '", tz, "'"), type = "error", duration = 6)
      return()
    }

    tryCatch({
      df <- rv$raw_data

      # Datetime
      dt_raw <- df[[input$datetime_col]]
      dt     <- parse_datetime_col(dt_raw, input$dt_format, tz)
      n_fail <- sum(is.na(dt) & !is.na(dt_raw))
      if (n_fail > 0) {
        bad <- head(unique(as.character(dt_raw[is.na(dt) & !is.na(dt_raw)])), 3)
        showNotification(paste0(n_fail, " datetime values could not be parsed and were removed. e.g. ",
                                paste(bad, collapse = " | ")),
                         type = "warning", duration = 15)
      }

      # Variables + QC flags
      var_map <- c(temp = input$temp_col, ws = input$ws_col, wd = input$wd_col)
      if (!is.null(input$rain_col) && input$rain_col != "") var_map["mm"] <- input$rain_col

      result <- data.frame(date = dt)
      qc     <- data.frame(row.names = seq_along(dt))
      for (v in names(var_map)) {
        q <- qc_numeric(df[[var_map[[v]]]])
        result[[v]] <- q$value
        qc[[paste0(v, "_blank")]]   <- q$blank
        qc[[paste0(v, "_invalid")]] <- q$invalid
      }

      # Wind speed <= 0.5 m/s treated as calm (0)
      result$ws <- ifelse(result$ws <= 0.5, 0, result$ws)

      # Keep every row with a valid datetime (blank rows are kept for QC counts)
      keep <- !is.na(result$date)
      result <- result[keep, , drop = FALSE]
      qc     <- qc[keep, , drop = FALSE]

      ord <- order(result$date)
      result <- result[ord, , drop = FALSE]
      qc     <- qc[ord, , drop = FALSE]
      rownames(result) <- NULL; rownames(qc) <- NULL

      validate(need(nrow(result) > 0, "No valid datetimes found."))

      rv$tz   <- tz
      rv$day  <- as.Date(result$date, tz = tz)
      rv$qc   <- qc
      rv$data <- result

      # Default date range = full range of the file
      rng <- range(rv$day)
      updateDateRangeInput(session, "date_range", start = rng[1], end = rng[2],
                           min = rng[1], max = rng[2])

      showNotification("Data processed successfully.", type = "message", duration = 4)
      updateTabItems(session, "tabs", "check")
    }, error = function(e) {
      showNotification(paste("Error processing data:", e$message), type = "error", duration = 10)
    })
  })

  output$data_ready <- reactive(!is.null(rv$data))
  outputOptions(output, "data_ready", suspendWhenHidden = FALSE)

  # ---- Date range ----
  full_range <- reactive({ req(rv$day); range(rv$day) })

  observeEvent(input$reset_range, {
    rng <- full_range()
    updateDateRangeInput(session, "date_range", start = rng[1], end = rng[2])
  })

  # Debounced so plots do not redraw while the user is still picking dates
  date_range_d <- debounce(reactive(input$date_range), 600)

  sel <- reactive({
    req(rv$data)
    full <- full_range()
    r    <- date_range_d()
    s <- if (length(r) == 2 && !is.na(r[1])) as.Date(r[1]) else full[1]
    e <- if (length(r) == 2 && !is.na(r[2])) as.Date(r[2]) else full[2]
    s <- max(s, full[1]); e <- min(e, full[2])        # clamp to available data
    validate(need(s <= e, "Start date must be on or before end date."))
    idx <- rv$day >= s & rv$day <= e                  # inclusive, whole days
    validate(need(any(idx), "No data in the selected date range."))
    list(idx = idx, start = s, end = e, label = fmt_period(s, e))
  })

  fdata <- reactive(rv$data[sel()$idx, , drop = FALSE])
  fqc   <- reactive(rv$qc[sel()$idx, , drop = FALSE])

  fig_title <- reactive(if (isTRUE(input$show_period)) sel()$label else NULL)

  output$range_info <- renderText({
    req(rv$data)
    s <- tryCatch(sel(), error = function(e) NULL)
    if (is.null(s)) return("No data in selected range")
    paste0(format(sum(s$idx), big.mark = ","), " of ",
           format(nrow(rv$data), big.mark = ","), " records selected")
  })

  # ---- QC statistics ----
  qc_stats <- reactive({
    fd  <- fdata()
    ud  <- sort(unique(fd$date))
    stp <- as.numeric(diff(ud), units = "mins")
    stp <- stp[stp > 0]
    interval <- if (length(stp) > 0) as.numeric(names(which.max(table(stp)))) else NA

    if (!is.na(interval)) {
      expected <- seq(min(ud), max(ud), by = interval * 60)
      n_exp  <- length(expected)
      n_miss <- sum(!(expected %in% ud))
    } else {
      n_exp <- length(ud); n_miss <- 0
    }

    qd   <- fqc()
    vars <- setdiff(names(fd), "date")
    tbl <- data.frame(
      Variable             = unname(VAR_LABELS[vars]),
      Blank                = sapply(vars, function(v) sum(qd[[paste0(v, "_blank")]])),
      Invalid              = sapply(vars, function(v) sum(qd[[paste0(v, "_invalid")]])),
      `Missing timestamps` = n_miss,
      Valid                = sapply(vars, function(v) sum(!is.na(fd[[v]]))),
      check.names = FALSE
    )
    tbl$`Capture (%)` <- sprintf("%.1f", tbl$Valid / n_exp * 100)

    list(interval = interval, n_exp = n_exp, n_miss = n_miss,
         n_dup = sum(duplicated(fd$date)), table = tbl)
  })

  output$vb_period <- renderValueBox({
    valueBox(tags$span(sel()$label, style = "font-size: 18px;"), "Selected period",
             icon = icon("calendar"), color = "blue")
  })
  output$vb_records <- renderValueBox({
    valueBox(format(nrow(fdata()), big.mark = ","),
             paste0("Records (expected ", format(qc_stats()$n_exp, big.mark = ","), ")"),
             icon = icon("database"), color = "light-blue")
  })
  output$vb_interval <- renderValueBox({
    st <- qc_stats()
    valueBox(ifelse(is.na(st$interval), "Unknown", paste(st$interval, "min")),
             paste0("Logging interval (", st$n_dup, " duplicate times)"),
             icon = icon("clock"), color = "teal")
  })
  output$vb_missing <- renderValueBox({
    st <- qc_stats()
    valueBox(format(st$n_miss, big.mark = ","),
             sprintf("Missing timestamps (%.1f%%)", st$n_miss / st$n_exp * 100),
             icon = icon("exclamation-triangle"),
             color = if (st$n_miss == 0) "green" else "orange")
  })

  output$qc_table <- renderTable(qc_stats()$table, striped = TRUE, bordered = TRUE,
                                 digits = 0, align = "lrrrrr")

  output$data_summary <- renderPrint({
    fd <- fdata()
    cat("Full data:", fmt_period(full_range()[1], full_range()[2]), "\n")
    cat("Selected :", sel()$label, "\n\n")
    print(summary(fd[, setdiff(names(fd), "date"), drop = FALSE]))
  })

  output$data_preview <- renderDT({
    fd <- fdata()
    fd$date <- format(fd$date, "%d/%m/%Y %H:%M")
    datatable(fd, rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = 10, searching = FALSE))
  })

  # ---- Wind roses ----
  selected_wind <- reactive(names(WR_SETTINGS)[names(WR_SETTINGS) %in% input$wind_selected])

  observeEvent(input$wind_all, {
    updateCheckboxGroupInput(session, "wind_selected", selected = names(WR_SETTINGS))
  })
  observeEvent(input$wind_none, {
    updateCheckboxGroupInput(session, "wind_selected", selected = character(0))
  })
  observeEvent(selected_wind(), {
    keys <- selected_wind()
    current <- isolate(input$wind_type)
    selected <- if (length(current) == 1 && current %in% keys) current else head(keys, 1)
    updateSelectInput(session, "wind_type",
                      choices = setNames(keys, sapply(WR_SETTINGS[keys], `[[`, "label")),
                      selected = selected)
  }, ignoreNULL = FALSE)

  wind_limits <- reactive({
    limits <- setNames(vector("list", length(selected_wind())), selected_wind())
    if (identical(input$wind_scale, "manual")) {
      for (key in names(limits)) {
        value <- input[[paste0("max_freq_", key)]]
        validate(need(is.numeric(value) && length(value) == 1 && is.finite(value) &&
                        value > 0 && value <= 100,
                      paste(WR_SETTINGS[[key]]$label, "maximum frequency must be greater than 0 and at most 100%.")))
        limits[key] <- list(value)
      }
    }
    limits
  })

  output$wind_title <- renderUI({
    if (!length(selected_wind())) return("Wind Roses")
    req(input$wind_type %in% selected_wind())
    paste0("Wind Rose - ", WR_SETTINGS[[input$wind_type]]$label, " (", sel()$label, ")")
  })
  output$windrose_plot <- renderPlot({
    validate(need(length(selected_wind()) > 0, "No wind roses selected."))
    req(input$wind_type %in% selected_wind())
    plot_windrose(fdata(), input$wind_type, title = if (is.null(fig_title())) "" else fig_title(),
                  max.freq = wind_limits()[[input$wind_type]])
  })

  # ---- Climate plots ----
  output$temp_title <- renderUI(paste0("Temperature (", sel()$label, ")"))
  output$rain_title <- renderUI(paste0("Rainfall (", sel()$label, ")"))

  output$has_rain <- reactive({
    fd <- tryCatch(fdata(), error = function(e) NULL)
    !is.null(fd) && "mm" %in% names(fd) && any(!is.na(fd$mm))
  })
  outputOptions(output, "has_rain", suspendWhenHidden = FALSE)

  output$temp_plot <- renderPlot({
    req("temp" %in% input$climate_selected)
    plot_temperature(fdata(), rv$tz, base_size = 14, txt_size = 4, title = fig_title())
  })
  output$rain_plot <- renderPlot({
    req("rain" %in% input$climate_selected)
    fd <- fdata()
    req("mm" %in% names(fd))
    plot_rainfall(fd, rv$tz, base_size = 14, txt_size = 4, title = fig_title())
  })

  # ---- Stability ----
  cloud_data <- reactive({
    req(input$cloud_file)
    read_cloud_file(input$cloud_file$datapath)
  })

  stab <- reactive({
    validate(need(!is.null(input$cloud_file), "Upload a cloud data CSV in the Stability tab."))
    validate(need(is.numeric(input$stab_lat) && length(input$stab_lat) == 1 &&
                    is.finite(input$stab_lat) && abs(input$stab_lat) <= 90,
                  "Enter a valid latitude between -90 and 90."))
    res <- calc_stability(fdata(), cloud_data(), input$stab_lat)
    validate(need(!is.null(res), "No overlapping met and cloud hours in the selected date range."))
    res
  })

  output$stab_plot <- renderPlot({
    s <- stab()
    plot_stability(s$counts, base_size = 14, title = fig_title())
  })

  output$stab_table <- renderTable({
    s <- stab()
    data.frame(Class = as.character(s$counts$class), Hours = s$counts$n,
               `Percentage (%)` = sprintf("%.1f", s$counts$percentage), check.names = FALSE)
  }, striped = TRUE, bordered = TRUE, align = "lrr")

  output$download_stab_csv <- downloadHandler(
    filename = function() {
      s <- sel()
      paste0("Stability_class_frequency_", format(s$start, "%Y%m%d"), "_",
             format(s$end, "%Y%m%d"), ".csv")
    },
    content = function(file) {
      s <- stab()
      out <- s$counts
      out$percentage <- round(out$percentage, 2)
      write_csv(out, file)
    }
  )

  # ---- Exports ----
  output$export_period <- renderText(sel()$label)

  export_plan <- reactive({
    fd <- fdata()
    wr_ok <- all(c("ws", "wd") %in% names(fd)) && any(complete.cases(fd[, c("date", "ws", "wd")]))
    c(if (wr_ok) selected_wind(),
      if ("temp" %in% input$climate_selected && any(!is.na(fd$temp))) "temp",
      if ("rain" %in% input$climate_selected && "mm" %in% names(fd) && any(!is.na(fd$mm))) "rain",
      if (isTRUE(input$stab_in_pdf) && !is.null(input$cloud_file) && !is.null(tryCatch(stab(), error = function(e) NULL))) "stab")
  })

  pdf_filename <- reactive({
    period <- sel()
    prefix <- trimws(input$export_filename)
    prefix <- sub("\\.pdf$", "", prefix, ignore.case = TRUE)
    prefix <- gsub('[<>:"/\\\\|?*[:cntrl:]]', "_", prefix)
    if (!nzchar(prefix)) prefix <- "met_analysis"
    paste0(prefix, "_", format(period$start, "%Y%m%d"), "-",
           format(period$end, "%Y%m%d"), ".pdf")
  })
  output$pdf_filename <- renderText(pdf_filename())

  output$export_summary <- renderUI({
    plan <- export_plan()
    if (!length(plan)) return(p("No selected plots have valid data in this date range."))
    limits <- wind_limits()
    tags$ul(lapply(plan, function(key) {
      if (key %in% names(WR_SETTINGS)) {
        scale <- if (is.null(limits[[key]])) "Auto" else paste0("max.freq = ", limits[[key]], "%")
        tags$li(paste0(WR_SETTINGS[[key]]$label, " (", scale, ")"))
      } else {
        tags$li(if (key == "stab") "Stability class frequency" else if (key == "temp") "Temperature" else "Rainfall")
      }
    }))
  })

  output$pdf_download_ui <- renderUI({
    if (!length(export_plan())) return(NULL)
    wind_limits()
    downloadButton("download_pdf", "Download PDF Report", class = "btn-success btn-lg")
  })

  output$download_pdf <- downloadHandler(
    filename = function() pdf_filename(),
    content = function(file) {
      fd    <- fdata()
      tz    <- rv$tz
      ttl   <- fig_title()
      plan <- export_plan()
      validate(need(length(plan) > 0, "Select at least one plot with valid data before downloading."))
      limits <- wind_limits()
      n_steps <- length(plan)

      withProgress(message = "Creating PDF...", value = 0, {
        pdf(file, paper = "a4r", width = 18, height = 10)
        on.exit(dev.off(), add = TRUE)

        for (key in plan) {
          if (key %in% names(WR_SETTINGS)) {
            incProgress(1 / n_steps, detail = paste("Wind rose:", WR_SETTINGS[[key]]$label))
            plot_windrose(fd, key, title = if (is.null(ttl)) "" else ttl, max.freq = limits[[key]])
          } else if (key == "temp") {
            incProgress(1 / n_steps, detail = "Temperature plot")
            print(plot_temperature(fd, tz, 16, 5, ttl))
          } else if (key == "stab") {
            incProgress(1 / n_steps, detail = "Stability plot")
            s <- stab()
            print(plot_stability(s$counts, base_size = 16, title = ttl))
          } else {
            incProgress(1 / n_steps, detail = "Rainfall plot")
            print(plot_rainfall(fd, tz, 16, 5, ttl))
          }
        }
      })
    }
  )

  output$download_csv <- downloadHandler(
    filename = function() {
      s <- sel()
      paste0("processed_met_data_", format(s$start, "%Y%m%d"), "_",
             format(s$end, "%Y%m%d"), ".csv")
    },
    content = function(file) {
      fd <- fdata()
      fd$date <- format(fd$date, "%Y-%m-%d %H:%M:%S")
      write.csv(fd, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
