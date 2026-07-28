###############################################################################
# LIFE Fitness - Membership Retention Dashboard (Shiny)
# MSc Statistical Data Science, University College Dublin
# Hemanth Sai Kumar Vadlapudi (25259069)
#
# An interactive dashboard that sits on top of the churn analysis. It has five
# tabs: an overview with the headline numbers, a churn explorer with a slider
# so you can move the inactivity threshold and watch the rate change, the
# monthly joins-vs-churn trend, the seasonal breakdown by site, and a ranked
# at-risk list driven by the logistic model.
#
# It expects the cleaned member table and the fitted model already in memory.
# The prep block below rebuilds them from the Master Sheet so the app runs on
# its own; if you source your analysis script first you can delete that block.
###############################################################################

library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(readxl)
library(DT)          # the sortable table on the at-risk tab


## ---------------------------------------------------------------------------
## Data prep (runs once when the app starts, not on every click)
## ---------------------------------------------------------------------------
# Same steps as the main analysis script, condensed. If you've already run
# churn_analysis.R in the session, you can remove this whole block and the app
# will use the objects that are already there.

members <- read_excel("Master_Sheet.xlsx", sheet = "Member Information")
visits  <- read_excel("Master_Sheet.xlsx", sheet = "Visits")

members <- members %>%
  mutate(across(c(`Date of birth`, `Last visit`,
                  `Last interaction`, `Joined on`), as_date))
visits <- visits %>%
  mutate(CheckInDate = as_datetime(CheckInDate)) %>%
  filter(!is.na(CheckInDate))

# keep the newest record per member
members <- members %>%
  arrange(`Cloud Id`, desc(`Last interaction`), desc(`Joined on`)) %>%
  distinct(`Cloud Id`, .keep_all = TRUE) %>%
  rename(CloudId = `Cloud Id`)

# label the sites
visits <- visits %>%
  mutate(site = case_when(
    str_detect(FacilityName, "Jetland")  ~ "Jetland",
    str_detect(FacilityName, "Corbally") ~ "Corbally",
    TRUE ~ "Other"
  ))

# first-month visits feature
first_month <- visits %>%
  inner_join(members %>% select(CloudId, `Joined on`), by = "CloudId") %>%
  mutate(d = as.numeric(difftime(CheckInDate, `Joined on`, units = "days"))) %>%
  filter(d >= 0, d <= 30) %>%
  count(CloudId, name = "first_month_visits")

REFERENCE_DATE <- max(members$`Last visit`, na.rm = TRUE)

# member table with age, inactivity and duration ready to go
members <- members %>%
  left_join(first_month, by = "CloudId") %>%
  mutate(
    first_month_visits = coalesce(first_month_visits, 0),
    age = as.numeric(difftime(REFERENCE_DATE, `Date of birth`,
                              units = "days")) / 365.25,
    days_inactive = as.numeric(difftime(REFERENCE_DATE, `Last visit`,
                                        units = "days")),
    membership_duration = as.numeric(difftime(`Last visit`, `Joined on`,
                                              units = "days"))
  )

# fit the churn model once so the at-risk tab can score members
model_df <- members %>%
  filter(!is.na(days_inactive), age >= 16, age <= 90) %>%
  mutate(churned_binary = if_else(days_inactive > 30, 1, 0),
         Gender = as.factor(Gender))

churn_model <- glm(churned_binary ~ first_month_visits + age + Gender,
                   data = model_df, family = binomial)

# attach each member's predicted churn probability for the ranked list
model_df$risk <- predict(churn_model, type = "response")


## ---------------------------------------------------------------------------
## UI - what the user sees
## ---------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("LIFE Fitness - Membership Retention Dashboard"),

  tabsetPanel(

    # --- Tab 1: Overview -----------------------------------------------------
    # Three headline numbers across the top, then the churned vs active bar.
    tabPanel("Overview",
      br(),
      fluidRow(
        column(4, wellPanel(h4("Total members"), textOutput("kpi_total"))),
        column(4, wellPanel(h4("Churned"),       textOutput("kpi_churned"))),
        column(4, wellPanel(h4("Churn rate"),    textOutput("kpi_rate")))
      ),
      h4("Churned vs active members"),
      plotOutput("overview_plot")
    ),

    # --- Tab 2: Churn Explorer ----------------------------------------------
    # The interactive bit. The slider changes the inactivity cut-off and the
    # rate and chart update live, which shows how little the result depends on
    # the exact threshold.
    tabPanel("Churn Explorer",
      br(),
      sidebarLayout(
        sidebarPanel(
          sliderInput("threshold", "Inactivity threshold (days):",
                      min = 15, max = 120, value = 30, step = 5),
          helpText("Adjust how many days of inactivity count as churned.")
        ),
        mainPanel(
          h4(textOutput("explorer_rate")),
          plotOutput("explorer_plot")
        )
      )
    ),

    # --- Tab 3: Trends (Corbally) -------------------------------------------
    tabPanel("Trends (Corbally)",
      br(),
      h4("Monthly joins vs churn - Corbally"),
      plotOutput("trend_plot")
    ),

    # --- Tab 4: Seasonality --------------------------------------------------
    tabPanel("Seasonality",
      br(),
      h4("Visits by season and site"),
      plotOutput("season_plot")
    ),

    # --- Tab 5: At-Risk Members ---------------------------------------------
    # The practical output: members sorted by predicted churn probability, so
    # staff know who to reach out to first.
    tabPanel("At-Risk Members",
      br(),
      h4("Members ranked by predicted churn risk"),
      DT::dataTableOutput("risk_table")
    )
  )
)


## ---------------------------------------------------------------------------
## Server - the logic behind each output
## ---------------------------------------------------------------------------
server <- function(input, output) {

  # helper: churn flag for members at whatever threshold is passed in
  churn_at <- function(days) {
    members %>%
      filter(!is.na(days_inactive)) %>%
      mutate(state = if_else(days_inactive > days, "Churned", "Active"))
  }

  # --- Overview KPIs (fixed at the 30-day definition) ----------------------
  base <- churn_at(30)

  output$kpi_total   <- renderText({ nrow(base) })
  output$kpi_churned <- renderText({ sum(base$state == "Churned") })
  output$kpi_rate    <- renderText({
    scales::percent(mean(base$state == "Churned"), accuracy = 0.1)
  })

  output$overview_plot <- renderPlot({
    base %>%
      count(state) %>%
      ggplot(aes(state, n, fill = state)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      scale_fill_manual(values = c(Active = "#2E8B57", Churned = "#CD4F39")) +
      labs(x = NULL, y = "Members")
  })

  # --- Churn Explorer (reacts to the slider) -------------------------------
  # reactive() means this recomputes only when the slider moves, and both the
  # text and the plot below share the one calculation.
  explorer <- reactive({ churn_at(input$threshold) })

  output$explorer_rate <- renderText({
    rate <- mean(explorer()$state == "Churned")
    paste0("Churn rate at ", input$threshold, " days: ",
           scales::percent(rate, accuracy = 0.1))
  })

  output$explorer_plot <- renderPlot({
    explorer() %>%
      count(state) %>%
      ggplot(aes(state, n, fill = state)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      scale_fill_manual(values = c(Active = "#2E8B57", Churned = "#CD4F39")) +
      labs(x = NULL, y = "Members")
  })

  # --- Trends: monthly joins vs churn at Corbally --------------------------
  output$trend_plot <- renderPlot({
    home <- visits %>%
      filter(site %in% c("Corbally", "Jetland")) %>%
      group_by(CloudId) %>%
      summarise(home = names(which.max(table(site))), .groups = "drop")

    cor <- members %>%
      left_join(home, by = "CloudId") %>%
      filter(home == "Corbally") %>%
      mutate(churned = days_inactive > 30)

    joins  <- cor %>% mutate(m = floor_date(`Joined on`, "month")) %>%
      count(m, name = "Joins")
    churns <- cor %>% filter(churned) %>%
      mutate(m = floor_date(`Last visit`, "month")) %>%
      count(m, name = "Churns")

    inner_join(joins, churns, by = "m") %>%
      filter(m >= as.Date("2025-04-01")) %>%
      tidyr::pivot_longer(c(Joins, Churns), names_to = "type",
                          values_to = "n") %>%
      ggplot(aes(m, n, colour = type)) +
      geom_line(linewidth = 1.1) +
      scale_colour_manual(values = c(Joins = "#2E8B57", Churns = "#CD4F39")) +
      labs(x = NULL, y = "Members", colour = NULL)
  })

  # --- Seasonality: visits by season and site ------------------------------
  output$season_plot <- renderPlot({
    season_of <- function(m) {
      case_when(m %in% 3:5 ~ "Spring", m %in% 6:8 ~ "Summer",
                m %in% 9:11 ~ "Autumn", TRUE ~ "Winter")
    }
    visits %>%
      filter(site %in% c("Corbally", "Jetland")) %>%
      mutate(season = factor(season_of(month(CheckInDate)),
                             levels = c("Spring","Summer","Autumn","Winter"))) %>%
      count(season, site) %>%
      ggplot(aes(season, n, fill = site)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c(Corbally = "#F08080", Jetland = "#20B2AA")) +
      labs(x = NULL, y = "Visits", fill = "site")
  })

  # --- At-risk table: members sorted by model risk, highest first ----------
  output$risk_table <- DT::renderDataTable({
    model_df %>%
      arrange(desc(risk)) %>%
      transmute(
        Member = CloudId,
        Age = round(age),
        `First month visits` = first_month_visits,
        `Days inactive` = round(days_inactive),
        `Churn risk` = scales::percent(risk, accuracy = 1)
      )
  }, options = list(pageLength = 15))
}


## ---------------------------------------------------------------------------
## Launch
## ---------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
