
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
library(R.utils)
library(DT)

function_files <- sourceDirectory("functions")
for (file in function_files){
  source(file, local = TRUE)
}

ui <- navbarPageWithInputs(
  title = h4("KPIs Health"),
  theme = shinytheme("cosmo"),
  inputs = selectInput(
    "brand",
    label = "",
    choices = list("BRAND1" = "brand1", "BRAND2" = "brand2"),
    selected = "brand1",
    width = "200"
  ),
  tabPanel(
    title = h4("Summary"),
    sidebarPanel(
      h2("Settings"),
      hr(),
      radioButtons(
        "summary_compare_versus",
        label = h3("Compare versus"),
        choices = list("Last Month" = "last_month", "Last Quarter" = "last_quarter", "Last Year" = "last_year"),
        selected = "last_quarter"
      ),
      checkboxGroupInput(
        "grouping_variables",
        label = h3("Group by"),
        choices = list("Region" = "Region", "Gender" = "Gender", "Age" = "Age"),
        selected= c()
      ),
      width = 2
    ),
    mainPanel(
      column(
        12,
        align = "center",
        h3("Brand Status"),
        dataTableOutput("summary_plot", height=500)
      ),
      width = 10
    )
  ),
  tabPanel(
    title = h4("Analyse"),
    sidebarPanel(
      h2("Settings"),
      hr(),
      radioButtons(
        "compare_versus",
        label = h3("Compare versus"),
        choices = list("Last Month" = "last_month", "Last Quarter" = "last_quarter", "Last Year" = "last_year"),
        selected = "last_quarter"
      ),
      # radioButtons(
      #   "comparison_type",
      #   label = h3("Comparison type"),
      #   choices = list("Differences" = "differences", "Two points" = "two_points"),
      #   selected= c("differences")
      # ),
      # radioButtons(
      #   "breakpoints_criteria",
      #   label = h3("Find structural changes on:"),
      #   choices = list("Trend" = "kalman_series", "Original series" = "original_series"),
      #   selected = "kalman_series"
      # ),
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "LATEST CHANGE",
          # h2(textOutput("doing_title")),
          column(
            12,
            align = "center",
            div(
              style="display:inline-block",
              h3("Latest Change"),
              hr(),
              plotOutput("brand_latest_change_distribution", height = 300)
            )
          )
        ),
        tabPanel(
          "HEALTH MAP",
          # h2(textOutput("doing_title")),
          column(
            12,
            align = "center",
            h3("Brand Health Map"),
            plotOutput("brand_health", height = 500)
          )
        ),
        tabPanel(
          "TREND",
          h2(textOutput("trend_title")),
          column(
            3,
            align = "center",
            div(
              style="display:inline-block",
              uiOutput("kpi",inline = TRUE)
              # selectInput(
              #   "kpi",
              #   label = h3("Select KPI"),
              #   choices = list("KPI1" = "KPI1", "KPI2" = "KPI2", "KPI3" = "KPI3", "KPI4" = "KPI4"),
              #   selected = "KPI1",
              #   width = "100%"
              # )
            )
          ),
          column(
            3,
            # align = "center",
            div(
              style="display:inline-block",
              radioButtons(
                "noise_filter",
                label = h3("Noise filter"),
                choices = list("Level" = "kalman","Momentum" = "trends"),
                selected = "kalman",
                width = "100%"
              )
            )
          ),
          column(
            2,
            align = "center",
            div(
              style="display:inline-block",
              h3("Base"),
              h4(textOutput("mean_base")),
              height = 200
            )
          ),
          column(
            4,
            align = "center",
            div(
              style="display:inline-block",
              h3("Latest Change"),
              plotOutput("latest_change", height = 120),
              width = "100%"
            )
          ),
          column(
            12,
            align = "center",
            plotOutput("time_series", height = 300, width = "100%")
          )
        )
      ),
      width = 8
    ),
    sidebarPanel(
      h2("Filters"),
      hr(),
      dropdownButton(
        label = "Age", status = "default", width = 30,
        checkboxGroupInput(
          "age",
          label = h5("Select Age"),
          choices = list("21-30" = "21-30", "31-40" = "31-40", "41-50" = "41-50"),
          selected= c("21-30","31-40","41-50")
        )
      ),
      hr(),
      dropdownButton(
        label = "Gender", status = "default", width = 30,
        checkboxGroupInput(
          "gender",
          label = h5("Select Gender"),
          choices = list("Female" = "Female", "Male" = "Male"),
          selected= c("Female","Male")
        )
      ),
      hr(),
      dropdownButton(
        label = "Region", status = "default", width = 30,
        checkboxGroupInput(
          "region",
          label = h5("Select Region"),
          choices = list("North" = "North", "South" = "South", "East" = "East", "West" = "West"),
          selected= c("North","South","East","West")
        )
      ),
      hr(),
      width = 2
    )
  )
)
