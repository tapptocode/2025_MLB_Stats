library(shiny)
library(bslib)
library(plotly)
library(DT)
library(tidyverse)
library(baseballr)
library(lubridate)
library(dataRetrieval)
library(forcats)
library(broom)

# === Get Data ===

startDate <- "2025-03-27"
endDate <- "2025-07-14"

# Batter Data

first_half_stats <- bref_daily_batter(startDate, endDate)

clean_first_half_stats <- first_half_stats |> 
  select(Name,Age,Level,Team:BB,SO,SB,BA:OPS)

team_lookup <- tribble(
  ~Level, ~City, ~Team_Name, ~Team_ABV,
  "Maj-AL", "New York", "Yankees", "NYY",
  "Maj-AL", "Los Angeles", "Angels", "LAA",
  "Maj-AL", "Boston", "Red Sox", "BOS",
  "Maj-AL", "Chicago", "White Sox", "CWS",
  "Maj-AL", "Cleveland", "Guardians", "CLE",
  "Maj-AL", "Detroit", "Tigers", "DET",
  "Maj-AL", "Kansas City", "Royals", "KAN",
  "Maj-AL", "Minnesota", "Twins", "MIN",
  "Maj-AL", "Baltimore", "Orioles", "BAL",
  "Maj-AL", "Tampa Bay", "Rays", "TB",
  "Maj-AL", "Toronto", "Blue Jays", "TOR",
  "Maj-AL", "Athletics", "Athletics", "ATH",
  "Maj-AL", "Houston", "Astros", "HOU",
  "Maj-AL", "Seattle", "Mariners", "SEA",
  "Maj-AL", "Texas", "Rangers", "TEX",
  "Maj-NL", "New York", "Mets", "NYM",
  "Maj-NL", "Los Angeles", "Dodgers", "LAD",
  "Maj-NL", "Chicago", "Cubs", "CHC",
  "Maj-NL", "Cincinnati", "Reds", "CIN",
  "Maj-NL", "Milwaukee", "Brewers", "MIL",
  "Maj-NL", "Pittsburgh", "Pirates", "PIT",
  "Maj-NL", "St. Louis", "Cardinals", "STL",
  "Maj-NL", "Atlanta", "Braves", "ATL",
  "Maj-NL", "Miami", "Marlins", "MIA",
  "Maj-NL", "Philadelphia", "Phillies", "PHI",
  "Maj-NL", "Washington", "Nationals", "WAS",
  "Maj-NL", "Arizona", "Diamondbacks", "ARI",
  "Maj-NL", "Colorado", "Rockies", "COL",
  "Maj-NL", "San Diego", "Padres", "SD",
  "Maj-NL", "San Francisco", "Giants", "SF"
)

# Team Color Lookup Table

team_colors <- team_lookup |> 
  mutate(
    Fill_Color = case_when(
      Team_Name == "Angels" ~ "#BA0021",
      Team_Name == "Giants" ~ "#FD5A1E",
      Team_Name == "Diamondbacks" ~ "#A71930",
      Team_Name == "Braves" ~ "#CE1141",
      Team_Name == "Orioles" ~ "#DF4601",
      Team_Name == "Red Sox" ~ "#BD3039",
      Team_Name == "Cubs" ~ "#0E3386",
      Team_Name == "White Sox" ~ "#27251F",
      Team_Name == "Reds" ~ "#C6011F",
      Team_Name == "Guardians" ~ "#00385D",
      Team_Name == "Rockies" ~ "#333366",
      Team_Name == "Tigers" ~ "#0C2340",
      Team_Name == "Astros" ~ "#002D62",
      Team_Name == "Royals" ~ "#004687",
      Team_Name == "Dodgers" ~ "#005A9C",
      Team_Name == "Marlins" ~ "#00A3E0",
      Team_Name == "Brewers" ~ "#12284B",
      Team_Name == "Twins" ~ "#002B5C",
      Team_Name == "Mets" ~ "#002D72",
      Team_Name == "Yankees" ~ "#003087",
      Team_Name == "Athletics" ~ "#003831",
      Team_Name == "Phillies" ~ "#E81828",
      Team_Name == "Pirates" ~ "#FDB827",
      Team_Name == "Padres" ~ "#2F241D",
      Team_Name == "Mariners" ~ "#0C2C56",
      Team_Name == "Cardinals" ~ "#C41E3A",
      Team_Name == "Rays" ~ "#092C5C",
      Team_Name == "Rangers" ~ "#003278",
      Team_Name == "Blue Jays" ~ "#134A8E",
      Team_Name == "Nationals" ~ "#AB0003",
      TRUE ~ "gray"
    ),
    Border_Color = "black"
  )

# Step 1: Separate the Teams and Levels and add Row ID numbers
Separated <- clean_first_half_stats %>%
  mutate(Row_ID = row_number()) %>%
  separate(Level, into = c("Level_1", "Level_2", "Level_3"), sep = ",", fill = "right") %>%
  separate(Team, into = c("Team_1", "Team_2", "Team_3"), sep = ",", fill = "right")

#Step 2: Isolate the traded players. Make sure the players that were traded 
# within their league or within their city have Level_2 and Team_2 filled to 
# reflect that. Fix individual issue players. 
traded_players <- Separated |> 
  filter(!is.na(Team_2) | !is.na(Level_2)) |> 
  mutate(Level_2 = coalesce(Level_2, Level_1)) |> 
  mutate(Team_2 = coalesce(Team_2,Team_1)) |> 
  mutate(Level_3 = if_else(is.na(Level_3) & Name == "Travis Jankowski",
                           "Maj-AL",Level_3)) |> 
  mutate(Level_3 = if_else(is.na(Level_3) & Name == "Garrett Hampson",
                           "Maj-NL",Level_3)) |> 
  mutate(Level_1 = if_else(Name == "Jonah Bride",
                           "Maj-NL",Level_1)) |> 
  mutate(Level_2 = if_else(Name == "Jonah Bride",
                           "Maj-AL",Level_2)) |> 
  mutate(Level_1 = if_else(Name == "Chadwick Tromp",
                           "Maj-NL",Level_1)) |> 
  mutate(Level_2 = if_else(Name == "Chadwick Tromp",
                           "Maj-AL",Level_2))

# Step 3: Create df for just non-traded players
non_traded_players <- Separated |> 
  filter(is.na(Level_2)) |> 
  filter(is.na(Team_2))

#Step 4: Combine non-traded and traded players to get full df
clean_trades_n_all <- bind_rows(traded_players,non_traded_players)

# Step 5: Pivot Longer for both Teams and Levels
clean_long <- clean_trades_n_all |> 
  pivot_longer(cols = c(Level_1, Level_2, Level_3),
               names_to = "Level_Num",
               values_to = "Level") %>%
  mutate(Team_Num = gsub("Level", "Team", Level_Num)) %>%
  left_join(
    Separated %>%
      select(Row_ID, starts_with("Team_")) %>%
      pivot_longer(cols = starts_with("Team_"),
                   names_to = "Team_Num",
                   values_to = "City"),
    by = c("Row_ID", "Team_Num")
  ) %>%
  filter(!is.na(Level) & !is.na(City))

# Step 6: Join with lookup table
final_df <- clean_long |> 
  left_join(team_lookup, by = c("City","Level")) |> 
  select(Name,Age,Level,City:Team_ABV,G:OPS)


# UI
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  titlePanel("2025 First Half MLB Statistics"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("view_type", "View:",
                  choices = c("League-Wide", "Individual Team"),
                  selected = "League-Wide"),
      
      conditionalPanel(
        condition = "input.view_type == 'Individual Team'",
        selectInput("team_select", "Choose a Team:",
                    choices = sort(unique(team_lookup$Team_Name)))
      ),
      
      selectInput("yvar", "Stat to Display:",
                  choices = c("BA" = "AVG_BA", "OBP" = "AVG_OBP", 
                              "SLG" = "AVG_SLG", "OPS" = "AVG_OPS")),
      
      dateRangeInput("dateRange", "Select Date Range:", 
                     start = startDate, end = endDate)
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.view_type == 'League-Wide'",
        plotlyOutput("team_plot")
      ),
      conditionalPanel(
        condition = "input.view_type == 'Individual Team'",
        plotlyOutput("player_plot"),
        DTOutput("player_table")
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$dateRange)
    final_df  # currently not filtered by date since daily stats aren't aggregated
  })
  
  team_averages <- reactive({
    filtered_data() |> 
      filter(PA >= 100) |>
      group_by(Team_Name, Team_ABV) |> 
      summarize(
        AVG_BA = round(mean(BA, na.rm = TRUE), 4),
        AVG_OBP = round(mean(OBP, na.rm = TRUE), 4),
        AVG_SLG = round(mean(SLG, na.rm = TRUE), 4),
        AVG_OPS = round(mean(OPS, na.rm = TRUE), 4),
        .groups = "drop"
      ) |>
      left_join(team_colors, by = "Team_ABV") 
  })
  
  output$team_plot <- renderPlotly({
    data <- team_averages()
    stat_labels <- c("BA" = "AVG_BA", "OBP" = "AVG_OBP", "SLG" = "AVG_SLG", "OPS" = "AVG_OPS")
    ycol <- input$yvar
    y_label <- names(stat_labels)[stat_labels == ycol]
    
    
    p <- data |> 
      ggplot(aes(x = fct_reorder(Team_ABV, .data[[ycol]], .desc = TRUE),
                 y = .data[[ycol]],
                 fill = Fill_Color)) +
      geom_col(color = "black") +
      scale_fill_identity() +  
      labs(x = "Team", y = y_label, title = paste0("League-Wide ", y_label, " by Team")) +
      theme_minimal() +
      theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "none"
    )
    
    ggplotly(p)
  })
  
  output$player_plot <- renderPlotly({
    req(input$team_select)
    
    color_info <- team_colors %>%
      filter(Team_Name == input$team_select)
    
    fill_col <- color_info$Fill_Color %||% "gray"
    border_col <- color_info$Border_Color %||% "black"
    
    # Filter your team data
    team_data <- filtered_data() %>%
      filter(Team_Name == input$team_select, PA >= 30)
    
    ycol <- gsub("AVG_", "", input$yvar)
    
    # Create the plot
    p <- team_data %>%
      ggplot(aes(x = fct_reorder(Name, .data[[ycol]], .desc = TRUE),
                 y = .data[[ycol]])) +
      geom_col(fill = fill_col, color = border_col) +
      labs(x = "Player", y = ycol, 
           title = paste0(input$team_select, " Players' ", ycol, " in Descending Order")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    ggplotly(p)
  })
  
    output$player_table <- renderDT({
      req(input$team_select)
      
      final_df %>%
        filter(Team_Name == input$team_select, PA >= 30) %>%
        select(Name, Age, BA, G:H,HR:OPS) %>%
        arrange(desc(OPS)) %>%
        datatable(options = list(pageLength = 10, order = list(list(5, 'desc'))))
  
    
  })
}

shinyApp(ui, server)