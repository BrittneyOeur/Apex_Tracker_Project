library(httr)
library(jsonlite)
library(dplyr)
library(jpeg)
library(dotenv)

readRenviron(".env")
api_key <- Sys.getenv("API_KEY")

get_user <- function(platform, player) {
  base_url <- "https://api.mozambiquehe.re"
  endpoint <- "/bridge"
  resource_uri <- paste0(base_url, endpoint)

  query_params <- list(auth = api_key, player = player, platform = platform)
  
  response <- GET(
    resource_uri,
    query = query_params
  )
  
  # Extract the content of the response
  response_text <- content(response, type = "text")
  
  # Convert the JSON string to a list
  response_data <- fromJSON(response_text)
  
  if (!is.null(player) && !is.na(player)) {
    name_placeholder <- h1("STEAM")
    
    if (is.null(response_data$global$name)) {
      steamName <- h3("NA")
      block <- paste(name_placeholder, steamName)
    } else {
      if (platform == "PS4" || platform == "X1") {
        steamName <- h3("NA")
        block <- paste(name_placeholder, steamName)
      } else { # If player has steam account:
        steamName <- h3(response_data$global$name)
        block <- paste(name_placeholder, steamName)
      }
    }

    legend_placeholder <- h1("LEGEND")
    most_played_legend <- response_data$legends$selected$LegendName
    most_played_legend <- as.character(most_played_legend)
    
    if (!is.na(most_played_legend) || is.character(most_played_legend)) {
      if(is.null(response_data$legends$selected$LegendName)) {
        most_played_legend <- h3("NA")
        blockB <- paste(legend_placeholder, most_played_legend)
      } else {
        most_played_legend <- response_data$legends$selected$LegendName
        blockB <- paste(legend_placeholder, h3(most_played_legend))
      }
      
      # Selects the list where the 'kills' are stored
      kills_list <- response_data$legends$selected$data$name
      
      # Word to find for
      find_kills <- "BR Kills"
      
      # Check if the word exists in the list using grepl()
      result <- grepl(find_kills, unlist(kills_list))
      
      kills_placeholder <- h1("KILLS")
      
      # Output the result
      if (any(result)) {
        if (is.null(response_data$legends$selected$data$value)) {
          player_kills <- h3("NA")
          blockC <- paste(kills_placeholder, player_kills)
        } else {
          # Find the index(es) where the word exists in the list using logical indexing
          indices <- which(unlist(kills_list) == find_kills)
          
          player_kills <- response_data$legends$selected$data$value
          blockC <- paste(kills_placeholder, h3(player_kills[indices]))
        }
      } else {
        player_kills <- h3("NA")
        blockC <- paste(kills_placeholder, player_kills)
      }
      
      # Selects the list where the 'kills' are stored
      damage_list <- response_data$legends$selected$data$name
      
      # Word to find for
      find_dmg <- "BR Damage"
      
      # Check if the word exists in the list using grepl()
      result <- grepl(find_kills, unlist(damage_list))
      
      damage_placeholder <- h1("DAMAGE")
      # Output the result
      if (any(result)) {
        if (is.null(response_data$legends$selected$data$value)) {
          player_dmg <- h3("NA")
          blockD <- paste(damage_placeholder, player_dmg)
        } else {
          # Find the index(es) where the word exists in the list using logical indexing
          indices <- which(unlist(damage_list) == find_dmg)
          
          player_dmg <- response_data$legends$selected$data$value
          blockD <- paste(damage_placeholder, h3(player_dmg[indices]))
        }
      } else {
        player_dmg <- h3("NA")
        blockD <- paste(damage_placeholder, player_dmg)
      }
      
      # Selects the list where the 'kills' are stored
      win_list <- response_data$legends$selected$data$name
      
      # Word to find for
      find_win <- "BR Wins"
      
      # Check if the word exists in the list using grepl()
      result <- grepl(find_kills, unlist(win_list))
      
      wins_placeholder <- h1("WINS")
      if (any(result)) {
        if (is.null(response_data$legends$selected$data$value)) {
          player_wins <- h3("NA")
          blockE <- paste(wins_placeholder, player_wins)
        } else {
          # Find the index(es) where the word exists in the list using logical indexing
          indices <- which(unlist(win_list) == find_win)
          
          player_wins <- response_data$legends$selected$data$value
          blockE <- paste(wins_placeholder, h3(player_wins[indices]))
        }
      } else {
        player_wins <- h3("NA")
        blockE <- paste(wins_placeholder, player_wins)
      }
      
      prints_Player <- fluidRow(
        column(2,
               HTML(block)
        ),
        column(2,
               HTML(blockB)
        ),
        column(2,
               HTML(blockC)
        ),
        column(2,
               HTML(blockD)
        ),
        column(2,
               HTML(blockE))
      )
      return(prints_Player)
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}