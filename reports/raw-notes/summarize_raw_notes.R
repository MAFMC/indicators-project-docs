# Script to knit notes together from .md files and get a draft summary from Claude API

# could use claudeR: https://github.com/yrvelez/claudeR

api_key <- Sys.getenv("ANTHROPIC_API_KEY")  # Store in environment variable

# suggested code using library(claudeR) didnt work call directly

# function to concatenate notes
# takes raw notes folder as argument
# outputs long file in same folder

concatmd <- function(rawdir = dirname){
  
  # Get a list of all markdown files in the directory
  file_list <- list.files(dirname, pattern = "\\.md$", full.names = TRUE)
  
  # Read the content of each file and store it in a list
  file_contents <- lapply(file_list, readLines, warn = FALSE)
  
  # Combine all file contents into a single string, including file names for context
  combined_content <- ""
  for (i in 1:length(file_list)) {
    combined_content <- paste(combined_content, 
                              sprintf("--- Start of file: %s ---\n", basename(file_list[i])), 
                              paste(file_contents[[i]], collapse = "\n"), 
                              sprintf("\n--- End of file: %s ---\n\n", basename(file_list[i])),
                              sep = "\n")
  }
  
  return(combined_content)
  
}

# Function to call Claude API
call_claude <- function(prompt, api_key, model = "claude-sonnet-4-5-20250929") {
  
  url <- "https://api.anthropic.com/v1/messages"
  
  # Prepare the request body
  body <- list(
    model = model,
    max_tokens = 4000,  # Adjust based on desired summary length
    messages = list(
      list(
        role = "user",
        content = prompt
      )
    )
  )
  
  # Make the API request
  response <- httr2::request(url) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "x-api-key" = api_key,
      "anthropic-version" = "2023-06-01"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_perform()
  
  # Parse response
  response_data <- httr2::resp_body_json(response)
  
  # Extract the text content
  summary <- response_data$content[[1]]$text
  
  return(summary)
}



# function to summarize the concatenated notes
# takes concatenated notes file as argument
# call claude API and write draft summary same folder as concatenated notes
claudesumm <- function(rawdir = dirname, api_key = api_key){
  
  # Construct the prompt for the AI
  
  system_prompt <- "You will be summarizing multiple sets of meeting notes into a single comprehensive meeting report. You may receive notes from different note-takers or from different segments of the same meeting. Your goal is to create one unified, well-organized summary that captures all important information without redundancy."
  
  combined_content <- concatmd(rawdir = dirname)
  
  user_prompt <- paste("Here are the contents of several markdown files:\n\n", 
                       combined_content, 
                       "\n\nPlease provide a detailed and well-structured summary of all the information contained within these files, ignoring speaker names.",
                       sep = "")
  
  prompt <- paste(system_prompt, "\n\n", user_prompt)
  
  # # Prepare messages for the API call using the recommended list format for recent models
  # messages <- list(
  #   list(role = "user", content = user_prompt)
  # )
  # 
  # # Call the Claude API
  # # The 'claudeR' function uses the ANTHROPIC_API_KEY environment variable automatically
  # response <- claudeR::claudeR(
  #   api_key = api_key,
  #   prompt = messages,
  #   model = "claude-sonnet-4-5-20250929", # Use an appropriate model like claude-3-opus or claude-3.5-sonnet
  #   max_tokens = 3000,           # Adjust max_tokens based on the expected length of the summary
  #   system = system_prompt       # Set the system prompt
  # )
  
  summary <- call_claude(prompt, api_key)
  
  # Save the summary
  readr::write_file(summary, paste0(dirname, "claudesumm.Rmd"))
  
  
  # Print the summary
  cat("--- Comprehensive Summary ---\n")
  cat(summary)
}


# specify the notes directory and call the functions

dirname <- here::here("reports/raw-notes/WK1")

claudesumm(rawdir = dirname, api_key = api_key)

