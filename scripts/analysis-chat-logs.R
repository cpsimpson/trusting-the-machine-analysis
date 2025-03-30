library(chatlogr)

process_chatlogs <- function(data){
  
  chatdata <- parse_users_chat_data(
    dat = data,  # data file (or provide a dataframe via dat parameter)
    idcol = "ResponseId",  # unique id column in data
    chat_col_patterns = c("ChatHistory"),  # columns containing chat history
    nrows = 10  # number of rows to parse (fewer for debugging/testing)
  )
  
  
  cd_s <- chatdata$df_success
  cd_i <- chatdata$df_info
  
  

  
}

chat_anthropomorphism_analysis <- function(data){
  # library(claudeR)
  # library(keyring)
  # 
  # claude_key = key_get("anthropic_api_key")
  # 
  # response <- claudeR(
  #   system_prompt =  "Given the conversation in",
  #   model = "claude-3-7-sonnet-latest",
  #   max_tokens = 50, 
  #   api_key = claude_key
  # )
  # 
  # cat(response)
}