library(httptest)

# setup fake auth
Sys.setenv(
  rtweet_api_key = "",
  rtweet_api_secret_key = "",
  rtweet_access_token = "",
  rtweet_access_token_secret = ""
)
