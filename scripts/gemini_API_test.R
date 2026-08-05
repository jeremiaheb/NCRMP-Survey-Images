library(httr2)
library(gargle)

# 1. Define Core Variables
project_id <- "ggn-nmfs-sencrmp-prod-1"
location <- "us"
model_id <- "gemini-3.5-flash"

# 2. Authenticate
# Point this to your NOAA service account JSON key
token <- credentials_service_account(
  scopes = "https://www.googleapis.com/auth/cloud-platform",
  path = "D:/Personal/NOAA/ggn-nmfs-sencrmp-prod-1-7bc592943e42.json"
)

# 3. Build the Endpoint URL (Fixed for multi-region)
url <- sprintf(
  "https://aiplatform.%s.rep.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:generateContent",
  location, project_id, location, model_id
)

# 4. Create a Simple Text Payload
payload <- list(
  contents = list(
    list(
      role = "user",
      parts = list(
        list(text = "Hello, Vertex AI. Are you online?")
      )
    )
  )
)

# 5. Send the Request (with req_error bypassed)
req <- request(url) |>
  req_headers(
    Authorization = paste("Bearer", token$credentials$access_token),
    `Content-Type` = "application/json"
  ) |>
  req_body_json(payload) |>
  req_error(is_error = ~ FALSE) # Tells R not to crash on a 400 error

# 6. Execute and Print Raw Response Body
resp <- req_perform(req)
cat(resp_body_string(resp))
