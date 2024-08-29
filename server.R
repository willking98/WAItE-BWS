#################################
# Deifne the Conjoint Questions #
#################################

# Read in the full survey design file
design <- readr::read_csv("choice_questions.csv")

# Sample a random respondentID
respondentID <- sample(design$respID, 1)

# Store the respondentID
sd_store_value(respondentID, "respID")

# Filter for the rows for the chosen respondentID
df <- design %>%
  filter(respID == respondentID) %>%
  mutate(image = paste0("images/", image))

# Function to create the labels for a choice question
# based on the values in df

make_cbc_options <- function(df) {
  alt1 <- df |> filter(altID == 1)
  alt2 <- df |> filter(altID == 2)
  alt3 <- df |> filter(altID == 3)

  options <- c("option_1", "option_2", "option_3")

  names(options) <- c(
    glue("
      **Option 1**<br>
      <img src='{alt1$image}' width=100><br>
      **Type**: {alt1$type}<br>
      **Price**: $ {alt1$price} / lb
    "),
    glue("
      **Option 2**<br>
      <img src='{alt2$image}' width=100><br>
      **Type**: {alt2$type}<br>
      **Price**: $ {alt2$price} / lb
    "),
    glue("
      **Option 3**<br>
      <img src='{alt3$image}' width=100><br>
      **Type**: {alt3$type}<br>
      **Price**: $ {alt3$price} / lb
    ")
  )
  return(options)
}

# Create the options for each choice question

cbc1_options <- make_cbc_options(df |> filter(qID == 1))
cbc2_options <- make_cbc_options(df |> filter(qID == 2))
cbc3_options <- make_cbc_options(df |> filter(qID == 3))
cbc4_options <- make_cbc_options(df |> filter(qID == 4))
cbc5_options <- make_cbc_options(df |> filter(qID == 5))
cbc6_options <- make_cbc_options(df |> filter(qID == 6))

# Create each choice question

sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q1',
  label  = "(1 of 6) If these were your only options, which would you choose?",
  option = cbc1_options,
  reactive = TRUE
)

sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q2',
  label  = "(2 of 6) If these were your only options, which would you choose?",
  option = cbc2_options,
  reactive = TRUE
)

sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q3',
  label  = "(3 of 6) If these were your only options, which would you choose?",
  option = cbc3_options,
  reactive = TRUE
)

sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q4',
  label  = "(4 of 6) If these were your only options, which would you choose?",
  option = cbc4_options,
  reactive = TRUE
)

sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q5',
  label  = "(5 of 6) If these were your only options, which would you choose?",
  option = cbc5_options,
  reactive = TRUE
)

sd_question(
  type   = 'mc_buttons',
  id     = 'cbc_q6',
  label  = "(6 of 6) If these were your only options, which would you choose?",
  option = cbc6_options,
  reactive = TRUE
)

################
# Server Setup #
################

db <- sd_database(
  host   = "",
  dbname = "",
  port   = "",
  user   = "",
  table  = "",
  pause  = TRUE
)

################
# config setup #
################

config <- sd_config(
  skip_if = tibble::tribble(
    ~question_id,         ~question_value, ~target,
    "screenout",          "blue",          "end_screenout",
    "consent_age",        "no",            "end_consent",
    "consent_understand", "no",            "end_consent"
  ),
  show_if = tibble::tribble(
    ~question_id,         ~question_value, ~target,
    "like_fruit",         "yes",           "fav_fruit",
    "like_fruit",         "kind_of",       "fav_fruit"
  ),
  all_questions_required = TRUE
)
