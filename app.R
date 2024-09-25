# remotes::install_github("surveydown-dev/surveydown", force = TRUE)
library(surveydown)
library(dplyr)
library(glue)

# Database setup

# surveydown stores data on a database that you define at https://supabase.com/
# To connect to a database, update the sd_database() function with details
# from your supabase database. For this demo, we set ignore = TRUE, which will
# ignore the settings and won't attempt to connect to the database. This is
# helpful for local testing if you don't want to record testing data in the
# database table. See the documentation for details:
# https://surveydown.org/store-data

# database setup
db <- sd_database(
  host   = "aws-0-eu-west-2.pooler.supabase.com",
  dbname = "postgres",
  port   = "6543",
  user   = "postgres.msujfghwfbdvnnejymgi",
  table  = "AdultBWS",
  ignore = TRUE
)

# Server setup
server <- function(input, output, session) {

  # Using URL parameters to obtain prolific PIDs
  sd_store_value(sd_get_url_pars("PROLIFIC_PID"), id = "PROLIFIC_PID")
  
  # Read in the full survey design file
  design <- readr::read_csv("BWS_design_for_1000_participants.csv")

  # Sample a random respondentID
  respondentID <- sample(design$respID, 1)

  # Store the respondentID
  sd_store_value(respondentID, "respID")

  # Filter for the rows for the chosen respondentID
  df <- design %>%
    filter(respID == respondentID)

  # Function to create the question labels based on df values
  make_cbc_options <- function(df) {
    alt1 <- df |> filter(altID == 1)

    options <- c("tired", "walking", "sports", "concentration", "embarrassed", "unhappiness", "treated")
    
    names(options) <- c(
      glue("I **{alt1$tired}** get tired"),
      glue("I **{alt1$walking}** struggle to keep up when I am walking around with others"),
      glue("I **{alt1$sports}** avoid doing sports"),
      glue("I **{alt1$concentration}** struggle to concentrate on my studies/work"),
      glue("I **{alt1$embarrassed}** feel embarrassed shopping for clothes"),
      glue("I **{alt1$unhappiness}** feel unhappy because I am unable to do the same things as others"),
      glue("People **{alt1$treated}** treat me differently when I go out")
    )
    
    return(options)
  }

  #########################################################################
  # Functions for each attribute to be missing in the worst question
  #########################################################################
  make_cbc_options_tired <- function(df) {
    alt1 <- df |> filter(altID == 1)
    
    options <- c("walking", "sports", "concentration", "embarrassed", "unhappiness", "treated")
    
    names(options) <- c(
      glue("I **{alt1$walking}** struggle to keep up when I am walking around with others"),
      glue("I **{alt1$sports}** avoid doing sports"),
      glue("I **{alt1$concentration}** struggle to concentrate on my studies/work"),
      glue("I **{alt1$embarrassed}** feel embarrassed shopping for clothes"),
      glue("I **{alt1$unhappiness}** feel unhappy because I am unable to do the same things as others"),
      glue("People **{alt1$treated}** treat me differently when I go out")
    )
    
    return(options)
  }
  
  make_cbc_options_walking <- function(df) {
    alt1 <- df |> filter(altID == 1)
    
    options <- c("tired", "sports", "concentration", "embarrassed", "unhappiness", "treated")
    
    names(options) <- c(
      glue("I **{alt1$tired}** get tired"),
      glue("I **{alt1$sports}** avoid doing sports"),
      glue("I **{alt1$concentration}** struggle to concentrate on my studies/work"),
      glue("I **{alt1$embarrassed}** feel embarrassed shopping for clothes"),
      glue("I **{alt1$unhappiness}** feel unhappy because I am unable to do the same things as others"),
      glue("People **{alt1$treated}** treat me differently when I go out")
    )
    
    return(options)
  }
  
  make_cbc_options_sports <- function(df) {
    alt1 <- df |> filter(altID == 1)
    
    options <- c("tired", "walking", "concentration", "embarrassed", "unhappiness", "treated")
    
    names(options) <- c(
      glue("I **{alt1$tired}** get tired"),
      glue("I **{alt1$walking}** struggle to keep up when I am walking around with others"),
      glue("I **{alt1$concentration}** struggle to concentrate on my studies/work"),
      glue("I **{alt1$embarrassed}** feel embarrassed shopping for clothes"),
      glue("I **{alt1$unhappiness}** feel unhappy because I am unable to do the same things as others"),
      glue("People **{alt1$treated}** treat me differently when I go out")
    )
    
    return(options)
  }
  
  make_cbc_options_concentration <- function(df) {
    alt1 <- df |> filter(altID == 1)
    
    options <- c("tired", "walking", "sports", "embarrassed", "unhappiness", "treated")
    
    names(options) <- c(
      glue("I **{alt1$tired}** get tired"),
      glue("I **{alt1$walking}** struggle to keep up when I am walking around with others"),
      glue("I **{alt1$sports}** avoid doing sports"),
      glue("I **{alt1$embarrassed}** feel embarrassed shopping for clothes"),
      glue("I **{alt1$unhappiness}** feel unhappy because I am unable to do the same things as others"),
      glue("People **{alt1$treated}** treat me differently when I go out")
    )
    
    return(options)
  }
  make_cbc_options_embarrassed <- function(df) {
    alt1 <- df |> filter(altID == 1)
    
    options <- c("tired", "walking", "sports", "concentration", "unhappiness", "treated")
    
    names(options) <- c(
      glue("I **{alt1$tired}** get tired"),
      glue("I **{alt1$walking}** struggle to keep up when I am walking around with others"),
      glue("I **{alt1$sports}** avoid doing sports"),
      glue("I **{alt1$concentration}** struggle to concentrate on my studies/work"),
      glue("I **{alt1$unhappiness}** feel unhappy because I am unable to do the same things as others"),
      glue("People **{alt1$treated}** treat me differently when I go out")
    )
    
    return(options)
  }
  make_cbc_options_unhappiness <- function(df) {
    alt1 <- df |> filter(altID == 1)
    
    options <- c("tired", "walking", "sports", "concentration", "embarrassed", "treated")
    
    names(options) <- c(
      glue("I **{alt1$tired}** get tired"),
      glue("I **{alt1$walking}** struggle to keep up when I am walking around with others"),
      glue("I **{alt1$sports}** avoid doing sports"),
      glue("I **{alt1$concentration}** struggle to concentrate on my studies/work"),
      glue("I **{alt1$embarrassed}** feel embarrassed shopping for clothes"),
      glue("People **{alt1$treated}** treat me differently when I go out")
    )
    
    return(options)
  }
  make_cbc_options_treated <- function(df) {
    alt1 <- df |> filter(altID == 1)
    
    options <- c("tired", "walking", "sports", "concentration", "embarrassed", "unhappiness")
    
    names(options) <- c(
      glue("I **{alt1$tired}** get tired"),
      glue("I **{alt1$walking}** struggle to keep up when I am walking around with others"),
      glue("I **{alt1$sports}** avoid doing sports"),
      glue("I **{alt1$concentration}** struggle to concentrate on my studies/work"),
      glue("I **{alt1$embarrassed}** feel embarrassed shopping for clothes"),
      glue("I **{alt1$unhappiness}** feel unhappy because I am unable to do the same things as others")
    )
    
    return(options)
  }
  # Create the options for each choice question
  
  cbc1_options <- make_cbc_options(df |> filter(qID == 1))
  cbc1_options_w_tired <- make_cbc_options_tired(df |> filter(qID == 1))
  cbc1_options_w_walking <- make_cbc_options_walking(df |> filter(qID == 1))
  cbc1_options_w_sports <- make_cbc_options_sports(df |> filter(qID == 1))
  cbc1_options_w_concentration <- make_cbc_options_concentration(df |> filter(qID == 1))
  cbc1_options_w_embarrassed <- make_cbc_options_embarrassed(df |> filter(qID == 1))
  cbc1_options_w_unhappiness <- make_cbc_options_unhappiness(df |> filter(qID == 1))
  cbc1_options_w_treated <- make_cbc_options_treated(df |> filter(qID == 1))
  
  ###############################################################################
  
  cbc2_options <- make_cbc_options(df |> filter(qID == 2))
  cbc2_options_w_tired <- make_cbc_options_tired(df |> filter(qID == 2))
  cbc2_options_w_walking <- make_cbc_options_walking(df |> filter(qID == 2))
  cbc2_options_w_sports <- make_cbc_options_sports(df |> filter(qID == 2))
  cbc2_options_w_concentration <- make_cbc_options_concentration(df |> filter(qID == 2))
  cbc2_options_w_embarrassed <- make_cbc_options_embarrassed(df |> filter(qID == 2))
  cbc2_options_w_unhappiness <- make_cbc_options_unhappiness(df |> filter(qID == 2))
  cbc2_options_w_treated <- make_cbc_options_treated(df |> filter(qID == 2))
  
  ###############################################################################
  
  cbc3_options <- make_cbc_options(df |> filter(qID == 3))
  cbc3_options_w_tired <- make_cbc_options_tired(df |> filter(qID == 3))
  cbc3_options_w_walking <- make_cbc_options_walking(df |> filter(qID == 3))
  cbc3_options_w_sports <- make_cbc_options_sports(df |> filter(qID == 3))
  cbc3_options_w_concentration <- make_cbc_options_concentration(df |> filter(qID == 3))
  cbc3_options_w_embarrassed <- make_cbc_options_embarrassed(df |> filter(qID == 3))
  cbc3_options_w_unhappiness <- make_cbc_options_unhappiness(df |> filter(qID == 3))
  cbc3_options_w_treated <- make_cbc_options_treated(df |> filter(qID == 3))
  
  ###############################################################################
  
  cbc4_options <- make_cbc_options(df |> filter(qID == 4))
  cbc4_options_w_tired <- make_cbc_options_tired(df |> filter(qID == 4))
  cbc4_options_w_walking <- make_cbc_options_walking(df |> filter(qID == 4))
  cbc4_options_w_sports <- make_cbc_options_sports(df |> filter(qID == 4))
  cbc4_options_w_concentration <- make_cbc_options_concentration(df |> filter(qID == 4))
  cbc4_options_w_embarrassed <- make_cbc_options_embarrassed(df |> filter(qID == 4))
  cbc4_options_w_unhappiness <- make_cbc_options_unhappiness(df |> filter(qID == 4))
  cbc4_options_w_treated <- make_cbc_options_treated(df |> filter(qID == 4))
  
  ###############################################################################
  
  cbc5_options <- make_cbc_options(df |> filter(qID == 5))
  cbc5_options_w_tired <- make_cbc_options_tired(df |> filter(qID == 5))
  cbc5_options_w_walking <- make_cbc_options_walking(df |> filter(qID == 5))
  cbc5_options_w_sports <- make_cbc_options_sports(df |> filter(qID == 5))
  cbc5_options_w_concentration <- make_cbc_options_concentration(df |> filter(qID == 5))
  cbc5_options_w_embarrassed <- make_cbc_options_embarrassed(df |> filter(qID == 5))
  cbc5_options_w_unhappiness <- make_cbc_options_unhappiness(df |> filter(qID == 5))
  cbc5_options_w_treated <- make_cbc_options_treated(df |> filter(qID == 5))
  
  ###############################################################################
  
  cbc6_options <- make_cbc_options(df |> filter(qID == 6))
  cbc6_options_w_tired <- make_cbc_options_tired(df |> filter(qID == 6))
  cbc6_options_w_walking <- make_cbc_options_walking(df |> filter(qID == 6))
  cbc6_options_w_sports <- make_cbc_options_sports(df |> filter(qID == 6))
  cbc6_options_w_concentration <- make_cbc_options_concentration(df |> filter(qID == 6))
  cbc6_options_w_embarrassed <- make_cbc_options_embarrassed(df |> filter(qID == 6))
  cbc6_options_w_unhappiness <- make_cbc_options_unhappiness(df |> filter(qID == 6))
  cbc6_options_w_treated <- make_cbc_options_treated(df |> filter(qID == 6))
  
  ###############################################################################
  
  cbc7_options <- make_cbc_options(df |> filter(qID == 7))
  cbc7_options_w_tired <- make_cbc_options_tired(df |> filter(qID == 7))
  cbc7_options_w_walking <- make_cbc_options_walking(df |> filter(qID == 7))
  cbc7_options_w_sports <- make_cbc_options_sports(df |> filter(qID == 7))
  cbc7_options_w_concentration <- make_cbc_options_concentration(df |> filter(qID == 7))
  cbc7_options_w_embarrassed <- make_cbc_options_embarrassed(df |> filter(qID == 7))
  cbc7_options_w_unhappiness <- make_cbc_options_unhappiness(df |> filter(qID == 7))
  cbc7_options_w_treated <- make_cbc_options_treated(df |> filter(qID == 7))
  
  ###############################################################################
  
  cbc8_options <- make_cbc_options(df |> filter(qID == 8))
  cbc8_options_w_tired <- make_cbc_options_tired(df |> filter(qID == 8))
  cbc8_options_w_walking <- make_cbc_options_walking(df |> filter(qID == 8))
  cbc8_options_w_sports <- make_cbc_options_sports(df |> filter(qID == 8))
  cbc8_options_w_concentration <- make_cbc_options_concentration(df |> filter(qID == 8))
  cbc8_options_w_embarrassed <- make_cbc_options_embarrassed(df |> filter(qID == 8))
  cbc8_options_w_unhappiness <- make_cbc_options_unhappiness(df |> filter(qID == 8))
  cbc8_options_w_treated <- make_cbc_options_treated(df |> filter(qID == 8))
  
  ###############################################################################
  
  cbc9_options <- make_cbc_options(df |> filter(qID == 9))
  cbc9_options_w_tired <- make_cbc_options_tired(df |> filter(qID == 9))
  cbc9_options_w_walking <- make_cbc_options_walking(df |> filter(qID == 9))
  cbc9_options_w_sports <- make_cbc_options_sports(df |> filter(qID == 9))
  cbc9_options_w_concentration <- make_cbc_options_concentration(df |> filter(qID == 9))
  cbc9_options_w_embarrassed <- make_cbc_options_embarrassed(df |> filter(qID == 9))
  cbc9_options_w_unhappiness <- make_cbc_options_unhappiness(df |> filter(qID == 9))
  cbc9_options_w_treated <- make_cbc_options_treated(df |> filter(qID == 9))
  
  ###############################################################################
  
  cbc10_options <- make_cbc_options(df |> filter(qID == 10))
  cbc10_options_w_tired <- make_cbc_options_tired(df |> filter(qID == 10))
  cbc10_options_w_walking <- make_cbc_options_walking(df |> filter(qID == 10))
  cbc10_options_w_sports <- make_cbc_options_sports(df |> filter(qID == 10))
  cbc10_options_w_concentration <- make_cbc_options_concentration(df |> filter(qID == 10))
  cbc10_options_w_embarrassed <- make_cbc_options_embarrassed(df |> filter(qID == 10))
  cbc10_options_w_unhappiness <- make_cbc_options_unhappiness(df |> filter(qID == 10))
  cbc10_options_w_treated <- make_cbc_options_treated(df |> filter(qID == 10))
  
  ###############################################################################
  
  # Create each choice question
  
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q1',
      label  = "(1 of 10) Select the **best** option",
      option = cbc1_options,
      direction = "vertical"
  )
  
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q1_w_tired',
      label  = "(1 of 10) Select the **worst** option",
      option = cbc1_options_w_tired,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q1_w_walking',
      label  = "(1 of 10) Select the **worst** option",
      option = cbc1_options_w_walking,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q1_w_sports',
      label  = "(1 of 10) Select the **worst** option",
      option = cbc1_options_w_sports,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q1_w_concentration',
      label  = "(1 of 10) Select the **worst** option",
      option = cbc1_options_w_concentration,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q1_w_embarrassed',
      label  = "(1 of 10) Select the **worst** option",
      option = cbc1_options_w_embarrassed,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q1_w_unhappiness',
      label  = "(1 of 10) Select the **worst** option",
      option = cbc1_options_w_unhappiness,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q1_w_treated',
      label  = "(1 of 10) Select the **worst** option",
      option = cbc1_options_w_treated,
      direction = "vertical"
  )
  
  ###############################################################################
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q2',
      label  = "(2 of 10) Select the **best** option",
      option = cbc2_options,
      direction = "vertical"
  )
  
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q2_w_tired',
      label  = "(2 of 10) Select the **worst** option",
      option = cbc2_options_w_tired,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q2_w_walking',
      label  = "(2 of 10) Select the **worst** option",
      option = cbc2_options_w_walking,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q2_w_sports',
      label  = "(2 of 10) Select the **worst** option",
      option = cbc2_options_w_sports,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q2_w_concentration',
      label  = "(2 of 10) Select the **worst** option",
      option = cbc2_options_w_concentration,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q2_w_embarrassed',
      label  = "(2 of 10) Select the **worst** option",
      option = cbc2_options_w_embarrassed,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q2_w_unhappiness',
      label  = "(2 of 10) Select the **worst** option",
      option = cbc2_options_w_unhappiness,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q2_w_treated',
      label  = "(2 of 10) Select the **worst** option",
      option = cbc2_options_w_treated,
      direction = "vertical"
  )
  
  ###############################################################################
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q3',
      label  = "(3 of 10) Select the **best** option",
      option = cbc3_options,
      direction = "vertical"
  )
  
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q3_w_tired',
      label  = "(3 of 10) Select the **worst** option",
      option = cbc3_options_w_tired,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q3_w_walking',
      label  = "(3 of 10) Select the **worst** option",
      option = cbc3_options_w_walking,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q3_w_sports',
      label  = "(3 of 10) Select the **worst** option",
      option = cbc3_options_w_sports,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q3_w_concentration',
      label  = "(3 of 10) Select the **worst** option",
      option = cbc3_options_w_concentration,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q3_w_embarrassed',
      label  = "(3 of 10) Select the **worst** option",
      option = cbc3_options_w_embarrassed,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q3_w_unhappiness',
      label  = "(3 of 10) Select the **worst** option",
      option = cbc3_options_w_unhappiness,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q3_w_treated',
      label  = "(3 of 10) Select the **worst** option",
      option = cbc3_options_w_treated,
      direction = "vertical"
  )
  
  ###############################################################################
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q4',
      label  = "(4 of 10) Select the **best** option",
      option = cbc4_options,
      direction = "vertical"
  )
  
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q4_w_tired',
      label  = "(4 of 10) Select the **worst** option",
      option = cbc4_options_w_tired,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q4_w_walking',
      label  = "(4 of 10) Select the **worst** option",
      option = cbc4_options_w_walking,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q4_w_sports',
      label  = "(4 of 10) Select the **worst** option",
      option = cbc4_options_w_sports,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q4_w_concentration',
      label  = "(4 of 10) Select the **worst** option",
      option = cbc4_options_w_concentration,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q4_w_embarrassed',
      label  = "(4 of 10) Select the **worst** option",
      option = cbc4_options_w_embarrassed,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q4_w_unhappiness',
      label  = "(4 of 10) Select the **worst** option",
      option = cbc4_options_w_unhappiness,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q4_w_treated',
      label  = "(4 of 10) Select the **worst** option",
      option = cbc4_options_w_treated,
      direction = "vertical"
  )
  
  ###############################################################################
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q5',
      label  = "(5 of 10) Select the **best** option",
      option = cbc5_options,
      direction = "vertical"
  )
  
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q5_w_tired',
      label  = "(5 of 10) Select the **worst** option",
      option = cbc5_options_w_tired,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q5_w_walking',
      label  = "(5 of 10) Select the **worst** option",
      option = cbc5_options_w_walking,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q5_w_sports',
      label  = "(5 of 10) Select the **worst** option",
      option = cbc5_options_w_sports,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q5_w_concentration',
      label  = "(5 of 10) Select the **worst** option",
      option = cbc5_options_w_concentration,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q5_w_embarrassed',
      label  = "(5 of 10) Select the **worst** option",
      option = cbc5_options_w_embarrassed,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q5_w_unhappiness',
      label  = "(5 of 10) Select the **worst** option",
      option = cbc5_options_w_unhappiness,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q5_w_treated',
      label  = "(5 of 10) Select the **worst** option",
      option = cbc5_options_w_treated,
      direction = "vertical"
  )
  
  ###############################################################################
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q6',
      label  = "(6 of 10) Select the **best** option",
      option = cbc6_options,
      direction = "vertical"
  )
  
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q6_w_tired',
      label  = "(6 of 10) Select the **worst** option",
      option = cbc6_options_w_tired,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q6_w_walking',
      label  = "(6 of 10) Select the **worst** option",
      option = cbc6_options_w_walking,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q6_w_sports',
      label  = "(6 of 10) Select the **worst** option",
      option = cbc6_options_w_sports,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q6_w_concentration',
      label  = "(6 of 10) Select the **worst** option",
      option = cbc6_options_w_concentration,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q6_w_embarrassed',
      label  = "(6 of 10) Select the **worst** option",
      option = cbc6_options_w_embarrassed,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q6_w_unhappiness',
      label  = "(6 of 10) Select the **worst** option",
      option = cbc6_options_w_unhappiness,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q6_w_treated',
      label  = "(6 of 10) Select the **worst** option",
      option = cbc6_options_w_treated,
      direction = "vertical"
  )
  
  ###############################################################################
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q7',
      label  = "(7 of 10) Select the **best** option",
      option = cbc7_options,
      direction = "vertical"
  )
  
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q7_w_tired',
      label  = "(7 of 10) Select the **worst** option",
      option = cbc7_options_w_tired,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q7_w_walking',
      label  = "(7 of 10) Select the **worst** option",
      option = cbc7_options_w_walking,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q7_w_sports',
      label  = "(7 of 10) Select the **worst** option",
      option = cbc7_options_w_sports,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q7_w_concentration',
      label  = "(7 of 10) Select the **worst** option",
      option = cbc7_options_w_concentration,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q7_w_embarrassed',
      label  = "(7 of 10) Select the **worst** option",
      option = cbc7_options_w_embarrassed,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q7_w_unhappiness',
      label  = "(7 of 10) Select the **worst** option",
      option = cbc7_options_w_unhappiness,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q7_w_treated',
      label  = "(7 of 10) Select the **worst** option",
      option = cbc7_options_w_treated,
      direction = "vertical"
  )
  
  ###############################################################################
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q8',
      label  = "(8 of 10) Select the **best** option",
      option = cbc8_options,
      direction = "vertical"
  )
  
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q8_w_tired',
      label  = "(8 of 10) Select the **worst** option",
      option = cbc8_options_w_tired,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q8_w_walking',
      label  = "(8 of 10) Select the **worst** option",
      option = cbc8_options_w_walking,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q8_w_sports',
      label  = "(8 of 10) Select the **worst** option",
      option = cbc8_options_w_sports,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q8_w_concentration',
      label  = "(8 of 10) Select the **worst** option",
      option = cbc8_options_w_concentration,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q8_w_embarrassed',
      label  = "(8 of 10) Select the **worst** option",
      option = cbc8_options_w_embarrassed,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q8_w_unhappiness',
      label  = "(8 of 10) Select the **worst** option",
      option = cbc8_options_w_unhappiness,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q8_w_treated',
      label  = "(8 of 10) Select the **worst** option",
      option = cbc8_options_w_treated,
      direction = "vertical"
  )
  ###############################################################################
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q9',
      label  = "(9 of 10) Select the **best** option",
      option = cbc9_options,
      direction = "vertical"
  )
  
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q9_w_tired',
      label  = "(9 of 10) Select the **worst** option",
      option = cbc9_options_w_tired,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q9_w_walking',
      label  = "(9 of 10) Select the **worst** option",
      option = cbc9_options_w_walking,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q9_w_sports',
      label  = "(9 of 10) Select the **worst** option",
      option = cbc9_options_w_sports,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q9_w_concentration',
      label  = "(9 of 10) Select the **worst** option",
      option = cbc9_options_w_concentration,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q9_w_embarrassed',
      label  = "(9 of 10) Select the **worst** option",
      option = cbc9_options_w_embarrassed,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q9_w_unhappiness',
      label  = "(9 of 10) Select the **worst** option",
      option = cbc9_options_w_unhappiness,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q9_w_treated',
      label  = "(9 of 10) Select the **worst** option",
      option = cbc9_options_w_treated,
      direction = "vertical"
  )
  ###############################################################################
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q10',
      label  = "(10 of 10) Select the **best** option",
      option = cbc10_options,
      direction = "vertical"
  )
  
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q10_w_tired',
      label  = "(10 of 10) Select the **worst** option",
      option = cbc10_options_w_tired,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q10_w_walking',
      label  = "(10 of 10) Select the **worst** option",
      option = cbc10_options_w_walking,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q10_w_sports',
      label  = "(10 of 10) Select the **worst** option",
      option = cbc10_options_w_sports,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q10_w_concentration',
      label  = "(10 of 10) Select the **worst** option",
      option = cbc10_options_w_concentration,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q10_w_embarrassed',
      label  = "(10 of 10) Select the **worst** option",
      option = cbc10_options_w_embarrassed,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q10_w_unhappiness',
      label  = "(10 of 10) Select the **worst** option",
      option = cbc10_options_w_unhappiness,
      direction = "vertical"
  )
  sd_question(
      type   = 'mc_buttons',
      id     = 'cbc_q10_w_treated',
      label  = "(10 of 10) Select the **worst** option",
      option = cbc10_options_w_treated,
      direction = "vertical"
  )
  ###############################################################################
  


  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_if(
    input$screenout == "blue" ~ "end_screenout",
    input$consent == "no" ~ "end_consent",
    input$consent_understand == "no" ~ "end_consent"
  )

  # Define any conditional display logic here (show a question if a condition is true)
  sd_show_if(
    input$cbc_practice_best %in% c("tired") ~ "cbc_practice_w_tired",
    input$cbc_practice_best %in% c("walking") ~ "cbc_practice_w_walking",
    input$cbc_practice_best %in% c("sports") ~ "cbc_practice_w_sports",
    input$cbc_practice_best %in% c("concentration") ~ "cbc_practice_w_concentration",
    input$cbc_practice_best %in% c("embarrassed") ~ "cbc_practice_w_embarrassed",
    input$cbc_practice_best %in% c("unhappiness") ~ "cbc_practice_w_unhappiness",
    input$cbc_practice_best %in% c("treated") ~ "cbc_practice_w_treated",
    
    input$cbc_q1 %in% c("tired") ~ "cbc_q1_w_tired",
    input$cbc_q1 %in% c("walking") ~ "cbc_q1_w_walking",
    input$cbc_q1 %in% c("sports") ~ "cbc_q1_w_sports",
    input$cbc_q1 %in% c("concentration") ~ "cbc_q1_w_concentration",
    input$cbc_q1 %in% c("embarrassed") ~ "cbc_q1_w_embarrassed",
    input$cbc_q1 %in% c("unhappiness") ~ "cbc_q1_w_unhappiness",
    input$cbc_q1 %in% c("treated") ~ "cbc_q1_w_treated",
    
    input$cbc_q2 %in% c("tired") ~ "cbc_q2_w_tired",
    input$cbc_q2 %in% c("walking") ~ "cbc_q2_w_walking",
    input$cbc_q2 %in% c("sports") ~ "cbc_q2_w_sports",
    input$cbc_q2 %in% c("concentration") ~ "cbc_q2_w_concentration",
    input$cbc_q2 %in% c("embarrassed") ~ "cbc_q2_w_embarrassed",
    input$cbc_q2 %in% c("unhappiness") ~ "cbc_q2_w_unhappiness",
    input$cbc_q2 %in% c("treated") ~ "cbc_q2_w_treated",

    input$cbc_q3 %in% c("tired") ~ "cbc_q3_w_tired",
    input$cbc_q3 %in% c("walking") ~ "cbc_q3_w_walking",
    input$cbc_q3 %in% c("sports") ~ "cbc_q3_w_sports",
    input$cbc_q3 %in% c("concentration") ~ "cbc_q3_w_concentration",
    input$cbc_q3 %in% c("embarrassed") ~ "cbc_q3_w_embarrassed",
    input$cbc_q3 %in% c("unhappiness") ~ "cbc_q3_w_unhappiness",
    input$cbc_q3 %in% c("treated") ~ "cbc_q3_w_treated",

    input$cbc_q4 %in% c("tired") ~ "cbc_q4_w_tired",
    input$cbc_q4 %in% c("walking") ~ "cbc_q4_w_walking",
    input$cbc_q4 %in% c("sports") ~ "cbc_q4_w_sports",
    input$cbc_q4 %in% c("concentration") ~ "cbc_q4_w_concentration",
    input$cbc_q4 %in% c("embarrassed") ~ "cbc_q4_w_embarrassed",
    input$cbc_q4 %in% c("unhappiness") ~ "cbc_q4_w_unhappiness",
    input$cbc_q4 %in% c("treated") ~ "cbc_q4_w_treated",

    input$cbc_q5 %in% c("tired") ~ "cbc_q5_w_tired",
    input$cbc_q5 %in% c("walking") ~ "cbc_q5_w_walking",
    input$cbc_q5 %in% c("sports") ~ "cbc_q5_w_sports",
    input$cbc_q5 %in% c("concentration") ~ "cbc_q5_w_concentration",
    input$cbc_q5 %in% c("embarrassed") ~ "cbc_q5_w_embarrassed",
    input$cbc_q5 %in% c("unhappiness") ~ "cbc_q5_w_unhappiness",
    input$cbc_q5 %in% c("treated") ~ "cbc_q5_w_treated",

    input$cbc_q6 %in% c("tired") ~ "cbc_q6_w_tired",
    input$cbc_q6 %in% c("walking") ~ "cbc_q6_w_walking",
    input$cbc_q6 %in% c("sports") ~ "cbc_q6_w_sports",
    input$cbc_q6 %in% c("concentration") ~ "cbc_q6_w_concentration",
    input$cbc_q6 %in% c("embarrassed") ~ "cbc_q6_w_embarrassed",
    input$cbc_q6 %in% c("unhappiness") ~ "cbc_q6_w_unhappiness",
    input$cbc_q6 %in% c("treated") ~ "cbc_q6_w_treated",

    input$cbc_q7 %in% c("tired") ~ "cbc_q7_w_tired",
    input$cbc_q7 %in% c("walking") ~ "cbc_q7_w_walking",
    input$cbc_q7 %in% c("sports") ~ "cbc_q7_w_sports",
    input$cbc_q7 %in% c("concentration") ~ "cbc_q7_w_concentration",
    input$cbc_q7 %in% c("embarrassed") ~ "cbc_q7_w_embarrassed",
    input$cbc_q7 %in% c("unhappiness") ~ "cbc_q7_w_unhappiness",
    input$cbc_q7 %in% c("treated") ~ "cbc_q7_w_treated",

    input$cbc_q8 %in% c("tired") ~ "cbc_q8_w_tired",
    input$cbc_q8 %in% c("walking") ~ "cbc_q8_w_walking",
    input$cbc_q8 %in% c("sports") ~ "cbc_q8_w_sports",
    input$cbc_q8 %in% c("concentration") ~ "cbc_q8_w_concentration",
    input$cbc_q8 %in% c("embarrassed") ~ "cbc_q8_w_embarrassed",
    input$cbc_q8 %in% c("unhappiness") ~ "cbc_q8_w_unhappiness",
    input$cbc_q8 %in% c("treated") ~ "cbc_q8_w_treated",

    input$cbc_q9 %in% c("tired") ~ "cbc_q9_w_tired",
    input$cbc_q9 %in% c("walking") ~ "cbc_q9_w_walking",
    input$cbc_q9 %in% c("sports") ~ "cbc_q9_w_sports",
    input$cbc_q9 %in% c("concentration") ~ "cbc_q9_w_concentration",
    input$cbc_q9 %in% c("embarrassed") ~ "cbc_q9_w_embarrassed",
    input$cbc_q9 %in% c("unhappiness") ~ "cbc_q9_w_unhappiness",
    input$cbc_q9 %in% c("treated") ~ "cbc_q9_w_treated",

    input$cbc_q10 %in% c("tired") ~ "cbc_q10_w_tired",
    input$cbc_q10 %in% c("walking") ~ "cbc_q10_w_walking",
    input$cbc_q10 %in% c("sports") ~ "cbc_q10_w_sports",
    input$cbc_q10 %in% c("concentration") ~ "cbc_q10_w_concentration",
    input$cbc_q10 %in% c("embarrassed") ~ "cbc_q10_w_embarrassed",
    input$cbc_q10 %in% c("unhappiness") ~ "cbc_q10_w_unhappiness",
    input$cbc_q10 %in% c("treated") ~ "cbc_q10_w_treated"
  )

  # Database designation and other settings
  sd_server(
    db = db,
    all_questions_required = TRUE
  )

}

# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
