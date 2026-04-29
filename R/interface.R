


#' @export
create_ui <- function(header)
  shiny::fluidPage(
    shiny::titlePanel(header),
    shiny::uiOutput("page")
  )




#' @export
create_server <- function(repository_name,
                          repository_location,
                          local_data_directory,
                          forms,
                          databases,
                          variables,
                          config,
                          cleaning_function,
                          zenodo,
                          cache)

  function(input, output, session){

    rv <- shiny::reactiveValues(
      step         = 0,
      language     = NULL,
      messages     = list(),
      token        = NULL,
      kobo         = NULL,
      log          = character(0),
      proc         = NULL,
      log_file     = NULL,
      running      = FALSE
    )




    # ---------------------------  UIs  ---------------------------- #



    output$page <- shiny::renderUI({

      if(rv$step == 0){
        shiny::tagList(
          shiny::h1("\n"),
          shiny::actionButton("lang_en", "English"),
          shiny::actionButton("lang_pt", "Português"),
          shiny::actionButton("lang_es", "Español")
        )
      } else

        if(rv$step == 1){
          shiny::tagList(
            shiny::h4(rv$messages$zenodo_authentication_TITLE),
            shiny::textInput("token", rv$messages$enter_your_zenodo_token),
            shiny::actionButton("submit_token", "OK"),
            shiny::uiOutput("zenodo_wrong_token")
          )
        } else

          if(rv$step == 2){
            shiny::tagList(
              shiny::h4(rv$messages$zenodo_authentication_TITLE),
              shiny::textOutput("zenodo_confirm"),
              shiny::actionButton("yes_token", rv$messages$yes),
              shiny::actionButton("no_token",  rv$messages$no)
            )
          } else

            if(rv$step == 3){
              shiny::tagList(
                shiny::h4(rv$messages$kobotoolbox_authentication_TITLE),
                shiny::textInput("kobo_url",       rv$messages$input_kobo_url),
                shiny::textInput("kobo_user",      rv$messages$kobo_username),
                shiny::passwordInput ("kobo_pwd",  rv$messages$kobo_password),
                shiny::actionButton("kobo_login",  rv$messages$kobo_login),
                shiny::uiOutput("kobo_wrong_credentials")
              )
            } else

              if(rv$step == 4){
                shiny::tagList(
                  shiny::h4(rv$messages$kobotoolbox_authentication_TITLE),
                  shiny::textOutput("kobo_confirm"),
                  shiny::actionButton("yes_kobo", rv$messages$yes),
                  shiny::actionButton("no_kobo",  rv$messages$no)
                )
              } else

                if(rv$step == 5){

                  if(rv$running){
                    shiny::tagList(
                      shiny::h4("Running..."),
                      shiny::verbatimTextOutput("log_output")
                    )
                  } else {
                    shiny::tagList(
                      shiny::actionButton("run_1", rv$messages$download_new_data),
                      shiny::br(), shiny::br(),
                      shiny::actionButton("run_2", rv$messages$collate_new_data),
                      shiny::br(), shiny::br(),
                      shiny::actionButton("run_3", rv$messages$update_data_from_local),
                      shiny::br(), shiny::br(),
                      shiny::actionButton("run_4", rv$messages$delete_processed_instances),
                      shiny::br(), shiny::br(),
                      shiny::actionButton("run_5", rv$messages$estimate_catch),
                      shiny::hr(),
                      shiny::verbatimTextOutput("log_output")
                    )
                  }
                }
    })



    # ---------------------  STEP 0: LANGUAGE  --------------------- #





    shiny::observeEvent(input$lang_en, {
      rv$messages <- interface_messages("en")
      rv$language <- "en"
      rv$step <- 1
    })

    shiny::observeEvent(input$lang_pt, {
      rv$messages <- interface_messages("pt")
      rv$language <- "pt"
      rv$step <- 1
    })

    shiny::observeEvent(input$lang_es, {
      rv$messages <- interface_messages("es")
      rv$language <- "es"
      rv$step <- 1
    })


    # ----------------------  STEP 1: ZENODO  ---------------------- #


    # Get zenodo link based on zenodo instance
    if(zenodo[["instance"]] == "zenodo")
      zenodo_link <- "https://zenodo.org/api/deposit/depositions"
    if(zenodo[["instance"]] == "sandbox")
      zenodo_link <- "https://sandbox.zenodo.org/api/deposit/depositions"



    # Functions to test whether zenodo credentials are correct
    test_zenodo <- function(token){
      res <- try(httr::GET(zenodo_link,
                           httr::add_headers(Authorization = paste("Bearer", token)),
                           query = list(size = 1)),
                 silent = TRUE)
      !inherits(res, "try-error") && httr::status_code(res) == 200
    }


    # Check for cached file and test it
    token_path  <- file.path(cache,"zenodo.rds")

    shiny::observe({
      shiny::req(rv$step == 1)

      if(file.exists(token_path)){

        token <- readRDS(token_path)

        if(test_zenodo(token)){
          rv$token <- token
          rv$step <- 2
        } else {
          file.remove(token_path)
        }
      }
    })


    # Check credentials inputted by user and test them
    shiny::observeEvent(input$submit_token, {

      token <- input$token

      if(test_zenodo(token)){
        saveRDS(token, token_path)
        rv$token <- token
        rv$step <- 2
      } else {
        output$zenodo_wrong_token <- shiny::renderUI(
          shiny::tags$p(rv$messages$invalid_token, style = "color: red; font-size: 18px;")
        )
      }
    })


    # Display confirmation panel
    output$zenodo_confirm <- shiny::renderText({
      paste0(rv$messages$using_zenodo_token, " '", rv$token, "'\n", rv$messages$continue)
    })

    shiny::observeEvent(input$yes_token, { rv$step <- 3 })
    shiny::observeEvent(input$no_token, {
      if(file.exists(token_path)) file.remove(token_path)
      rv$step <- 1
    })





    # -----------------------  STEP 2: KOBO  ----------------------- #




    # Functions to test whether kobo credentials are correct
    test_kobo <- function(kobo){
      res <- try(httr::GET(paste0("https://", kobo$url, "/api/v2/assets.json"),
                           httr::authenticate(user     = kobo$username,
                                              password = kobo$password)),
                 silent = TRUE)
      !inherits(res, "try-error") && httr::status_code(res) == 200
    }



    # Check for cached credentials and test it
    kobo_path <- file.path(cache,"kobo.rds")
    shiny::observe({
      shiny::req(rv$step == 3)

      if(file.exists(kobo_path)){

        kobo <- readRDS(kobo_path)

        if(!all(c("url","username","password") %in% names(kobo))){
          file.remove(kobo_path)
          return()
        }

        if(test_kobo(kobo)){
          saveRDS(kobo, kobo_path)
          rv$kobo <- kobo
          rv$step <- 4
        } else {
          file.remove(kobo_path)
          rv$step <- 3
        }

      }
    })



    # Check credentials inputted by user and test them
    shiny::observeEvent(input$kobo_login, {

      kobo <- list(
        url      = input$kobo_url,
        username = input$kobo_user,
        password = input$kobo_pwd
      )

      if(test_kobo(kobo)){
        saveRDS(kobo, kobo_path)
        rv$kobo <- kobo
        rv$step <- 4
      } else {
        output$kobo_wrong_credentials <- shiny::renderUI(
          shiny::tags$p(rv$messages$kobo_login_failed, style = "color: red; font-size: 18px;")
        )
      }
    })



    # Display confirmation panel
    output$kobo_confirm <- shiny::renderText({
      paste0(rv$messages$logged_in_kobotoolbox_as, " ", rv$kobo$username, ". \n", rv$messages$continue)
    })

    shiny::observeEvent(input$yes_kobo, { rv$step <- 5 })

    shiny::observeEvent(input$no_kobo, {
      if(file.exists(kobo_path)) file.remove(kobo_path)
      rv$step <- 3
    })





    # ----------------  STEP 3: RUNNING KOBOMANAGER  --------------- #


    ### Function that runs functions with even logger

    buttons_with_logger <- function(fun, args) {

      rv$log      <- character(0)
      rv$log_file <- tempfile()
      rv$running  <- TRUE

      rv$proc <- callr::r_bg(
        func = function(log_file,
                        fun,
                        args) {


          # Redirect cat() output to the log file
          con <- file(log_file, open = "at")
          sink(con, type = "output")
          sink(con, type = "message")

          log <- function(...) {
            cat(..., sep = "")
            flush(con)
          }

          withCallingHandlers({
            do.call(fun, args)
          },
          message = function(m) {
            log(conditionMessage(m))
            invokeRestart("muffleMessage")
          },
          warning = function(w) {
            log("WARNING:", conditionMessage(w))
            invokeRestart("muffleWarning")
          },
          error = function(e) {
            log("ERROR:", conditionMessage(e))
          })

          sink(type = "output")
          sink(type = "message")
          close(con)
        },
        args = list(
          log_file = rv$log_file,
          fun      = fun,
          args     = args
        )
      )
    }

    ### Button events

    shiny::observeEvent(input$run_1, {
      buttons_with_logger(
        fun = download_new_submissions,
        args = list(kobo                = rv$kobo,
                    repository_name     = repository_name,
                    repository_location = repository_location,
                    forms               = forms,
                    databases           = databases,
                    variables           = variables,
                    config              = config,
                    cleaning_function   = cleaning_function,
                    zenodo              = zenodo,
                    token               = rv$token,
                    language            = rv$language)
      )
    })

    shiny::observeEvent(input$run_2, {    })

    shiny::observeEvent(input$run_3, {    })

    shiny::observeEvent(input$run_4, {    })

    shiny::observeEvent(input$run_5, {    })





    # ----------------  STEP 3: RUNNING KOBOMANAGER  --------------- #


    shiny::observe({

      shiny::req(rv$proc)

      shiny::invalidateLater(100, session)

      if(file.exists(rv$log_file)){
        rv$log <- readLines(rv$log_file, warn = FALSE)
      }

      if(!rv$proc$is_alive()){
        rv$running <- FALSE
        rv$proc <- NULL
      }
    })

    output$log_output <- shiny::renderText({
      paste(rv$log, collapse = "\n")
    })

  }
