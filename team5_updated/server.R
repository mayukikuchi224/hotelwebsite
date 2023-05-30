server <- function(input, output, session) {
  
  # reactive values----
  ho <- reactiveValues(
    pg = 1, # the selected webpage number
    hm = 1  # the selected image on the home page
  )
  
  tm <- reactiveValues(
    yt = 0, # hide/show youtube video
    vi = 0  # selected video index
  )
  
  ac <- reactiveValues(
    vr = 0, # view avail rooms (0-5)
    br = 0, # book room type (0-5)
    rr = 0, # show room rate (o or 1)
    pn = 0, # per night room rate
    rn = 0, # runif for room rate
    to = 0, # total for the room booking
  )
  
  fa <- reactiveValues(
    pg = 1, # facilities page (1 or 2)
  )
  
  sp <- reactiveValues(
    op = 0, # spa option (1-3)
  )
  
  di <- reactiveValues(
    ne = 0, # in or out of hotel (1 or 2, 0 is neither)
    re = 0  # the restaurant selected for reservation
  )
  
  at <- reactiveValues(
    op = 1, # attr option (1-5)
    bu = 0, # buy tickets (0 or 1)
  )
  
  pa <- reactiveValues(
    op = 0, # package option (1-3)
  )
  
  # wallpaper----
  output$wallppr <- renderUI(
    {
      setBackgroundImage(paste0('hotel', floor(ho$pg), '.jpg'))
    }
  )
  
  # ui tabs----
  output$uiTabs <- renderUI(
    div(
      style = 'padding:10px 0 0 0;',
      align = 'center',
      lapply(
        1:8,
        function(i) {
          tabBttn(i)
        }
      ),
      hr(style = 'border: single white 3px;')
    )
  )
  
  # function to change look of tab when selected
  tabBttn <- function(i) {
    t <- actionBttn(
      inputId = ti[i],
      label = tl[i],
      style = ifelse(ho$pg == i, 'simple', 'stretch'),
      color = ifelse(ho$pg == i, 'default', 'default'),
      size = 'sm'
    )
    return(t)
  }
  
  # _event tab buttons----
  observeEvent(eventExpr = input$home, {ho$pg <- 1})
  observeEvent(eventExpr = input$acco, {ho$pg <- 2})
  observeEvent(eventExpr = input$faci, {ho$pg <- 3})
  observeEvent(eventExpr = input$dini, {ho$pg <- 4})
  observeEvent(eventExpr = input$attr, {ho$pg <- 5})
  observeEvent(eventExpr = input$wedd, {ho$pg <- 6})
  observeEvent(eventExpr = input$pack, {ho$pg <- 7})
  observeEvent(eventExpr = input$abou, {ho$pg <- 8})
  
  # >>>>>>>>>>>>----
  # ui home page----
  output$uiHome <- renderUI(
    if (ho$pg == 1) {
      div(
        align = 'center',
        style = 'padding:4vh 15% 0 15%;',
        div(
          align = 'center',
          style = 'padding:0 10% 27px 10%;',
          wellPanel(
            style = paste0(
              'background-color:rgba(250,250,250,0.5); ',
              'padding:2px 2px 2px 2px;'
            ),
          img(
            src = 'tne.png',
            width = '85%'
          ))
        ),
        fluidRow(
          column(
            width = 6,
            align = 'right',
            img(
              src = 'home1.jpg',
              width = '100%',
              style = 'border:solid white 10px; border-radius:20px;'
            ),
            br(), br(),
            actionBttn(
              inputId = 'bookTrip',
              label = 'Book Your Next Trip',
              style = 'gradient',
              color = 'default',
              size = 'lg',
              block = TRUE
            )
          ),
          column(
            width = 6,
            align = 'left',
            img(
              src = 'home2.jpg',
              width = '100%',
              style = 'border:solid white 10px; border-radius:20px;'
            ),
            br(), br(),
            actionBttn(
              inputId = 'bookWed',
              label = 'Book Your Wedding',
              style = 'gradient',
              color = 'default',
              size = 'lg',
              block = TRUE
            )
          )
        )
      )
    }
  )
  
  # _event book trip----
  observeEvent(
    input$bookTrip,
    {
      ho$pg <- 2
    }
  )
  
  # _event book wedding----
  observeEvent(
    input$bookWed,
    {
      ho$pg <- 6
    }
  )
  
  # >>>>>>>>>>>>----
  # ui acco page----
  output$uiAcco <- renderUI(
    if (ho$pg == 2) {
      div(
        style = 'padding:0 5% 0 5%;',
        align = 'center',
        wellPanel(
          style = paste0(
            'background-color:rgba(250,250,250,0.8); ',
            'padding:10px 20px 20px 20px;'
          ),
          h3('Book a Room for Your Next Visit'),
          hr(),
          fluidRow(
            lapply(
              1:4,
              function(i) {
                column(
                  width = 3,
                  h4(
                    acco$room_type[i]
                  ),
                  img(
                    src = paste0('acco', i, '.jpg'),
                    width = '100%',
                    style = 'border-radius:10px;'
                  ),
                  br(),
                  br(),
                  actionBttn(
                    inputId = paste0('readRm', i),
                    label = 'Read More',
                    style = 'gradient',
                    color = 'primary',
                    size = 'md',
                    block = TRUE
                  ),
                  br(),
                  actionBttn(
                    inputId = paste0('bookRm', i),
                    label = 'Book This Room',
                    style = 'gradient',
                    color = 'success',
                    size = 'md',
                    block = TRUE
                  )
                )
              }
            )
          ),
          uiOutput('uiMoreRm'),
          uiOutput('uiBookRm'),
          uiOutput('uiRoomRt')
        )
      )
    }
  )
  
  # _events read more room----
  observeEvent(input$readRm1, {ac$vr <- 1; ac$br <- 0; ac$rr <- 0})
  observeEvent(input$readRm2, {ac$vr <- 2; ac$br <- 0; ac$rr <- 0})
  observeEvent(input$readRm3, {ac$vr <- 3; ac$br <- 0; ac$rr <- 0})
  observeEvent(input$readRm4, {ac$vr <- 4; ac$br <- 0; ac$rr <- 0})
  
  # _events book room----
  observeEvent(input$bookRm1, {ac$vr <- 0; ac$br <- 1; ac$rr <- 0})
  observeEvent(input$bookRm2, {ac$vr <- 0; ac$br <- 2; ac$rr <- 0})
  observeEvent(input$bookRm3, {ac$vr <- 0; ac$br <- 3; ac$rr <- 0})
  observeEvent(input$bookRm4, {ac$vr <- 0; ac$br <- 4; ac$rr <- 0})
  
  # _ui read more rooms----
  output$uiMoreRm <- renderUI(
    if (ac$vr > 0) {
      aa <- dbGetQuery(
        con,
        paste0(
          'SELECT * FROM accommodation_amenities ',
          'WHERE room_type_id = ',
          ac$vr,
          ';'
        )
      )
      div(
        style = 'padding:25px 15% 0 15%;',
        align = 'left',
        h4(acco$description[ac$vr]),
        br(),
        lapply(
          1:15, 
          function(i) {
            actionBttn(
              inputId = paste0('amen', i),
              label = amen$amenities_name[i],
              style = 'gradient',
              color = ifelse(
                i %in% aa$amenities_id,
                'primary',
                'default'
              ),
              size = 'sm',
              icon = icon(
                ifelse(
                  i %in% aa$amenities_id,
                  'check',
                  'remove'
                )
              )
            )
          }
        )
      )
    }
  )
  
  # _ui book rooms----
  output$uiBookRm <- renderUI(
    if (ac$br > 0) {
      div(
        style = 'padding:25px 15% 0 15%;',
        fluidRow(
          column(
            width = 4,
            dateInput(
              inputId = 'chkinDt',
              label = 'Check In Date',
              value = Sys.Date(),
              min = Sys.Date()
            )
          ),
          column(
            width = 4,
            pickerInput(
              inputId = 'numNights',
              label = 'Number of Nights',
              choices = c(1:7),
              selected = 2
            )
          ),
          column(
            width = 4,
            style = 'padding:20px 0 0 0;',
            actionBttn(
              inputId = 'checkRate',
              label = 'Check Rate',
              style = 'simple',
              color = 'primary',
              size = 'md'
            )
          )
        )
      )
    }
  )
  
  # _events check rate----
  observeEvent(input$checkRate, {ac$rr <- 1; ac$rn <- runif(1)})
  
  # _ui room rate----
  output$uiRoomRt <- renderUI(
    if (ac$rr == 1) {
      ac$pn <- floor(acco$price[ac$br] * (ac$rn + 0.5))
      ac$to <- as.numeric(input$numNights) * ac$pn + 
        round(0.15 * ac$pn, 0) + 
        round(0.12 * ac$pn, 0)
      div(
        h3(
          paste0(
            'Room Rate $', ac$pn, ' per night x ',
            input$numNights, ' + Service Fee $', round(0.15 * ac$pn, 0),
            ' + Tax $', round(0.12 * ac$pn, 0), ' = $',
            ac$to
          )
        ),
        actionBttn(
          inputId = 'confRm',
          label = 'Confirm Booking',
          style = 'simple',
          color = 'warning',
          size = 'md'
        )
      )
    }
  )
  
  # _event confirm room----
  observeEvent(
    input$confRm, 
    {
      sendSweetAlert(
        session = session,
        title = 'Confirmed!',
        text = paste0(
          'Your booking starting on ',
          input$chkinDt, 
          ' for ', 
          input$numNights,
          ' nights is confirmed. ',
          'The total amount due is $',
          ac$to, '. ',
          'Thank you!'
        ),
        type = 'success'
      )
    }
  )
  
  # >>>>>>>>>>>>----
  # ui faci page----
  output$uiFaci <- renderUI(
    if (ho$pg == 3) {
      if (fa$pg == 1) {
        uiOutput('uiFaci1')
      } else if (fa$pg == 2) {
        uiOutput('uiFaci2')
      }
    }
  )
 
  # _ui faci1----
  output$uiFaci1 <- renderUI(
    {
      div(
        style = 'padding:0 5% 0 5%;',
        align = 'center',
        wellPanel(
          style = paste0(
            'background-color:rgba(250,250,250,0.8); ',
            'padding:10px 20px 20px 20px;'
          ),
          h3('Our Facilities'),
          wellPanel(
            style = 'background-color:rgba(250,250,250,0.75);',
            fluidRow(
              column(
                width = 4,
                h3(
                  # style = 'color:white;',
                  faci$facility_name[1]
                ),
                img(
                  src = 'faci1.jpg',
                  width = '100%',
                  style = 'border-radius:10px;'
                )
              ),
              column(
                width = 8,
                div(
                  style = 'padding:100px 25px 0 25px;',
                  align = 'left',
                  h4(
                    # style = 'color:white;',
                    faci$facility_description[1]
                  ),
                  actionBttn(
                    inputId = 'exploreSpa',
                    label = 'Explore',
                    style = 'unite',
                    color = 'primary',
                    size = 'md'
                  )
                )
              )
            )
          ),
          wellPanel(
            style = 'background-color:rgba(250,250,250,0.75);',
            fluidRow(
              column(
                width = 8,
                div(
                  style = 'padding:125px 25px 0 25px;',
                  align = 'left',
                  h4(
                    # style = 'color:white;',
                    faci$facility_description[2]
                  )
                )
              ),
              column(
                width = 4,
                h3(
                  # style = 'color:white;',
                  faci$facility_name[2]
                ),
                img(
                  src = 'faci2.jpg',
                  width = '100%',
                  style = 'border-radius:10px;'
                )
              )
            )
          ),
          wellPanel(
            style = 'background-color:rgba(250,250,250,0.75);',
            fluidRow(
              column(
                width = 4,
                h3(
                  # style = 'color:white;',
                  faci$facility_name[3]
                ),
                img(
                  src = 'faci3.jpg',
                  width = '100%',
                  style = 'border-radius:10px;'
                )
              ),
              column(
                width = 8,
                div(
                  style = 'padding:125px 25px 0 25px;',
                  align = 'left',
                  h4(
                    # style = 'color:white;',
                    faci$facility_description[3]
                  )
                )
              )
            )
          ),
          wellPanel(
            style = 'background-color:rgba(250,250,250,0.75);',
            fluidRow(
              column(
                width = 8,
                div(
                  style = 'padding:125px 25px 0 25px;',
                  align = 'left',
                  h4(
                    # style = 'color:white;',
                    faci$facility_description[4]
                  )
                )
              ),
              column(
                width = 4,
                h3(
                  # style = 'color:white;',
                  faci$facility_name[4]
                ),
                img(
                  src = 'faci4.jpg',
                  width = '100%',
                  style = 'border-radius:10px;'
                )
              )
            )
          )
        )
      )
    }
  )
  
  # _ui faci2----
  output$uiFaci2 <- renderUI(
    {
      div(
        style = 'padding:0 10% 0 10%;',
        align = 'center',
        wellPanel(
          style = 'background-color:rgba(250,250,250,0.75);',
          fluidRow(
            column(
              width = 3,
              actionBttn(
                inputId = 'backFaci',
                label = 'Back to Facilities',
                style = 'minimal',
                color = 'default',
                size = 'sm',
                icon = icon('arrow-left')
              )
            ),
            column(
              width = 6,
              h3('Spa Menu')
            )
          ),
          hr(),
          lapply(
            1:3, 
            function(j) {
              fluidRow(
                column(
                  width = 6,
                  h4(spa$spamenu_type[j]),
                  img(
                    src = paste0(
                      'spa', j, '.jpg'
                    ),
                    width = '80%',
                    style = 'border-radius:10px;'
                  )
                ),
                column(
                  width = 6,
                  div(
                    align = 'left',
                    style = 'padding:50px 0 0 0;',
                    h4(spa$spamenu_description[j])
                  ),
                  actionBttn(
                    inputId = paste0('bookSpa', j),
                    label = 'Book an Appointment',
                    style = 'gradient',
                    color = 'success',
                    size = 'md'
                  ),
                  uiOutput(paste0('uiBookSpa', j))
                )
              )
            }
          )
        )
      )
    }
  )
  
  # _event back faci----
  observeEvent(input$backFaci, {fa$pg <- 1})
  
  # _event explore spa----
  observeEvent(input$exploreSpa, {fa$pg <- 2})
  
  # _events book spa----
  observeEvent(input$bookSpa1, {sp$op <- 1})
  observeEvent(input$bookSpa2, {sp$op <- 2})
  observeEvent(input$bookSpa3, {sp$op <- 3})
  
  # _ui BookSpa1----
  output$uiBookSpa1 <- renderUI(
    if (sp$op == 1) {
      div(
        hr(),
        fluidRow(
          column(
            width = 4,
            img(
              src = 'icon_date.png',
              width = '20%'
            ),
            br(),
            br(),
            dateInput(
              inputId = 'dtInputSpa1',
              label = NULL,
              value = Sys.Date(),
              min = Sys.Date(),
              width = '50%'
            )
          ),
          column(
            width = 4,
            img(
              src = 'icon_time.png',
              width = '20%'
            ),
            br(),
            br(),
            pickerInput(
              inputId = 'tmInputSpa1',
              label = NULL,
              choices = c(paste0(9:16, ':00')),
              selected = max(
                format(
                  as.POSIXct(
                    Sys.time() + lubridate::hours(1)
                  ), 
                  format = '%H:00'
                ),
                '9:00'
              )
            )
          ),
          column(
            width = 4,
            img(
              src = 'icon_pers.png',
              width = '20%'
            ),
            br(),
            br(),
            radioGroupButtons(
              inputId = 'ppInputSpa1',
              label = NULL,
              choices = c(1:4),
              selected = 2
            )
          )
        ),
        actionBttn(
          inputId = paste0('submitSpa1'),
          label = 'Confirm',
          style = 'simple',
          color = 'warning',
          size = 'md'
        )
      )
      
    }
  )
  
  # _ui BookSpa2----
  output$uiBookSpa2 <- renderUI(
    if (sp$op == 2) {
      div(
        hr(),
        fluidRow(
          column(
            width = 4,
            img(
              src = 'icon_date.png',
              width = '20%'
            ),
            br(),
            br(),
            dateInput(
              inputId = 'dtInputSpa2',
              label = NULL,
              value = Sys.Date(),
              min = Sys.Date(),
              width = '50%'
            )
          ),
          column(
            width = 4,
            img(
              src = 'icon_time.png',
              width = '20%'
            ),
            br(),
            br(),
            pickerInput(
              inputId = 'tmInputSpa2',
              label = NULL,
              choices = c(paste0(9:16, ':00')),
              selected = max(
                format(
                  as.POSIXct(
                    Sys.time() + lubridate::hours(1)
                  ), 
                  format = '%H:00'
                ),
                '9:00'
              )
            )
          ),
          column(
            width = 4,
            img(
              src = 'icon_pers.png',
              width = '20%'
            ),
            br(),
            br(),
            radioGroupButtons(
              inputId = 'ppInputSpa2',
              label = NULL,
              choices = c(1:4),
              selected = 2
            )
          )
        ),
        actionBttn(
          inputId = paste0('submitSpa2'),
          label = 'Confirm',
          style = 'gradient',
          color = 'success',
          size = 'md'
        )
      )
    }
  )
  
  # _ui BookSpa3----
  output$uiBookSpa3 <- renderUI(
    if (sp$op == 3) {
      div(
        hr(),
        fluidRow(
          column(
            width = 4,
            img(
              src = 'icon_date.png',
              width = '20%'
            ),
            br(),
            br(),
            dateInput(
              inputId = 'dtInputSpa3',
              label = NULL,
              value = Sys.Date(),
              min = Sys.Date(),
              width = '50%'
            )
          ),
          column(
            width = 4,
            img(
              src = 'icon_time.png',
              width = '20%'
            ),
            br(),
            br(),
            pickerInput(
              inputId = 'tmInputSpa3',
              label = NULL,
              choices = c(paste0(9:16, ':00')),
              selected = max(
                format(
                  as.POSIXct(
                    Sys.time() + lubridate::hours(1)
                  ), 
                  format = '%H:00'
                ),
                '9:00'
              )
            )
          ),
          column(
            width = 4,
            img(
              src = 'icon_pers.png',
              width = '20%'
            ),
            br(),
            br(),
            radioGroupButtons(
              inputId = 'ppInputSpa3',
              label = NULL,
              choices = c(1:4),
              selected = 2
            )
          )
        ),
        actionBttn(
          inputId = paste0('submitSpa3'),
          label = 'Confirm',
          style = 'gradient',
          color = 'success',
          size = 'md'
        )
      )
    }
  )
  
  # _event submit spa1----
  observeEvent(
    input$submitSpa1, 
    {
      sendSweetAlert(
        session = session,
        title = 'Confirmed!',
        text = paste0(
          'Your reservation for ',
          spa$spamenu_type[1],
          ' is confirmed. The cost will be added ',
          'to your hotel bill. ',
          'Thank you!'
        ),
        type = 'success'
      )
    }
  )
  
  # _event submit spa2----
  observeEvent(
    input$submitSpa2, 
    {
      sendSweetAlert(
        session = session,
        title = 'Confirmed!',
        text = paste0(
          'Your reservation for ',
          spa$spamenu_type[2],
          ' is confirmed. The cost will be added ',
          'to your hotel bill. ',
          'Thank you!'
        ),
        type = 'success'
      )
    }
  )
  
  # _event submit spa3----
  observeEvent(
    input$submitSpa3, 
    {
      sendSweetAlert(
        session = session,
        title = 'Confirmed!',
        text = paste0(
          'Your reservation for ',
          spa$spamenu_type[3],
          ' is confirmed. The cost will be added ',
          'to your hotel bill. ',
          'Thank you!'
        ),
        type = 'success'
      )
    }
  )
  
  # >>>>>>>>>>>>----
  # ui dini page----
  output$uiDini <- renderUI(
    if (ho$pg == 4) {
      div(
        style = 'padding:0 5% 0 5%;',
        align = 'center',
        wellPanel(
          style = paste0(
            'background-color:rgba(250,250,250,0.8); ',
            'padding:10px 20px 20px 20px;'
          ),
          h3('Where Would You Like to Dine?'),
          br(),
          br(),
          div(
            style = 'padding:0 25% 0 25%;',
            fluidRow(
              column(
                width = 6,
                actionBttn(
                  inputId = 'restIn',
                  label = 'At Our Hotel',
                  style = 'unite',
                  color = 'success',
                  size = 'lg',
                  block = TRUE
                )
              ),
              column(
                width = 6,
                actionBttn(
                  inputId = 'restOut',
                  label = 'Nearby Our Hotel',
                  style = 'unite',
                  color = 'royal',
                  size = 'lg',
                  block = TRUE
                )
              )
            )
          ),
          br(),
          uiOutput('uiRests'),
          uiOutput('uiResTbl')
        )
      )
    }
  )
  
  # _events rest in/out----
  observeEvent(input$restIn,  {di$ne <- 1; di$re <- 0})
  observeEvent(input$restOut, {di$ne <- 2; di$re <- 0})
  
  # _ui rests----
  output$uiRests <- renderUI(
    if (di$ne > 0) {
      fluidRow(
        lapply(
          1:4,
          function(i) {
            column(
              width = 3,
              h4(
                # style = 'color:white;',
                rest$rest_name[i + 4 * (di$ne - 1)]
              ),
              img(
                src = paste0('rest', i + 4 * (di$ne - 1), '.jpg'),
                width = '100%',
                style = 'border-radius:10px;'
              ),
              br(),
              br(),
              actionBttn(
                inputId = paste0('resTbl', i),
                label = 'Reserve',
                style = 'simple',
                color = 'primary',
                size = 'md'
              ),
              br(),
              br()
            )
          }
        )
      )
    }
  )
  
  # _event reserve table----
  observeEvent(input$resTbl1, {di$re <- 1 + 4 * (di$ne - 1)})
  observeEvent(input$resTbl2, {di$re <- 2 + 4 * (di$ne - 1)})
  observeEvent(input$resTbl3, {di$re <- 3 + 4 * (di$ne - 1)})
  observeEvent(input$resTbl4, {di$re <- 4 + 4 * (di$ne - 1)})
  
  # _ui restbl----
  output$uiResTbl <- renderUI(
    if (di$re > 0) {
      div(
        style = 'padding:0 25% 0 25%;',
        wellPanel(
          style = 'background-color:rgba(200,200,200,0.9);',
          h4(
            paste0(
              'Reserve a Table @ ',
              rest$rest_name[di$re]
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 4,
              img(
                src = 'icon_date.png',
                width = '20%'
              ),
              br(),
              br(),
              dateInput(
                inputId = 'dtInput',
                label = NULL,
                value = Sys.Date(),
                min = Sys.Date()
              )
            ),
            column(
              width = 4,
              img(
                src = 'icon_time.png',
                width = '20%'
              ),
              br(),
              br(),
              pickerInput(
                inputId = 'tmInput',
                label = NULL,
                choices = c(paste0(12:23, ':00')),
                selected = max(
                  format(
                    as.POSIXct(
                      Sys.time() + lubridate::hours(1)
                    ), 
                    format = '%H:00'
                  ),
                  '12:00'
                )
              )
            ),
            column(
              width = 4,
              img(
                src = 'icon_pers.png',
                width = '20%'
              ),
              br(),
              br(),
              radioGroupButtons(
                inputId = 'ppInput',
                label = NULL,
                choices = c(1:4),
                selected = 2
              )
            )
          ),
          hr(),
          actionBttn(
            inputId = 'submitRes',
            label = 'Submit',
            style = 'simple',
            color = 'warning',
            size = 'md'
          )
        )
      )
    }
  )
  
  # _event submit reservation----
  observeEvent(
    input$submitRes,
    {
      sendSweetAlert(
        session = session,
        title = 'Confirmed!',
        text = paste0(
          'Your table for ',
          input$ppInput, 
          ' on ',
          input$dtInput,
          ' ',
          input$tmInput,
          ' at ',
          rest$rest_name[di$re],
          ' is confirmed. See you soon!'
        ),
        type = 'success'
      )
      di$re <- 0
    }
  )
  
  # >>>>>>>>>>>>----
  # ui attr page----
  output$uiAttr <- renderUI(
    if (ho$pg == 5) {
      div(
        style = 'padding:0 5% 0 5%;',
        align = 'center',
        wellPanel(
          style = paste0(
            'background-color:rgba(250,250,250,0.8); ',
            'padding:10px 20px 20px 20px;'
          ),
          h3('Visit Our Nearby Attractions'),
          br(),
          fluidRow(
            column(
              width = 6,
              fluidRow(
                column(
                  width = 1,
                  style = 'padding:20vh 0 0 0;',
                  actionBttn(
                    inputId = 'attrBack',
                    label = NULL,
                    style = 'simple',
                    color = 'primary',
                    size = 'xs',
                    icon = icon('arrow-left')
                  )
                ),
                column(
                  width = 10,
                  img(
                    src = paste0('attr', at$op, '.jpg'),
                    width = '100%',
                    style = 'border-radius:10px 10px 0 0;'
                  ),
                  wellPanel(
                    style = 'background-color:rgba(250,250,250,0.8); border-radius:0 0 10px 10px;',
                    h4(
                      paste0(
                        attr$attractions_name[at$op],
                        ' - $',
                        attr$price[at$op]
                      )
                    ),
                    div(
                      align = 'left',
                      h5(attr$description[at$op])
                    )
                  ),
                  actionBttn(
                    inputId = paste0('buyAttr', at$op),
                    label = paste0('Buy Tickets'),
                    style = 'simple',
                    color = 'success',
                    size = 'sm'
                  )
                ),
                column(
                  width = 1,
                  style = 'padding:20vh 0 0 0;',
                  actionBttn(
                    inputId = 'attrFwd',
                    label = NULL,
                    style = 'simple',
                    color = 'primary',
                    size = 'xs',
                    icon = icon('arrow-right')
                  )
                )
              )
            ),
            column(
              width = 6,
              div(
                style = 'border-radius:10px;',
                leafletOutput(
                  outputId = 'attrMap',
                  height = '50vh'
                )
              )
            )
          )
        ),
        uiOutput('uiBuyAttr')
      )
    }
  )
  
  # _events attr arrows----
  observeEvent(input$attrBack, {at$op <- ifelse(at$op > 1, at$op - 1, 5)})
  observeEvent(input$attrFwd, {at$op <- ifelse(at$op < 5, at$op + 1, 1)})
  
  # _attr map----
  output$attrMap <- renderLeaflet(
    {
      z <- dbGetQuery(
        con,
        paste0(
          'SELECT attractions_id, attractions_name, lat, lng ',
          'FROM location JOIN attractions USING (locn_id);'
        )
      )
      leaflet(
        data = z
      ) %>% 
        addTiles() %>% 
        addAwesomeMarkers(
          lat = ~lat,
          lng = ~lng,
          icon = ~awesomeIcons(
            markerColor = c('green', 'green', 'green', 'green', 'green', 'red'),
            text = ifelse(attractions_id < 6, attractions_id, 'NE')
          ),
          popup = ~attractions_name
        )
    }
  )
  
  # _events buy tickets----
  observeEvent(input$buyAttr1, {at$bu <- 1})
  observeEvent(input$buyAttr2, {at$bu <- 1})
  observeEvent(input$buyAttr3, {at$bu <- 1})
  observeEvent(input$buyAttr4, {at$bu <- 1})
  observeEvent(input$buyAttr5, {at$bu <- 1})
  
  # _ui buyAttr----
  output$uiBuyAttr <- renderUI(
    if (at$bu == 1) {
      div(
        style = 'padding:0 25% 0 25%;',
        wellPanel(
          style = 'background-color:rgba(200,200,200,0.9);',
          h4(
            paste0(
              'Buy Tickets for ',
              attr$attractions_name[at$op]
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 6,
              img(
                src = 'icon_date.png',
                width = '20%'
              ),
              br(),
              br(),
              dateInput(
                inputId = 'dtInputA',
                label = NULL,
                value = Sys.Date(),
                min = Sys.Date()
              )
            ),
            column(
              width = 6,
              img(
                src = 'icon_pers.png',
                width = '20%'
              ),
              br(),
              br(),
              radioGroupButtons(
                inputId = 'ppInputA',
                label = NULL,
                choices = c(1:4),
                selected = 2
              )
            )
          ),
          hr(),
          actionBttn(
            inputId = 'submitTix',
            label = 'Submit',
            style = 'simple',
            color = 'warning',
            size = 'md'
          )
        )
      )
    }
  )
  
  # _event submit tix----
  observeEvent(input$submitTix, {buyMsg()})
  
  buyMsg <- function() {
    sendSweetAlert(
      session = session,
      title = 'Confirmed!',
      text = paste0(
        'Your tickets are confirmed. The cost will be added ',
        'to your hotel bill. ',
        'Thank you!'
      ),
      type = 'success'
    )
  }
  
  # >>>>>>>>>>>>----
  # ui wedd page----
  output$uiWedd <- renderUI(
    if (ho$pg == 6) {
      div(
        style = 'padding:0 5% 0 5%;',
        align = 'center',
        wellPanel(
          style = paste0(
            'background-color:rgba(250,250,250,0.8); ',
            'padding:10px 20px 20px 20px;'
          ),
          h3('Book Your Dream Wedding with Us'),
          fluidRow(
            lapply(
              1:4,
              function(i) {
                column(
                  width = 3,
                  h4(
                    # style = 'color:white;',
                    wedd$wedding_component[i]
                  ),
                  img(
                    src = paste0('wed', i, '.jpg'),
                    width = '100%',
                    style = 'border-radius:10px;'
                  ),
                  br(), br(),
                  actionBttn(
                    inputId = paste0('readMore', i),
                    label = 'Read More',
                    style = 'gradient',
                    color = 'primary',
                    block = TRUE
                  )
                )
              }
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 4,
              div(
                style = 'padding:15vh 0 0 0;',
                tags$button(
                  id = 'weddBtn',
                  class = 'btn action-button',
                  style = 'background-color:rgba(0,0,0,0); padding:5px 0 0 0;',
                  img(
                    src = 'rings.png',
                    width = '50%'
                  ),
                  h4('For reservations,'),
                  h4('contact us.')
                )
              )
            ),
            column(
              width = 8,
              tags$video(
                src = 'SQL Project.mp4',
                width = '100%',
                controls = TRUE,
                style = 'border-radius:10px;'
              )
            )
          )
        )
      )
    }
  )
  
  # _event read more----
  observeEvent(input$readMore1, {readMore(1)})
  observeEvent(input$readMore2, {readMore(2)})
  observeEvent(input$readMore3, {readMore(3)})
  observeEvent(input$readMore4, {readMore(4)})
  
  readMore <- function(i) {
    shinyalert(
      title = wedd$wedding_component[i],
      text = paste0(
        '<style>.myDiv {text-align:left;}</style>',
        '<div class = "myDiv"><p>', 
        wedd$wedding_description[i], 
        '</p></div>'
      ),
      showConfirmButton = FALSE,
      html = TRUE,
      closeOnClickOutside = TRUE
    )
  }
  
  # _event contact us---
  observeEvent(
    input$weddBtn, 
    {
      shinyalert(
        title = 'Contact Us',
        text = paste0(
          '<style>.myDiv {text-align:center;}</style>',
          '<div class = "myDiv">', 
          '<p>info@thenorthernescape.com</p>',
          '<p>+358 50 517 6909</p>',
          '<p>Guest service is available from Monday to Friday | 9 am - 4 pm</p>',
          '</div>'
        ),
        showConfirmButton = FALSE,
        html = TRUE,
        closeOnClickOutside = TRUE
      )
    }
  )
  
  # >>>>>>>>>>>>----
  # ui pack page----
  output$uiPack <- renderUI(
    if (ho$pg == 7) {
      div(
        style = 'padding:0 5% 0 5%;',
        align = 'center',
        wellPanel(
          style = paste0(
            'background-color:rgba(250,250,250,0.8); ',
            'padding:10px 20px 20px 20px;'
          ),
          h3('Try One of Our Package Deals'),
          fluidRow(
            lapply(
              1:3,
              function(i) {
                column(
                  width = 4,
                  h4(
                    # style = 'color:white;',
                    pack$package_name[i]
                  ),
                  img(
                    src = paste0('pack', i, '.jpg'),
                    width = '100%',
                    style = 'border-radius:10px;'
                  ),
                  div(
                    align = 'left',
                    h4(
                      # style = 'color:white;',
                      pack$package_description[i]
                    )
                  ),
                  actionBttn(
                    inputId = paste0('resPack', i),
                    label = paste0('$', pack$package_price[i]),
                    style = 'simple',
                    color = 'primary',
                    size = 'md'
                  )
                )
              }
            )
          )
        ),
        uiOutput('uiBuyPack')
      )
    }
  )
 
  # _events respack----
  observeEvent(input$resPack1, {pa$op <- 1})
  observeEvent(input$resPack2, {pa$op <- 2})
  observeEvent(input$resPack3, {pa$op <- 3})
  
  # _ui buypack----
  output$uiBuyPack <- renderUI(
    if (pa$op > 0) {
      div(
        style = 'padding:0 25% 0 25%;',
        wellPanel(
          style = 'background-color:rgba(200,200,200,0.9);',
          h4(
            paste0(
              'Buy ', pack$package_name[pa$op]
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 6,
              img(
                src = 'icon_date.png',
                width = '20%'
              ),
              br(),
              br(),
              dateInput(
                inputId = 'dtInputP',
                label = NULL,
                value = Sys.Date(),
                min = Sys.Date(),
                width = '50%'
              )
            ),
            column(
              width = 6,
              img(
                src = 'icon_pers.png',
                width = '20%'
              ),
              br(),
              br(),
              radioGroupButtons(
                inputId = 'ppInputP',
                label = NULL,
                choices = c(1:4),
                selected = 2
              )
            )
          ),
          hr(),
          actionBttn(
            inputId = 'submitPack',
            label = 'Submit',
            style = 'simple',
            color = 'warning',
            size = 'md'
          )
        )
      )
    }
  )
  
  # _event submit pack----
  observeEvent(
    input$submitPack,
    {
      sendSweetAlert(
        session = session,
        title = 'Confirmed!',
        text = paste0(
          'Your have purchased the ',
          pack$package_name[pa$op], '. ',
          'Thank you and enjoy!'
        ),
        type = 'success'
      )
    }
  )
  
  # >>>>>>>>>>>>----
  # ui abou page----
  output$uiAbou <- renderUI(
    if (ho$pg == 8) {
      div(
        style = 'padding:15vh 2% 0 2%;',
        fluidRow(
          column(
            width = 3,
            align = 'right',
            actionBttn(
              inputId = 'teamTitle',
              label = 'Our Team',
              style = 'bordered',
              color = 'default',
              size = 'lg',
              block = TRUE
            ),
            lapply(
              1:4, 
              function(i) {
                div(
                  actionBttn(
                    inputId = paste0('mem', i),
                    label = mem[i],
                    style = 'minimal',
                    color = 'default',
                    size = 'lg',
                    block = TRUE
                  )
                )
              }
            )
          )
        )
      )
    }
  )
  
  # _event team title bttn----
  observeEvent(
    input$teamTitle,
    {
      tm$yt <- 1
      tm$vi <- sample(1:5, 1)
    }
  )
  
  # _events mem bttns----
  observeEvent(input$mem1, {memInfo(1)})
  observeEvent(input$mem2, {memInfo(2)})
  observeEvent(input$mem3, {memInfo(3)})
  observeEvent(input$mem4, {memInfo(4)})
  
  memInfo <- function(i) {
    shinyalert(
      title = mem[i],
      text = paste0(
        '<style>.myDiv {text-align:left;}</style>',
        '<style>.myImg {border:solid lightblue 10px; border-radius:10%;}</style>',
        '<img class = "myImg" src = "member', i, '.jpg" width = "200px"><br><br>',
        '<div class = "myDiv"><p>', bio[i], '</p></div>'
      ),
      showConfirmButton = FALSE,
      html = TRUE,
      closeOnClickOutside = TRUE
    )
  }
  
}
