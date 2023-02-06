fluidPage(
  useShinyjs(),
  titlePanel("Where are the whales?"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "filepathway",
        "File Pathway",
        choices = c("Network", "Local"),
        selected = "Network",
        inline = FALSE
      ),
      textInput("filepathinput", (HTML(
        paste(
          "Local pathway for edit_data folders and photo submission csv",
          '<br/>',
          "Example: C:/Users/leah.crowe/Desktop/Canada Data Processing/"
        )
      ))),
      radioButtons(
        "tzone",
        "Camera Time Zone",
        choices = c("Atlantic Time", "Eastern Time"),
        selected = "Eastern Time",
        inline = FALSE
      ),
      textInput("photoyear", "Year (YYYY)"),
      textInput(
        "photofile",
        "Filename (must be a csv, but do not enter the extension)"
      ),
      textInput("permit", "Permit Number", placeholder = "MMPA #####"),
      actionButton("photogo", "Get positions")
    ),
    mainPanel(
      (HTML(
        paste(
          '<br/>',
          "CSV must include these columns:",
          '<br/>',
          '<br/>',
          "Field EGNO, EG Letter, Local Time, Day, Month, Year, Latitude, Longitude, Area, Obs, Platform, Image Type, Assoc. Type, Behaviors, Notes, Photographer, Frames, First Edit, Second Edit, Final Edit",
          '<br/>',
          '<br/>',
          'Latitude, Longitude, Area, Obs, Platform, and Image Type <strong>can be blank</strong>.',
          '<br/>',
          '<br/>',
          "Local Time and EG Letter should <strong>not be blank</strong>.",
          '<br/>',
          '<br/>'
        )
      )),
      br(),
      textOutput("finalmess"),
      br(),
      leafletOutput("finalleaf")
    )
  )
)
