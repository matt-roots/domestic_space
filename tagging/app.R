library(shiny)
library(googlesheets4)
library(googledrive)
library(reactlog)
library(devtools)

#####!!!!! start by entering these commands
#drive_deauth()
#drive_auth()

# #####TD
# -make tagger a dropdown menu DONE
# -then it will give you the sentences
# -have annotation guidelines on page
# -do fewer calls
# -t-f-NA-trash options
# -a second option for "is this mixed?"

# -will draw from a single spreadsheet with passages and two assigned taggers
# -then output results to a different single sheet where each row is a passage and a tag by one coder (so two times as many rows as the first sheet)


#a df that matches names to the Google Drive ids of the spreadsheet
NAME_DRIVEID <<- as.data.frame(list(c("Alex",
                                      "Jessica",
                                      "Mark",
                                      "Matt",
                                      "Svenja",
                                      "TEST"),
                                    c("https://docs.google.com/spreadsheets/d/1ltilf36NgZiWfSoj7DG3EIYmcKhi9YR89WcDyPqTyLg/edit?usp=sharing",
                                      "JESSICA-ID",
                                      "MARK-ID",
                                      "MATT-ID",
                                      "SVENJA-ID",
                                      "https://docs.google.com/spreadsheets/d/1meaYmEfQr2P2Gk30dai2rvIjglQzjOegWh7zRo_Xlks/edit?usp=sharing"
                                    )))

#ADJUST FOR DOMESTIC SPACE
ui <- fluidPage(
  titlePanel("Domestic Space Tagging"),
  sidebarLayout(
    position = "left",
    fluid = TRUE,
    sidebarPanel(
      width = 4,
      fluidRow(
        selectInput("name",
                  "Please choose your name so you only tag your assigned passages. 
                  NOTE: Hit the 'Get a new passage' button AFTER changing your name—DO NOT hit 'Submit'!",
                  list("Alex",
                    "Jessica",
                    "Mark",
                    "Matt",
                    "Svenja",
                    "TEST")),
        h3(textOutput("nPassages")),
        radioButtons("spaceTag", 
                     "Is this passage mostly in a domestic space? (See guidelines below for clarification.)",
                     choiceNames = list("TRUE-This is mostly in a domestic space",
                                        "FALSE-This is not mostly in a domestic space",
                                        "NA-It is uncertain whether this is in a domestic space (see guidelines)",
                                        "TRASH-This is not narrative text (see guidelines)"),
                     choiceValues = list("T","F","NA","TRASH"),
                     selected = character(0)
        ),
        radioButtons("mixedTag",
                     "Is this passage entirely in a domestic or non-domestic space?",
                     choiceNames = list("TRUE-This is wholly domestic or not",
                                        "FALSE-This is partially domestic or not",
                                        "NA-I answered NA or TRASH above"),
                     choiceValues = list("T", "F", "NA"),
                     selected = character(0)
                     ),
        actionButton(inputId = "submit", label = "Submit your tags. Please only hit this once."),
        h3(textOutput("submitError")),
        #note that nothing happens if you hit this but haven't submitted
        actionButton(inputId = "newPassage", label = "Get a new passage. Only hit this when you're ready to move on."),
       )
    ),
    mainPanel(
      #print the passage
      h2(textOutput("passage")),
      br(),
      includeMarkdown("tagging_guidelines.md")
    )
  )
)




server <- function(input, output, session) {
  
  #problems with publishing: people need to authenticate (doesn't even work on my computer)
  #this is proving to be very difficult. i want people to be prompted to 
  #drive_auth()
  
  #holder variable
  #taggerDf <- as.data.frame(character())
  
  #save the name of the tagger when it changes
  #always check this first
  nameTag <- reactive(input$name)

  

  
  # #a function to save the spreadsheet of each tagger's passages CAN AVOID WITH TABLE ABOVE
  # passageDfMaker <- function(passageFilename) {
  #   #make the name of the tagger's source spreadsheet
  #   taggerFnID = NAME_DRIVEID[which(nameTag() == NAME_DRIVEID[,1]),2]
  #   return(read_sheet(taggerFn)
  # }
  
  #holder variable to avoid multiple calls to read_sheet
  #this only activates here if they change their name!
  taggerDf <- eventReactive(input$name, {
    taggerDf <- read_sheet(NAME_DRIVEID[which(nameTag() == NAME_DRIVEID[,1]),2])}, 
    #read_sheet("https://docs.google.com/spreadsheets/d/1meaYmEfQr2P2Gk30dai2rvIjglQzjOegWh7zRo_Xlks/edit?usp=sharing")},
    ignoreNULL = FALSE)
  
  
  #figure out how far down the assigned passages the tagger is
  #THIS IS NOT UPDATING???
  passageNumber <- eventReactive( input$newPassage, {
    #check if they're done first!!!
    if(!any(taggerDf()$IsTagged == "F")){
      Inf
    } else {
      #pick the first row/passage where the isTagged column is "F"
      which(taggerDf()$IsTagged == "F")[1]}
  }, ignoreNULL = FALSE)
  
  #print(taggerDf)
  
  #OUTSTANDING QUESTION: how do I determine this? I think it could be as simple as checking the taggerDf to see how far they are?
  #for now: check if it's Inf and they're done, then print otherwise
  output$nPassages <- renderText({if(passageNumber() == Inf){"You are done!"}else{passageNumber()}})
  
  
  #the below does not seem to work but doesn't hurt... 
  #has the user submitted a value?
  hasSubmitted <- eventReactive (input$submit, {
    TRUE
  })
  
  #has the user asked for a new sentence?
  hasNew <- eventReactive (input$newPassage, {
    if(hasNew()) TRUE 
  })
  
  #choose a random text
  #longer texts are more likely to be picked
  # textFn <- eventReactive( input$newSent, {
  #   filePicker("c18 Metadata")
  # }, ignoreNULL = FALSE)
  
 
  
  #save the space tag when it changes
  spaceTag <- eventReactive( input$submit, {
    input$spaceTag
  })
  
  
  #save the mixed tag when it changes
  mixedTag <- eventReactive(input$submit, {
    input$mixedTag
  })
  
  
  
  # #to show the sentence number
  # output$sentNum <- renderText({sentenceNumber()})
  
  # #to show the text name
  # #this will be changed to refer to the metadata table
  # output$textNameDisplay <- renderText({textFn()}) 
  
  #reminder to submit the tag, changes to thank the tagger after they submit
  #need to initalize it
  observeEvent( input$newPassage, {
    output$submitError <- renderText({"Please be sure to submit both tags before moving on to the next sentence."})},
    ignoreNULL = FALSE)
  #reset the prompt
  observeEvent( input$submit, {
    output$submitError <- renderText({"Thank you for submitting tags!"})})
  
  
  
  
  #output$sentNum <- renderText({as.character(sentenceNumber)})
  #output$sentence <- renderText({"TEST"})
  #specify the randomly chosen sentence
  #turn off for now to see if I can even get a sentence number
  output$passage <- renderText({
    taggerDf()$TextSegment[passageNumber()]
  })
  
  
  #add the sentence, its metadata, and the user's tag to the Tagged Description Sentences Googlesheet
  #need to test what happens if you don't do anything but hit submit
  #THIS WORKS, THEN IT CRASHES
  observeEvent(input$submit, {
    #append the new row. note this URL is hard coded for now
    sheet_append(drive_get("https://docs.google.com/spreadsheets/d/19ywlp8Z3QOByRSUA7CB1QXdg1pUdyB7lbggeJPqe0gs/edit?usp=share_link"), 
                 #make the new row by combining the existing data from the tagger sheet with the name and both tags
                 as.data.frame(list(
                   taggerDf()[passageNumber(),1:8],
                   #as.character(nameTag()),
                   as.character(spaceTag()),
                   as.character(mixedTag()))))
  }, ignoreNULL = FALSE)
  
  #add the tags to the tagger's own sheet, plus change the flag for that row to "T"
  #need to test what happens if you don't do anything but hit submit
  observeEvent(input$submit, {
    #note this is hard coded for a format like Sample Tagger's Individual Passages
    range_write(NAME_DRIVEID[which(nameTag() == NAME_DRIVEID[,1]),2],
                as.data.frame(list(list(as.character(hasSubmitted())),list(as.character(spaceTag())),list(as.character(mixedTag())))),
                range = paste("I",passageNumber()+1, sep = ""),
                col_names = FALSE)
  }, ignoreNULL = FALSE)
  
  #this hsould activate when they ask for a new passage
  #SEEMS TO WORK
  taggerDf <- eventReactive(input$newPassage, {
    taggerDf <- read_sheet(NAME_DRIVEID[which(nameTag() == NAME_DRIVEID[,1]),2])}, 
    #read_sheet("https://docs.google.com/spreadsheets/d/1meaYmEfQr2P2Gk30dai2rvIjglQzjOegWh7zRo_Xlks/edit?usp=sharing")},
    ignoreNULL = FALSE)
  
  #reset checkboxes
  observeEvent(input$newPassage, {
    updateRadioButtons(session, "spaceTag", 
                       "Is this passage mostly in a domestic space? (See guidelines below for clarification.)",
                       choiceNames = list("TRUE-This is mostly in a domestic space",
                                          "FALSE-This is not mostly in a domestic space",
                                          "NA-It is uncertain whether this is in a domestic space (see guidelines)",
                                          "TRASH-This is not narrative text (see guidelines)"),
                       choiceValues = list("T","F","NA","TRASH"),
                       selected = NULL)})
  
  observeEvent(input$newPassage, {
    updateRadioButtons(session, "mixedTag",
                       "Is this passage entirely in a domestic or non-domestic space?",
                       choiceNames = list("TRUE-This is wholly domestic or not",
                                          "FALSE-This is partially domestic or not",
                                          "NA-I answered NA or TRASH above"),
                       choiceValues = list("T", "F", "NA"),
                       selected = NULL)})

  #remove the thank you
  
  #a flag to see if they're done
  #isDone <- eventReactive (input$newPassage, {
  #  if(passageNumber() == Inf) TRUE 
  #})
  
  #reset flags 
  #NEED TO CHECK FOR NUMBER OF PASSAGES
  reactive(if(hasSubmitted() & hasNew()) hasSubmitted <- eventReactive(input$newSent & hasSubmitted & hasNew(), {
    FALSE}))
  
  reactive(if(hasSubmitted() & hasNew()) hasNew <- eventReactive(input$newSent & hasSubmitted & hasNew(), {
    FALSE}))
  
  
}
shinyApp(ui = ui, server = server) 