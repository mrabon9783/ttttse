#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MITIE)
library(RSentiment)
library(stringr)

ner_model_path <- "library/MITIE-models/english/ner_model.dat"
ner <- NamedEntityExtractor$new(ner_model_path)

tttse<-function(fpath,rtxt) {
  #print(as.character(fpath))
  spfilepath<-fpath
  # Print out what kind of tags this tagger can predict
  
  tag_names <- ner$get_possible_ner_tags()
  #tag_names
  
  #  [1] "PERSON"       "LOCATION"     "ORGANIZATION" "MISC"
  
  # Prepare some data
  if(nchar(rtxt)<5){
    sp<- mitie_load_entire_file(spfilepath)
  } else {
    sp<-rtxt
  }
  tokens <- mitie_tokenize(sp)#"Worf threw the captain off the deck of the Enterprise!")
  
  
  entities <- ner$extract_entities(tokens)
  for (i in 1:length(entities)) {
    #print (i)
    entity <- entities[[i]]
    entityminnear<-max(1,c(i-2))
    #print(entityminnear)
    enitiynearfar<-min(i,i+2)
    #print(enitiynearfar)
    context <- paste(tokens[entities[[entityminnear]]$start:entities[[enitiynearfar]]$end], collapse=" ")
    context<- str_replace_all(context, "[[:punct:]]", " ")
    position <- paste("(", entity$start, ",", entity$end, ")", sep="")
    text <- paste(tokens[entity$start:entity$end], collapse=" ")
    tag <- tag_names[entity$tag]
    nearsentiment = as.character(calculate_sentiment(context)[[2]][1])
    if(i ==1){
      rdf<-cbind(text,tag,nearsentiment)
    } else {
      tdf<-cbind(text,tag,nearsentiment)
      rdf<-rbind(rdf,tdf)
    }
  }
  
  
  return (rdf)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Text Token-Tag-Sentiment Extraction"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         fileInput("ifile","Text File Path", multiple = FALSE, accept = NULL),
         textAreaInput("itext","Manual Text Submission", value = "")
      ),
      
      # Show a Table
      mainPanel(
         tableOutput("rtable")
      )
   )
)

# Define server logic required
server <- function(input, output) {
   
   output$rtable <- renderTable({
     if(input$itext==""){
       inFile <- input$ifile
       
       if (is.null(inFile))
         return(NULL)
       
       path<-inFile$datapath
       outdf<-tttse(as.character(path),"")
     } else {
       rtxt<-input$itext
       outdf<-tttse("",rtxt)
       
     }

      colnames(outdf)<-c("Token","Tag","Sentiment")
      outdf
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

