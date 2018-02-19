#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

suppressWarnings(library(shiny))
suppressWarnings(library(stringi))
suppressWarnings(library(quanteda))
suppressWarnings(library(data.table))
suppressWarnings(library(stringr))

CleanInputString <- function(inStrs)
{
  data.input <- gsub("[^0-9A-Za-z///' ]", "", inStrs)
  
  data.input <- stri_trans_general(data.input, "latin-ascii")
  myinputCorpus <- corpus(data.input)
  
  ttinput<-tokens(myinputCorpus,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE,remove_twitter=TRUE)
  ttinput<-tokens_tolower(ttinput)
  ttinput<-as.character(ttinput)
  ttlen<-length(ttinput)
  mycleaninput <- paste(ttinput[1:ttlen], collapse=" ");
  
  # Return the cleaned resulting sentense
  # If the resulting string is empty return empty and string.
  
    if (nchar(mycleaninput,keepNA = TRUE) > 0) {
      return(mycleaninput); 
    } else {
      return(NULL);
    }
}

load("df1.RData")
load("df2.RData")
load("df3.RData")
load("df4.RData")

df1<-data.table(df1)
df2<-data.table(df2)
df3<-data.table(df3)
df4<-data.table(df4)

predNxtTerm <- function(inStr)
{

  inStr <- CleanInputString(inStr);
  inStr <- unlist(strsplit(inStr, split=" "));
  inStrLen <- length(inStr);
  
  nxtTermFound <- FALSE;
  predNxtTerm <- as.character(NULL);
  searchStr<-as.character(NULL);
  instr1<-as.character(NULL);
  
  if (inStrLen >= 4 & !nxtTermFound)
  {
    # Assemble the terms of the input string separated by one white space each
    inStr1 <- paste(inStr[(inStrLen-2):inStrLen], collapse=" ");
    
    # Subset the four Gram data frame 
    searchStr <- inStr1;
    fDF4Temp <- df4[srcw==searchStr];
    # Check to see if any matching record returned
    if (nrow(fDF4Temp) >= 1)
    {
      predNxtTerm <- fDF4Temp[1,1];
      nxtTermFound <- TRUE;

    }
    fDF4Temp <- NULL;
    
     
       }
  
  #  test the Three Gram using the three gram data frame
  
  if (inStrLen >= 2 & !nxtTermFound)
  {
    # Assemble the terms of the input string separated by one white space each
    inStr1 <- paste(inStr[(inStrLen-1):inStrLen], collapse=" ");
    
    # Subset the three Gram data frame 
    searchStr <- inStr1;
    fDF3Temp <- df3[srcw==searchStr];

    # Check to see if any matching record returned
    if (nrow(fDF3Temp) >= 1)
    {
      predNxtTerm <- fDF3Temp[1,1];
      nxtTermFound <- TRUE;
    }
    fDF3Temp <- NULL;
  }
  
  
  if (inStrLen >= 1 & !nxtTermFound)
  {
    # Assemble the terms of the input string separated by one white space each
    inStr1 <- inStr[inStrLen];
    
    searchStr <- inStr1;
    fDF2Temp <- df2[srcw==searchStr];

    # Check to see if any matching record returned
    if (nrow(fDF2Temp) >= 1)
    {
      predNxtTerm <- fDF2Temp[1,1];
      nxtTermFound <- TRUE;
    }
    fDF2Temp <- NULL;
  }
  
  if (!nxtTermFound & inStrLen > 0)
  {
    predNxtTerm <- df1$Content[1];
  }
  nextTerm <- word(predNxtTerm, -1);
  
  if (inStrLen > 0){
    dfTemp1 <- nextTerm;
    return(dfTemp1);
  } else {
    nextTerm <- "";
    dfTemp1 <- nextTerm;
    return(dfTemp1);
  }
  
}
shinyServer(function(input, output) {
  output$prediction <- renderText({
    str2 <- CleanInputString(input$inputString);
    strDF <- predNxtTerm(str2);
   input$action;
if (!strDF=="na"){
  strDF
}
  })


}
)
