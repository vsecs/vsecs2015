library(rvest)
library(RISmed)


webScraping<-function(result){
  
  if (is.na(result))
  {
    cat("webScraping(): invalid target\n")
  }
  
  content<-character(0)

  for (i in 1:nrow(result)) {
    url<-as.character(result$ret.items.link[i])
    webContents<-html(url)

    x<-try(scrapeContents<-html_node(webContents,"#content"))
    if (class(x) == "try-error") 
      y<-try(scrapeContents<-html_node(webContents,"#maincontent"))
        else
        z<-try(scrapeContents<-html_node(webContents,"article"))
  
    content[i]<-as.character(html_text(scrapeContents))
    webContents<-NULL
  }
  
  return(content)
}

#contents<-webScraping(result)
