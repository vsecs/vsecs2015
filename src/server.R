source("CSE.R")
source("WebScraping.R")
source("Classification3.R")



shinyServer
(
  function(input, output, clientData, session)
  {
    query = reactiveValues(data = NULL)
    cache = reactiveValues(data = NULL)
    
    updateTable = reactive({
    
      if (is.null(query$kw) || is.null(query$pg))
      {
        cat("updateTable(): invalid input\n")
        return
      }
      
      cat("updateTable(): ")
      cat("keyword: ")
      cat(query$kw)
      cat(", ")
      cat("page: ")
      cat(query$pg)
      cat("\n")
      
      keyword = query$kw
      page = query$pg
  
      if ((!is.null(page))&&(!is.null(cache[[paste(page)]])))
      {
        cat("updateTable(): hit cache\n")
        cache[[paste(page)]]
      }
      else
      {
        cat("updateTable(): miss cache\n")
        result = get_list(keyword, (page - 1) * 10 + 1)
        #cbind(c(keyword, 2:10), c(21:30)) #debug
        
        if (length(result) != 0)
        {
          #load("abstracts.RData") # debug
          contents<-webScraping(result) # abstracts #debug
          
          rpKeywordsFile = "..\\kb\\rp_tfidf_95.csv"
          drKeywordsFile = "..\\kb\\dr_tfidf_95.csv"
          
          rp.tfidf<-read.csv(rpKeywordsFile,header=T,sep="|", stringsAsFactors = FALSE)
          dr.tfidf<-read.csv(drKeywordsFile,header=T,sep="|", stringsAsFactors = FALSE)
          
          result = class_cosine(contents, rp.tfidf, dr.tfidf, result)
          result = class_bayesian(contents, rp.tfidf, dr.tfidf, result)
          result = class_wordcount(query$kw, contents, rp.tfidf, dr.tfidf, result)
          result = class_vote(result)
          
          titles = result[,1]
          urls = result[,2]
          Results = paste0("<a href='",  urls, "' target='_blank'>", titles, "</a>")
          
          cache$pg=c(cache$pg, paste(page))
          cache[[paste(page)]] = data.frame(Results, result$Class, result$wordCount, result$NBayesianPred, result$cosineSim)
          #table =  query$cache[page]
        }
      }
    })
    
    observeEvent(input$srh, {
      
      if (is.null(input$kw))
      {
        cat("input$srh: no keyword\n")
        return
      }
      
      if ((!is.null(query$kw)) && (query$kw != input$kw))
      {
        cat("input$srh: clear cache\n")
 
        all_pages = cache$pg

        cache$pg = NULL
        for (i in 1: length(all_pages))
        {
          cache[[all_pages[i]]] = NULL
        }
      }
      
      query$kw = input$kw
      
      if (is.na(input$pg))
      {
        query$pg = 1
      }
      else
      {
        query$pg = input$pg
      }

      updateTextInput(session, "pg", value = query$pg)

    })

    observeEvent(input$nxt, {
      
      if (!is.null(query$kw))
      {
        updateTextInput(session, "kw", value = query$kw)
      }
      
      page = query$pg
      
      if (is.null(page) || (page <= 0))
      {
        cat("input$nxt: invaid page\n")
        return
      }
      else
      {
        page = page + 1
        query$pg = page
        updateTextInput(session, "pg", value = page)
      }
    })
    
    observeEvent(input$prv, {

      if (!is.null(query$kw))
      {
        updateTextInput(session, "kw", value = query$kw)
      }
      
      page = query$pg
      
      if (is.null(page) || (page <= 1))
      {
        cat("input$prv: invaid page\n")
        return
      }
      else
      {
        page = page - 1
        query$pg = page
        updateTextInput(session, "pg", value = page)
      }
    })
    
    output$table = renderTable({
      updateTable()
    }, sanitize.text.function = function(x) x)
    
    on.exit({
      cat("clean up\n")
      file.remove("..\\kb\\naive_bayesian_model")
    })
  }
)