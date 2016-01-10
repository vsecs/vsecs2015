#
# Reference:
# https://developers.google.com/custom-search/json-api/v1/reference/cse/list
#

library(RCurl)
library(jsonlite)

ENGINE_URL = "https://www.googleapis.com/customsearch/v1"
KEY = "key=AIzaSyARybWI1GkqwW7cJY1YZAhdes4xy7T3pqY"
ENGINE_ID = "cx=017094818225409580931:2j2ja-aibvo"


get_list = function(keyword, index)
{
  if (is.null(keyword) || identical(keyword, character(0)) || is.null(index) || is.na (index))
  {
    cat("get_list(): invalid query\n")
    return
  }

  keyword = gsub(" ", "+", keyword)
  query = paste("q", keyword, sep = "=")


  start_index = paste("start", index, sep = "=")

  para = paste(KEY, ENGINE_ID, query, start_index, sep = "&")
  req = paste(ENGINE_URL, para, sep = "?")

  ret = getURL(req, ssl.verifypeer = FALSE)
  ret = gsub("\n", "", ret)
  ret = data.frame(fromJSON(ret), row.names = NULL)

  result = data.frame(ret$items.title, ret$items.link, row.names = NULL)
}
