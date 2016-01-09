# vsecs2015


**File Structure**

*Knowledge Base*
 - kb\dr_tfidf_95.csv
 - kb\rp_tfidf_95.csv
 - kb\naive_bayesian.csv

*Source Code*
 - src\ui.R
 - src\server.R
 - src\cse.R
 - src\WebScraping.R
 - src\Classification3.R

**Execution**

 1. Launch RStudio
 
 2. Ensure the below listed R libraries are installed
  - shiny
  - RISmed
  - tm
  - SnowballC
  - igraph
  - lsa
  - e1071
  - RCurl
  - jsonlite
  - httr
  - reshape
  - cluster
  - stringr
  - RCurl
  - jsonlite
  - rvest
  - RISmed
 
 3. Set directory "vsecs2015" as the working directory
 
 4. Issue Command
  - library(shiny)
  - runApp("src")

**Remarks**

 - ICMDB query in WordCount part is temporarily disabled due to low performance. (Refer to Line 269 - 273 in vsecs/Classification3.R)
 - All the viewed pages get cached when the user browsing different
 - Once the user searches another keyword,    the cache gets flushed and starts to cache for the new keyword.
