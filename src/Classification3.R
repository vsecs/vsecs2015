source("kb.R")

library(RISmed)
library(tm)
library(SnowballC)
library(igraph)
library(lsa)
library(e1071)
library(RCurl)
library(jsonlite)
library(httr)
library(reshape)

require(cluster)
require(stringr)


gSub<-function(abstract) {
  abstract = tolower(abstract)
  abstract = gsub("or =","oddsratio =",abstract)
  abstract = gsub("OR =","oddsratio =",abstract)
  abstract = gsub("hr =","hazardratio =",abstract)
  abstract = gsub("HR =","hazardratio =",abstract)
  abstract = gsub("odds ratio","oddsratio",abstract)
  abstract = gsub("Odds Ratio","OddsRatio",abstract)
  abstract = gsub("hazard ratio","hazardratio",abstract)
  abstract = gsub("Hazard Ratio","HazardRatio",abstract)
  abstract = gsub("nib", " drug_name", abstract)
  abstract = gsub("mab", " drug_name", abstract)
  abstract = gsub(" percent","%",abstract)
  abstract = gsub("[,.;():']","",abstract)
  abstract = gsub("[-]","",abstract)
  abstract = gsub("\u2009","",abstract)
  abstract = gsub("\u2013"," ",abstract)
  abstract = gsub("\u2265","",abstract)
  abstract = gsub("\u2014","-",abstract)
  abstract = gsub("\u201c","",abstract)
  abstract = gsub("\u201d","",abstract)
  abstract = gsub("&quot","",abstract)
  #   abstract = gsub("&lt","",abstract)
  #   abstract = gsub("&gt","",abstract)
  #    abstract = gsub("<b1>","",abstract)
  abstract = gsub("[/]"," ", abstract)
  #   abstract = gsub('[[:punct:]]', '', abstract)
  #   abstract = gsub('[[:cntrl:]]', '', abstract)
  #   abstract = gsub('\\d+', '', abstract)
  
  return (abstract)
}

word.count<-function(sentences, rp.words, dr.words, mutation.symbol.words, mutation.details.words, .progress='none'){
  require(plyr)
  
  scores = laply(sentences, function(sentence, rp.words, dr.words, mutation.symbol.words, mutation.details.words) {
    sentence<-gSub(sentence)
    
    all.corpus<-Corpus(VectorSource(sentence))

    all.corpus<-tm_map(all.corpus, stripWhitespace)
    all.corpus<-tm_map(all.corpus, content_transformer(tolower))
    all.cstop<-tm_map(all.corpus, removeWords, stopwords("english"))

    sentence <- tm_map(all.cstop, stemDocument)
    
    word.list = str_split(sentence$content, '\\s+')
    words = unlist(word.list)
    words = wordStem(words,language = "english")
    
    rp.matches = match(words, rp.words)
    dr.matches = match(words, dr.words)
    mutation.matches = match(words, mutation.symbol.words)
    amino.acid.match = match(words, mutation.details.words)
    drug.name.matches = match(words, "drug_name")
    
    rp.matches = !is.na(rp.matches)
    dr.matches = !is.na(dr.matches)
    mutation.matches = !is.na(mutation.matches)
    amino.acid.match = !is.na(amino.acid.match)
    drug.matches = !is.na(drug.name.matches)
    
    count_rp = sum(rp.matches) + sum(mutation.matches) + sum(amino.acid.match)
    count_dr = sum(dr.matches) + sum(drug.matches)
    score = count_rp - count_dr
    count_mutation = sum(mutation.matches)
    count_amino_acid = sum(amino.acid.match)
    count_drug_name = sum(drug.matches)
    word_count = length(words)
    
    return(c(count_mutation,count_amino_acid,count_drug_name,word_count,score,count_rp,count_dr))
  }, rp.words, dr.words, mutation.symbol.words, mutation.details.words, .progress=.progress)
  scores.df = data.frame(score=scores)
  names(scores.df)[1]<-paste("mutation")
  names(scores.df)[2]<-paste("amino_acid")
  names(scores.df)[3]<-paste("drug_name")
  names(scores.df)[4]<-paste("word_count")
  names(scores.df)[5]<-paste("diff")
  names(scores.df)[6]<-paste("risk_prediction")
  names(scores.df)[7]<-paste("drug_response")
  
  return(scores.df)
}

mutation.symbols<-function(disease) {
  disease<-gsub("q=","",disease)
  disease<-gsub(" ","%20",disease)
  
  url<-paste("http://www.geneup.com/rest/v1/DiseaseMutations/",disease,"?auth_name=nus_test&auth_key=7ab014decd2549a36e7ae8aead289733",sep="")
  diseaseOnto<-jsonlite::fromJSON(url,flatten=T)
  
  mutationSymbol<-diseaseOnto$mutation.biomarker.symbol
  mutationSymbolSynonym<-diseaseOnto$mutation.biomarker.synonym
  
  mutationSymbolSynonym<-str_split(mutationSymbolSynonym,'; ')
  mutationSymbolSynonym<-unlist(mutationSymbolSynonym)
  mutationSymbolSynonym <-unique(mutationSymbolSynonym)
  mutationSymbol <-unique(mutationSymbol)
  mutations<-c(mutationSymbolSynonym, mutationSymbol)
  mutations<-tolower(mutations)
  mutations<-gSub(mutations)
  
  
  return(mutations)
}

mutation.details<-function(disease){
  disease<-gsub("q=","",disease)
  disease<-gsub(" ","%20",disease)
  
  url<-paste("http://www.geneup.com/rest/v1/DiseaseMutations/",disease,"?auth_name=nus_test&auth_key=7ab014decd2549a36e7ae8aead289733",sep="")
  diseaseOnto<-jsonlite::fromJSON(url,flatten=T)
  
  mutationDetails<-diseaseOnto$mutation.mutationDetails
  amino.acid.substitution<-unlist(mutationDetails,use.names=F)
  amino.acid.substitution<-tolower(amino.acid.substitution)
  amino.acid.substitution<-gSub(amino.acid.substitution)
  
  return(amino.acid.substitution)
}

transform2df = function(sentences,col.names,mining.set,lexi,read.cat,.progress='none'){
  require(plyr)
  require(stringr)
  
  transformDF = laply(sentences, function(sentence,lexi){
    sentence<-gSub(sentence)
    
    all.corpus<-Corpus(VectorSource(sentence))
    
    all.corpus<-tm_map(all.corpus, stripWhitespace)
    all.corpus<-tm_map(all.corpus, content_transformer(tolower))
    all.cstop<-tm_map(all.corpus, removeWords, stopwords("english"))
    
    sentence <- tm_map(all.cstop, stemDocument)
    
    word.list = str_split(sentence$content, '\\s+')
    words = unlist(word.list)
#   words = wordStem(words,language = "english")
    
    cat.matches<-match(words, lexi)
    cat.matches.id<-unique(sort(cat.matches))
    match.words<-col.names[cat.matches.id]
    classifier<-as.data.frame(match.words)
    
    df<-rep(NA,len=length(mining.set)-1)
    for (i in 1:length(cat.matches.id)) {
        df[cat.matches.id[i]]<-as.factor("Y")
    }
    
    return(df)
  },lexi,.progress=.progress)
  cat<-c("risk_prediction","drug_response")
  df.xx<-data.frame(category=cat[read.cat],transformDF)
  return(df.xx)
}

transposeDF<-function(tfidf,read.cat) {
  #tfidf$term<-sapply(tfidf$term,as.character)
  term<-tfidf$term
  tfidf[!is.na(tfidf)]<-"Y"
  tfidf[is.na(tfidf)]<-"N"
  tfidf$term<-term
  tfidf<-as.data.frame(t(tfidf[,-1])) #transpose data frame
  colnames(tfidf)<-term
  tfidf<-data.frame(category=cat.option(read.cat),tfidf)
  
  return(tfidf)
}

prep.set=function(train.set,col_names) {
  colnames(train.set)[-1]<-col_names[-1]
  train.set[-1][!is.na(train.set[-1])]<-"Y"
  train.set[-1][is.na(train.set[-1])]<-"N"
  train.set[,col_names]<-lapply(train.set[,col_names],factor)
  
  return(train.set)
}

cat.option<-function(read.cat){
  cat<-c("risk_prediction","drug_response")
  
  return(cat[read.cat])
}

is.one<-function(result){
  ret<-0
  for (x in 1:nrow(result)) {
  if (result[x,1]==1) ret[x]<-1
    else if (result[x,2]==1) ret[x]<-2  
      else ret[x]<-3
  }
  return(ret)
}

tf.count<-function(abstract,index){
  
  corpus<-gSub(abstract)
  
  ## Load coupus ###
  all.corpus<-Corpus(VectorSource(corpus))
  
  ## Corpus cleansing ##
  all.corpus<-tm_map(all.corpus, stripWhitespace)
  all.corpus<-tm_map(all.corpus, content_transformer(tolower))
  all.cstop<-tm_map(all.corpus, removeWords, stopwords("english"))
  
  ## word stemming step ##
  all.cstem <- tm_map(all.cstop, stemDocument)
  
  ## tf-idf weighting index/method, after word stemming ##
  allstemns.tf <- DocumentTermMatrix(all.cstem, control = list(removePunctuation = TRUE,
                                                                stopwords = TRUE))
  
  # construct data frame 
  col_name<-paste("doc", index, sep="")
  terms<-data.frame(sort(colSums(as.matrix(allstemns.tf)), decreasing = T))
  colnames(terms)<-col_name
  terms<-cbind(term=rownames(terms),terms)
  rownames(terms)<-NULL
  
  return(terms)
}

calc_cosine = function(train, test, doc_no)
{
  train = as.data.frame(cbind(train$term, rowMeans(train[,-1])), stringsAsFactors = FALSE)
  colnames(train) = c("term", "mean")
  train$mean = as.numeric(train$mean)
  
  colnames(test)[1] = "term"
  colnames(test)[2 : (doc_no + 1)] = c(1 : doc_no)
  
  merge = merge(train, test, by = "term", all.x = TRUE, incomparables = 0)
  merge[is.na(merge)] = 0
  
  cos = cosine(merge$mean, merge[,3])
  for (i in 2:doc_no)
  {
    cos = c(cos, cosine(merge$mean, merge[,2+i]))
  }
  
  return(cos)
}

#omega<-10

# Word Count
class_wordcount = function(query, abstracts, rp.tfidf, dr.tfidf, list)
{
  mutation.symbol.words = character(0)
  mutation.details.words = character(0)
  
  #if (!is.null(query)) #fix me: add me back
  #{
  #  mutation.symbol.words<-mutation.symbols(query)
  #  mutation.details.words<-mutation.details(query)
  #}
  
  rp.words<-rp.tfidf$term
  dr.words<-dr.tfidf$term
  
  classification<-word.count(abstracts, rp.words, dr.words, mutation.symbol.words, mutation.details.words, .progress='text')
  x<-classification[,c("risk_prediction","drug_response")]
  wordCount<-colnames(x)[apply(x,1,which.max)]
  data.frame(list,wordCount)
}

# Naive Bayesian
class_bayesian = function(abstracts, rp.tfidf, dr.tfidf, list)
{
  train.df<-read.csv(NAIVE_BAYESIAN_DATA_FILE, header=T, sep="|")
  dr.tfidf1<-transposeDF(dr.tfidf,2)
  rp.tfidf1<-transposeDF(rp.tfidf,1)
  mining.set<-Reduce(function(x, y) merge(x, y, all=T), list(dr.tfidf1,rp.tfidf1))
  col.names<-names(mining.set)

  if ((file.exists(NAIVE_BAYESIAN_MODEL_FILE)) &&
      (file.info(NAIVE_BAYESIAN_MODEL_FILE)$mtime > 
       file.info(NAIVE_BAYESIAN_DATA_FILE)$mtime))
  {
    load(NAIVE_BAYESIAN_MODEL_FILE)
  }
  else
  {
    pred.model<-naiveBayes(category~., data=train.df)
    save(pred.model, file = NAIVE_BAYESIAN_MODEL_FILE)
  }
  
  lexi<-colnames(train.df[-1])
  test.df<-transform2df(abstracts,col.names,mining.set,lexi,1,.progress='text')
  test.df<-prep.set(test.df,col.names)
  NBayesianPred<-predict(pred.model,test.df[-1])
  data.frame(list,NBayesianPred)
}

# Cosine Similarity
class_cosine = function(abstracts, rp.tfidf, dr.tfidf, list)
{
  doc_no = length(abstracts)
  
  abstracts.tf = tf.count(abstracts[1],1)
  for (i in 2:doc_no)
  {
    abstract.tf = tf.count(abstracts[i],i)
    abstracts.tf = merge(abstracts.tf, abstract.tf, by = "term", all.x = TRUE, incomparables = 0)
    abstracts.tf[is.na(abstracts.tf)] = 0
  }
  
  drug_response = calc_cosine(dr.tfidf, abstracts.tf, doc_no)
  risk_prediction = calc_cosine(rp.tfidf, abstracts.tf, doc_no)
  
  drug_response[is.nan(drug_response)] = 0
  risk_prediction[is.nan(risk_prediction)] = 0
  
  class = as.data.frame(cbind(drug_response, risk_prediction, "class"), stringsAsFactors = FALSE)
  
  colnames(class)[3] = "cosineSim"
  cosineSim = colnames(class)[apply(class[1:2],1,which.max)]
  
  data.frame(list,cosineSim)
}

# Voting
class_vote = function(list)
{
  model<-3
  
  x<-list[,c("wordCount","NBayesianPred","cosineSim")]
  class.result<-table(row(x),as.matrix(x))
  class.result<-as.data.frame.matrix(class.result)
    
  class.result$drug_response<-class.result$drug_response/model
  class.result$risk_prediction<-class.result$risk_prediction/model
    
  Class<-colnames(class.result)[outcome<-is.one(class.result)]
  
  data.frame(list, Class)
}
