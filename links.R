# create a PubPsych search string with term boosting
####################################################

# https://stackoverflow.com/questions/28117556/clickable-links-in-shiny-datatable
# https://lucene.apache.org/core/2_9_4/queryparsersyntax.html#Boosting%20a%20Term


# boosting by beta probabilities
################################

# PubPsych search terms are boosted according to the relations of beta probabilites
# Factors were computed by dividing the beta probabilites of Terms 1-4 by beta of Term 5


createLink <- function(val, boost) {
  list <- list()
  for (i in 1:length(val)){
    list[[i]] <- unlist(strsplit(val[i], ", ", fixed = TRUE))
    for (j in 1:4){
      list[[i]][j] <- paste0('"', list[[i]][j], '"^', boost[[i]][j]) # add boost factors for first 4 terms
    }
    list[[i]][5] <- paste0('"', list[[i]][5], '"') # Term 5 is reference, so no boosting
    list[[i]] <- paste0(list[[i]], collapse=" OR ")
  }
  val <- unlist(list)
  paste0("<a href='https://pubpsych.zpid.de/pubpsych/Search.action?search=&q=%28CT%3D%28", 
         val,"%29%29+DB%3DPSYNDEX&stats=TOP' target='_blank' class='btn btn-primary'>Suche in PSYNDEX</a>")
}



# fixed boosting
################

#createLink <- function(val) {
#  list <- list()
#  for (i in 1:length(val)){
#    list[[i]] <- unlist(strsplit(val[i], ", ", fixed = TRUE))
#    list[[i]][1] <- paste0('"', list[[i]][1], '"^10') # boost first term with factor 10
#    list[[i]][2] <- paste0('"', list[[i]][2], '"^2') # boost second term with factor 10
#    list[[i]][3] <- paste0('"', list[[i]][3], '"')
#    list[[i]][4] <- paste0('"', list[[i]][4], '"')
#    list[[i]][5] <- paste0('"', list[[i]][5], '"')
#    list[[i]] <- paste0(list[[i]], collapse=" OR ")
#  }
#  val <- unlist(list)
#  paste0("<a href='https://pubpsych.zpid.de/pubpsych/Search.action?search=&q=%28CT%3D%28", 
#         val,"%29%29+DB%3DPSYNDEX&stats=TOP' target='_blank' class='btn btn-primary'>Suche in PSYNDEX</a>")
#}



# no boosting
#############

#createLink <- function(val) {
#  val <- gsub(', ','" OR "' , val)
#  val <- paste0('"', val, '"')
#  paste0("<a href='https://pubpsych.zpid.de/pubpsych/Search.action?search=&q=%28CT%3D%28", 
#         val,"%29%29+DB%3DPSYNDEX&stats=TOP' target='_blank' class='btn btn-primary'>Suche in PSYNDEX</a>")
#}