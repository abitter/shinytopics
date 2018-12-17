# create a PubPsych search string with term boosting
####################################################

# https://stackoverflow.com/questions/28117556/clickable-links-in-shiny-datatable
# https://lucene.apache.org/core/2_9_4/queryparsersyntax.html#Boosting%20a%20Term


# fixed boosting
################

createLink <- function(val) {
  list <- list()
  for (i in 1:length(val)){
    list[[i]] <- unlist(strsplit(val[i], ", ", fixed = TRUE))
    list[[i]][1] <- paste0('"', list[[i]][1], '"^10') # boost first term with factor 10
    list[[i]][2] <- paste0('"', list[[i]][2], '"^2') # boost second term with factor 10
    list[[i]][3] <- paste0('"', list[[i]][3], '"')
    list[[i]][4] <- paste0('"', list[[i]][4], '"')
    list[[i]][5] <- paste0('"', list[[i]][5], '"')
    list[[i]] <- paste0(list[[i]], collapse=" OR ")
  }
  val <- unlist(list)
  paste0("<a href='https://pubpsych.zpid.de/pubpsych/Search.action?search=&q=%28CT%3D%28", 
         val,"%29%29+DB%3DPSYNDEX&stats=TOP' target='_blank' class='btn btn-primary'>Suche in PSYNDEX</a>")
}



# no boosting
#############

#createLink <- function(val) {
#  val <- gsub(', ','" OR "' , val)
#  val <- paste0('"', val, '"')
#  paste0("<a href='https://pubpsych.zpid.de/pubpsych/Search.action?search=&q=%28CT%3D%28", 
#         val,"%29%29+DB%3DPSYNDEX&stats=TOP' target='_blank' class='btn btn-primary'>Suche in PSYNDEX</a>")
#}