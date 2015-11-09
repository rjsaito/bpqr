#http://stackoverflow.com/questions/26182348/use-all-variables-in-a-model-with-plm-in-r

expand_formula <- function(form="A ~.",varNames=c("A","B","C")){
  has_dot <- any(grepl('.',form,fixed=TRUE))
  if(has_dot){
    ii <- intersect(as.character(as.formula(form)),varNames)
    varNames <- varNames[!grepl(paste0(ii,collapse='|'),varNames)]
    exp <- paste0(varNames,collapse='+')
    as.formula(gsub('.',exp,form,fixed=TRUE))
  }
  else as.formula(form)
}
