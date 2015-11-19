## Join conditions with main queries (accidents, cbs, victims, parties)

JoinCondition <- function(query, condition) {

  FinalQuery <- paste(query, condition, sep = " ", collapse = "\n")
  FinalQuery <- gsub("\n", "", FinalQuery)
}