

directory_setup <- function(){
  if (!dir.exists("./outputs/")) {
    dir.create("./outputs")
  }
  
  if (!dir.exists("./outputs/s1/")) {
    dir.create("./outputs/s1")
  }
  
  if (!dir.exists("./outputs/s2/")) {
    dir.create("./outputs/s2")
  }
}
