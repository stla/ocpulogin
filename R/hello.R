#' login
#' @export
login <- function(username, password){
  users <- read.csv(system.file(package = "ocpulogin", "databases", "Users.csv"), stringsAsFactors = FALSE)
  if(!(username %in% users$username)) return(list(success="false", error="unknown"))
  userpassword <- users$password[which(users$username==username)]
  if(userpassword==password){
    return(list(success="true", hash=users$hash[which(users$username==username)]))
  }else{
    return(list(success="false"))
  }
}

#' check login
#' @export
checkLogin <- function(username, hash){
  if(username=="" || hash=="") return("false")
  users <- read.csv(system.file(package = "ocpulogin", "databases", "Users.csv"), stringsAsFactors = FALSE)
  if(!(username %in% users$username)) return("false")
  userhash <- users$hash[which(users$username==username)]
  if(userhash==hash){
    return("true")
  }else{
    return("false")
  }
}
