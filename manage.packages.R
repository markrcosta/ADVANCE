## manage.packages
manage.packages = function(packagelist){
  
  for (l in 1:length(packagelist)){
    if (packagelist[[l]] %in% installed.packages() & !(packagelist[[l]] %in% (.packages()))){
      library(packagelist[[l]],character.only = TRUE)
    } else if (!(packagelist[[l]]) %in% installed.packages()) {
      install.packages(packagelist[[l]])
      library(packagelist[[l]],character.only = TRUE)
    }
  }
  
}
