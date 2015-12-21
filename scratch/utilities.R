find_numeric_vars <- function(data) {
  isNum <- function(name, data) {
    is.numeric(get(name, envir = as.environment(data)))
  }
  numNames <- sapply(names(data), isNum, data = data)
  names(data)[numNames]
}

eval(parse(text="cloud(Petal.Length~ Sepal.Length * Sepal.Width, data = iris)"))
