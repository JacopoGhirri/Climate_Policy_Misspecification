get.cb <- function(emissions){#carbon budget
  return(colSums(t(emissions)))
}

get.pos.cb <- function(emissions){#peak emissions
  return(colSums(pmax(t(emissions), 0)))
}
