getPosition <- function(ordVec, ins) {
  # Get the position in the ordered vector, after which the value should be 
  # inserted
  # ordVec = ordered vector (ascending)
  # ins = value to be inserted
  point <- findInterval(ins, ordVec)
}

insPosition <- function(ordVec, ins, point) {
  # Inserts ins to the point place in ordVec
  # ordVec = vector
  # ins = value to be inserted
  # point = position to which we should insert
  append(ordVec, ins, after = point)
}

insertOrdered <- function(ordVec, ins) {
  # Insert in to an ordered vector
  # ordVec = ordered vector (ascending)
  # ins = value to be inserted
  point <- getPosition(ordVec, ins)
  insPosition(ordVec, ins, point)
}
