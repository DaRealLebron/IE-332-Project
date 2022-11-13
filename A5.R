library(stringr)
string<-"stephen pangas"
stringVector<- as.vector(str_split_fixed(string, pattern = "", n = nchar(string)))# convert string to vecotr of characters
convertToNum<-function(newChar)  #convert one character to a num
{
  output <- switch(
    newChar,
    "-" = 0,
    "a" = 1,
    "b" = 2,
    "c" = 3,
    "d" = 4,
    "e" = 5,
    "f" = 6,
    "g" = 7,
    "h" = 8,
    "i" = 9,
    "j" = 10,
    "k" = 11,
    "l" = 12,
    "m" = 13,
    "n" = 14,
    "o" = 15,
    "p" = 16,
    "q" = 17,
    "r" = 18,
    "s" = 19,
    "t" = 20,
    "u" = 21,
    "v" = 22,
    "w" = 23,
    "x" = 24,
    "y" = 25,
    "z" = 26,
  )
  return(output)
}

numVect<-as.vector(unlist(sapply(stringVector,convertToNum))) #apply to whole string of characters


