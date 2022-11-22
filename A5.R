library(stringr)
string<-"stephen-pangas"
stringVector<- as.vector(str_split_fixed(string, pattern = "", n = nchar(string)))# convert string to vecotr of characters
convertToNum<-function(newChar)  #convert one character to a num
{
  output <- switch(
    newChar,
    "-" = "00",
    "a" = "01",
    "b" = "02",
    "c" = "03",
    "d" = "04",
    "e" = "05",
    "f" = "06",
    "g" = "07",
    "h" = "08",
    "i" = "09",
    "j" = "10",
    "k" = "11",
    "l" = "12",
    "m" = "13",
    "n" = "14",
    "o" = "15",
    "p" = "16",
    "q" = "17",
    "r" = "18",
    "s" = "19",
    "t" = "20",
    "u" = "21",
    "v" = "22",
    "w" = "23",
    "x" = "24",
    "y" = "25",
    "z" = "26",
  )
  return(output)
}

#turn string into given numbers
numVect<-as.vector(unlist(sapply(stringVector,convertToNum))) #apply to whole string of characters

#length of numVect
len<-length(numVect)

#generate random string of length >= length of string
encryptionKey<-sample(1:26,len)

#encryptedString<-letters[encryptionKey[numVect]]

fibonacciAddtition<-function(x,y) 
{
  digit1Sum<- (x+y)%%10
  x<-x%/%10
  y<-y%/%10
  digit2Sum<- (x+y)%%10
  sum<-digit1Sum+10*digit2Sum
  return(sum)
}

fibonacciSubstraction<-function(x,y) # y assumed negative, x assumed positive
{
  digit1Sum<- (x-y)%%10
  x<-x%/%10
  y<-abs(y)%/%10
  digit2Sum<-(x-y)%%10
  sum<-digit1Sum+10*digit2Sum
  return(sum)
}

#RSA method
p<- 1153
q<- 997
n<- p*q
phi<- (p-1)*(q-1)
e<- 7 # used calculator on http://www.alcula.com/calculators/math/gcd/#gsc.tab=0 to verify gcd
d<- 983479 #using wolfram alpha, https://www.wolframalpha.com/input?key=&i2d=true&i=983479+%2B+1147392+n%5C%2844%29+n%3D0
priv<- c(d,n)
publ<- c(e,n)

encrypt<-function(m, e, n){return((m^e) %% n)} # encrypt using RSA encrypt formula

#encrypt original string:
L1<-fibonacciAddtition(as.numeric(numVect),encryptionKey)
# RSA encryption with chosen e, n values
L2<-encrypt(L1,e,n)
# RSA decryption with chosen d, n values using wolfram alpha
L3<-c(34, 43, 29, 36, 16, 26, 10, 3, 11, 2, 11, 14, 17, 23) #from https://www.wolframalpha.com/input?key=&i=mod%28%28821854%2C++445329%2C+1013604%2C+1103667%2C++592403%2C+1116750%2C++803672%2C++++2187%2C+1094515+%2C++++128%2C+1094515%2C++805273%2C+1102077%2C+1034546%29%5E983479%2C1149541%29
#decrypt original string:
L4<-fibonacciSubstraction(L3,encryptionKey)