install.packages('rsconnect')

rsconnect::setAccountInfo(name='pnlabeyetrackingstudy',
                          token='9D866A163E625F4D3373347053672FD5',
                          secret='itF0MXpLiI4Y6nUqbVb33vfWHwpjPE3p0v3KmchC')
library(rsconnect)
rsconnect::deployApp('/Users/Rgao/Desktop/Eyetracking')

