
# how many matches within each element of x vector?

grepn = function(pattern, x) {

  info = gregexec(pattern = pattern, text = x)
  sapply(info, function(z) ifelse(z[1] == -1, 0, length(z)))
}
