# # Reading in out data
# library(readr)
# twitterTextData <- read_lines(unz("data/textData.zip", "final/en_US/en_US.twitter.txt"),
#                 n_max = -1)
# blogTextData <- read_lines(unz("data/textData.zip", "final/en_US/en_US.blogs.txt"),
#                               n_max = -1)
# newsTextData <- read_lines(unz("data/textData.zip", "final/en_US/en_US.news.txt"),
#                               n_max = -1)
#
# nchar(twitterTextData)[order(nchar(twitterTextData), decreasing = TRUE)][1:5]
# nchar(blogTextData)[order(nchar(blogTextData), decreasing = TRUE)][1:5]
# nchar(newsTextData)[order(nchar(newsTextData), decreasing = TRUE)][1:5]
#
# # Ratio of love to hate in twitter
# length(grep("love", twitterTextData))/length(grep("hate", twitterTextData))
#
# # What does the tweet with biostats say ?
# grep("biostats", twitterTextData, value = TRUE)[1:5]
#
# # Match a string, and find the number of tweets with this string
# length(grep("A computer once beat me at chess, but it was no match for me at kickboxing",
#      twitterTextData))
# grep("A computer once beat me at chess, but it was no match for me at kickboxing",
#      twitterTextData, value = TRUE)
