# Download the capstone data
if(!file.exists("data/Coursera-SwiftKey.zip")) {
    url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    download.file(url, destfile = "data/Coursera-SwiftKey.zip", method = "curl")
}
