# Old, very slow model built using kgrams package

# library(kgrams)
# # Defining a preprocess function for our data
# .preprocess <- function(x) {
#     # Remove \r
#     x <- gsub("\r", "", x)
#     # Remove RT
#     x <- gsub("RT", "", x)
#     # Lower-case everything and remove characters which are not alphanumeric
#     # or punctuation
#     x <- kgrams::preprocess(x)
#     # Returning x
#     return(x)
# }
#
# y <- .preprocess(train)
#
# # Defining a function which will convert all text into sentences and delineate
# # <bos> and <eos>
# .tknz_sent <- function(x) {
#     # Collapse everything to a single string
#     x <- paste(x, collapse = " ")
#     # Tokenize sentences
#     x <- kgrams::tknz_sent(x, keep_first = TRUE)
#     # Remove empty sentences
#     x <- x[x != ""]
#     # Returining x
#     return(x)
# }
#
# y <- .tknz_sent(y)
#
# library(tokenizers)
# trigrams <- tokenize_ngrams(y, lowercase = FALSE,
#                           n = 3L, simplify = TRUE) %>%
#     unlist() %>%
#     na.omit()
#
# trigram_count <- tibble(token = trigrams) %>%
#     separate()
#
#
# # Defining a function which will calculate frequencies(counts) up to trigrams
# N <- 3 # Tried various values, N <= 3 is good
# freq <- kgram_freqs(N, .preprocess = .preprocess, .tknz_sent = .tknz_sent)
# summary(freq)
#
# # Applying the function
# process_sentences(text = train, freqs = freq, verbose = FALSE)

# # Trying a add_k model (Katz smoothing)
# modelK <- language_model(freq, smoother = "add_k", k = 5)
# summary(modelK)
#
# # Defining a Absolute Discontinuing model
# modelAbs <- language_model(freq, smoother = "abs", D = 0.5)
# summary(modelAbs)
#
# # Defining a Kneser-Ney Smoothing model
# modelKn <- language_model(freq, smoother = "kn", D = 0.5)
# summary(modelKn)

# # Defining a modified Kneser-Ney Smoothing model
# model_mkn <- language_model(freq, smoother = "mkn", D1 = 0.5, D2 = 0.5, D3 = 0.5)
# summary(model_mkn)

# # Compare all 4 models on the validation set
# model_compare <- tibble(katz = perplexity(text = validation, model = modelK),
#                         abs = perplexity(text = validation, model  = modelAbs),
#                         KN = perplexity(text = validation, model = modelKn),
#                         modKN = perplexity(text = validation, model = model_mkn))
#
# model_compare
#' Katz = 20000
#' Absolute Discounting = 650
#' Kneser-Ney = 501
#' Kneser-Ney = 501
# We can go for either kneser ney or modified kneser ney, both are much better
# and Katz is the worst

# # We will go for modified kneser ney model
# model <- language_model(freq, smoother = "mkn", D1 = 0.5, D2 = 0.5, D3 = 0.5)
# summary(model)

# Tuning parameters are N and D1, D2, and D3
# First we will tune for N
# Literature suggests N = 3 is usually enough
# Pentagram = 501
# param(model, "N") <- N-1
# parameters(model)
# perplexity(validation, model) # Quadgram = 479.7906
# gc()
# param(model, "N") <- N-2
# parameters(model)
# perplexity(validation, model) # Trigram = 434.7916
# gc()
# param(model, "N") <- N-3
# parameters(model)
# perplexity(validation, model) # Bigram = 419.4605
# gc()
# Final decision reserved till we can reduce the sample size

# parameters(model)
# # Defining a tune function
# tune <- function(D1_grid, D2_grid, D3_grid) {
#     res <- list(D1 = 0,  D2 = 0, D3 = 0, perplexity = Inf)
#     for (D1 in D1_grid)
#         for (D2 in D2_grid)
#             for (D3 in D3_grid) {
#                 param(model, "D1") <- D1
#                 param(model, "D2") <- D2
#                 param(model, "D3") <- D3
#                 perplexity <- perplexity(validation_subset, model)
#                 if (perplexity < res$perplexity)
#                     res <- list(D1 = D1,
#                                 D2 = D2,
#                                 D3 = D3,
#                                 perplexity = perplexity)
#             }
#     return(res)
# }
#
# # Defining our grid
# D1_grid <- c(0.8, 0.9); D2_grid <- D3_grid <- 0.99
# set.seed(9923)
# validation_subset <- validation[sample(1:30000, size = 10000)]
# best_vals <- tune(D1_grid, D2_grid, D3_grid)
# gc()
# best_vals
# $D1
# [1] 0.99, 0.9
#
# $D2
# [1] 0.99, 0.99
#
# $D3
# [1] 0.99, 0.99
#
# $perplexity
# [1] 355.0438, 342.1818

#' # Final model
#' model_final <- language_model(freq, smoother = "mkn", D1 = 0.9, D2 = 0.99, D3 = 0.99)
#' model_kn <- language_model(freq, smoother = "kn", D = 0.9)
#' # model_sbo <- language_model(freq, smoother = "sbo", lambda = 0.5)
#' summary(model_kn)
#' summary(model_final)
#' summary(model_sbo)
#'
#' # Quiz 2
#' options = c("insane", "callous", "asleep", "insensitive")
#' tibble(output = options,
#'        prob = probability(options %|% "then you must be", model_kn))
#'
#' #' Model is slow in predicting
#' #'
#' #' Ideas:
#' #'
#' #' 1. Reduce the length of input string
#' #' 2. Reduce the sample size
#' #' 3. Include built in dictionaries to increase the accuracy for small sample sizes
#'
#' # Trying idea 1
#' system.time({
#'     probability(options %|% "If this isn't the cutest thing you've ever seen, then you must be", model_final)
#' }) # user  system elapsed
#' #   62.94  180.55  243.76
#' system.time({
#'     probability(options %|% "you must be", model_final)
#' }) # user  system elapsed
#' #   65.86  178.82  245.02
#' # Does not make a difference
#'
#' # Trying Idea 2
#' # N = 10000
#' perplexity(validation, model_final)
#' # 83.22676
#' system.time({
#'     probability(options %|% "If this isn't the cutest thing you've ever seen, then you must be", model_final)
#' }) # user  system elapsed
#' #   61.23  195.66  257.46
#' system.time({
#'     probability(options %|% "then you must be", model_final)
#' }) # user  system elapsed
#' #   58.88  199.03  258.44
#' system.time({
#'     probability(options %|% "you must be", model_final)
#' }) # user  system elapsed
#' #   59.78  198.26  258.39
