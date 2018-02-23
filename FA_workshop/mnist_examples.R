download.file("http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz",
              "data/t10k-images-idx3-ubyte.gz")
download.file("http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz",
              "data/t10k-labels-idx1-ubyte.gz")

# gunzip the files
# R.utils::gunzip("train-images-idx3-ubyte.gz")
# R.utils::gunzip("train-labels-idx1-ubyte.gz")
# utils::unzip("t10k-images-idx3-ubyte.gz")
# utils::unzip("t10k-labels-idx1-ubyte.gz")

# https://gist.github.com/daviddalpiaz/ae62ae5ccd0bada4b9acd6dbc9008706


# helper function for visualization
show_digit = function(arr784, col = gray(12:1 / 12), ...) {
  image(matrix(as.matrix(arr784), nrow = 28)[, 28:1], col = col, ...)
}

# load image files
load_image_file = function(filename) {
  ret = list()
  f = file(filename, 'rb')
  readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  n    = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  nrow = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  ncol = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  x = readBin(f, 'integer', n = n * nrow * ncol, size = 1, signed = FALSE)
  close(f)
  data.frame(matrix(x, ncol = nrow * ncol, byrow = TRUE))
}

# load label files
load_label_file = function(filename) {
  f = file(filename, 'rb')
  readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  n = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  y = readBin(f, 'integer', n = n, size = 1, signed = FALSE)
  close(f)
  y
}

# load images
# train = load_image_file("data/train-images-idx3-ubyte")
test  = load_image_file("data/t10k-images-idx3-ubyte")

# load labels
# train$y = as.factor(load_label_file("data/train-labels-idx1-ubyte"))
test$y  = as.factor(load_label_file("data/t10k-labels-idx1-ubyte")) 

# view test image
# show_digit(test[1,])
threes = test[test$y==3, -785]
threes_label=test$y
save(threes, threes_label, file = 'data/threes_mnist.RData')

which_digit = 10
show_digit(threes[which_digit,])

# many of the columns are useless
test_pca = princomp(threes)


ncomp  = c(0,2, 25, 100, 250, 784)
par(mfrow=c(2,3))
for(i in 1:length(ncomp)) {
  if(i == 1) show_digit(threes[which_digit,], main='Original', col = viridis::viridis(500))
  else {
    recon = tcrossprod(test_pca$scores[,1:ncomp[i]], test_pca$loadings[,1:ncomp[i]])
    # scores = sweep(recon, 2, FUN = '*', test_pca$sdev)
    scores = sweep(recon, 2, FUN = '+', test_pca$center)
    show_digit(scores[which_digit, ], main = paste0('N factors = ', ncomp[i]), 
               col = viridis::viridis(500, begin = 0, end = 1),
               xaxt='n',
               yaxt='n')
  }
}

layout(1)



cars_pca = princomp(mtcars)
sweep(tcrossprod(cars_pca$scores, cars_pca$loadings), 2, FUN = '+', cars_pca$center)
mtcars



which_digit = 10
show_digit(threes[which_digit,])
%>% %>% %>% %>% %>% %>%  %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% 
%>% %>% %>% %>% %>% %>%  %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% # many of the columns are useless
zero_idx = colSums(three %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% s) == 0
test_fa = psych::fa( %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% )

ncomp  = c(0,1:3, 25, 50)
fa_scores = psych::factor.scores(threes[,!zero_idx], f=test_fa, method='Bartlett')

par(mfrow=c(2,3))
for(i in 1:length(ncomp)) {
  if(i == 1) show_digit(threes[which_digit,], main='Original', col = viridis::viridis(500),
                        xaxt='n',
                        yaxt='n')
  else {
    recon = matrix(0, dim(threes)[1], dim(threes)[2])
    recon[,!zero_idx] = tcrossprod(fa_scores$scores[,1:ncomp[i]], test_fa$loadings[,1:ncomp[i]]) #%>% scale()
    # recon = sweep(recon, 2, FUN = '*', apply(threes, 2, sd))
    recon = sweep(recon, 2, FUN = '+', colMeans(threes))
    recon = round(recon, 2)
    show_digit(recon[which_digit, ], main = paste0('N factors = ', ncomp[i]), 
               col = viridis::viridis(500, begin = 0, end = 1),
               xaxt='n',
               yaxt='n')   
  }
}

# range(apply(recon, 2, sd))
# range(apply(threes, 2, sd))
# range(apply(recon, 2, mean))
# range(apply(threes, 2, mean))

layout(1)



library(NMF)
test_nmf50 = nmf(threes[,!zero_idx], rank = 50)
test_nmf = nmf(threes[,!zero_idx], rank = 10)
ncomp  = c(0, 1:3, 5, 10)

par(mfrow=c(2,3))
for(i in 1:length(ncomp)) {
  if(i == 1) show_digit(threes[which_digit,], main='Original', col = viridis::viridis(500),
                        xaxt='n',
                        yaxt='n')
  else {
    recon = matrix(0, dim(threes)[1], dim(threes)[2])
    recon[,!zero_idx] =  test_nmf@fit@W[,1:ncomp[i]] %>% as.matrix() %*% test_nmf@fit@H[1:ncomp[i],] %>% as.matrix()
    show_digit(recon[which_digit, ], main = paste0('N factors = ', ncomp[i]), 
               col = viridis::viridis(500, begin = 0, end = 1),
               xaxt='n',
               yaxt='n')
  }
}

layout(1)


library(text2vec)

lda_model = LDA$new(n_topics = 50)
test_lda = lda_model$fit_transform(as(as.matrix(threes), 'CsparseMatrix'), n_iter = 1000, doc_topic_prior = 0.1, topic_word_prior = 0.01)
# lda_model$get_top_words(n = 784, topic_number = 1:10, lambda = 1)
doc_word_probs = test_lda %*% lda_model$topic_word_distribution %>% cbind(rowSums(threes))
simdat = apply(doc_word_probs, 1, function(x) rmultinom(1, size = x[785], prob = x[-785])) %>% t
# show_digit(simdat[which_digit,], main = paste0('N factors = ', 10), 
#            col = viridis::viridis(500, begin = 0, end = 1),
#            xaxt='n',
#            yaxt='n')


ncomp  = c(0,  15, 20, 30, 40, 50)

par(mfrow=c(2,3))
for(i in 1:length(ncomp)) {
  if(i == 1) show_digit(threes[which_digit,], main='Original', col = viridis::viridis(500),
                        xaxt='n',
                        yaxt='n')
  else {
    recon = test_lda[,1:ncomp[i]] %*% lda_model$topic_word_distribution[1:ncomp[i],] %>% cbind(rowSums(threes))
    recon = apply(recon, 1, function(x) rmultinom(1, size = x[785], prob = x[-785])) %>% t
    show_digit(recon[which_digit,], main = paste0('N factors = ', ncomp[i]), 
               col = viridis::viridis(500, begin = 0, end = 1),
               xaxt='n',
               yaxt='n')
  }
}




save(test_pca,
     test_fa,
     test_nmf,
     test_lda,
     file='data/digit_results.RData')
