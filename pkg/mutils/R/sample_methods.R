setMethod("sample", "data.frame", function (x, size, replace = FALSE, prob = NULL){
    if (missing(size))
        size <- nrow(x)
    ii <- sample.int(nrow(x), size, replace, prob)
    x[ii,,drop=FALSE]
})

