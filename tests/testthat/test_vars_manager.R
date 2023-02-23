test_that("human_bytes returns bytes", {
    expect_equal(human_bytes(1023), "1023 Bytes")
    expect_equal(human_bytes(1024), "1.00 KB")
    expect_equal(human_bytes(1024), "1.00 KB")
    expect_equal(human_bytes(1048575), "1024.00 KB")
    expect_equal(human_bytes(1048576), "1.00 MB")
    expect_equal(human_bytes(1073741823), "1024.00 MB")
    expect_equal(human_bytes(1073741824), "1.00 GB")
})

test_that("format_vars single element character vector", {
    string1 <- "efs"
    vars <- format_vars(environment())
    parsed_vars = rjson::fromJSON(vars)
    expect_equal(length(parsed_vars), 1)
    expect_equal(parsed_vars[[1]]$name, "string1")
    expect_equal(parsed_vars[[1]]$type, "character")
    expect_equal(parsed_vars[[1]]$abbreviated, FALSE)
    expect_equal(parsed_vars[[1]]$summary, "efs")
    expect_equal(parsed_vars[[1]]$value$single_value, "efs")
})

test_that("format_vars single element numeric vector", {
    numeric1 <- 342.3
    vars <- format_vars(environment())
    parsed_vars = rjson::fromJSON(vars)
    expect_equal(length(parsed_vars), 1)
    expect_equal(parsed_vars[[1]]$name, "numeric1")
    expect_equal(parsed_vars[[1]]$type, "numeric")
    expect_equal(parsed_vars[[1]]$abbreviated, FALSE)
    expect_equal(parsed_vars[[1]]$summary, "342.3")
    expect_equal(parsed_vars[[1]]$value$single_value, "342.3")
})

test_that("format_vars single element integer vector", {
    integer1 <- 9
    vars <- format_vars(environment())
    parsed_vars = rjson::fromJSON(vars)
    expect_equal(length(parsed_vars), 1)
    expect_equal(parsed_vars[[1]]$name, "integer1")
    expect_equal(parsed_vars[[1]]$type, "numeric")
    expect_equal(parsed_vars[[1]]$abbreviated, FALSE)
    expect_equal(parsed_vars[[1]]$summary, "9")
    expect_equal(parsed_vars[[1]]$value$single_value, "9")
})

test_that("format_vars single element logical vector", {
    logical1 <- TRUE
    vars <- format_vars(environment())
    parsed_vars = rjson::fromJSON(vars)
    expect_equal(length(parsed_vars), 1)
    expect_equal(parsed_vars[[1]]$name, "logical1")
    expect_equal(parsed_vars[[1]]$type, "logical")
    expect_equal(parsed_vars[[1]]$abbreviated, FALSE)
    expect_equal(parsed_vars[[1]]$summary, "TRUE")
    expect_equal(parsed_vars[[1]]$value$single_value, "TRUE")
})

test_that("format_vars single element complex vector", {
    complex1 <- 1 + 2i 
    vars <- format_vars(environment())
    parsed_vars = rjson::fromJSON(vars)
    expect_equal(length(parsed_vars), 1)
    expect_equal(parsed_vars[[1]]$name, "complex1")
    expect_equal(parsed_vars[[1]]$type, "complex")
    expect_equal(parsed_vars[[1]]$abbreviated, FALSE)
    expect_equal(parsed_vars[[1]]$summary, "1+2i")
    expect_equal(parsed_vars[[1]]$value$single_value, "1+2i")
})

test_that("format_vars multi element vector", {
    vector1 <- c(4, 3)
    vars <- format_vars(environment())
    parsed_vars = rjson::fromJSON(vars)
    expect_equal(length(parsed_vars), 1)
    expect_equal(parsed_vars[[1]]$name, "vector1")
    expect_equal(parsed_vars[[1]]$type, "numeric")
    expect_equal(parsed_vars[[1]]$abbreviated, FALSE)
    expect_equal(parsed_vars[[1]]$summary, "Length: 2")
    expect_equal(parsed_vars[[1]]$value$multi_value$column_count, 1)
    expect_equal(parsed_vars[[1]]$value$multi_value$row_count, 2)
    expect_equal(parsed_vars[[1]]$value$multi_value$column_names, c("vector1"))
    expect_equal(parsed_vars[[1]]$value$multi_value$row_names, c("1", "2"))
    expect_equal(parsed_vars[[1]]$value$multi_value$data, c("4", "3"))
})

test_that("format_vars multi element list", {
    list1 <- list(4, 3)
    vars <- format_vars(environment())
    parsed_vars = rjson::fromJSON(vars)
    expect_equal(length(parsed_vars), 1)
    expect_equal(parsed_vars[[1]]$name, "list1")
    expect_equal(parsed_vars[[1]]$type, "list")
    expect_equal(parsed_vars[[1]]$abbreviated, FALSE)
    expect_equal(parsed_vars[[1]]$summary, "Length: 2")
    expect_equal(parsed_vars[[1]]$value$multi_value$column_count, 1)
    expect_equal(parsed_vars[[1]]$value$multi_value$row_count, 2)
    expect_equal(parsed_vars[[1]]$value$multi_value$column_names, c("list1"))
    expect_equal(parsed_vars[[1]]$value$multi_value$row_names, c("1", "2"))
    expect_equal(parsed_vars[[1]]$value$multi_value$data, c("4", "3"))
})

test_that("format_vars multi element list abbreviated", {
    list1 <- as.list(300:1)
    vars <- format_vars(environment())
    parsed_vars = rjson::fromJSON(vars)
    expect_equal(length(parsed_vars), 1)
    expect_equal(parsed_vars[[1]]$name, "list1")
    expect_equal(parsed_vars[[1]]$type, "list")
    expect_equal(parsed_vars[[1]]$abbreviated, TRUE)
    expect_equal(parsed_vars[[1]]$summary, "Length: 300")
    expect_equal(parsed_vars[[1]]$value$multi_value$column_count, 1)
    expect_equal(parsed_vars[[1]]$value$multi_value$row_count, 300)
    expect_equal(parsed_vars[[1]]$value$multi_value$column_names, c("list1"))
    expect_equal(parsed_vars[[1]]$value$multi_value$row_names, as.character(1:50))
    expect_equal(parsed_vars[[1]]$value$multi_value$data, as.character(300:251))
})

test_that("format_vars single element named list", {
    list1 <- list(cool="kat")
    vars <- format_vars(environment())
    parsed_vars = rjson::fromJSON(vars)
    expect_equal(length(parsed_vars), 1)
    expect_equal(parsed_vars[[1]]$name, "list1")
    expect_equal(parsed_vars[[1]]$type, "list")
    expect_equal(parsed_vars[[1]]$abbreviated, FALSE)
    expect_equal(parsed_vars[[1]]$summary, "Length: 1")
    expect_equal(parsed_vars[[1]]$value$multi_value$column_count, 1)
    expect_equal(parsed_vars[[1]]$value$multi_value$row_count, 1)
    expect_equal(parsed_vars[[1]]$value$multi_value$column_names, c("list1"))
    expect_equal(parsed_vars[[1]]$value$multi_value$row_names, c("cool"))
    expect_equal(parsed_vars[[1]]$value$multi_value$data, c("kat"))
})

test_that("format_vars multiple element named list abbreviated", {
    list1 <- list()
    for (i in 1:300) {
        list1[toString(i)] = i
    }
    rm(i)
    vars <- format_vars(environment())
    parsed_vars = rjson::fromJSON(vars)
    expect_equal(length(parsed_vars), 1)
    expect_equal(parsed_vars[[1]]$name, "list1")
    expect_equal(parsed_vars[[1]]$type, "list")
    expect_equal(parsed_vars[[1]]$abbreviated, TRUE)
    expect_equal(parsed_vars[[1]]$summary, "Length: 300")
    expect_equal(parsed_vars[[1]]$value$multi_value$column_count, 1)
    expect_equal(parsed_vars[[1]]$value$multi_value$row_count, 300)
    expect_equal(parsed_vars[[1]]$value$multi_value$column_names, c("list1"))
    expect_equal(parsed_vars[[1]]$value$multi_value$row_names, as.character(1:50))
    expect_equal(parsed_vars[[1]]$value$multi_value$data, as.character(1:50))
})

test_that("format_vars matrix", {
    matrix1 <- matrix(nrow=4, ncol=7)
    for (i in 1:4) {
        for (j in 1:7) {
            matrix1[i,j] = i * 4 + j
        }
    }
    rm(i)
    rm(j)
    vars <- format_vars(environment())
    parsed_vars = rjson::fromJSON(vars)
    expect_equal(length(parsed_vars), 1)
    expect_equal(parsed_vars[[1]]$name, "matrix1")
    expect_equal(grepl("matrix", parsed_vars[[1]]$type), TRUE)
    expect_equal(parsed_vars[[1]]$abbreviated, FALSE)
    expect_equal(startsWith(parsed_vars[[1]]$summary, "Size: 4x7 Memory: "), TRUE)
    expect_equal(parsed_vars[[1]]$value$multi_value$column_count, 7)
    expect_equal(parsed_vars[[1]]$value$multi_value$row_count, 4)
    expect_equal(parsed_vars[[1]]$value$multi_value$column_names, as.character(1:7))
    expect_equal(parsed_vars[[1]]$value$multi_value$row_names, as.character(1:4))
    mdata <- c()
    for (i in 1:4) {
        mdata[[i]] = as.character(i * 4 + 1:7)
    }
    expect_equal(parsed_vars[[1]]$value$multi_value$data, mdata)
})

test_that("format_vars matrix abbreviated", {
    matrix1 <- matrix(nrow=80, ncol=7)
    for (i in 1:80) {
        for (j in 1:7) {
            matrix1[i,j] = i * 4 + j
        }
    }
    rm(i)
    rm(j)
    vars <- format_vars(environment(), 7)
    parsed_vars = rjson::fromJSON(vars)
    expect_equal(length(parsed_vars), 1)
    expect_equal(parsed_vars[[1]]$name, "matrix1")
    expect_equal(grepl("matrix", parsed_vars[[1]]$type), TRUE)
    expect_equal(parsed_vars[[1]]$abbreviated, TRUE)
    expect_equal(startsWith(parsed_vars[[1]]$summary, "Size: 80x7 Memory: "), TRUE)
    expect_equal(parsed_vars[[1]]$value$multi_value$column_count, 7)
    expect_equal(parsed_vars[[1]]$value$multi_value$row_count, 80)
    expect_equal(parsed_vars[[1]]$value$multi_value$column_names, as.character(1:7))
    expect_equal(parsed_vars[[1]]$value$multi_value$row_names, as.character(1:7))
    mdata <- c()
    for (i in 1:7) {
        mdata[[i]] = as.character(i * 4 + 1:7)
    }
    expect_equal(parsed_vars[[1]]$value$multi_value$data, mdata)
})

test_that("format_vars data.frame abbreviated", {
    dataframe1 <- read.csv("iris.csv", colClasses=c("numeric", "numeric", "numeric", "numeric", "factor"))
    vars <- format_vars(environment(), 7)
    parsed_vars = rjson::fromJSON(vars)
    expect_equal(length(parsed_vars), 1)
    expect_equal(parsed_vars[[1]]$name, "dataframe1")
    expect_equal(parsed_vars[[1]]$type, "data.frame")
    expect_equal(parsed_vars[[1]]$abbreviated, TRUE)
    expect_equal(startsWith(parsed_vars[[1]]$summary, "Size: 150x5 Memory: "), TRUE)
    expect_equal(parsed_vars[[1]]$value$multi_value$column_count, 5)
    expect_equal(parsed_vars[[1]]$value$multi_value$row_count, 150)
    expect_equal(
        parsed_vars[[1]]$value$multi_value$column_names, 
        c(
            "sepallength (numeric)",
            "sepalwidth (numeric)",
            "petallength (numeric)",
            "petalwidth (numeric)",
            "class (factor)"
        )
    )
    expect_equal(parsed_vars[[1]]$value$multi_value$row_names, as.character(1:7))
    expect_equal(length(parsed_vars[[1]]$value$multi_value$data), 7)
})

test_that("format_var multi element list long but not abbreviated", {
    list1 <- as.list(300:1)
    vars <- format_var(environment(), "list1", NULL)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "list1")
    expect_equal(parsed_var$type, "list")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(parsed_var$summary, "Length: 300")
    expect_equal(parsed_var$value$multi_value$column_count, 1)
    expect_equal(parsed_var$value$multi_value$row_count, 300)
    expect_equal(parsed_var$value$multi_value$column_names, c("list1"))
    expect_equal(parsed_var$value$multi_value$row_names, as.character(1:300))
    expect_equal(parsed_var$value$multi_value$data, as.character(300:1))
})

test_that("format_var multi element named list long but not abbreviated", {
    list1 <- list()
    for (i in 1:300) {
        list1[toString(i)] = i
    }
    rm(i)
    vars <- format_var(environment(), "list1", NULL)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "list1")
    expect_equal(parsed_var$type, "list")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(parsed_var$summary, "Length: 300")
    expect_equal(parsed_var$value$multi_value$column_count, 1)
    expect_equal(parsed_var$value$multi_value$row_count, 300)
    expect_equal(parsed_var$value$multi_value$column_names, c("list1"))
    row_names = as.character(1:300)
    expect_equal(parsed_var$value$multi_value$row_names, row_names)
    expect_equal(parsed_var$value$multi_value$data, as.character(1:300))
})

test_that("format_var matrix long but not abbreviated", {
    matrix1 <- matrix(nrow=80, ncol=7)
    for (i in 1:80) {
        for (j in 1:7) {
            matrix1[i,j] = i * 4 + j
        }
    }
    rm(i)
    rm(j)
    vars <- format_var(environment(), "matrix1", NULL)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "matrix1")
    expect_equal(grepl("matrix", parsed_var$type), TRUE)
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 80x7 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 7)
    expect_equal(parsed_var$value$multi_value$row_count, 80)
    expect_equal(parsed_var$value$multi_value$column_names, as.character(1:7))
    expect_equal(parsed_var$value$multi_value$row_names, as.character(1:80))
    mdata <- c()
    for (i in 1:80) {
        mdata[[i]] = as.character(i * 4 + 1:7)
    }
    expect_equal(parsed_var$value$multi_value$data, mdata)
})

test_that("format_var matrix abbreviated", {
    matrix1 <- matrix(nrow=80, ncol=7)
    for (i in 1:80) {
        for (j in 1:7) {
            matrix1[i,j] = i * 4 + j
        }
    }
    rm(i)
    rm(j)
    vars <- format_var(environment(), "matrix1", 5)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "matrix1")
    expect_equal(grepl("matrix", parsed_var$type), TRUE)
    expect_equal(parsed_var$abbreviated, TRUE)
    expect_equal(startsWith(parsed_var$summary, "Size: 80x7 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 7)
    expect_equal(parsed_var$value$multi_value$row_count, 80)
    expect_equal(parsed_var$value$multi_value$column_names, as.character(1:7))
    expect_equal(parsed_var$value$multi_value$row_names, as.character(1:5))
    mdata <- c()
    for (i in 1:5) {
        mdata[[i]] = as.character(i * 4 + 1:7)
    }
    expect_equal(parsed_var$value$multi_value$data, mdata)
})

test_that("format_var matrix sort by single column", {
    matrix1 <- matrix(c(5, 4, 2, 2, 7, 9, 12, 10, 15, 4, 6, 3), ncol=2)

    vars <- format_var(environment(), "matrix1", NULL, c(1))
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "matrix1")
    expect_equal(grepl("matrix", parsed_var$type), TRUE)
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 6x2 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 2)
    expect_equal(parsed_var$value$multi_value$row_count, 6)
    expect_equal(parsed_var$value$multi_value$column_names, as.character(1:2))
    expect_equal(parsed_var$value$multi_value$row_names, as.character(1:6))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("2", "15"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("2", "4"))
    expect_equal(parsed_var$value$multi_value$data[[3]], c("4", "10"))
    expect_equal(parsed_var$value$multi_value$data[[4]], c("5", "12"))
    expect_equal(parsed_var$value$multi_value$data[[5]], c("7", "6"))
    expect_equal(parsed_var$value$multi_value$data[[6]], c("9", "3"))
})

test_that("format_var matrix sort by single column descending abbreviated", {
    matrix1 <- matrix(c(5, 4, 2, 2, 7, 9, 12, 10, 15, 4, 6, 3), ncol=2)

    vars <- format_var(environment(), "matrix1", 4, c(1), FALSE)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "matrix1")
    expect_equal(grepl("matrix", parsed_var$type), TRUE)
    expect_equal(parsed_var$abbreviated, TRUE)
    expect_equal(startsWith(parsed_var$summary, "Size: 6x2 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 2)
    expect_equal(parsed_var$value$multi_value$row_count, 6)
    expect_equal(parsed_var$value$multi_value$column_names, as.character(1:2))
    expect_equal(parsed_var$value$multi_value$row_names, as.character(1:4))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("9", "3"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("7", "6"))
    expect_equal(parsed_var$value$multi_value$data[[3]], c("5", "12"))
    expect_equal(parsed_var$value$multi_value$data[[4]], c("4", "10"))
})

test_that("format_var matrix sort by multiple columns", {
    matrix1 <- matrix(c(5, 4, 2, 2, 7, 9, 12, 10, 15, 4, 6, 3), ncol=2)

    vars <- format_var(environment(), "matrix1", NULL, c(1, 2), c(FALSE, TRUE))
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "matrix1")
    expect_equal(grepl("matrix", parsed_var$type), TRUE)
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 6x2 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 2)
    expect_equal(parsed_var$value$multi_value$row_count, 6)
    expect_equal(parsed_var$value$multi_value$column_names, as.character(1:2))
    expect_equal(parsed_var$value$multi_value$row_names, as.character(1:6))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("9", "3"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("7", "6"))
    expect_equal(parsed_var$value$multi_value$data[[3]], c("5", "12"))
    expect_equal(parsed_var$value$multi_value$data[[4]], c("4", "10"))
    expect_equal(parsed_var$value$multi_value$data[[5]], c("2", "4"))
    expect_equal(parsed_var$value$multi_value$data[[6]], c("2", "15"))
})

test_that("format_var matrix sort by multiple columns abbreviated", {
    matrix1 <- matrix(c(5, 4, 2, 2, 7, 9, 12, 10, 15, 4, 6, 3), ncol=2)

    vars <- format_var(environment(), "matrix1", 3, c(1, 2), c(FALSE, TRUE))
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "matrix1")
    expect_equal(grepl("matrix", parsed_var$type), TRUE)
    expect_equal(parsed_var$abbreviated, TRUE)
    expect_equal(startsWith(parsed_var$summary, "Size: 6x2 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 2)
    expect_equal(parsed_var$value$multi_value$row_count, 6)
    expect_equal(parsed_var$value$multi_value$column_names, as.character(1:2))
    expect_equal(parsed_var$value$multi_value$row_names, as.character(1:3))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("9", "3"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("7", "6"))
    expect_equal(parsed_var$value$multi_value$data[[3]], c("5", "12"))
})

test_that("format_var matrix sort by multiple columns with single ascending value", {
    matrix1 <- matrix(c(5, 4, 2, 2, 7, 9, 12, 10, 15, 4, 6, 3), ncol=2)

    vars <- format_var(environment(), "matrix1", NULL, c(2, 1), FALSE)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "matrix1")
    expect_equal(grepl("matrix", parsed_var$type), TRUE)
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 6x2 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 2)
    expect_equal(parsed_var$value$multi_value$row_count, 6)
    expect_equal(parsed_var$value$multi_value$column_names, as.character(1:2))
    expect_equal(parsed_var$value$multi_value$row_names, as.character(1:6))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("2", "15"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("5", "12"))
    expect_equal(parsed_var$value$multi_value$data[[3]], c("4", "10"))
    expect_equal(parsed_var$value$multi_value$data[[4]], c("7", "6"))
    expect_equal(parsed_var$value$multi_value$data[[5]], c("2", "4"))
    expect_equal(parsed_var$value$multi_value$data[[6]], c("9", "3"))
})

test_that("format_var matrix sort with characters", {
    matrix1 <- matrix(c("hello","hi","ab","a","A","BC","Z","cool"), ncol=2)

    vars <- format_var(environment(), "matrix1", NULL, c(1))
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "matrix1")
    expect_equal(grepl("matrix", parsed_var$type), TRUE)
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 4x2 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 2)
    expect_equal(parsed_var$value$multi_value$row_count, 4)
    expect_equal(parsed_var$value$multi_value$column_names, as.character(1:2))
    expect_equal(parsed_var$value$multi_value$row_names, as.character(1:4))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("a", "cool"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("ab", "Z"))
    expect_equal(parsed_var$value$multi_value$data[[3]], c("hello", "A"))
    expect_equal(parsed_var$value$multi_value$data[[4]], c("hi", "BC"))
})


test_that("format_var matrix filtering search", {
    matrix1 <- matrix(c("hello","hi","ab","a","A","BC","Z","cool"), ncol=2)

    filters <- data.frame(
        col = c(1),
        search = c("h"),
        min = c(NA),
        max = c(NA)
    )

    vars <- format_var(environment(), "matrix1", NULL, NULL, NULL, filters)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "matrix1")
    expect_equal(grepl("matrix", parsed_var$type), TRUE)
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 4x2 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 2)
    expect_equal(parsed_var$value$multi_value$row_count, 2)
    expect_equal(parsed_var$value$multi_value$column_names, as.character(1:2))
    expect_equal(parsed_var$value$multi_value$row_names, as.character(1:2))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("hello", "A"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("hi", "BC"))
})

test_that("format_var matrix filtering min/max characters", {
    matrix1 <- matrix(c("hello","hi","ab","a","A","BC","Z","cool"), ncol=2)

    filters <- data.frame(
        col = c(1),
        search = c(NA),
        min = c(4),
        max = c("B")
    )

    vars <- format_var(environment(), "matrix1", NULL, NULL, NULL, filters)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "matrix1")
    expect_equal(grepl("matrix", parsed_var$type), TRUE)
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 4x2 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 2)
    expect_equal(parsed_var$value$multi_value$row_count, 4)
    expect_equal(parsed_var$value$multi_value$column_names, as.character(1:2))
    expect_equal(parsed_var$value$multi_value$row_names, as.character(1:4))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("hello", "A"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("hi", "BC"))
    expect_equal(parsed_var$value$multi_value$data[[3]], c("ab", "Z"))
    expect_equal(parsed_var$value$multi_value$data[[4]], c("a", "cool"))
})

test_that("format_var matrix filtering min/max numeric", {
    matrix1 <- matrix(c(5, 4, 2, 2, 7, 9, 12, 10, 15, 4, 6, 3), ncol=2)

    filters <- data.frame(
        col = c(2),
        search = c(NA),
        min = c(4),
        max = c(10.0)
    )

    vars <- format_var(environment(), "matrix1", NULL, NULL, NULL, filters)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "matrix1")
    expect_equal(grepl("matrix", parsed_var$type), TRUE)
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 6x2 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 2)
    expect_equal(parsed_var$value$multi_value$row_count, 3)
    expect_equal(parsed_var$value$multi_value$column_names, as.character(1:2))
    expect_equal(parsed_var$value$multi_value$row_names, as.character(1:3))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("4", "10"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("2", "4"))
    expect_equal(parsed_var$value$multi_value$data[[3]], c("7", "6"))
})

test_that("format_var matrix filtering multiple columns", {
    matrix1 <- matrix(c(5, 4, 2, 2, 7, 9, 12, 10, 15, 4, 6, 3), ncol=2)

    filters <- data.frame(
        col = c(2, 1),
        search = c(NA, NA),
        min = c(4, 4),
        max = c(10.0, 9)
    )

    vars <- format_var(environment(), "matrix1", NULL, NULL, NULL, filters)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "matrix1")
    expect_equal(grepl("matrix", parsed_var$type), TRUE)
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 6x2 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 2)
    expect_equal(parsed_var$value$multi_value$row_count, 2)
    expect_equal(parsed_var$value$multi_value$column_names, as.character(1:2))
    expect_equal(parsed_var$value$multi_value$row_names, as.character(1:2))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("4", "10"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("7", "6"))
})


test_that("format_var data.frame long but not abbreviated", {
    dataframe1 <- read.csv("iris.csv", colClasses=c("numeric", "numeric", "numeric", "numeric", "character"))
    vars <- format_var(environment(), "dataframe1", NULL)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "dataframe1")
    expect_equal(parsed_var$type, "data.frame")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 150x5 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 5)
    expect_equal(parsed_var$value$multi_value$row_count, 150)
    expect_equal(
        parsed_var$value$multi_value$column_names,
        c(
            "sepallength (numeric)",
            "sepalwidth (numeric)",
            "petallength (numeric)",
            "petalwidth (numeric)",
            "class (character)"
        )
    )
    expect_equal(parsed_var$value$multi_value$row_names, as.character(1:150))
    expect_equal(length(parsed_var$value$multi_value$data), 150)
})

test_that("format_var data.frame abbreviated", {
    dataframe1 <- read.csv("iris.csv")
    vars <- format_var(environment(), "dataframe1", 5)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "dataframe1")
    expect_equal(parsed_var$type, "data.frame")
    expect_equal(parsed_var$abbreviated, TRUE)
    expect_equal(startsWith(parsed_var$summary, "Size: 150x5 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 5)
    expect_equal(parsed_var$value$multi_value$row_count, 150)
    expect_equal(
        parsed_var$value$multi_value$column_names,
        c(
            "sepallength (numeric)",
            "sepalwidth (numeric)",
            "petallength (numeric)",
            "petalwidth (numeric)",
            "class (character)"
        )
    )
    expect_equal(parsed_var$value$multi_value$row_names, as.character(1:5))
    expect_equal(length(parsed_var$value$multi_value$data), 5)
})

test_that("format_var data.frame sorting by single column", {
    dataframe1 <- data.frame(
        Name = c("Jon", "Bill", "Maria", "Ben", "Tina"),
        Age = c(23, 41, 32, 58, 26),
        Employed = c(TRUE, FALSE, TRUE, TRUE, FALSE)
    )
    vars <- format_var(environment(), "dataframe1", NULL, c("Age"))
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "dataframe1")
    expect_equal(parsed_var$type, "data.frame")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 5x3 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 3)
    expect_equal(parsed_var$value$multi_value$row_count, 5)
    expect_equal(
        parsed_var$value$multi_value$column_names,
        c("Name (character)", "Age (numeric)", "Employed (logical)")
    )
    expect_equal(parsed_var$value$multi_value$row_names, c("1", "5", "3", "2", "4"))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("Jon", "23", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("Tina", "26", "FALSE"))
    expect_equal(parsed_var$value$multi_value$data[[3]], c("Maria", "32", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[4]], c("Bill", "41", "FALSE"))
    expect_equal(parsed_var$value$multi_value$data[[5]], c("Ben", "58", "TRUE"))
})

test_that("format_var data.frame sorting by single column abbreviated", {
    dataframe1 <- data.frame(
        Name = c("Jon", "Bill", "Maria", "Ben", "Tina"),
        Age = c(23, 41, 32, 58, 26),
        Employed = c(TRUE, FALSE, TRUE, TRUE, FALSE)
    )
    vars <- format_var(environment(), "dataframe1", abbrev_len=3, sort_by=c("Age"))
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "dataframe1")
    expect_equal(parsed_var$type, "data.frame")
    expect_equal(parsed_var$abbreviated, TRUE)
    expect_equal(startsWith(parsed_var$summary, "Size: 5x3 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 3)
    expect_equal(parsed_var$value$multi_value$row_count, 5)
    expect_equal(parsed_var$value$multi_value$column_names, c("Name (character)", "Age (numeric)", "Employed (logical)"))
    expect_equal(parsed_var$value$multi_value$row_names, c("1", "5", "3"))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("Jon", "23", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("Tina", "26", "FALSE"))
    expect_equal(parsed_var$value$multi_value$data[[3]], c("Maria", "32", "TRUE"))
})

test_that("format_var data.frame sorting by multiple columns", {
    dataframe1 <- data.frame(
        Name = c("Jon", "Bill", "Maria", "Ben", "Tina"),
        Age = c(23, 41, 32, 58, 26),
        Employed = c(TRUE, FALSE, TRUE, TRUE, FALSE)
    )
    vars <- format_var(environment(), "dataframe1", NULL, c("Employed", "Age"))
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "dataframe1")
    expect_equal(parsed_var$type, "data.frame")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 5x3 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 3)
    expect_equal(parsed_var$value$multi_value$row_count, 5)
    expect_equal(parsed_var$value$multi_value$column_names, c("Name (character)", "Age (numeric)", "Employed (logical)"))
    expect_equal(parsed_var$value$multi_value$row_names, c("5", "2", "1", "3", "4"))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("Tina", "26", "FALSE"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("Bill", "41", "FALSE"))
    expect_equal(parsed_var$value$multi_value$data[[3]], c("Jon", "23", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[4]], c("Maria", "32", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[5]], c("Ben", "58", "TRUE"))
})

test_that("format_var data.frame sorting by multiple columns, one descending", {
    dataframe1 <- data.frame(
        Name = c("Jon", "Bill", "Maria", "Ben", "Tina"),
        Age = c(23, 41, 32, 58, 26),
        Employed = c(TRUE, FALSE, TRUE, TRUE, FALSE)
    )
    vars <- format_var(environment(), "dataframe1", NULL, c("Employed", "Age"), c(TRUE, FALSE))
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "dataframe1")
    expect_equal(parsed_var$type, "data.frame")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 5x3 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 3)
    expect_equal(parsed_var$value$multi_value$row_count, 5)
    expect_equal(parsed_var$value$multi_value$column_names, c("Name (character)", "Age (numeric)", "Employed (logical)"))
    expect_equal(parsed_var$value$multi_value$row_names, c("2", "5", "4", "3", "1"))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("Bill", "41", "FALSE"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("Tina", "26", "FALSE"))
    expect_equal(parsed_var$value$multi_value$data[[3]], c("Ben", "58", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[4]], c("Maria", "32", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[5]], c("Jon", "23", "TRUE"))
})

test_that("format_var data.frame sorting by multiple columns, both descending", {
    dataframe1 <- data.frame(
        Name = c("Jon", "Bill", "Maria", "Ben", "Tina"),
        Age = c(23, 41, 32, 58, 26),
        Employed = c(TRUE, FALSE, TRUE, TRUE, FALSE)
    )
    vars <- format_var(environment(), "dataframe1", NULL, c("Employed", "Age"), FALSE)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "dataframe1")
    expect_equal(parsed_var$type, "data.frame")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 5x3 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 3)
    expect_equal(parsed_var$value$multi_value$row_count, 5)
    expect_equal(parsed_var$value$multi_value$column_names, c("Name (character)", "Age (numeric)", "Employed (logical)"))
    expect_equal(parsed_var$value$multi_value$row_names, c("4", "3", "1", "2", "5"))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("Ben", "58", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("Maria", "32", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[3]], c("Jon", "23", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[4]], c("Bill", "41", "FALSE"))
    expect_equal(parsed_var$value$multi_value$data[[5]], c("Tina", "26", "FALSE"))
})

test_that("format_var data.frame sorting by index descending", {
    dataframe1 <- data.frame(
        Name = c("Jon", "Bill", "Maria", "Ben", "Tina"),
        Age = c(23, 41, 32, 58, 26),
        Employed = c(TRUE, FALSE, TRUE, TRUE, FALSE)
    )

    vars <- format_var(environment(), "dataframe1", NULL, c("InDEx"), c(FALSE))
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "dataframe1")
    expect_equal(parsed_var$type, "data.frame")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 5x3 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 3)
    expect_equal(parsed_var$value$multi_value$row_count, 5)
    expect_equal(parsed_var$value$multi_value$column_names, c("Name (character)", "Age (numeric)", "Employed (logical)"))
    expect_equal(parsed_var$value$multi_value$row_names, c("5", "4", "3", "2", "1"))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("Tina", "26", "FALSE"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("Ben", "58", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[3]], c("Maria", "32", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[4]], c("Bill", "41", "FALSE"))
    expect_equal(parsed_var$value$multi_value$data[[5]], c("Jon", "23", "TRUE"))
})

test_that("format_var data.frame sorting by string index descending", {
    dataframe1 <- data.frame(
        newIndex = c("Row1", "Row6", "Row10", "Row8", "Row2"),
        Name = c("Jon", "Bill", "Maria", "Ben", "Tina"),
        Age = c(23, 41, 32, 58, 26),
        Employed = c(TRUE, FALSE, TRUE, TRUE, FALSE)
    )
    rownames(dataframe1) <- dataframe1$newIndex
    dataframe1$newIndex <- NULL

    vars <- format_var(environment(), "dataframe1", NULL, "InDEx", FALSE)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "dataframe1")
    expect_equal(parsed_var$type, "data.frame")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 5x3 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 3)
    expect_equal(parsed_var$value$multi_value$row_count, 5)
    expect_equal(parsed_var$value$multi_value$column_names, c("Name (character)", "Age (numeric)", "Employed (logical)"))
    expect_equal(parsed_var$value$multi_value$row_names, c("Row8", "Row6", "Row2", "Row10", "Row1"))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("Ben", "58", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("Bill", "41", "FALSE"))
    expect_equal(parsed_var$value$multi_value$data[[3]], c("Tina", "26", "FALSE"))
    expect_equal(parsed_var$value$multi_value$data[[4]], c("Maria", "32", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[5]], c("Jon", "23", "TRUE"))
})

test_that("format_var data.frame filtering search", {
    dataframe1 <- data.frame(
        Name = c("Jon", "Billy J", "Maria", "Jen", "Tina"),
        Age = c(23, 41, 32, 58, 26),
        Employed = c(TRUE, FALSE, TRUE, TRUE, FALSE)
    )

    filters <- data.frame(
        col = c("Name"),
        search = c("j"),
        min = c(NA),
        max = c(NA)
    )

    vars <- format_var(environment(), "dataframe1", NULL, NULL, NULL, filters)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "dataframe1")
    expect_equal(parsed_var$type, "data.frame")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 5x3 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 3)
    expect_equal(parsed_var$value$multi_value$row_count, 3)
    expect_equal(parsed_var$value$multi_value$column_names, c("Name (character)", "Age (numeric)", "Employed (logical)"))
    expect_equal(parsed_var$value$multi_value$row_names, c("1", "2", "4"))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("Jon", "23", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("Billy J", "41", "FALSE"))
    expect_equal(parsed_var$value$multi_value$data[[3]], c("Jen", "58", "TRUE"))
})

test_that("format_var data.frame filtering search multiple columns", {
    dataframe1 <- data.frame(
        Name = c("Jon", "Billy J", "Maria", "Jen", "Tina"),
        Age = c(23, 41, 416, 4136, 26),
        Employed = c(TRUE, FALSE, TRUE, TRUE, FALSE)
    )

    filters <- data.frame(
        col = c("Name", "Age"),
        search = c("j", "41"),
        min = c(NA, NA),
        max = c(NA, NA)
    )

    vars <- format_var(environment(), "dataframe1", NULL, NULL, NULL, filters)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "dataframe1")
    expect_equal(parsed_var$type, "data.frame")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 5x3 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 3)
    expect_equal(parsed_var$value$multi_value$row_count, 2)
    expect_equal(parsed_var$value$multi_value$column_names, c("Name (character)", "Age (numeric)", "Employed (logical)"))
    expect_equal(parsed_var$value$multi_value$row_names, c("2", "4"))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("Billy J", "41", "FALSE"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("Jen", "4136", "TRUE"))
})

test_that("format_var data.frame filtering min/max numeric", {
    dataframe1 <- data.frame(
        Name = c("Jon", "Billy J", "Maria", "Jen", "Tina"),
        Age = c(23, 41, 416, 4136, 26),
        Employed = c(TRUE, FALSE, TRUE, TRUE, FALSE)
    )

    filters <- data.frame(
        col = c("Age"),
        search = c(NA),
        min = c(26),
        max = c(416)
    )

    vars <- format_var(environment(), "dataframe1", NULL, NULL, NULL, filters)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "dataframe1")
    expect_equal(parsed_var$type, "data.frame")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 5x3 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 3)
    expect_equal(parsed_var$value$multi_value$row_count, 3)
    expect_equal(parsed_var$value$multi_value$column_names, c("Name (character)", "Age (numeric)", "Employed (logical)"))
    expect_equal(parsed_var$value$multi_value$row_names, c("2", "3", "5"))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("Billy J", "41", "FALSE"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("Maria", "416", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[3]], c("Tina", "26", "FALSE"))
})

test_that("format_var data.frame filtering min/max characters", {
    dataframe1 <- data.frame(
        Name = c("Jon", "Billy J", "Maria", "Jen", "Tina"),
        Age = c(23, 41, 416, 4136, 26),
        Employed = c(TRUE, FALSE, TRUE, TRUE, FALSE)
    )

    filters <- data.frame(
        col = c("Name"),
        search = c(NA),
        min = c(26),
        max = c(416)
    )

    vars <- format_var(environment(), "dataframe1", NULL, NULL, NULL, filters)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "dataframe1")
    expect_equal(parsed_var$type, "data.frame")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 5x3 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 3)
    expect_equal(parsed_var$value$multi_value$row_count, 5)
    expect_equal(parsed_var$value$multi_value$column_names, c("Name (character)", "Age (numeric)", "Employed (logical)"))
    expect_equal(parsed_var$value$multi_value$row_names, c("1", "2", "3", "4", "5"))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("Jon", "23", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("Billy J", "41", "FALSE"))
    expect_equal(parsed_var$value$multi_value$data[[3]], c("Maria", "416", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[4]], c("Jen", "4136", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[5]], c("Tina", "26", "FALSE"))
})


test_that("format_var data.frame filtering on separate columns with search and min/max", {
    dataframe1 <- data.frame(
        Name = c("Jon", "Billy J", "Maria", "Jen", "Tina"),
        Age = c(23, 41, 416, 4136, 26),
        Employed = c(TRUE, FALSE, TRUE, TRUE, FALSE)
    )

    filters <- data.frame(
        col = c("Age", "Name"),
        search = c(NA, "a"),
        min = c(26, NA),
        max = c(416, NA)
    )

    vars <- format_var(environment(), "dataframe1", NULL, NULL, NULL, filters)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "dataframe1")
    expect_equal(parsed_var$type, "data.frame")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(startsWith(parsed_var$summary, "Size: 5x3 Memory: "), TRUE)
    expect_equal(parsed_var$value$multi_value$column_count, 3)
    expect_equal(parsed_var$value$multi_value$row_count, 2)
    expect_equal(parsed_var$value$multi_value$column_names, c("Name (character)", "Age (numeric)", "Employed (logical)"))
    expect_equal(parsed_var$value$multi_value$row_names, c("3", "5"))
    expect_equal(parsed_var$value$multi_value$data[[1]], c("Maria", "416", "TRUE"))
    expect_equal(parsed_var$value$multi_value$data[[2]], c("Tina", "26", "FALSE"))
})


test_that("format_var vector multi element", {
    vector1 <- c(2, 3, 5, 1, 6, 7)
    vars <- format_var(environment(), "vector1", NULL)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "vector1")
    expect_equal(parsed_var$type, "numeric")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(parsed_var$summary, "Length: 6")
    expect_equal(parsed_var$value$multi_value$column_count, 1)
    expect_equal(parsed_var$value$multi_value$row_count, 6)
    expect_equal(parsed_var$value$multi_value$column_names, c("vector1"))
    row_names = as.character(1:6)
    expect_equal(parsed_var$value$multi_value$row_names, row_names)
    expect_equal(parsed_var$value$multi_value$data, c("2", "3", "5", "1", "6", "7"))
})

test_that("format_var vector sorting integers", {
    vector1 <- c(2, 3, 5, 1, 6, 7)
    vars <- format_var(environment(), "vector1", NULL, "value")
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "vector1")
    expect_equal(parsed_var$type, "numeric")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(parsed_var$summary, "Length: 6")
    expect_equal(parsed_var$value$multi_value$column_count, 1)
    expect_equal(parsed_var$value$multi_value$row_count, 6)
    expect_equal(parsed_var$value$multi_value$column_names, c("vector1"))
    row_names = as.character(1:6)
    expect_equal(parsed_var$value$multi_value$row_names, row_names)
    expect_equal(parsed_var$value$multi_value$data, c("1", "2", "3", "5", "6", "7"))
})

test_that("format_var vector sorting strings", {
    vector1 <- c("cc", "A", "aa", NA, "Z", "100", "11", NA)
    vars <- format_var(environment(), "vector1", NULL, "value")
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "vector1")
    expect_equal(parsed_var$type, "character")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(parsed_var$summary, "Length: 8")
    expect_equal(parsed_var$value$multi_value$column_count, 1)
    expect_equal(parsed_var$value$multi_value$row_count, 8)
    expect_equal(parsed_var$value$multi_value$column_names, c("vector1"))
    row_names = as.character(1:8)
    expect_equal(parsed_var$value$multi_value$row_names, row_names)
    expect_equal(parsed_var$value$multi_value$data, c("NA", "NA", "100", "11", "A", "aa", "cc", "Z"))
})

test_that("format_var vector sorting strings descending", {
    vector1 <- c("cc", "A", "aa", NA, "Z", "100", "11", NA)
    vars <- format_var(environment(), "vector1", NULL, "value", FALSE)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "vector1")
    expect_equal(parsed_var$type, "character")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(parsed_var$summary, "Length: 8")
    expect_equal(parsed_var$value$multi_value$column_count, 1)
    expect_equal(parsed_var$value$multi_value$row_count, 8)
    expect_equal(parsed_var$value$multi_value$column_names, c("vector1"))
    row_names = as.character(1:8)
    expect_equal(parsed_var$value$multi_value$row_names, row_names)
    expect_equal(parsed_var$value$multi_value$data, c("Z", "cc", "aa", "A", "11", "100", "NA", "NA"))
})

test_that("format_var vector sorting abbreviated", {
    vector1 <- c("cc", "A", "aa", "Z", "100", "11")
    vars <- format_var(environment(), "vector1", 4, c("value"))
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "vector1")
    expect_equal(parsed_var$type, "character")
    expect_equal(parsed_var$abbreviated, TRUE)
    expect_equal(parsed_var$summary, "Length: 6")
    expect_equal(parsed_var$value$multi_value$column_count, 1)
    expect_equal(parsed_var$value$multi_value$row_count, 6)
    expect_equal(parsed_var$value$multi_value$column_names, c("vector1"))
    row_names = as.character(1:4)
    expect_equal(parsed_var$value$multi_value$row_names, row_names)
    expect_equal(parsed_var$value$multi_value$data, c("100", "11", "A", "aa"))
})

test_that("format_var vector sorting integers descending", {
    vector1 <- c(2, 3, 5, 1, 6, 7)
    vars <- format_var(environment(), "vector1", NULL, "value", c(FALSE))
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "vector1")
    expect_equal(parsed_var$type, "numeric")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(parsed_var$summary, "Length: 6")
    expect_equal(parsed_var$value$multi_value$column_count, 1)
    expect_equal(parsed_var$value$multi_value$row_count, 6)
    expect_equal(parsed_var$value$multi_value$column_names, c("vector1"))
    row_names = as.character(1:6)
    expect_equal(parsed_var$value$multi_value$row_names, row_names)
    expect_equal(parsed_var$value$multi_value$data, c("7", "6", "5", "3", "2", "1"))
})

test_that("format_var vector search characters", {
    vector1 <- c("cc", "Bird", "abiRda", "Z", "100bird", "11")

    filters <- data.frame(
        col = c("value"),
        search = c("bird"),
        min = c(NA),
        max = c(NA)
    )

    vars <- format_var(environment(), "vector1", 4, NULL, NULL, filters)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "vector1")
    expect_equal(parsed_var$type, "character")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(parsed_var$summary, "Length: 6")
    expect_equal(parsed_var$value$multi_value$column_count, 1)
    expect_equal(parsed_var$value$multi_value$row_count, 3)
    expect_equal(parsed_var$value$multi_value$column_names, c("vector1"))
    expect_equal(parsed_var$value$multi_value$row_names, c("1", "2", "3"))
    expect_equal(parsed_var$value$multi_value$data, c("Bird", "abiRda", "100bird"))
})

test_that("format_var vector search numeric abbreviated", {
    vector1 <- c(10, 3, 100, 101, 34, 3501)

    filters <- data.frame(
        col = c("value"),
        search = c("10"),
        min = c(NA),
        max = c(NA)
    )

    vars <- format_var(environment(), "vector1", 2, "value", FALSE, filters)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "vector1")
    expect_equal(parsed_var$type, "numeric")
    expect_equal(parsed_var$abbreviated, TRUE)
    expect_equal(parsed_var$summary, "Length: 6")
    expect_equal(parsed_var$value$multi_value$column_count, 1)
    expect_equal(parsed_var$value$multi_value$row_count, 3)
    expect_equal(parsed_var$value$multi_value$column_names, c("vector1"))
    expect_equal(parsed_var$value$multi_value$row_names, c("1", "2"))
    expect_equal(parsed_var$value$multi_value$data, c("101", "100"))
})

test_that("format_var vector min/max numeric", {
    vector1 <- c(10, 3, 100, 101, 34, 3501)

    filters <- data.frame(
        col = c("value"),
        search = c(NA),
        min = c(34),
        max = c(3000)
    )

    vars <- format_var(environment(), "vector1", 2, "value", NULL, filters)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "vector1")
    expect_equal(parsed_var$type, "numeric")
    expect_equal(parsed_var$abbreviated, TRUE)
    expect_equal(parsed_var$summary, "Length: 6")
    expect_equal(parsed_var$value$multi_value$column_count, 1)
    expect_equal(parsed_var$value$multi_value$row_count, 3)
    expect_equal(parsed_var$value$multi_value$column_names, c("vector1"))
    expect_equal(parsed_var$value$multi_value$row_names, c("1", "2"))
    expect_equal(parsed_var$value$multi_value$data, c("34", "100"))
})

test_that("format_var vector min/max characters", {
    vector1 <- c("cc", "Bird", "abiRda", "Z", "100bird", "11")

    filters <- data.frame(
        col = c("value"),
        search = c(NA),
        min = c(3),
        max = c("100")
    )

    vars <- format_var(environment(), "vector1", 10, NULL, NULL, filters)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "vector1")
    expect_equal(parsed_var$type, "character")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(parsed_var$summary, "Length: 6")
    expect_equal(parsed_var$value$multi_value$column_count, 1)
    expect_equal(parsed_var$value$multi_value$row_count, 6)
    expect_equal(parsed_var$value$multi_value$column_names, c("vector1"))
    expect_equal(parsed_var$value$multi_value$row_names, c("1", "2", "3", "4", "5", "6"))
    expect_equal(parsed_var$value$multi_value$data, c("cc", "Bird", "abiRda", "Z", "100bird", "11"))
})


test_that("format_var error", {
    expect_error(format_var(environment(), "fakevar", NULL), "object 'fakevar' not found")
})

test_that("create_exception_var returns formatted error", {
    err_resp <- tryCatch({
        stop("something exploded")
    }, error=function(e) {
        return(e)
    })
    error_var = create_exception_var(err_resp)
    expect_equal(error_var$name, "Introspection Error")
    expect_equal(error_var$type, "simpleError, error, condition")
    expect_equal(error_var$abbreviated, FALSE)
    expect_equal(error_var$summary, "Error in doTryCatch(return(expr), name, parentenv, handler): something exploded\n")
    expect_equal(error_var$value$multi_value$column_count, 1)
    expect_equal(error_var$value$multi_value$row_count > 1, TRUE)
    expect_equal(error_var$value$multi_value$column_names, list("Traceback"))
    expect_equal(length(error_var$value$multi_value$row_names) > 1, TRUE)
    expect_equal(length(error_var$value$multi_value$data) > 1, TRUE)
})
