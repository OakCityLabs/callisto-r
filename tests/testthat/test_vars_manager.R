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
    vars <- format_vars(environment())
    parsed_vars = rjson::fromJSON(vars)
    expect_equal(length(parsed_vars), 1)
    expect_equal(parsed_vars[[1]]$name, "matrix1")
    expect_equal(grepl("matrix", parsed_vars[[1]]$type), TRUE)
    expect_equal(parsed_vars[[1]]$abbreviated, TRUE)
    expect_equal(startsWith(parsed_vars[[1]]$summary, "Size: 80x7 Memory: "), TRUE)
    expect_equal(parsed_vars[[1]]$value$multi_value$column_count, 7)
    expect_equal(parsed_vars[[1]]$value$multi_value$row_count, 80)
    expect_equal(parsed_vars[[1]]$value$multi_value$column_names, as.character(1:7))
    expect_equal(parsed_vars[[1]]$value$multi_value$row_names, as.character(1:8))
    mdata <- c()
    for (i in 1:8) {
        mdata[[i]] = as.character(i * 4 + 1:7)
    }
    expect_equal(parsed_vars[[1]]$value$multi_value$data, mdata)
})

test_that("format_vars data.frame abbreviated", {
    dataframe1 <- read.csv("iris.csv", colClasses=c("numeric", "numeric", "numeric", "numeric", "factor"))
    vars <- format_vars(environment(), 33)
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

test_that("format_var error", {
    vars <- format_var(environment(), "fakevar", NULL)
    parsed_var = rjson::fromJSON(vars)
    expect_equal(parsed_var$name, "Introspection Error")
    expect_equal(parsed_var$type, "simpleError, error, condition")
    expect_equal(parsed_var$abbreviated, FALSE)
    expect_equal(parsed_var$summary, "Error in get(name, envir = envir): object 'fakevar' not found\n")
    expect_equal(parsed_var$value$multi_value$column_count, 1)
    expect_equal(parsed_var$value$multi_value$row_count > 1, TRUE)
    expect_equal(parsed_var$value$multi_value$column_names, c("Traceback"))
    expect_equal(length(parsed_var$value$multi_value$row_names) > 1, TRUE)
    expect_equal(length(parsed_var$value$multi_value$data) > 1, TRUE)
})
