test_that("stat summary with data.frame", {
    dataframe1 <- read.csv(
        "vegetables.csv",
        colClasses=c(
            "character",
            "numeric",
            "numeric",
            "Date",
            "Date",
            "numeric",
            "numeric",
            "factor"
        )
    )

    stats <- get_var_stats(environment(), "dataframe1")
    parsed_stats = rjson::fromJSON(stats)

    expect_equal(
        parsed_stats$Name,
        list(type="misc", unique_count=9, na_count=0)
    )
    expect_equal(
        parsed_stats$Seed_spacing,
        list(min=2, max=12, mean=4.875, type="numeric", na_count=1)
    )
    expect_equal(
        parsed_stats$Pot_size,
        list(min=4, max=10, mean=6.375, type="numeric", na_count=1)
    )
    expect_equal(
        parsed_stats$Plant_date,
        list(min="2023-03-02", max="2023-05-20", unique_count=7, type="date", na_count=0)
    )
    expect_equal(
        parsed_stats$Harvest_date,
        list(min="2023-06-06", max="2023-10-05", unique_count=9, type="date", na_count=0)
    )
    expect_true(all.equal(
        parsed_stats$Germination,
        list(min=3, max=11, mean=7.888889, type="numeric", na_count=0),
        tolerance=0.000001
    ))
    expect_true(all.equal(
        parsed_stats$Maturation,
        list(min=50, max=120, mean=80.77778, type="numeric", na_count=0),
        tolerance=0.000001
    ))
    expect_equal(
        parsed_stats$Type,
        list(
            type="category",
            top_values=list(Veggie=4, fruit=3),
            unique_count=2,
            na_count=0
        )
    )
    
})


test_that("stat summary list numeric", {
    list1 <- list(4, 3, 5.5, NA, 1, -2, 35.2)

    stats <- get_var_stats(environment(), "list1")
    parsed_stats = rjson::fromJSON(stats)

    expect_equal(
        parsed_stats$list1,
        list(type="misc", unique_count=7, na_count=1)
    )
})

test_that("stat summary vector strings", {
    list1 <- c("Jon", "Bill", NA, "Maria", "Ben", "Tina")

    stats <- get_var_stats(environment(), "list1")
    parsed_stats = rjson::fromJSON(stats)

    expect_equal(
        parsed_stats$list1,
        list(type="misc", unique_count=5, na_count=1)
    )
})

test_that("stat summary vector complex", {
    list1 <- c(5+3i, 1+4i, NA, 3+3i, 2+7i, 1+3i)

    stats <- get_var_stats(environment(), "list1")
    parsed_stats = rjson::fromJSON(stats)

    expect_equal(
        parsed_stats$list1,
        list(type="misc", unique_count=5, na_count=1)
    )
})

test_that("stat summary vector logical", {
    list1 <- c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)

    stats <- get_var_stats(environment(), "list1")
    parsed_stats = rjson::fromJSON(stats)

    expect_equal(
        parsed_stats$list1,
        list(type="misc", unique_count=2, na_count=0)
    )
})

test_that("stat summary vector dates", {
    list1 <- c(
        as.Date("2021-01-10"),
        as.Date("2021-05-23"),
        as.Date("2020-01-30"),
        NA,
        as.Date("2020-03-14"),
        as.Date("2023-01-24")
    )

    stats <- get_var_stats(environment(), "list1")
    parsed_stats = rjson::fromJSON(stats)

    expect_equal(
        parsed_stats$list1,
        list(min="2020-01-30", max="2023-01-24", unique_count=5, type="date", na_count=1)
    )
})

test_that("stat summary vector factors", {
    list1 <- factor(c(
        as.Date("2021-01-10"),
        as.Date("2021-05-23"),
        as.Date("2020-01-10"),
        NA,
        as.Date("2020-03-14"),
        as.Date("2023-01-24"),
        as.Date("2023-03-14")
    ))

    stats <- get_var_stats(environment(), "list1")
    parsed_stats = rjson::fromJSON(stats)

    expect_equal(
        parsed_stats$list1,
        list(
            type="category",
            top_values=list('2020-01-10'=1, '2020-03-14'=1),
            unique_count=2,
            na_count=1
        )
    )
})


test_that("stat summary matrix", {

    mx1 <- matrix(
        c(11, -34, 1, 5, NA, 3),
        nrow = 2,
        ncol = 3,
        byrow = 1
    )

    stats <- get_var_stats(environment(), "mx1")
    parsed_stats = rjson::fromJSON(stats)

    expect_equal(
        parsed_stats$`1`,
        list(min=5, max=11, mean=8, type="numeric", na_count=0)
    )
    expect_equal(
        parsed_stats$`2`,
        list(min=-34, max=-34, mean=-34, type="numeric", na_count=1)
    )
    expect_equal(
        parsed_stats$`3`,
        list(min=1, max=3, mean=2, type="numeric", na_count=0)
    )
    
})