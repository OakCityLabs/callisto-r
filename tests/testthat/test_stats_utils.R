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
        list(min="2023-03-02", max="2023-05-20", type="Date", na_count=0)
    )
    expect_equal(
        parsed_stats$Harvest_date,
        list(min="2023-06-06", max="2023-10-05", type="Date", na_count=0)
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
