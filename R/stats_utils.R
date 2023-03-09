
TOP_VALUES_LEN <- 3

get_stat_summary <- function(obj, name) {
    obj_type <- typeof(obj)
    obj_class <- class(obj)
    obj_length <- length(obj)

    intrinsic_types = c("character", "numeric", "integer", "logical", "complex", "raw")

    if (is.data.frame(obj)) {
        stats <- get_stats_dataframe(obj, name)
    }
    # } else if (is.list(obj)) {
    #     stats <- 
    # } else if (is.matrix(obj) && length(dim(obj)) == 2) {
    #     stats <-
    # }

    return(stats)
}


get_stats_column <- function(col) {
    col_stats <- list()

    if (is.numeric(col)) {
        min <- min(col, na.rm=TRUE)
        max <- max(col, na.rm=TRUE)
        mean <- mean(col, na.rm=TRUE)
        col_stats <- list(min=min, max=max, mean=mean, type="numeric")
    } else if (is.factor(col)) {
        # Get frequency of each factor in a data frame
        top_values_table <- as.data.frame(table(col))
        # Sort by the frequency, and convert the factors to characters
        top_values_table <- top_values_table[order(top_values_table[,2], decreasing=TRUE),]
        top_values_table[,1] <- as.character(top_values_table[,1])

        # Only return <= TOP_VALUES_LEN results
        unique_count <- length(top_values_table)
        len_top_values <- min(unique_count, TOP_VALUES_LEN)
        top_values_list <- list()
        for (i in 1:len_top_values) {
            top_values_list[top_values_table[i,1]] <- top_values_table[i,2]
        }

        col_stats <- list(
            type="category",
            top_values=top_values_list,
            unique_count=unique_count
        )
    } else if (inherits(col, c("Date", "POSIXt"))) {
        min <- as.character(min(col, na.rm=TRUE))
        max <- as.character(max(col, na.rm=TRUE))
        col_stats <- list(min=min, max=max, type="Date")
    } else {
        n_unique <- length(unique(na.omit(col)))
        col_stats <- list(
            type="misc",
            unique_count=n_unique
        )
    }
    na_count <- sum(is.na(col))
    col_stats["na_count"] <- na_count

    return(col_stats)
}


get_stats_dataframe <- function(obj, name) {
    stats <- list()
    for(i in 1:ncol(obj)) {
        col <- obj[,i]
        col_name <- colnames(obj)[i]
        stats[[col_name]] <- get_stats_column(col)
    }

    return(stats)
}


create_exception_stats <- function(e) {
    stack <- sys.calls()
    row_names <- list()
    i <- 1
    for (row_name in `if`(is.null(names(stack)), 1:length(stack), names(stack))) {
        row_names[i] = as.character(row_name)
        i <- i + 1
    }
    return(
        list(
            name="Error retrieving statistical summary",
            type=toString(class(e)),
            summary=toString(e),
            error=as.list(as.character(stack))
        )
    )
}



get_var_stats <- function(envir, name) {
    stats <- NULL
    obj <- get(name, envir=envir)
    err_resp <- tryCatch({
        stats <- get_stat_summary(obj, name)
    }, error=function(e) {
        return(create_exception_stats(e))
    })
    return(rjson::toJSON(`if`(is.null(err_resp), stats, err_resp)))
}