
TOP_VALUES_LEN <- 3

get_stat_summary <- function(obj, name, column) {
    obj_type <- typeof(obj)
    obj_class <- class(obj)
    obj_length <- length(obj)

    intrinsic_types = c(
        "character", "numeric", "integer", "logical", "complex", "raw", "Date", "factor"
    )

    stats <- list()
    if (length(obj_class) == 1 && obj_class %in% intrinsic_types) {
        stats[[name]] <- get_stats_column(obj)
    } else if (is.data.frame(obj)) {
        if (is.null(column)) {
            for(i in 1:ncol(obj)) {
                col <- obj[,i]
                col_name <- colnames(obj)[i]
                stats[[col_name]] <- get_stats_column(col)
            }
        } else if (column %in% colnames(obj)) {
            col <- obj[,column]
            stats[[column]] <- get_stats_column(col)
        }
        
    } else if (length(obj_class) == 1 && is.list(obj)) {
        stats[[name]] <- get_stats_column(obj)
    } else if (is.matrix(obj) && length(dim(obj)) == 2) {
        if (is.null(column)) {
            for (i in 1:ncol(obj)) {
                col <- obj[,i]
                col_name <- as.character(i)
                stats[[col_name]] <- get_stats_column(col)
            }
        } else if (is.numeric(column) && column >= 0 && column < ncol(obj)) {
            col <- obj[,as.integer(column) + 1]

            stats[[as.character(column + 1)]] <- get_stats_column(col)
        }
    }

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
    } else if (inherits(col, c("Date"))) {
        min <- as.character(min(col, na.rm=TRUE))
        max <- as.character(max(col, na.rm=TRUE))
        n_unique <- length(unique(na.omit(col)))
        col_stats <- list(min=min, max=max, unique_count=n_unique, type="date")
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


#' Get string containing json statistical summary for each column in a variable
#'
#' @param envir An environment 
#' @param name Name of a variable in the environment
#' @param column Name of the column to return (returns all columns if not specified)
#'
#' @return A string with a json statistical summary for each column
#' @export
#'
#' @examples
#' x <- c(5,6)
#' stats <- get_var_stats(environment(), "x")
get_var_stats <- function(envir, name, column=NULL) {
    stats <- NULL
    obj <- get(name, envir=envir)
    err_resp <- tryCatch({
        stats <- get_stat_summary(obj, name, column)
    }, error=function(e) {
        return(create_exception_stats(e))
    })
    return(rjson::toJSON(`if`(is.null(err_resp), stats, err_resp)))
}