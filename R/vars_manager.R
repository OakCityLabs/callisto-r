KB <- 1024
MB <- KB ** 2  # 1,048,576
GB <- KB ** 3  # 1,073,741,824
TB <- KB ** 4  # 1,099,511,627,776

DEFAULT_ABBREV_LEN <- 50

human_bytes <- function(B) {
    # Return the given bytes as a human friendly KB, MB, GB, or TB string
    if (B < KB) {
        return(sprintf("%.0f %s", B, `if`(B > 1, "Bytes", "Byte")))
    } else if (KB <= B & B < MB) {
        return(sprintf("%.2f KB", (B / KB)))
    } else if (MB <= B & B < GB) {
        return(sprintf("%.2f MB", (B / MB)))
    } else if (GB <= B & B < TB) {
        return(sprintf("%.2f GB", (B / GB)))
    } else if (TB <= B) {
        return(sprintf("%.2f TB", (B / TB)))
    }
}

make_multi_dict <- function(
    row_names, column_names, data, total_row_count=NULL, total_column_count=NULL
) {
    # column_count and row_count should be values for the whole data structure
    # column_names and row_names are values for the possibly abbreviated structure
    if (typeof(column_names) == "character") {
        column_names <- list(column_names)
    }
    if (typeof(row_names) == "character") {
        row_names <- list(row_names)
    }
    value <- list(
        column_count=`if`(!is.null(total_column_count), total_column_count, length(column_names)),
        row_count=`if`(!is.null(total_row_count), total_row_count, length(row_names)),
        column_names=column_names,
        row_names=row_names,
        data=data
    )
    return(list(multi_value=value))
}

get_single_vector_var <- function(obj, name) {
    return(list(
        summary=as.character(obj),
        abbrev=FALSE,
        value=list(single_value=as.character(obj))
    ))
}

get_vector_var <- function(obj, name, abbrev_len=DEFAULT_ABBREV_LEN, sort_by=NULL, ascending=NULL) {
    # sort_by should be a single string, "value"
    # ascending should be a single boolean
    summary <- sprintf("Length: %d", length(obj))
    abbrev <- !is.null(abbrev_len) && length(obj) > abbrev_len

    if (is.character(sort_by) && tolower(sort_by) == "value") {
        decreasing <- !`if`(is.logical(ascending), ascending, TRUE)
        obj_sorted <- sort(obj, decreasing=decreasing, na.last=TRUE)
        obj_pre <- `if`(abbrev, obj_sorted[1:abbrev_len], obj_sorted)
    } else {
        obj_pre <- `if`(abbrev, obj[1:abbrev_len], obj)
    }

    data <- as.character(obj_pre)
    row_names <- list()
    i <- 1
    for (row_name in `if`(is.null(names(obj)), 1:length(obj_pre), names(obj_pre))) {
        row_names[i] = as.character(row_name)
        i <- i + 1
    }
    return(list(
        summary=summary,
        abbrev=abbrev,
        value=make_multi_dict(row_names, name, data, total_row_count=length(obj))
    ))
}

get_matrix_var <- function(obj, name, abbrev_len=DEFAULT_ABBREV_LEN, sort_by=NULL, ascending=NULL) {
    # sort_by should be a vector of column indexes
    # ascending should be a vector of booleans, or a single boolean
    dims <- dim(obj)
    summary <- sprintf("Size: %dx%d Memory: %s", dims[[1]], dims[[2]], human_bytes(utils::object.size(obj)))
    abbrev <- !is.null(abbrev_len) && length(obj) > abbrev_len

    if (is.vector(sort_by) && length(sort_by) > 0) {
        cols <- obj[,sort_by]
        order_params <- as.list(as.data.frame(cols))

        if (is.vector(ascending) && length(ascending) == length(sort_by)) {
            i <- 0
            for (col in order_params) {
                i<-i+1
                order_params[[i]] <- `if`(ascending[[i]], order_params[[i]], order_params[[i]]*-1)
            }
        } else if (is.logical(ascending) && ascending == FALSE) {
            i <- 0
            for (col in order_params) {
                i <- i+1
                order_params[[i]] <- order_params[[i]]*-1
            }
        }

        obj_sorted <- obj[do.call(order, order_params),]
        obj_pre <- `if`(abbrev, obj_sorted[1:abbrev_len,], obj_sorted)
    } else {
        obj_pre <- `if`(abbrev, obj[1:abbrev_len,], obj)
    }

    data <- list()
    for (i in 1:nrow(obj_pre)) {
        data[[i]] = as.list(as.character(obj_pre[i,]))
    }
    col_names <- list()
    i <- 1
    for (col_name in 1:ncol(obj_pre)) {
        col_names[i] <- as.character(col_name)
        i <- i + 1
    }
    row_names <- list()
    i <- 1
    for (row_name in 1:nrow(obj_pre)) {
        row_names[i] <- as.character(row_name)
        i <- i + 1
    }
    return(list(
        summary=summary,
        abbrev=abbrev,
        value=make_multi_dict(row_names, col_names, data, total_row_count=dims[[1]], total_column_count=dims[[2]])
    ))
}

get_dataframe_var <- function(obj, name, abbrev_len=DEFAULT_ABBREV_LEN, sort_by=NULL, ascending=NULL) {
    # sort_by should be a vector of column names, or a single string: "index"
    # ascending should be a vector of booleans, or a single boolean
    dims <- dim(obj)
    summary <- sprintf("Size: %dx%d Memory: %s", dims[[1]], dims[[2]], human_bytes(utils::object.size(obj)))
    abbrev <- !is.null(abbrev_len) && nrow(obj) > abbrev_len

    if (is.character(sort_by) && tolower(sort_by) == "index") {
        descending <- `if`(is.logical(ascending), !ascending, FALSE)
        obj_sorted <- obj[order(attr(obj, "row.names"), decreasing=descending),]
        obj_pre <- `if`(abbrev, obj_sorted[1:abbrev_len,], obj_sorted)
    } else if (is.vector(sort_by) && length(sort_by) > 0) {
        order_params <- obj[match(sort_by, names(obj))]

        if (is.vector(ascending) && length(ascending) == length(sort_by)) {
            i <- 0
            for (col in order_params) {
                i<-i+1
                order_params[i] <- `if`(ascending[i], order_params[i], order_params[i]*-1)
            }
        } else if (is.logical(ascending) && ascending == FALSE) {
            order_params <- order_params * -1
        }

        obj_sorted <- obj[do.call(order, order_params),]
        obj_pre <- `if`(abbrev, obj_sorted[1:abbrev_len,], obj_sorted)
    } else {
        obj_pre <- `if`(abbrev, obj[1:abbrev_len,], obj)
    }

    data <- list()
    if (length(obj) > 0) {
        for (i in 1:nrow(obj_pre)) {
            # If we use as.character on the whole row at once factors are represented as 
            # numbers and not their text value, so we as.character each cell
            data_row = list()
            for (j in 1:ncol(obj_pre)) {
                data_row[[j]] = as.character(obj_pre[i,j])
            }
            data[[i]] = data_row
        }
    }
    col_names <- list()
    if (ncol(obj_pre) > 0) {
        i <- 1
        for (col_name in `if`(length(names(obj_pre)) > 1, names(obj_pre), 1:ncol(obj_pre))) {
            col_names[i] <- sprintf("%s (%s)", as.character(col_name), toString(class(obj_pre[,i])))
            i <- i + 1
        }
    }
    row_names <- list()
    if (nrow(obj_pre) > 0) {
        i <- 1
        for (row_name in `if`(length(rownames(obj_pre)) > 1, rownames(obj_pre), 1:nrow(obj_pre))) {
            row_names[i] <- as.character(row_name)
            i <- i + 1
        }
    }
    return(list(
        summary=summary,
        abbrev=abbrev,
        value=make_multi_dict(row_names, col_names, data, total_row_count=dims[[1]], total_column_count=dims[[2]])
    ))
}

get_var_details <- function(obj, name, abbrev_len=DEFAULT_ABBREV_LEN, sort_by=NULL, ascending=NULL) {
    # Get formatted info for a specific var. Pass abbrev_len=NULL
    # to get non-abbreviated variable info
    obj_type <- typeof(obj)
    obj_class <- class(obj)
    obj_length <- length(obj)

    intrinsic_types = c("character", "numeric", "integer", "logical", "complex", "raw")

    # vectors
    if (length(obj_class) == 1 && obj_class %in% intrinsic_types) {
        if (length(obj) == 1) {
            var_info <- get_single_vector_var(obj, name)
        } else {
            var_info <- get_vector_var(obj, name, abbrev_len, sort_by, ascending)
        }
    } else if (is.data.frame(obj)) {
        var_info <- get_dataframe_var(obj, name, abbrev_len, sort_by, ascending)
    } else if (is.list(obj)) {
        var_info <- get_vector_var(obj, name, abbrev_len, sort_by, ascending)
    } else if (is.matrix(obj) && length(dim(obj)) == 2) {
        var_info <- get_matrix_var(obj, name, abbrev_len, sort_by, ascending)
    } else {
        var_info = list(
            abbrev=FALSE,
            summary=noquote(toString(obj)),
            value=list(single_value=noquote(toString(obj)))
        )
    }

    return(list(
        name=name,
        type=toString(obj_class),
        abbreviated=var_info[["abbrev"]],
        summary=substr(var_info[["summary"]], 1, 140),  # limit summary length
        value=var_info[["value"]]
    ))
}

create_exception_var <- function(e) {
    stack <- sys.calls()
    row_names <- list()
    i <- 1
    for (row_name in `if`(is.null(names(stack)), 1:length(stack), names(stack))) {
        row_names[i] = as.character(row_name)
        i <- i + 1
    }
    return(
        list(
            name="Introspection Error",
            type=toString(class(e)),
            summary=toString(e),
            abbreviated=FALSE,
            value=list(
                multi_value=list(
                    column_count=1,
                    row_count=length(stack),
                    column_names=list("Traceback"),
                    row_names=row_names,
                    data=as.list(as.character(stack))
                )
            )
        )
    )
}

#' Get string containing json representation of defined vars in the provided environment
#'
#' @param envir An environment 
#' @param abbrev_len The length of elements at which vars should be abbreviated. Pass NULL to prevent abbreviation.
#'
#' @return A string with a json list of vars.
#' @export
#'
#' @examples
#' vars_json <- format_vars(environment())
format_vars <- function(envir, abbrev_len=DEFAULT_ABBREV_LEN) {
    # Get string containing json representation of currently defined vars
    # (pass the output of `environment()` to this function)
    current_vars <- list()

    err_resp <- tryCatch({
        idx <- 1
        for (name in ls(envir)) {
            obj <- get(name, envir=envir)
            if (startsWith(name, "__")) {
                next
            }
            obj_type <- typeof(obj)
            obj_class <- class(obj)
            if (grepl("closure", toString(obj_type)) || grepl("closure", toString(obj_class)) ) {
                next
            }
            var_details <- get_var_details(obj, name, abbrev_len)

            current_vars[[idx]] <- var_details
            idx <- idx + 1
        }
    }, error=function(e) {
        return(list(create_exception_var(e)))
    })
    return(rjson::toJSON(`if`(is.null(err_resp), current_vars, err_resp)))
}

#' Get string containing json representation of a single var in the provided environment
#'
#' @param envir An environment 
#' @param name Name of a variable in the environment
#' @param abbrev_len The length of elements at which vars should be abbreviated. Pass NULL to prevent abbreviation.
#' @param sort_by A vector containing columns to sort (primary first)
#' @param ascending A vector of booleans or a single boolean, depending on the data type
#'
#' @return A string with a json representation of a var.
#' @export
#'
#' @examples
#' x <- c(5,6)
#' vars_json <- format_var(environment(), "x", NULL)
format_var <- function(envir, name, abbrev_len=DEFAULT_ABBREV_LEN, sort_by=NULL, ascending=NULL) {
    var_details <- NULL
    obj <- get(name, envir=envir)
    err_resp <- tryCatch({
        var_details <- get_var_details(obj, name, abbrev_len, sort_by, ascending)
    }, error=function(e) {
        return(create_exception_var(e))
    })
    return(rjson::toJSON(`if`(is.null(err_resp), var_details, err_resp)))
}
