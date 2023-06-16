
D1_COMMAND_DELIMITER <- "___callisto_d1_command___"

URL_DATA_EXPLORER <- "com.callistoapp.callisto://ui/data-explorer"
URL_SETTINGS <- "com.callistoapp.callisto://ui/settings"
URL_CLOUD_BROWSER <- "com.callistoapp.callisto://ui/cloud"
URL_PACKAGE_MANAGER <- "com.callistoapp.callisto://ui/xip"
URL_NOTEBOOK_CHECK <- "com.callistoapp.callisto://ui/notebook-check"
URL_CLIENT_LOG <- "com.callistoapp.callisto://ui/client-log"
URL_D1_LOG <- "com.callistoapp.callisto://ui/d1-log"

allowed_settings_pages <- c(
    "editor", "notebook", "r", "account", "advanced"
)

send_url_command <- function(url) {
    command <- list()
    command[["command_type"]] <- "client_command_url"
    command[["url"]] <- url
    msg <- paste(D1_COMMAND_DELIMITER, rjson::toJSON(command), D1_COMMAND_DELIMITER, sep="")
    write(msg, stdout())
}

open_data_explorer <- function() {
    send_url_command(URL_DATA_EXPLORER)
}

open_settings <- function(page=NULL) {
    if (is.null(page)) {
        send_url_command(URL_SETTINGS)
    } else if (is.character(page) && tolower(page) %in% allowed_settings_pages) {
        command <- paste(URL_SETTINGS, "/", page, sep="")
        send_url_command(command)
    } else {
        stop(paste("'", page, "'", " is not a valid page", sep=""))
    }
}

open_cloud_browser <- function() {
    send_url_command(URL_CLOUD_BROWSER)
}

open_package_manager <- function() {
    send_url_command(URL_PACKAGE_MANAGER)
}

open_notebook_checker <- function() {
    send_url_command(URL_NOTEBOOK_CHECK)
}

open_client_log <- function() {
    send_url_command(URL_CLIENT_LOG)
}

open_d1_log <- function() {
    send_url_command(URL_D1_LOG)
}