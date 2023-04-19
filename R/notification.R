
D1_COMMAND_DELIMITER <- "___callisto_d1_command___"

create_notification_payload <- function(title, message) {
    command <- list(
        command_type="notify",
        title=title,
        message=message
    )
    command_str = rjson::toJSON(command)
    return(sprintf("%s%s%s", D1_COMMAND_DELIMITER, command_str, D1_COMMAND_DELIMITER))
}


#' Send a push notification to the Callisto app running on Mac or iPad
#'
#' @param title A title for the notification 
#' @param message A mesage for the notification
#'
#' @export
#'
#' @examples
#' send_notification("Alert", "Your code has finished running")
send_notification <- function(title, message) {
    payload = create_notification_payload(title, message)
    cat(payload)
}
