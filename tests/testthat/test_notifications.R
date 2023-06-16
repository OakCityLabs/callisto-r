test_that("create a d1 notification payload", {
    payload <- create_notification_payload("welcome party", "Hy wake up3")

    expect_equal(
        payload,
        "___callisto_d1_command___{\"command_type\":\"notify\",\"title\":\"welcome party\",\"message\":\"Hy wake up3\"}___callisto_d1_command___"
    )
})

test_that("send a d1 notification payload", {
    expect_output(
        send_notification("welcome party", "Hy wake up3"),
        "___callisto_d1_command___\\{\"command_type\":\"notify\",\"title\":\"welcome party\",\"message\":\"Hy wake up3\"\\}___callisto_d1_command___"
    )
})