test_that("data explorer command", {
    expected_value <- '___callisto_d1_command___
      {
        "command_type":"client_command_url",
        "url":"com.callistoapp.callisto://ui/data-explorer"
      }
      ___callisto_d1_command___'

    expect_output(
      open_data_explorer(),
      cat(expected_value),
      fixed=TRUE
    )
})

test_that("settings command", {
    expected_value <- '___callisto_d1_command___
      {
        "command_type":"client_command_url",
        "url":"com.callistoapp.callisto://ui/settings"
      }
      ___callisto_d1_command___'

    expect_output(
      open_settings(),
      cat(expected_value),
      fixed=TRUE
    )
})

test_that("settings command with specific page 'advanced'", {
    expected_value <- '___callisto_d1_command___
      {
        "command_type":"client_command_url",
        "url":"com.callistoapp.callisto://ui/advanced"
      }
      ___callisto_d1_command___'

    expect_output(
      open_settings("advanced"),
      cat(expected_value),
      fixed=TRUE
    )
})

test_that("settings command with specific page 'r'", {
    expected_value <- '___callisto_d1_command___
      {
        "command_type":"client_command_url",
        "url":"com.callistoapp.callisto://ui/r"
      }
      ___callisto_d1_command___'

    expect_output(
      open_settings("R"),
      cat(expected_value),
      fixed=TRUE
    )
})

test_that("settings command with invalid page", {
    expect_error(
        open_settings("bad"),
        "'bad' is not a valid page"
    )
    expect_error(
        open_settings("advance"),
        "'advance' is not a valid page"
    )
})

test_that("cloud browser command", {
    expected_value <- '___callisto_d1_command___
      {
        "command_type":"client_command_url",
        "url":"com.callistoapp.callisto://ui/cloud"
      }
      ___callisto_d1_command___'

    expect_output(
      open_cloud_browser(),
      cat(expected_value),
      fixed=TRUE
    )
})

test_that("package manager command", {
    expected_value <- '___callisto_d1_command___
      {
        "command_type":"client_command_url",
        "url":"com.callistoapp.callisto://ui/xip"
      }
      ___callisto_d1_command___'

    expect_output(
      open_package_manager(),
      cat(expected_value),
      fixed=TRUE
    )
})

test_that("notebook checker command", {
    expected_value <- '___callisto_d1_command___
      {
        "command_type":"client_command_url",
        "url":"com.callistoapp.callisto://ui/notebook-check"
      }
      ___callisto_d1_command___'

    expect_output(
      open_notebook_checker(),
      cat(expected_value),
      fixed=TRUE
    )
})

test_that("client log command", {
    expected_value <- '___callisto_d1_command___
      {
        "command_type":"client_command_url",
        "url":"com.callistoapp.callisto://debug/client-log"
      }
      ___callisto_d1_command___'

    expect_output(
      open_client_log(),
      cat(expected_value),
      fixed=TRUE
    )
})

test_that("d1 log command", {
    expected_value <- '___callisto_d1_command___
      {
        "command_type":"client_command_url",
        "url":"com.callistoapp.callisto://debug/d1-log"
      }
      ___callisto_d1_command___'

    expect_output(
      open_d1_log(),
      cat(expected_value),
      fixed=TRUE
    )
})