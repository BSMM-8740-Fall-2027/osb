# created AUG 04, 2025

# R Code Grading MCP Server
# Conceptual implementation for educational code assessment

library(jsonlite)
library(testthat)
library(lintr)
library(styler)
library(rmarkdown)
library(knitr)
library(evaluate)

# Main MCP Server Class for R Code Grading
RGradingMCPServer <- R6::R6Class("RGradingMCPServer",
                                 public = list(
                                   # Initialize the server with grading configuration
                                   initialize = function(config_path = NULL) {
                                     private$load_config(config_path)
                                     private$setup_sandbox()
                                     message("R Grading MCP Server initialized")
                                   },

                                   # Main grading workflow
                                   grade_assignment = function(student_code, assignment_config) {
                                     results <- list(
                                       student_id = assignment_config$student_id,
                                       assignment_id = assignment_config$assignment_id,
                                       timestamp = Sys.time(),
                                       scores = list(),
                                       feedback = list(),
                                       total_score = 0
                                     )

                                     tryCatch({
                                       # 1. Syntax and Parse Check
                                       syntax_result <- self$check_syntax(student_code)
                                       results$scores$syntax <- syntax_result$score
                                       results$feedback$syntax <- syntax_result$feedback

                                       # 2. Code Style Assessment
                                       style_result <- self$assess_code_style(student_code)
                                       results$scores$style <- style_result$score
                                       results$feedback$style <- style_result$feedback

                                       # 3. Functional Testing
                                       if (syntax_result$valid) {
                                         test_result <- self$run_functional_tests(student_code, assignment_config$tests)
                                         results$scores$functionality <- test_result$score
                                         results$feedback$functionality <- test_result$feedback

                                         # 4. Statistical Analysis Validation
                                         stats_result <- self$validate_statistical_analysis(student_code, assignment_config$expected_results)
                                         results$scores$statistical_accuracy <- stats_result$score
                                         results$feedback$statistical_accuracy <- stats_result$feedback

                                         # 5. Data Visualization Assessment
                                         if ("visualization" %in% assignment_config$requirements) {
                                           viz_result <- self$assess_visualizations(student_code, assignment_config$viz_requirements)
                                           results$scores$visualization <- viz_result$score
                                           results$feedback$visualization <- viz_result$feedback
                                         }

                                         # 6. R Markdown Quality (if applicable)
                                         if (assignment_config$format == "rmarkdown") {
                                           rmd_result <- self$assess_rmarkdown_quality(student_code)
                                           results$scores$documentation <- rmd_result$score
                                           results$feedback$documentation <- rmd_result$feedback
                                         }
                                       }

                                       # Calculate total score
                                       results$total_score <- private$calculate_weighted_score(results$scores, assignment_config$weights)

                                     }, error = function(e) {
                                       results$error <- paste("Grading error:", e$message)
                                     })

                                     return(results)
                                   },

                                   # Check R syntax and parseability
                                   check_syntax = function(code) {
                                     result <- list(valid = FALSE, score = 0, feedback = c())

                                     tryCatch({
                                       # Try to parse the R code
                                       parsed <- parse(text = code, keep.source = TRUE)
                                       result$valid <- TRUE
                                       result$score <- 10  # Full points for valid syntax
                                       result$feedback <- c("✓ Code parses successfully")

                                       # Check for common syntax issues
                                       if (length(grep("\\t", code)) > 0) {
                                         result$feedback <- c(result$feedback, "⚠ Consider using spaces instead of tabs for indentation")
                                       }

                                       if (length(grep(";\\s*$", code)) > length(grep(";\\s*\\n", code))) {
                                         result$feedback <- c(result$feedback, "ℹ Unnecessary semicolons detected")
                                       }

                                     }, error = function(e) {
                                       result$feedback <- c(paste("✗ Syntax error:", e$message))
                                       result$score <- 0
                                     })

                                     return(result)
                                   },

                                   # Assess code style using lintr
                                   assess_code_style = function(code) {
                                     result <- list(score = 0, feedback = c())

                                     tryCatch({
                                       # Create temporary file for lintr analysis
                                       temp_file <- tempfile(fileext = ".R")
                                       writeLines(code, temp_file)

                                       # Run lintr checks
                                       lint_results <- lintr::lint(temp_file)

                                       # Style scoring based on linting results
                                       num_issues <- length(lint_results)
                                       max_issues <- 20  # Threshold for zero points

                                       if (num_issues == 0) {
                                         result$score <- 10
                                         result$feedback <- c("✓ Excellent code style - no linting issues")
                                       } else {
                                         result$score <- max(0, 10 - (num_issues * 10 / max_issues))
                                         result$feedback <- c(
                                           paste("⚠", num_issues, "style issues found:"),
                                           head(as.character(lint_results), 10)  # Show first 10 issues
                                         )
                                       }

                                       unlink(temp_file)

                                     }, error = function(e) {
                                       result$feedback <- c(paste("Style check error:", e$message))
                                       result$score <- 5  # Partial credit if style check fails
                                     })

                                     return(result)
                                   },

                                   # Run functional tests on student code
                                   run_functional_tests = function(code, test_specs) {
                                     result <- list(score = 0, feedback = c(), details = list())

                                     tryCatch({
                                       # Create isolated environment for student code
                                       student_env <- new.env()
                                       eval(parse(text = code), envir = student_env)

                                       total_tests <- length(test_specs)
                                       passed_tests <- 0

                                       for (i in seq_along(test_specs)) {
                                         test_spec <- test_specs[[i]]
                                         test_result <- private$run_single_test(student_env, test_spec)

                                         result$details[[paste0("test_", i)]] <- test_result

                                         if (test_result$passed) {
                                           passed_tests <- passed_tests + 1
                                           result$feedback <- c(result$feedback, paste("✓", test_spec$description))
                                         } else {
                                           result$feedback <- c(result$feedback,
                                                                paste("✗", test_spec$description, "-", test_result$message))
                                         }
                                       }

                                       result$score <- (passed_tests / total_tests) * 40  # 40 points max for functionality
                                       result$feedback <- c(
                                         result$feedback,
                                         paste("Passed", passed_tests, "out of", total_tests, "functional tests")
                                       )

                                     }, error = function(e) {
                                       result$feedback <- c(paste("Functional testing error:", e$message))
                                     })

                                     return(result)
                                   },

                                   # Validate statistical analysis results
                                   validate_statistical_analysis = function(code, expected_results) {
                                     result <- list(score = 0, feedback = c())

                                     tryCatch({
                                       # Execute code in controlled environment
                                       analysis_env <- new.env()
                                       eval(parse(text = code), envir = analysis_env)

                                       score_components <- c()

                                       # Check for required statistical objects
                                       for (expected_obj in names(expected_results)) {
                                         if (exists(expected_obj, envir = analysis_env)) {
                                           student_result <- get(expected_obj, envir = analysis_env)
                                           expected_value <- expected_results[[expected_obj]]

                                           # Compare results with tolerance for numerical precision
                                           if (private$compare_statistical_results(student_result, expected_value)) {
                                             score_components <- c(score_components, 1)
                                             result$feedback <- c(result$feedback, paste("✓ Correct", expected_obj))
                                           } else {
                                             score_components <- c(score_components, 0)
                                             result$feedback <- c(result$feedback, paste("✗ Incorrect", expected_obj))
                                           }
                                         } else {
                                           score_components <- c(score_components, 0)
                                           result$feedback <- c(result$feedback, paste("✗ Missing", expected_obj))
                                         }
                                       }

                                       result$score <- (sum(score_components) / length(score_components)) * 30  # 30 points max

                                     }, error = function(e) {
                                       result$feedback <- c(paste("Statistical validation error:", e$message))
                                     })

                                     return(result)
                                   },

                                   # Assess data visualizations
                                   assess_visualizations = function(code, viz_requirements) {
                                     result <- list(score = 0, feedback = c())

                                     tryCatch({
                                       # Execute code and capture plots
                                       viz_env <- new.env()

                                       # Redirect graphics to capture plots
                                       temp_plots <- tempfile()
                                       pdf(temp_plots)

                                       eval(parse(text = code), envir = viz_env)

                                       dev.off()

                                       # Analyze generated plots
                                       num_plots <- length(dev.list())

                                       score_factors <- c()

                                       # Check for required plot types
                                       if ("ggplot2" %in% viz_requirements$libraries) {
                                         if (any(grepl("ggplot|geom_", code))) {
                                           score_factors <- c(score_factors, 1)
                                           result$feedback <- c(result$feedback, "✓ Uses ggplot2 for visualization")
                                         } else {
                                           score_factors <- c(score_factors, 0)
                                           result$feedback <- c(result$feedback, "✗ Required ggplot2 usage not found")
                                         }
                                       }

                                       # Check for proper labeling
                                       if (any(grepl("(labs|xlab|ylab|title)", code))) {
                                         score_factors <- c(score_factors, 1)
                                         result$feedback <- c(result$feedback, "✓ Plot includes proper labels")
                                       } else {
                                         score_factors <- c(score_factors, 0.5)
                                         result$feedback <- c(result$feedback, "⚠ Consider adding more descriptive labels")
                                       }

                                       result$score <- mean(score_factors) * 20  # 20 points max for visualization

                                       unlink(temp_plots)

                                     }, error = function(e) {
                                       result$feedback <- c(paste("Visualization assessment error:", e$message))
                                     })

                                     return(result)
                                   },

                                   # Assess R Markdown document quality
                                   assess_rmarkdown_quality = function(code) {
                                     result <- list(score = 0, feedback = c())

                                     tryCatch({
                                       # Check R Markdown structure
                                       has_yaml <- grepl("^---", code[1])
                                       has_code_chunks <- any(grepl("```\\{r", code))
                                       has_markdown_text <- any(grepl("^[^`]", code))

                                       score_components <- c()

                                       if (has_yaml) {
                                         score_components <- c(score_components, 1)
                                         result$feedback <- c(result$feedback, "✓ Proper YAML header")
                                       } else {
                                         score_components <- c(score_components, 0)
                                         result$feedback <- c(result$feedback, "✗ Missing YAML header")
                                       }

                                       if (has_code_chunks) {
                                         score_components <- c(score_components, 1)
                                         result$feedback <- c(result$feedback, "✓ Contains R code chunks")
                                       } else {
                                         score_components <- c(score_components, 0)
                                         result$feedback <- c(result$feedback, "✗ No R code chunks found")
                                       }

                                       if (has_markdown_text) {
                                         score_components <- c(score_components, 1)
                                         result$feedback <- c(result$feedback, "✓ Includes markdown text/documentation")
                                       } else {
                                         score_components <- c(score_components, 0.5)
                                         result$feedback <- c(result$feedback, "⚠ Limited markdown documentation")
                                       }

                                       result$score <- mean(score_components) * 15  # 15 points max for R Markdown quality

                                     }, error = function(e) {
                                       result$feedback <- c(paste("R Markdown assessment error:", e$message))
                                     })

                                     return(result)
                                   }
                                 ),

                                 private = list(
                                   config = NULL,

                                   # Load grading configuration
                                   load_config = function(config_path) {
                                     if (!is.null(config_path) && file.exists(config_path)) {
                                       private$config <- jsonlite::fromJSON(config_path)
                                     } else {
                                       # Default configuration
                                       private$config <- list(
                                         timeout_seconds = 30,
                                         max_memory_mb = 512,
                                         allowed_packages = c("base", "stats", "graphics", "utils", "datasets",
                                                              "ggplot2", "dplyr", "tidyr", "readr", "knitr", "rmarkdown")
                                       )
                                     }
                                   },

                                   # Setup secure execution sandbox
                                   setup_sandbox = function() {
                                     # Configure resource limits and security
                                     # This would include setting up containerization or process limits
                                     message("Sandbox environment configured")
                                   },

                                   # Run individual test case
                                   run_single_test = function(env, test_spec) {
                                     result <- list(passed = FALSE, message = "")

                                     tryCatch({
                                       # Execute test in student environment
                                       if (test_spec$type == "function_exists") {
                                         result$passed <- exists(test_spec$function_name, envir = env)
                                         result$message <- ifelse(result$passed,
                                                                  "Function exists",
                                                                  "Function not found")

                                       } else if (test_spec$type == "function_output") {
                                         if (exists(test_spec$function_name, envir = env)) {
                                           func <- get(test_spec$function_name, envir = env)
                                           actual_output <- do.call(func, test_spec$inputs)
                                           result$passed <- private$compare_outputs(actual_output, test_spec$expected_output)
                                           result$message <- ifelse(result$passed,
                                                                    "Output matches expected",
                                                                    "Output differs from expected")
                                         } else {
                                           result$message <- "Function not found"
                                         }

                                       } else if (test_spec$type == "variable_value") {
                                         if (exists(test_spec$variable_name, envir = env)) {
                                           actual_value <- get(test_spec$variable_name, envir = env)
                                           result$passed <- private$compare_outputs(actual_value, test_spec$expected_value)
                                           result$message <- ifelse(result$passed,
                                                                    "Variable has correct value",
                                                                    "Variable value incorrect")
                                         } else {
                                           result$message <- "Variable not found"
                                         }
                                       }

                                     }, error = function(e) {
                                       result$message <- paste("Test execution error:", e$message)
                                     })

                                     return(result)
                                   },

                                   # Compare outputs with appropriate tolerance
                                   compare_outputs = function(actual, expected) {
                                     if (is.numeric(actual) && is.numeric(expected)) {
                                       return(all.equal(actual, expected, tolerance = 1e-6))
                                     } else {
                                       return(identical(actual, expected))
                                     }
                                   },

                                   # Compare statistical results with appropriate methods
                                   compare_statistical_results = function(actual, expected) {
                                     if (inherits(actual, "lm") && inherits(expected, "lm")) {
                                       # Compare linear model coefficients
                                       return(all.equal(coef(actual), coef(expected), tolerance = 1e-4))
                                     } else if (is.numeric(actual) && is.numeric(expected)) {
                                       return(all.equal(actual, expected, tolerance = 1e-4))
                                     } else {
                                       return(identical(actual, expected))
                                     }
                                   },

                                   # Calculate weighted total score
                                   calculate_weighted_score = function(scores, weights) {
                                     total <- 0
                                     for (component in names(scores)) {
                                       if (!is.null(weights[[component]])) {
                                         total <- total + (scores[[component]] * weights[[component]])
                                       }
                                     }
                                     return(round(total, 2))
                                   }
                                 )
)

# Example usage function
demo_r_grading <- function() {
  # Initialize the grading server
  grader <- RGradingMCPServer$new()

  # Example student code
  student_code <- '
  # Load required libraries
  library(ggplot2)

  # Create a simple linear regression function
  my_regression <- function(x, y) {
    model <- lm(y ~ x)
    return(model)
  }

  # Generate sample data
  set.seed(123)
  x_data <- rnorm(100, mean = 5, sd = 2)
  y_data <- 2 * x_data + rnorm(100, mean = 0, sd = 1)

  # Fit the model
  my_model <- my_regression(x_data, y_data)

  # Create a plot
  ggplot(data.frame(x = x_data, y = y_data), aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(title = "Linear Regression Example", x = "X Values", y = "Y Values")
  '

  # Example assignment configuration
  assignment_config <- list(
    student_id = "student_001",
    assignment_id = "regression_assignment",
    tests = list(
      list(
        type = "function_exists",
        function_name = "my_regression",
        description = "Function my_regression exists"
      ),
      list(
        type = "variable_value",
        variable_name = "my_model",
        expected_value = lm(y_data ~ x_data),
        description = "Linear model created correctly"
      )
    ),
    requirements = c("visualization"),
    format = "r_script",
    weights = list(
      syntax = 0.1,
      style = 0.15,
      functionality = 0.4,
      statistical_accuracy = 0.25,
      visualization = 0.1
    )
  )

  # Grade the assignment
  results <- grader$grade_assignment(student_code, assignment_config)

  # Display results
  cat("=== R Assignment Grading Results ===\n")
  cat("Student:", results$student_id, "\n")
  cat("Assignment:", results$assignment_id, "\n")
  cat("Total Score:", results$total_score, "/100\n\n")

  for (component in names(results$scores)) {
    cat("**", toupper(component), "**\n")
    cat("Score:", results$scores[[component]], "\n")
    cat("Feedback:\n")
    for (feedback in results$feedback[[component]]) {
      cat(" -", feedback, "\n")
    }
    cat("\n")
  }

  return(results)
}

# Run demo
# demo_results <- demo_r_grading()