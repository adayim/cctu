# Render statistics

test_that("Numeric", {
  data(mtcars)
  x <- render_numeric(mtcars$mpg)
  expect_identical(x, c(
    "Valid Obs." = "32",
    "Mean (SD)" = "20.1 (6.03)",
    "Median [Min, Max]" = "19.2 [10.4, 33.9]"
  ))
  x <- rep(NA, 10)

  expect_identical(render_numeric(x), c(
    "Valid Obs." = "0",
    "Mean (SD)" = "",
    "Median [Min, Max]" = ""
  ))

  x <- render_numeric(mtcars$mpg,
    what = c(
      "Geo. Mean (Geo. CV%)" = "GMean (GCV)",
      "Geo. SD" = "GSD",
      "Median [IQR]" = "Median [IQR]"
    )
  )
  expect_identical(x, c(
    "Valid Obs." = "32",
    "Mean (SD)" = "20.1 (6.03)",
    "Geo. Mean (Geo. CV%)" = "19.3 (30.4%)",
    "Geo. SD" = "1.35",
    "Median [IQR]" = "19.2 [7.38]"
  ))
  # Quantile
  x <- render_numeric(mtcars$mpg,
    what = c("Median [Q1, Q3]" = "Median [Q1, Q3]")
  )

  expect_identical(x, c(
    "Valid Obs." = "32",
    "Mean (SD)" = "20.1 (6.03)",
    "Median [Q1, Q3]" = "19.2 [15.4, 22.8]"
  ))

  x <- render_numeric(mtcars$mpg,
    what = c("GMean (GCV)",
      "Median [IQR]" = "Median [IQR]"
    )
  )
  expect_identical(x, c(
    "Valid Obs." = "32",
    "Mean (SD)" = "20.1 (6.03)",
    "GMean (GCV)" = "19.3 (30.4%)",
    "Median [IQR]" = "19.2 [7.38]"
  ))
  expect_error(
    render_numeric(mtcars$mpg, what = "Mdian [IQR]"),
    "Statistics Mdian is not a valid statistics"
  )
})


test_that("Character", {
  data(mtcars)
  y <- factor(mtcars$am, levels = c(0, 1), labels = c("automatic", "manual"))
  y[1:10] <- NA

  expect_identical(render_cat(y), c(
    "automatic" = "12/22 (54.5%)",
    "manual" = "10/22 (45.5%)"
  ))

  y <- c(rep(T, 8), rep(F, 10))
  expect_identical(render_cat(y), c(
    "Yes" = "8/18 (44.4%)",
    "No" = "10/18 (55.6%)"
  ))
})

test_that("a statistic name nested in a longer one is not substituted inside it", {
  # gsub() on the bare name replaced "Mean" inside "GMean", so "Mean (GMean)"
  # rendered as "20.1 (G20.1)" - a plausible-looking but wrong number. Names
  # must match as whole words only, and the order in the template must not
  # matter ("GMean (Mean)" happened to work; "Mean (GMean)" did not).
  data(mtcars)
  r <- function(what) render_numeric(mtcars$mpg, what = what)[[what]]

  expect_identical(r("Mean (GMean)"), "20.1 (19.3)")
  expect_identical(r("GMean (Mean)"), "19.3 (20.1)")
  expect_identical(r("CV (GCV)"), "30.0% (30.4%)")
  expect_identical(r("SD (GSD)"), "6.03 (1.35)")
  # 'N' sits inside 'MEAN' when the user spells the statistic in upper case.
  expect_identical(r("N (MEAN)"), "32 (20.1)")
})

test_that("unavailable statistics render '-', and an all-unavailable template renders ''", {
  # sd() of one observation is NA. The hard-coded row guarded MEAN but not SD
  # and printed a literal "5.00 (NA)", while what = "Mean (SD)" rendered ""
  # for the very same input - the two paths disagreed.
  one <- render_numeric(5)
  expect_identical(one[["Mean (SD)"]], "5.00 (-)")
  expect_identical(render_numeric(5, what = "Mean (SD)")[["Mean (SD)"]],
                   "5.00 (-)")

  # Nothing available at all -> blank cell, not "- [-, -]".
  allna <- render_numeric(rep(NA_real_, 3))
  expect_identical(allna[["Mean (SD)"]], "")
  expect_identical(allna[["Median [Min, Max]"]], "")
  expect_identical(allna[["Valid Obs."]], "0")

  # Partially available -> the available parts still show.
  expect_identical(
    render_numeric(c(1, 2, NA), what = "Median [Min, Max]")[["Median [Min, Max]"]],
    "1.50 [1.00, 2.00]"
  )
})

test_that("Inf is a real value and survives rendering", {
  # The old guard blanked any rendered string containing no digit, which
  # silently deleted Inf/-Inf (signif_pad deliberately preserves them).
  expect_identical(render_numeric(c(1, 2, Inf), what = "Max")[["Max"]], "Inf")
  expect_identical(render_numeric(c(-Inf, 1, 2), what = "Min")[["Min"]], "-Inf")
})

test_that("CV is unavailable when the mean is zero", {
  # sd/0 is Inf and 0/0 is NaN; these reached the table as "Inf%" and "NaN".
  expect_identical(
    render_numeric(c(-5, 5, -3, 3), what = "Mean (CV)")[["Mean (CV)"]],
    "0.00 (-)"
  )
  expect_identical(
    render_numeric(c(0, 0, 0), what = "Mean (CV)")[["Mean (CV)"]],
    "0.00 (-)"
  )
  # A non-zero mean still reports CV.
  expect_match(render_numeric(c(1, 2, 3), what = "CV")[["CV"]], "%$")
})

test_that("an explicit NA factor level counts as a category, not as missingness", {
  # Promoting NA to a level (exclude = NULL / addNA) declares missingness to BE
  # a category: is.na() on such a vector is all FALSE, so there is nothing left
  # to exclude from the denominator. It is relabelled "Missing" for display but
  # must still be counted, or the percentages sum past 100% and N < Nall while
  # nothing is actually missing.
  f <- factor(c("a", "a", "b", NA, NA), levels = c("a", "b", NA),
              exclude = NULL)
  expect_identical(sum(is.na(f)), 0L)

  expect_identical(
    render_cat(f),
    c("a" = "2/5 (40.0%)", "b" = "1/5 (20.0%)", "Missing" = "2/5 (40.0%)")
  )
  expect_identical(cat_stat(f)$a$N, 5L)
  expect_identical(cat_stat(f)$a$Nall, 5L)

  # A level whose LABEL is the string "NA" is an ordinary category: it is
  # neither relabelled to "Missing" nor treated as missing. (Only a real NA
  # level satisfies is.na(levels(.)); the string "NA" does not.)
  s <- factor(c("a", "a", "b", "NA", "NA"))
  expect_true("NA" %in% names(render_cat(s)))
  expect_false("Missing" %in% names(render_cat(s)))
  expect_identical(render_cat(s)[["NA"]], "2/5 (40.0%)")

  # Genuinely missing values (no NA level) are excluded from PCTnoNA as before.
  g <- factor(c("a", "a", "b", NA, NA), levels = c("a", "b"))
  expect_identical(render_cat(g), c("a" = "2/3 (66.7%)", "b" = "1/3 (33.3%)"))
})
