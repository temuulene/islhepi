# BC Data Catalogue access.
#
# Permanent catalogue identifiers are pinned here so routine analyses do not
# drift onto a similarly named record returned by search.

.islh_bc_sources <- list(
  population = list(
    record_id = "86839277-986a-4a29-9f70-fa9b1166f6cb",
    resources = c(
      lha = "d4bbb2a0-aff7-403f-b52a-a634d05ee70f",
      hsda = "ff17aca9-d049-4407-a5f9-b7ef163d4e28"
    )
  ),
  geography = list(
    ha = list(
      record_id = "7bc6018f-bb4f-4e5d-845e-c529e3d1ac3b",
      code = "HLTH_AUTHORITY_CODE",
      name = "HLTH_AUTHORITY_NAME",
      authority = "HLTH_AUTHORITY_NAME",
      width = 1L
    ),
    hsda = list(
      record_id = "71c930b9-563a-46da-a10f-ead49ccbc390",
      code = "HLTH_SERVICE_DLVR_AREA_CODE",
      name = "HLTH_SERVICE_DLVR_AREA_NAME",
      authority = "HLTH_AUTHORITY_NAME",
      width = 2L
    ),
    lha = list(
      record_id = "afd021d9-7722-4410-b506-d394c66e74fc",
      code = "LOCAL_HLTH_AREA_CODE",
      name = "LOCAL_HLTH_AREA_NAME",
      authority = "HLTH_AUTHORITY_NAME",
      width = 3L
    ),
    chsa = list(
      record_id = "68f2f577-28a7-46b4-bca9-7e9770f2f357",
      code = "CMNTY_HLTH_SERV_AREA_CODE",
      name = "CMNTY_HLTH_SERV_AREA_NAME",
      authority = "HLTH_AUTHORITY_NAME",
      width = 4L
    )
  )
)

#' BC population and health-geography catalogue sources
#'
#' Returns the permanent BC Data Catalogue record and resource identifiers used
#' by [islh_bc_population()] and [islh_bc_geography()]. Use this table to audit
#' or cite the source of a downloaded extract.
#'
#' @return A data frame with source type, geography, record ID, optional
#'   resource ID and catalogue URL.
#' @export
islh_bc_sources <- function() {
  population <- data.frame(
    source_type = "population",
    geography = names(.islh_bc_sources$population$resources),
    record_id = .islh_bc_sources$population$record_id,
    resource_id = unname(.islh_bc_sources$population$resources),
    stringsAsFactors = FALSE
  )

  geography <- data.frame(
    source_type = "boundary",
    geography = names(.islh_bc_sources$geography),
    record_id = vapply(
      .islh_bc_sources$geography,
      `[[`,
      character(1),
      "record_id"
    ),
    resource_id = NA_character_,
    stringsAsFactors = FALSE
  )

  out <- rbind(population, geography)
  out$catalogue_url <- paste0(
    "https://catalogue.data.gov.bc.ca/dataset/",
    out$record_id
  )
  rownames(out) <- NULL
  out
}

#' Download BC Stats population denominators
#'
#' Downloads a pinned BC Stats sub-provincial population resource through
#' `bcdata` and returns one row per geography, year, estimate type, sex and age
#' group. The output is ready to join to [islh_bc_geography()] using
#' `geography` and `geography_code`.
#'
#' `sex = c("F", "M")` is the default because those rows can be summed without
#' double counting. Total rows (`"T"`) must be requested on their own.
#'
#' @param geography `"lha"` or `"hsda"`.
#' @param years Optional numeric vector of years. `NULL` returns all years in
#'   the resource.
#' @param sex Either one or both of `"F"` and `"M"`, or `"T"` alone.
#' @param age_breaks Age bands passed to [islh_age_group()].
#' @param age_labels Optional labels passed to [islh_age_group()].
#'
#' @return A tibble with `geography`, `geography_code`, `geography_name`,
#'   `year`, `estimate_type`, `sex`, `age_group` and `population`.
#' @export
#'
#' @examples
#' \dontrun{
#' lha_population <- islh_bc_population("lha", years = 2021:2026)
#' }
islh_bc_population <- function(
  geography = c("lha", "hsda"),
  years = NULL,
  sex = c("F", "M"),
  age_breaks = "five_year",
  age_labels = NULL
) {
  geography <- match.arg(geography)
  .islh_require_packages(c("bcdata", "dplyr", "tidyr"), "population data")

  years <- .islh_check_years(years)
  sex <- .islh_check_sex(sex)
  source <- .islh_bc_sources$population

  raw <- .islh_fetch_bc_population(
    record_id = source$record_id,
    resource_id = unname(source$resources[[geography]])
  )

  out <- .islh_tidy_bc_population(
    raw = raw,
    geography = geography,
    years = years,
    sex = sex,
    age_breaks = age_breaks,
    age_labels = age_labels
  )

  attr(out, "bcdata_source") <- .islh_source_metadata(
    record_id = source$record_id,
    resource_id = unname(source$resources[[geography]])
  )
  out
}

#' Download BC health-geography boundaries
#'
#' Downloads a pinned BC Data Catalogue Web Feature Service through `bcdata`
#' and returns standardized `sf` columns. By default, only Island Health
#' boundaries are returned. Use `health_authority = NULL` for all of BC.
#'
#' @param geography One of `"ha"`, `"hsda"`, `"lha"` or `"chsa"`.
#' @param health_authority Health authority name. `"Island Health"` is accepted
#'   as an alias for the catalogue value `"Vancouver Island"`. Use `NULL` to
#'   return all health authorities.
#' @param crs EPSG code for the returned coordinate reference system. BC Albers
#'   (`3005`) is the default.
#' @param simplify_tolerance Optional non-negative simplification tolerance in
#'   the units of `crs`. `NULL` keeps the source geometry unchanged.
#'
#' @return An `sf` object with `geography`, `geography_code`,
#'   `geography_name`, `health_authority_name` and `geometry`.
#' @export
#'
#' @examples
#' \dontrun{
#' island_lha <- islh_bc_geography("lha")
#' bc_chsa <- islh_bc_geography("chsa", health_authority = NULL)
#' }
islh_bc_geography <- function(
  geography = c("lha", "hsda", "chsa", "ha"),
  health_authority = "Vancouver Island",
  crs = 3005,
  simplify_tolerance = NULL
) {
  geography <- match.arg(geography)
  .islh_require_packages(c("bcdata", "dplyr", "sf"), "boundary data")
  health_authority <- .islh_check_authority(health_authority)
  crs <- .islh_check_crs(crs)
  simplify_tolerance <- .islh_check_tolerance(simplify_tolerance)
  source <- .islh_bc_sources$geography[[geography]]

  raw <- .islh_fetch_bc_geography(source$record_id, crs = crs)
  out <- .islh_standardize_bc_geography(
    raw = raw,
    geography = geography,
    source = source,
    health_authority = health_authority
  )

  if (!is.null(simplify_tolerance) && simplify_tolerance > 0) {
    out <- sf::st_simplify(
      out,
      preserveTopology = TRUE,
      dTolerance = simplify_tolerance
    )
  }

  attr(out, "bcdata_source") <- .islh_source_metadata(source$record_id)
  out
}

.islh_fetch_bc_population <- function(record_id, resource_id) {
  .islh_with_bcdata_errors(
    bcdata::bcdc_get_data(
      record = record_id,
      resource = resource_id,
      verbose = FALSE
    )
  )
}

.islh_fetch_bc_geography <- function(record_id, crs) {
  .islh_with_bcdata_errors(
    bcdata::bcdc_query_geodata(record_id, crs = crs) |>
      dplyr::collect()
  )
}

.islh_tidy_bc_population <- function(
  raw,
  geography,
  years,
  sex,
  age_breaks,
  age_labels
) {
  required <- c("Region", "Region.Name", "Region.Type", "Year", "Type", "Gender")
  .islh_check_columns(raw, required)

  age_cols <- grep("^[0-9]+\\+?$", names(raw), value = TRUE)
  if (length(age_cols) == 0L) {
    .islh_abort("The population resource has no one-year age columns.")
  }
  numeric_age <- vapply(raw[age_cols], is.numeric, logical(1))
  if (!all(numeric_age)) {
    .islh_abort(c(
      "The population resource has non-numeric age columns.",
      x = "Affected column{?s}: {.field {age_cols[!numeric_age]}}."
    ))
  }
  population_values <- unlist(raw[age_cols], use.names = FALSE)
  if (anyNA(population_values) || any(!is.finite(population_values)) ||
      any(population_values < 0)) {
    .islh_abort(
      "Population age columns must contain finite, non-negative values."
    )
  }

  expected_type <- c(
    lha = "Local Health Area",
    hsda = "Health Service Delivery Area"
  )[[geography]]
  code_width <- c(lha = 3L, hsda = 2L)[[geography]]

  year <- suppressWarnings(as.integer(as.character(raw[["Year"]])))
  if (any(is.na(year) & !is.na(raw[["Year"]]))) {
    .islh_abort("The population resource contains a non-numeric year.")
  }

  prepared <- raw |>
    dplyr::mutate(
      year = .env$year,
      sex = as.character(.data$Gender),
      geography_code = .islh_format_geography_code(
        .data$Region,
        width = code_width
      )
    ) |>
    dplyr::filter(
      .data[["Region.Type"]] == expected_type,
      !grepl("^0+$", .data$geography_code),
      .data$sex %in% sex
    )

  if (!is.null(years)) {
    prepared <- dplyr::filter(prepared, .data$year %in% years)
  }

  out <- prepared |>
    dplyr::transmute(
      geography = .env$geography,
      geography_code = .data$geography_code,
      geography_name = as.character(.data[["Region.Name"]]),
      year = .data$year,
      estimate_type = as.character(.data$Type),
      sex = .data$sex,
      dplyr::across(dplyr::all_of(age_cols))
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(age_cols),
      names_to = "age",
      values_to = "population"
    ) |>
    dplyr::mutate(
      age = as.numeric(sub("\\+$", "", .data$age)),
      age_group = islh_age_group(
        .data$age,
        breaks = age_breaks,
        labels = age_labels
      )
    ) |>
    dplyr::summarise(
      population = sum(.data$population, na.rm = TRUE),
      .by = c(
        "geography",
        "geography_code",
        "geography_name",
        "year",
        "estimate_type",
        "sex",
        "age_group"
      )
    ) |>
    dplyr::filter(.data$population > 0, !is.na(.data$age_group)) |>
    dplyr::arrange(
      .data$geography_code,
      .data$year,
      .data$estimate_type,
      .data$sex,
      .data$age_group
    )

  if (nrow(out) == 0L) {
    .islh_abort(c(
      "The requested population extract has no rows.",
      i = "Check {.arg years}, {.arg sex} and the available catalogue years."
    ))
  }
  out
}

.islh_standardize_bc_geography <- function(
  raw,
  geography,
  source,
  health_authority
) {
  if (!inherits(raw, "sf")) {
    .islh_abort("The boundary query did not return an {.cls sf} object.")
  }
  .islh_check_columns(raw, c(source$code, source$name, source$authority))

  attributes <- sf::st_drop_geometry(raw)
  authority <- trimws(as.character(attributes[[source$authority]]))
  keep <- rep(TRUE, nrow(raw))

  if (!is.null(health_authority)) {
    target <- if (tolower(health_authority) == "island health") {
      "Vancouver Island"
    } else {
      health_authority
    }
    keep <- tolower(authority) == tolower(trimws(target))
    if (!any(keep)) {
      available <- sort(unique(authority[nzchar(authority)]))
      .islh_abort(c(
        "No boundary matched health authority {.val {health_authority}}.",
        i = "Catalogue values are {.val {available}}."
      ))
    }
  }

  attributes <- attributes[keep, , drop = FALSE]
  geometry <- sf::st_geometry(raw)[keep]
  out <- data.frame(
    geography = geography,
    geography_code = .islh_format_geography_code(
      attributes[[source$code]],
      width = source$width
    ),
    geography_name = trimws(as.character(attributes[[source$name]])),
    health_authority_name = trimws(
      as.character(attributes[[source$authority]])
    ),
    stringsAsFactors = FALSE
  )
  sf::st_sf(out, geometry = geometry)
}

.islh_require_packages <- function(packages, feature) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0L) {
    command <- paste0(
      "install.packages(c(",
      paste(sprintf("\"%s\"", missing), collapse = ", "),
      "), type = \"binary\")"
    )
    .islh_abort(c(
      "Missing package{?s} required for {feature}: {.pkg {missing}}.",
      i = "Install {?it/them} with {.code {command}}."
    ))
  }
  invisible(TRUE)
}

.islh_with_bcdata_errors <- function(expr) {
  tryCatch(
    expr,
    error = function(error) {
      .islh_abort(c(
        "The BC Data Catalogue request failed.",
        x = conditionMessage(error),
        i = paste(
          "Confirm that catalogue.data.gov.bc.ca is reachable from your",
          "network, then retry."
        )
      ))
    }
  )
}

.islh_check_columns <- function(data, required) {
  if (!is.data.frame(data)) {
    .islh_abort("The downloaded resource must be a data frame.")
  }
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    .islh_abort(c(
      "The BC Data Catalogue schema has changed.",
      x = "Missing required column{?s}: {.field {missing}}.",
      i = "Inspect the current record before using the extract."
    ))
  }
  invisible(TRUE)
}

.islh_check_years <- function(years) {
  if (is.null(years)) {
    return(NULL)
  }
  if (!is.numeric(years) || length(years) == 0L || anyNA(years) ||
      any(!is.finite(years)) || any(years != floor(years))) {
    .islh_abort("{.arg years} must be NULL or finite whole-number years.")
  }
  sort(unique(as.integer(years)))
}

.islh_check_sex <- function(sex) {
  if (!is.character(sex) || length(sex) == 0L || anyNA(sex)) {
    .islh_abort("{.arg sex} must contain {.val F}, {.val M} or {.val T}.")
  }
  sex <- unique(toupper(sex))
  unknown <- setdiff(sex, c("F", "M", "T"))
  if (length(unknown) > 0L) {
    .islh_abort("Unknown {.arg sex} value{?s}: {.val {unknown}}.")
  }
  if ("T" %in% sex && length(sex) > 1L) {
    .islh_abort(c(
      "Total-sex rows must be requested on their own.",
      i = "Combining {.val T} with {.val F} or {.val M} double counts people."
    ))
  }
  sex
}

.islh_check_authority <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    .islh_abort(
      "{.arg health_authority} must be NULL or one non-empty string."
    )
  }
  trimws(x)
}

.islh_check_crs <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x <= 0 || x != floor(x)) {
    .islh_abort("{.arg crs} must be one positive whole-number EPSG code.")
  }
  as.integer(x)
}

.islh_check_tolerance <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 0) {
    .islh_abort(
      "{.arg simplify_tolerance} must be NULL or one non-negative number."
    )
  }
  x
}

.islh_format_geography_code <- function(x, width) {
  out <- trimws(as.character(x))
  numeric <- !is.na(out) & grepl("^[0-9]+$", out)
  out[numeric] <- sprintf(paste0("%0", width, "d"), as.integer(out[numeric]))
  out
}

.islh_source_metadata <- function(record_id, resource_id = NULL) {
  list(
    record_id = record_id,
    resource_id = resource_id,
    retrieved_at = Sys.time(),
    bcdata_version = as.character(utils::packageVersion("bcdata"))
  )
}
