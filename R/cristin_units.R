#' @title create a tibble with information about selected Cristin units
#' @description Create a tibble with information about (nested) units in Cristin
#'  (e.g., A University -> Faculties -> Departments -> Groups). The tibble can
#'  than be used to extract data for each unit from Cristin. Used by
#'  `CristinMonthly`
#' @param unit What unit to search for
#' @param recursive Search for (nested) subunits, Default: FALSE
#' @param lang PARAM_DESCRIPTION, Default: `en`
#' @return A tibble containing information about selected units
#' @details Used with `CristinMontlhy` to create month-to-month bibliography of
#' selected units
#' @examples
#' \donttest{
#' # Find units for Inland University
#'   CristinUnits("209.0.0.0") |>
#'     dplyr::select(id, name) |>
#'     print(width = 80)
#' }
#' @seealso
#'  \code{\link[httr]{RETRY}}
#'  \code{\link[dplyr]{transmute}}, \code{\link[dplyr]{bind_rows}}
#'  \code{\link[purrr]{pmap}}
#' @rdname CristinUnits
#' @export
CristinUnits <- \(unit, recursive = FALSE, lang = "en") {

  cristin_unit_id <- unit_name <- parent_unit <- name <- parent_units <-
    lineage <- NULL

  # Set language to English if not en, nb, nn
  if (!lang %in% c("en", "nb", "nn")) {
    lang <- "en"
  }

  # Find units from Cristin API
  httr.get <- Online(
    httr::RETRY(
      "GET",
      sprintf("https://api.cristin.no/v2/units/%s/", unit),
      query = list(lang = lang),
      quiet = TRUE
    ),
    message = "Cristin API",
    silent = TRUE
  )

  # Return NULL data if error
  if (httr.get$error) {
    return(NULL)
  }

  # Define units data
  units.data <- httr.get$data |>
    JsonToTibble()

  # Define parent unit
  units <- units.data |>
    dplyr::transmute(
      id = cristin_unit_id,
      name = as.character(unit_name[[1]]),
      parent.name = GoFish(
        parent_unit[[1]]$unit_name[[1]],
        name
      ),
      ancestor.name = GoFish(
        units.data$parent_units[[1]]$unit_name[[1]][[1]],
        name
      ),
      lineage = list(unique(c(
        GoFish(unlist(parent_units[[1]]$unit_name), NULL),
        name
      ))),
      core.location = lineage
    )

  # Return if no sub units
  if (!"subunits" %in% names(units.data)) {
    return (units)
    # Else add unspecified to lineage
  } else {
    units$lineage[[1]] <- c(units$lineage[[1]], units$name[[1]])
  }

  # Define all units
  units <- dplyr::bind_rows(
    units,
    units.data$subunits[[1]] |>
      dplyr::transmute(
        id = cristin_unit_id,
        name = as.character(unit_name[[1]]),
        parent.name = units$name[[1]],
        ancestor.name = units$ancestor.name[[1]],
        lineage = purrr::pmap(list(name), ~ {
          unique(c(
            GoFish(unlist(units.data$parent_units[[1]]$unit_name)),
            parent.name[[1]],
            .x
          ))
        }),
        core.location = lineage
      )
  )

  # Add nested sub units if recursive is TRUE
  if (recursive) {
    units <- dplyr::bind_rows(
      units[1, ],
      lapply(seq_len(nrow(units))[-1], \(i) {
        CristinUnits(
          units[i, ]$id, recursive = TRUE, lang = lang
        )
      })
    )
  }

  return(units)

}
