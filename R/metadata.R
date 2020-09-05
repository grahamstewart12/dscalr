
md_make_template <- function(data, name = "", sites = "", creator = "", 
                             contact = "", ...) {
  
  dataset <- list(
    name = name,
    sites = sites,
    creator = creator,
    contact = contact,
    date_modified = lubridate::today()
  )
  
  vars <- data %>% 
    purrr::map_chr(readr::guess_parser, guess_integer = TRUE) %>%
    tibble::enframe(name = "name", value = "storage") %>%
    dplyr::mutate(unit = "") %>%
    dplyr::mutate(unit_name = "") %>%
    dplyr::mutate(type = "") %>%
    dplyr::mutate(description = "")
  
  session <- devtools::session_info() %>% 
    purrr::pluck("platform") %>% 
    unclass() %>%
    purrr::list_modify(ctype = rlang::zap(), date = rlang::zap()) %>% 
    tibble::enframe(name = "setting") %>% 
    dplyr::mutate(value = purrr::simplify(value))
  
  list(data = dataset, vars = vars, session = session)
}

md_update <- function(metadata, ...) {
  
  md <- metadata
  dots <- rlang::list2(...)
  
  md$data <- purrr::list_modify(md$data, !!!dots)
  md
}

md_collapse_suffix <- function(metadata, ...) {
  
  md <- metadata
  dots <- rlang::list2(...)
  
  # e.g. sfc = "surface"
  
  suffix_vars <- dots %>% 
    names() %>% 
    paste0("_", .) %>%
    rlang::set_names() %>%
    #purrr::map(~ stringr::str_subset(md$vars$name, paste0("_", .x))) %>%
    purrr::map(~ stringr::str_subset(md$vars$name, .x)) %>%
    tibble::enframe() %>%
    tidyr::unnest_longer(value) %>%
    #dplyr::relocate(value) %>%
    tibble::deframe()
  
  suffix_desc <- suffix_vars %>%
    names() %>%
    stringr::str_remove("_") %>%
    dplyr::recode(!!!dots) %>%
    paste0("\"", names(suffix_vars), "\"", " = ", .)
  
  add <- tibble::tibble(
    name = unname(suffix_vars),
    description = paste0('"', names(suffix_vars), '"', "=")
  )
  
  md$vars <- dplyr::rows_update(md$vars, add, by = "name")
  
  md$vars$name
  
  md
}

md_update_vars <- function(metadata, ..., .suffixes = list()) {
  
  md <- metadata
  dots <- rlang::list2(...)
  
  if (!length(dots)) {
    return(md)
  }
  
  add <- tibble::tibble(
    name = character(),
    unit = character(),
    unit_name = character(),
    type = character(),
    description = character()
  )
  
  dots <- purrr::map(dots, tibble::as_tibble)
  
  ref <- dots %>%
    purrr::keep(~ nrow(.x) > 1) %>%
    dplyr::bind_rows()
  
  new <- dots %>% 
    purrr::keep(~ nrow(.x) == 1) %>%
    purrr::imap(~ dplyr::mutate(.x, name = .y, .before = 1))
  
  if (length(ref)) {
    for (i in 1:length(md$vars$name)) {
      base_name <- stringr::str_remove(
        md$vars$name[i], stringr::str_c("_", .suffixes, "$", collapse = "|")
      )
      base_row <- dplyr::filter(ref, name == base_name)
      base_row$name <- md$vars$name[i]
      add <- dplyr::bind_rows(add, base_row)
    }
  }
  
  add <- dplyr::mutate(add, dplyr::across(.fns = ~ tidyr::replace_na(.x, "")))
  
  md$vars <- dplyr::rows_update(md$vars, add, by = "name")
  
  if (length(new)) {
    for (i in 1:length(new)) {
      md$vars <- dplyr::rows_update(md$vars, new[[i]], by = "name")
    }
  }
  
  md
}


md_write <- function(metadata, path, fill = "-") {
  
  md <- metadata
  
  # Reduce any vectors to single strings
  data <- md$data %>%
    purrr::map(as.character) %>%
    purrr::map(~ str_c(.x, collapse = ", ")) %>%
    tibble::enframe() %>%
    dplyr::mutate(value = purrr::simplify(value))
  
  # Set vars names as first row since header won't be written
  vars_names <- md$vars %>% names() %>% rlang::set_names() %>% as.list()
  vars <- md$vars %>%
    # Fill unspecified attributes
    dplyr::mutate(
      dplyr::across(.fns = ~ dplyr::if_else(.x == "", fill, .x))
    ) %>%
    #tibble::add_row(!!!vars_names, .before = 1) %>%
    dplyr::group_nest(name) %>% 
    dplyr::left_join(tibble::tibble(name = md$vars$name), ., by = "name") %>%
    tibble::deframe() %>% 
    purrr::map(tidyr::pivot_longer, dplyr::everything()) %>%
    purrr::map(dplyr::mutate, name = paste0(name, ":")) %>%
    purrr::map(dplyr::mutate, tab = "\t", .before = 1)
  
  # Write metadata components in sequence
  readr::write_lines(c("METADATA", "---"), path)
  readr::write_delim(data, path, delim = "\t", col_names = FALSE, append = TRUE)
  readr::write_lines(c("", "VARIABLES", "---"), path, append = TRUE)
  purrr::imap(
    vars, ~ {
      readr::write_lines(c(.y), path, append = TRUE)
      readr::write_delim(.x, path, col_names = FALSE, append = TRUE)
      readr::write_lines("", path, append = TRUE)
    }
  )
  #readr::write_lines("", path, append = TRUE)
  #readr::write_delim(vars, path, delim = "\t", append = TRUE)
  readr::write_lines(c("", "PLATFORM", "---"), path, append = TRUE)
  #readr::write_lines("", path, append = TRUE)
  readr::write_delim(md$session, path, delim = "\t", append = TRUE)
}

