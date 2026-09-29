# plot_audit_results.R
# Diagnostic figures for the reporting-unit audit.


plot_audit_results <- function(audit_results, output_dir) {
  
  required_tables <- c(
    "audit_summary_year",
    "audit_summary_reason",
    "audit_summary_size"
  )
  
  missing_tables <- setdiff(required_tables, names(audit_results))
  
  if (length(missing_tables) > 0) {
    stop("audit_results is missing: ", paste(missing_tables, collapse = ", "))
  }
  
  fs::dir_create(output_dir)
  
  save_plot <- function(
    file_stem,
    plot_fun,
    width = 8.5,
    height = 5.5,
    res = 150
  ) {
    
    png_path <- file.path(output_dir, paste0(file_stem, ".png"))
    pdf_path <- file.path(output_dir, paste0(file_stem, ".pdf"))
    
    grDevices::png(
      filename = png_path,
      width = round(width * res),
      height = round(height * res),
      res = res
    )
    tryCatch(
      plot_fun(),
      finally = grDevices::dev.off()
    )
    
    grDevices::pdf(
      file = pdf_path,
      width = width,
      height = height,
      useDingbats = FALSE
    )
    tryCatch(
      plot_fun(),
      finally = grDevices::dev.off()
    )
    
    invisible(
      c(
        png = png_path,
        pdf = pdf_path
      )
    )
  }
  
  clean_reason_label <- function(x) {
    
    x <- as.character(x)
    
    dplyr::case_when(
      x == "voters_gt_registered" ~ "Voters > registered",
      x == "negative_null" ~ "Negative null ballots",
      x == "missing_blank" ~ "Missing blank ballots",
      x == "negative_null; list_votes_gt_voters" ~
        "Negative null + list votes > voters",
      x == "negative_null|list_votes_gt_voters" ~
        "Negative null + list votes > voters",
      x == "negative_null + list_votes_gt_voters" ~
        "Negative null + list votes > voters",
      x == "missing_registered" ~ "Missing registered voters",
      x == "negative_list_votes" ~ "Negative list votes",
      TRUE ~ stringr::str_replace_all(x, "_", " ")
    )
  }
  
  clean_size_label <- function(x) {
    
    x <- as.character(x)
    
    dplyr::case_when(
      x %in% c("<1,000", "< 1,000", "lt_1000") ~ "<1,000",
      x %in% c("1,000-4,999", "1,000--4,999", "1000_4999") ~ "1,000--4,999",
      x %in% c("5,000-9,999", "5,000--9,999", "5000_9999") ~ "5,000--9,999",
      x %in% c("10,000-49,999", "10,000--49,999", "10000_49999") ~ "10,000--49,999",
      x %in% c("50,000+", "50000_plus") ~ "50,000+",
      x %in% c("missing_registered", "Missing registered") ~ "Missing registered",
      TRUE ~ x
    )
  }
  
  audit_year_raw <- audit_results$audit_summary_year
  
  if ("year" %in% names(audit_year_raw)) {
    audit_year <- audit_year_raw |>
      dplyr::mutate(
        year = as.integer(.data$year)
      )
  } else {
    audit_year <- audit_year_raw |>
      dplyr::mutate(
        year = as.integer(substr(.data$election_date, 1, 4))
      )
  }
  
  audit_year <- audit_year |>
    dplyr::mutate(
      excluded_reporting_unit_share_pct =
        100 * .data$excluded_reporting_unit_share,
      excluded_electorate_share_pct =
        100 * .data$excluded_electorate_share
    ) |>
    dplyr::arrange(.data$year)
  
  year_ticks <- audit_year$year
  
  # 1. Absolute excluded reporting units by year
  
  save_plot("audit_excluded_units_by_year", function() {
    
    old_par <- graphics::par(
      mar = c(5.2, 5.2, 3.5, 1.5)
    )
    on.exit(graphics::par(old_par), add = TRUE)
    
    graphics::plot(
      x = audit_year$year,
      y = audit_year$excluded_reporting_units,
      type = "b",
      pch = 16,
      lwd = 1.2,
      xaxt = "n",
      xlab = "Election year",
      ylab = "Excluded municipality-election units",
      main = "Audited exclusions by election year"
    )
    
    graphics::axis(
      side = 1,
      at = year_ticks,
      labels = year_ticks,
      las = 2,
      cex.axis = 0.75
    )
    
    graphics::grid(
      nx = NA,
      ny = NULL,
      lty = "dotted"
    )
  })
  
  # 2. Excluded electorate share by year
  
  save_plot("audit_excluded_electorate_share_by_year", function() {
    
    old_par <- graphics::par(
      mar = c(5.2, 5.2, 3.5, 1.5)
    )
    on.exit(graphics::par(old_par), add = TRUE)
    
    graphics::plot(
      x = audit_year$year,
      y = audit_year$excluded_electorate_share_pct,
      type = "b",
      pch = 16,
      lwd = 1.2,
      xaxt = "n",
      xlab = "Election year",
      ylab = "Excluded registered electorate (%)",
      main = "Excluded electorate share by election year"
    )
    
    graphics::axis(
      side = 1,
      at = year_ticks,
      labels = year_ticks,
      las = 2,
      cex.axis = 0.75
    )
    
    graphics::grid(
      nx = NA,
      ny = NULL,
      lty = "dotted"
    )
  })
  
  # 3. Main thesis plot: unit share and electorate share on the same scale
  
  save_plot("audit_exclusion_shares_by_year", function() {
    
    old_par <- graphics::par(
      mar = c(5.2, 5.4, 3.5, 1.5)
    )
    on.exit(graphics::par(old_par), add = TRUE)
    
    y_max <- max(
      audit_year$excluded_reporting_unit_share_pct,
      audit_year$excluded_electorate_share_pct,
      na.rm = TRUE
    )
    
    graphics::plot(
      x = audit_year$year,
      y = audit_year$excluded_reporting_unit_share_pct,
      type = "b",
      pch = 16,
      lty = 1,
      lwd = 1.2,
      ylim = c(0, y_max * 1.12),
      xaxt = "n",
      xlab = "Election year",
      ylab = "Excluded share (%)",
      main = "Audited exclusion shares by election year"
    )
    
    graphics::lines(
      x = audit_year$year,
      y = audit_year$excluded_electorate_share_pct,
      type = "b",
      pch = 2,
      lty = 2,
      lwd = 1.2
    )
    
    graphics::axis(
      side = 1,
      at = year_ticks,
      labels = year_ticks,
      las = 2,
      cex.axis = 0.75
    )
    
    graphics::grid(
      nx = NA,
      ny = NULL,
      lty = "dotted"
    )
    
    graphics::legend(
      "topright",
      legend = c(
        "Excluded reporting units",
        "Excluded registered electorate"
      ),
      pch = c(16, 2),
      lty = c(1, 2),
      lwd = c(1.2, 1.2),
      bty = "n"
    )
  })
  
  # 4. Excluded reporting units by reason
  
  reason_plot <- audit_results$audit_summary_reason |>
    dplyr::mutate(
      exclusion_reason_label = clean_reason_label(.data$exclusion_reason),
      share = .data$n_excluded_reporting_units /
        sum(.data$n_excluded_reporting_units)
    ) |>
    dplyr::arrange(.data$n_excluded_reporting_units)
  
  save_plot("audit_excluded_units_by_reason", function() {
    
    old_par <- graphics::par(
      mar = c(5.2, 11.5, 3.5, 1.5)
    )
    on.exit(graphics::par(old_par), add = TRUE)
    
    graphics::barplot(
      height = reason_plot$n_excluded_reporting_units,
      names.arg = reason_plot$exclusion_reason_label,
      horiz = TRUE,
      las = 1,
      xlab = "Excluded municipality-election units",
      main = "Audited exclusions by reason",
      cex.names = 0.85
    )
    
    graphics::grid(
      nx = NULL,
      ny = NA,
      lty = "dotted"
    )
  }, width = 9, height = 5.5)
  
  # 5. Excluded reporting units by municipality size class.
  # Categories are ordered substantively by electorate size, not by frequency.
  
  size_order <- c(
    "<1,000",
    "1,000--4,999",
    "5,000--9,999",
    "10,000--49,999",
    "50,000+",
    "Missing registered"
  )
  
  size_plot <- audit_results$audit_summary_size |>
    dplyr::mutate(
      municipality_size_class_label =
        clean_size_label(.data$municipality_size_class),
      municipality_size_class_label = factor(
        .data$municipality_size_class_label,
        levels = size_order
      )
    ) |>
    dplyr::filter(!is.na(.data$municipality_size_class_label)) |>
    dplyr::arrange(.data$municipality_size_class_label)
  
  # For a horizontal base-R barplot, reversing the order makes the smallest
  # municipality-size class appear at the top of the figure.
  size_plot_horizontal <- size_plot |>
    dplyr::arrange(dplyr::desc(.data$municipality_size_class_label))
  
  save_plot("audit_excluded_units_by_size_class", function() {
    
    old_par <- graphics::par(
      mar = c(5.2, 8.5, 3.5, 1.5)
    )
    on.exit(graphics::par(old_par), add = TRUE)
    
    graphics::barplot(
      height = size_plot_horizontal$n_excluded_reporting_units,
      names.arg = as.character(size_plot_horizontal$municipality_size_class_label),
      horiz = TRUE,
      las = 1,
      xlab = "Excluded municipality-election units",
      main = "Audited exclusions by municipality size",
      cex.names = 0.9
    )
    
    graphics::grid(
      nx = NULL,
      ny = NA,
      lty = "dotted"
    )
  }, width = 8.5, height = 5.5)
  
  # 6. Top raw province labels by exclusions.
  # Raw province is source-side information. Missing values are retained as a category
  # so that excluded units without a raw province field are not silently dropped.
  
  if ("audit_summary_province_raw" %in% names(audit_results)) {
    
    top_raw_provinces_plot <- audit_results$audit_summary_province_raw |>
      dplyr::mutate(
        province_raw_ref = dplyr::case_when(
          is.na(.data$province_raw_ref) ~ "(missing raw province)",
          .data$province_raw_ref == "" ~ "(missing raw province)",
          TRUE ~ as.character(.data$province_raw_ref)
        )
      ) |>
      dplyr::arrange(dplyr::desc(.data$n_excluded_reporting_units)) |>
      dplyr::slice_head(n = 10) |>
      dplyr::arrange(.data$n_excluded_reporting_units)
    
    if (nrow(top_raw_provinces_plot) > 0) {
      
      save_plot("audit_top_raw_province_labels_excluded", function() {
        
        old_par <- graphics::par(
          mar = c(5.2, 8.5, 3.5, 1.5)
        )
        on.exit(graphics::par(old_par), add = TRUE)
        
        graphics::barplot(
          height = top_raw_provinces_plot$n_excluded_reporting_units,
          names.arg = top_raw_provinces_plot$province_raw_ref,
          horiz = TRUE,
          las = 1,
          xlab = "Excluded municipality-election units",
          main = "Top raw province labels by audited exclusions",
          cex.names = 0.9
        )
        
        graphics::grid(
          nx = NULL,
          ny = NA,
          lty = "dotted"
        )
      }, width = 8.5, height = 5.5)
    }
  }
  
  invisible(output_dir)
}