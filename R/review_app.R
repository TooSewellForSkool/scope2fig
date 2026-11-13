# review_app.R — Shiny app for Module 2
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
# Tabs: Images (edit fields, include, bulk-fill, thumbnails) | Experiments & channels |
#       Groups | Import. "Save" writes the manifest + channel map; "Save & close" also
#       returns control to run_pipeline().

review_app <- function(cfg, man, chan, cmap, pp) {
  s <- asNamespace("shiny"); dt <- asNamespace("DT")
  tags <- shiny::tags; HTML <- shiny::HTML
  uf <- unlist(cfg$user_fields)
  edit_fields <- c("experiment", "group", uf)
  color_choices <- c(names(COLOR_NAMES)[!names(COLOR_NAMES) %in% c("grey", "white")], "white")

  dir.create(pp$thumbs, recursive = TRUE, showWarnings = FALSE)
  res_name <- paste0("thumbs_", as.integer(stats::runif(1, 1, 1e8)))
  shiny::addResourcePath(res_name, pp$thumbs)
  thumb_html <- function(uids, h = 56) {
    f <- paste0(safe_uid(uids), ".png")
    ifelse(file.exists(file.path(pp$thumbs, f)),
           sprintf('<img src="%s/%s" style="height:%dpx;border-radius:3px">', res_name, f, h), "")
  }

  make_disp <- function(m, show_excluded) {
    obj <- ifelse(is.na(m$objective_mag), "", paste0(m$objective_mag, "x"))
    d <- data.frame(
      thumb = thumb_html(m$image_uid), image_uid = m$image_uid,
      include = ifelse(m$include %in% TRUE, "yes", "no"),
      experiment = m$experiment, group = m$group,
      m[, uf, drop = FALSE],
      series_name = m$series_name, type = m$acq_type, objective = obj,
      size = paste0(m$size_x, "x", m$size_y), z = m$size_z,
      flags = m$flags, .src = m$user_fields_source, .row = seq_len(nrow(m)),
      stringsAsFactors = FALSE, check.names = FALSE)
    if (!show_excluded) d <- d[m$include %in% TRUE, , drop = FALSE]
    d
  }

  ui <- s$fluidPage(
    tags$head(tags$style(HTML("
      body { font-size: 13px; }
      .topbar { display:flex; justify-content:space-between; align-items:center; margin:10px 0; }
      .warn { background:#fff4e5; border-left:4px solid #f0a020; padding:6px 10px; margin:6px 0; }
      .ok { background:#eaf7ee; border-left:4px solid #2e9e5b; padding:6px 10px; margin:6px 0; }
      .detail img { width:100%; border-radius:4px; margin-bottom:8px; }
      .detail table td { padding:2px 6px; vertical-align:top; }
      .swatch { display:inline-block; padding:2px 8px; margin:2px; border-radius:3px;
                background:#111; font-weight:600; }
      table.dataTable td { vertical-align: middle; }
      table.dataTable td input, table.dataTable td textarea { width:100% !important; max-width:100%;
                                                              box-sizing:border-box; }
      .guide { background:#f4f8fd; border:1px solid #cfe0f5; border-radius:6px; padding:8px 14px; margin:6px 0 10px; }
      .guide summary { cursor:pointer; font-weight:600; font-size:14px; }
      .guide ol { margin:8px 0 4px; padding-left:20px; } .guide li { margin:3px 0; }
      .howto { background:#f7f7f7; border-left:4px solid #5b8def; padding:6px 10px; margin:8px 0; }
      .cmap-row { display:flex; align-items:center; gap:10px; margin:2px 0; }
      .cmap-row .form-group { margin-bottom:0; }
      .cmap-ch { width:34px; font-weight:600; } .cmap-dye { width:130px; color:#555; }
    "))),
    tags$div(class = "topbar",
      tags$h3(paste("Review —", cfg$session_name)),
      tags$div(s$actionButton("save", "Save", class = "btn-primary"),
               s$actionButton("save_close", "Save & close", class = "btn-success"))),
    tags$details(class = "guide", open = NA,
      tags$summary("How to use this app (start here)"),
      tags$ol(
        tags$li(tags$b("Images tab — "), "check which images go forward. ", tags$i("include = no"),
                " images are skipped downstream (individual tiles are excluded when a stitched version exists). ",
                "Select rows and use Include/Exclude to change."),
        tags$li(tags$b("Images tab — "), "fill in your fields for each image: ",
                tags$code(paste(uf, collapse = ", ")), ". Fastest: filter or shift-click to select several rows, ",
                "pick a field and value at the top, then ", tags$i("Set for selected rows"), ". ",
                "Double-click a single cell to edit it. Yellow cells were guessed from the file name."),
        tags$li(tags$b("Experiments & channels tab — "), "give each channel its name (e.g. ALEXA 488 → PRPH) and ",
                "display color. An experiment is one staining panel; rename it to something meaningful."),
        tags$li(tags$b("Groups tab — "), "confirm the groups. Images in a group share B&C settings in the next ",
                "step, so a group should hold only images you want to compare directly."),
        tags$li(tags$b("Save & close"), " when the orange box below is empty (or you're happy with what it says). ",
                "You can re-open this app any time with ", tags$code('steps = "review"'), "; your edits are kept."))),
    s$uiOutput("status"),
    s$tabsetPanel(id = "tabs",
      s$tabPanel("Images",
        s$fluidRow(
          s$column(9,
            s$wellPanel(s$fluidRow(
              s$column(3, s$selectInput("bulk_field", "Field", choices = edit_fields, selected = "experimental_id")),
              s$column(3, s$selectizeInput("bulk_value", "Value", choices = NULL,
                                           options = list(create = TRUE, placeholder = "type or pick"))),
              s$column(6, tags$label(HTML("&nbsp;")), tags$div(
                s$actionButton("bulk_apply", "Set for selected rows"),
                s$actionButton("inc", "Include selected"),
                s$actionButton("exc", "Exclude selected"),
                s$actionButton("sel_clear", "Clear selection"))))),
            s$checkboxInput("show_excluded", "Show excluded images (e.g. individual tiles)", TRUE),
            tags$div(class = "howto", "Select rows (click; shift-click for a range; or use the filter boxes ",
              "under the headers, then select), then set a field above or include/exclude them. ",
              "Double-click a cell in experiment, group or your fields to edit one value. ",
              "Yellow = guessed from the file name, please confirm. Click a row to see details on the right."),
            dt$DTOutput("img_tbl")),
          s$column(3, s$uiOutput("detail")))),
      s$tabPanel("Experiments & channels",
        s$fluidRow(
          s$column(5, tags$h4("Experiments (staining panels)"),
            tags$div(class = "howto", "One experiment per staining panel, detected from the dyes. ",
              "Double-click a name to rename it. To move images into a different experiment, change their ",
              tags$i("experiment"), " on the Images tab."),
            dt$DTOutput("exp_tbl")),
          s$column(7, tags$h4("Channel names and colors"),
            tags$div(class = "howto", "Type the name you want on figures (usually the target, e.g. PRPH) and ",
              "pick a display color. These are used for B&C, merges and montage labels. ",
              "For a custom color, type a hex code like #FF8800 in the color box."),
            s$uiOutput("cmap_ui"), tags$hr(), tags$b("Preview"), s$uiOutput("cmap_preview")))),
      s$tabPanel("Groups",
        tags$div(class = "howto", "Each experiment / group gets its own B&C settings. Double-click a group ",
          "name to rename it; give two groups the same name to merge them. To split a group, select images on ",
          "the Images tab and set a new group name for them. A red ", tags$i("check"),
          " means the group mixes images acquired with different settings."),
        dt$DTOutput("grp_tbl")),
      s$tabPanel("Import",
        tags$p("Fill your fields from a spreadsheet you already have. The CSV needs an ",
               tags$code("image_uid"), " or ", tags$code("series_name"), " column plus any of: ",
               tags$code(paste(c(edit_fields, "include"), collapse = ", ")), "."),
        s$fileInput("imp_file", NULL, accept = ".csv"),
        s$verbatimTextOutput("imp_msg"))
    )
  )

  server <- function(input, output, session) {
    rv <- s$reactiveValues(man = man, cmap = cmap, dirty = FALSE)
    shown <- s$reactiveVal(NULL)
    proxy <- dt$dataTableProxy("img_tbl")
    closing <- FALSE   # closing the browser tab also ends the app (so R is never left waiting)
    session$onSessionEnded(function() if (!closing) shiny::stopApp(invisible(list(saved = !s$isolate(rv$dirty)))))

    refresh <- function() {
      d <- make_disp(rv$man, isTRUE(input$show_excluded)); shown(d)
      dt$replaceData(proxy, d, resetPaging = FALSE, rownames = FALSE, clearSelection = "none")
    }
    selected_rows <- function() { d <- shown(); i <- input$img_tbl_rows_selected; if (length(i)) d$.row[i] else integer() }

    # ---- images table
    output$img_tbl <- dt$renderDT({
      d <- make_disp(s$isolate(rv$man), isTRUE(input$show_excluded)); shown(d)
      editable_cols <- which(names(d) %in% edit_fields) - 1
      tbl <- dt$datatable(d, rownames = FALSE, escape = setdiff(names(d), "thumb"),
        selection = "multiple", filter = "top", extensions = "Scroller",
        editable = list(target = "cell", disable = list(columns = setdiff(seq_along(d) - 1, editable_cols))),
        options = list(pageLength = 50, scrollX = TRUE, scrollY = "60vh", scroller = TRUE, deferRender = TRUE,
                       columnDefs = list(list(visible = FALSE, targets = which(names(d) %in% c(".row", ".src")) - 1),
                                         list(orderable = FALSE, searchable = FALSE, targets = 0))))
      guessed <- intersect(uf, names(d))
      if (length(guessed))
        tbl <- dt$formatStyle(tbl, guessed, ".src",
                              backgroundColor = dt$styleEqual("parsed_from_name", "#fff3bf"))
      dt$formatStyle(tbl, "include", color = dt$styleEqual(c("yes", "no"), c("#2e7d32", "#9e9e9e")),
                     fontWeight = "bold")
    }, server = TRUE)

    s$observeEvent(input$img_tbl_cell_edit, {
      info <- input$img_tbl_cell_edit; d <- shown(); m <- rv$man
      for (k in seq_len(nrow(info))) {
        col <- names(d)[info$col[k] + 1]; r <- d$.row[info$row[k]]
        if (!col %in% edit_fields) next
        v <- trimws(as.character(info$value[k])); m[[col]][r] <- if (v == "") NA else v
        if (col %in% uf) m$user_fields_source[r] <- "user"
      }
      rv$man <- m; rv$dirty <- TRUE; refresh()
    })

    s$observeEvent(input$bulk_field, {
      vals <- sort(unique(stats::na.omit(rv$man[[input$bulk_field]])))
      s$updateSelectizeInput(session, "bulk_value", choices = vals, selected = character(), server = TRUE)
    })
    s$observeEvent(input$bulk_apply, {
      r <- selected_rows(); v <- trimws(input$bulk_value %||% "")
      if (!length(r)) return(s$showNotification("Select rows first.", type = "warning"))
      m <- rv$man; f <- input$bulk_field
      m[[f]][r] <- if (v == "") NA else v
      if (f %in% uf) m$user_fields_source[r] <- "user"
      rv$man <- m; rv$dirty <- TRUE; refresh()
      s$showNotification(sprintf("%s set for %d image(s).", f, length(r)))
    })
    set_include <- function(val) {
      r <- selected_rows()
      if (!length(r)) return(s$showNotification("Select rows first.", type = "warning"))
      m <- rv$man; m$include[r] <- val; rv$man <- m; rv$dirty <- TRUE; refresh()
    }
    s$observeEvent(input$inc, set_include(TRUE))
    s$observeEvent(input$exc, set_include(FALSE))
    s$observeEvent(input$sel_clear, dt$selectRows(proxy, NULL))

    output$detail <- s$renderUI({
      i <- input$img_tbl_row_last_clicked; d <- shown()
      if (is.null(i) || is.null(d) || i > nrow(d)) return(tags$p(tags$em("Click an image to see details.")))
      m <- rv$man[d$.row[i], ]
      row <- function(k, v) tags$tr(tags$td(tags$b(k)), tags$td(ifelse(is.na(v) | v == "", "—", as.character(v))))
      tags$div(class = "detail",
        HTML(thumb_html(m$image_uid, h = 260)),
        tags$table(
          row("Image", m$image_uid), row("Series", m$series_name),
          row("Type", m$acq_type), row("Objective", m$objective_model), row("Zoom", m$zoom),
          row("Size", paste0(m$size_x, " x ", m$size_y, " px, ", round(m$pixel_size_um, 3), " µm/px")),
          row("Z", if (isTRUE(m$size_z > 1)) sprintf("%d slices, %.2f µm step", m$size_z, m$z_step_um) else "single plane"),
          row("Tiles", if (!is.na(m$tile_role %||% NA)) paste(m$tile_role, m$tile_index %||% "") else ""),
          row("Dyes", m$dyes), row("Lasers", m$laser_summary), row("Flags", m$flags)),
        tags$p(tags$small("Thumbnail contrast is automatic, for recognition only.")))
    })

    # ---- experiments + channel map
    exp_table <- s$reactive({
      m <- rv$man
      do.call(rbind, lapply(unique(m$experiment), function(e) {
        mm <- m[m$experiment == e, ]
        data.frame(experiment = e, dyes = paste(unique(stats::na.omit(mm$dyes)), collapse = " ; "),
                   n_images = nrow(mm), n_included = sum(mm$include %in% TRUE), stringsAsFactors = FALSE)
      }))
    })
    output$exp_tbl <- dt$renderDT(dt$datatable(exp_table(), rownames = FALSE, selection = "none",
      editable = list(target = "cell", disable = list(columns = 1:3)), options = list(dom = "t", pageLength = 50)))
    s$observeEvent(input$exp_tbl_cell_edit, {
      info <- input$exp_tbl_cell_edit; old <- exp_table()$experiment[info$row]; new <- trimws(info$value)
      if (new == "" || new == old) return()
      m <- rv$man; m$experiment[m$experiment == old] <- new; rv$man <- m
      cm <- rv$cmap; cm$experiment[cm$experiment == old] <- new; rv$cmap <- cm
      rv$dirty <- TRUE; refresh()
    })
    # experiments created on the Images tab get a channel map automatically
    s$observe({
      e_new <- setdiff(unique(rv$man$experiment), rv$cmap$experiment)
      if (length(e_new)) {
        add <- build_channel_map(rv$man[rv$man$experiment %in% e_new, ], chan)
        s$isolate(rv$cmap <- rbind(rv$cmap, add))
      }
    })
    # channel form: re-drawn only when the set of experiments/channels changes, so typing isn't interrupted
    cmap_keys <- s$reactiveVal(character())
    s$observe({
      cm <- rv$cmap[rv$cmap$experiment %in% unique(rv$man$experiment), ]
      cmap_keys(paste(cm$experiment, cm$channel_index, sep = "::"))
    })
    key_id <- function(prefix, k) paste0(prefix, safe_uid(k))
    output$cmap_ui <- s$renderUI({
      keys <- cmap_keys(); cm <- s$isolate(rv$cmap)
      cm <- cm[match(keys, paste(cm$experiment, cm$channel_index, sep = "::")), ]
      lapply(split(seq_len(nrow(cm)), factor(cm$experiment, levels = unique(cm$experiment))), function(ix) {
        tags$div(tags$h5(tags$b(cm$experiment[ix[1]])),
          lapply(ix, function(i) {
            k <- keys[i]; cur <- cm$color[i]
            tags$div(class = "cmap-row",
              tags$span(class = "cmap-ch", paste0("C", cm$channel_index[i])),
              tags$span(class = "cmap-dye", ifelse(is.na(cm$dye_name[i]), "", cm$dye_name[i])),
              s$textInput(key_id("lab_", k), NULL, value = cm$label[i], width = "180px"),
              s$selectizeInput(key_id("col_", k), NULL, choices = unique(c(cur, color_choices)),
                               selected = cur, width = "150px", options = list(create = TRUE)))
          }))
      })
    })
    s$observe({
      keys <- cmap_keys(); cm <- s$isolate(rv$cmap); changed <- FALSE
      ck <- paste(cm$experiment, cm$channel_index, sep = "::")
      for (k in keys) {
        r <- match(k, ck); lab <- input[[key_id("lab_", k)]]; col <- input[[key_id("col_", k)]]
        if (is.na(r) || is.null(lab) || is.null(col)) next
        if (!identical(trimws(lab), cm$label[r])) { cm$label[r] <- trimws(lab); changed <- TRUE }
        if (nzchar(col) && !identical(col, cm$color[r])) {
          if (tolower(col) %in% names(COLOR_NAMES) || grepl("^#[0-9A-Fa-f]{6}$", col)) {
            cm$color[r] <- col; changed <- TRUE
          } else s$showNotification(paste0("Unknown color '", col, "' - use a name from the list or #RRGGBB."),
                                    type = "error")
        }
      }
      if (changed) { rv$cmap <- cm; rv$dirty <- TRUE }
    })
    output$cmap_preview <- s$renderUI({
      cm <- rv$cmap[rv$cmap$experiment %in% unique(rv$man$experiment), ]
      tags$div(lapply(split(cm, cm$experiment), function(e) tags$p(tags$b(e$experiment[1]), ": ",
        lapply(seq_len(nrow(e)), function(i) tags$span(class = "swatch", style = paste0("color:", color_hex(e$color[i])),
                                                        e$label[i])))))
    })

    # ---- groups
    grp_table <- s$reactive({
      m <- rv$man; key <- paste(m$experiment, m$group, sep = "\r")
      do.call(rbind, lapply(unique(key), function(k) {
        mm <- m[key == k, ]; inc <- mm[mm$include %in% TRUE, ]
        data.frame(experiment = mm$experiment[1], group = mm$group[1], n_images = nrow(mm),
          n_included = nrow(inc), type = paste(unique(mm$acq_type), collapse = ", "),
          objective = paste(unique(mm$objective_mag), collapse = ", "),
          size = paste(unique(paste0(mm$size_x, "x", mm$size_y)), collapse = ", "),
          z_slices = paste(unique(range(mm$size_z)), collapse = "-"),
          laser_profiles = paste(unique(stats::na.omit(mm$laser_profile)), collapse = ", "),
          check = if (length(unique(inc$comparability_key)) > 1) "⚠ mixes acquisition settings" else "ok",
          stringsAsFactors = FALSE)
      }))
    })
    output$grp_tbl <- dt$renderDT({
      dt$formatStyle(dt$datatable(grp_table(), rownames = FALSE, selection = "none",
        editable = list(target = "cell", disable = list(columns = c(0, 2:9))), options = list(dom = "t", pageLength = 100)),
        "check", color = dt$styleEqual("ok", "#2e7d32", default = "#c62828"))
    })
    s$observeEvent(input$grp_tbl_cell_edit, {
      info <- input$grp_tbl_cell_edit; g <- grp_table()[info$row, ]; new <- trimws(info$value)
      if (new == "") return()
      m <- rv$man; hit <- m$experiment == g$experiment & m$group == g$group
      m$group[hit] <- new; rv$man <- m; rv$dirty <- TRUE; refresh()
    })

    # ---- import
    s$observeEvent(input$imp_file, {
      x <- tryCatch(read_csv_safe(input$imp_file$datapath), error = function(e) NULL)
      if (is.null(x)) { output$imp_msg <- s$renderText("Could not read that file as CSV."); return() }
      key <- if ("image_uid" %in% names(x)) "image_uid" else if ("series_name" %in% names(x)) "series_name" else NA
      if (is.na(key)) { output$imp_msg <- s$renderText("Needs an image_uid or series_name column."); return() }
      m <- rv$man; idx <- match(m[[key]], x[[key]]); hit <- !is.na(idx)
      cols <- intersect(c(edit_fields, "include"), names(x)); n_set <- 0
      for (cc in cols) {
        v <- x[[cc]][idx[hit]]; ok <- !is.na(v) & as.character(v) != ""
        if (cc == "include") m$include[which(hit)[ok]] <- as.logical(v[ok])
        else m[[cc]][which(hit)[ok]] <- as.character(v[ok])
        if (cc %in% uf) m$user_fields_source[which(hit)[ok]] <- "imported"
        n_set <- n_set + sum(ok)
      }
      rv$man <- m; rv$dirty <- TRUE; refresh()
      output$imp_msg <- s$renderText(sprintf(
        "Matched %d of %d images on '%s'. Columns used: %s. Values filled: %d.\nUnmatched rows in your file: %d.",
        sum(hit), nrow(m), key, paste(cols, collapse = ", "), n_set, sum(!x[[key]] %in% m[[key]])))
    })

    # ---- status + save
    output$status <- s$renderUI({
      w <- review_warnings(rv$man, rv$cmap, cfg)
      tags$div(
        if (rv$dirty) tags$div(class = "warn", "Unsaved changes."),
        if (length(w)) tags$div(class = "warn", tags$b("Before the B&C step: "),
                                tags$ul(lapply(w, tags$li)))
        else tags$div(class = "ok", "No open issues."))
    })
    do_save <- function() {
      save_review(cfg, rv$man, rv$cmap); rv$dirty <- FALSE
      s$showNotification("Saved manifest and channel map.", type = "message")
    }
    s$observeEvent(input$save, do_save())
    s$observeEvent(input$save_close, { do_save(); closing <<- TRUE; shiny::stopApp(invisible(list(saved = TRUE))) })
  }

  shiny::shinyApp(ui, server)
}
