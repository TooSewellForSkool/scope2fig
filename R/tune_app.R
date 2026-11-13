# tune_app.R — Shiny app for Module 3 (B&C per experiment / group)
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.

tune_app <- function(cfg, inc, units, pp) {
  s <- asNamespace("shiny"); tags <- shiny::tags; HTML <- shiny::HTML
  keys <- names(units)
  unit_label <- function(k) sprintf("%s  (%d image%s)%s", k, length(units[[k]]$uids),
                                    ifelse(length(units[[k]]$uids) == 1, "", "s"),
                                    ifelse(units[[k]]$saved, "  — saved", ""))
  img_label <- function(uids) {
    m <- inc[match(uids, inc$image_uid), ]
    f <- intersect(c("experimental_id", "animal_replicate", "section_coverslip", "fov"), names(m))
    extra <- if (length(f)) apply(m[, f, drop = FALSE], 1, function(r) paste(stats::na.omit(r[nzchar(r)]), collapse = " · "))
             else rep("", length(uids))
    stats::setNames(uids, ifelse(nzchar(extra), paste0(uids, "  [", extra, "]"), uids))
  }
  sid <- function(prefix, k, i = NULL) paste0(prefix, safe_uid(k), if (!is.null(i)) paste0("_", i))

  ui <- s$fluidPage(
    tags$head(tags$style(HTML("
      body { font-size: 13px; }
      .topbar { display:flex; justify-content:space-between; align-items:center; margin:10px 0; }
      .guide { background:#f4f8fd; border:1px solid #cfe0f5; border-radius:6px; padding:8px 14px; margin:6px 0 10px; }
      .guide summary { cursor:pointer; font-weight:600; font-size:14px; }
      .guide li { margin:3px 0; }
      .ch-box { border-left:6px solid #888; background:#fafafa; padding:6px 10px 2px; margin-bottom:8px; border-radius:4px; }
      .ch-box h5 { margin:2px 0 4px; font-weight:700; }
      .ch-box .form-group { margin-bottom:2px; }
      .sat { font-size:12px; color:#555; } .sat.bad { color:#c62828; font-weight:600; }
      .red { color:#c62828; font-weight:700; }
      .num-row { display:flex; gap:8px; align-items:center; } .num-row .form-group { margin-bottom:0; }
      .num-row label { font-weight:400; font-size:12px; margin:0 2px 0 0; }
      .viewer img { max-width:100%; height:auto; background:#000; border-radius:4px; }
      .warn { background:#fff4e5; border-left:4px solid #f0a020; padding:6px 10px; margin:6px 0; }
    "))),
    tags$div(class = "topbar",
      tags$h3(paste("Brightness & contrast —", cfg$session_name)),
      tags$div(s$actionButton("save", "Save", class = "btn-primary"),
               s$actionButton("save_close", "Save & close", class = "btn-success"))),
    tags$details(class = "guide", open = NA,
      tags$summary("How to use this app (start here)"),
      tags$ol(
        tags$li(tags$b("Pick a group"), " at the top. Every image in a group gets the same settings, so they stay comparable."),
        tags$li(tags$b("Adjust each channel's range"), " by typing exact ", tags$b("min"), " / ", tags$b("max"),
                " values (best for dim channels) or dragging the slider - they stay in sync. Pixels at or below min are ",
                "black; at or above max are full brightness. The histogram shows where the signal is (dashed lines = min/max)."),
        tags$li(tags$b("Watch the saturation readout"), " under each slider: ", tags$i("% of pixels clipped at max"),
                " in this image / in the worst image of the group. It turns ", tags$span(class = "red", "red"),
                " when more than 1% of pixels are clipped in any image - raise max (or accept it knowingly)."),
        tags$li(tags$b("Check several images"), " with the image menu or ", tags$i("Show whole group"),
                " so the settings work for the brightest and dimmest images, not just one."),
        tags$li(tags$b("Untick 'in merge'"), " for any channel you don't want in the merged image (it still gets its own panel)."),
        tags$li(tags$b("Save"), " — each visited group gets a settings file. Only linear min/max is used (no gamma), ",
                "so the adjustment is the same kind you'd make in Fiji's B&C window."),
        tags$li(tags$small("Previews are max projections downsampled to ≤512 px; saturation % is approximate. ",
                           "The final images are rendered from the full-resolution data.")))),
    s$uiOutput("status"),
    s$fluidRow(
      s$column(5, s$selectInput("unit", "Group (experiment / group)", choices = stats::setNames(keys, vapply(keys, unit_label, "")),
                                width = "100%")),
      s$column(4, s$selectInput("img", "Image", choices = NULL, width = "100%")),
      s$column(3, s$radioButtons("panel_mode", "Channel panels", c("Grayscale" = "gray", "Color" = "color"), inline = TRUE),
               s$checkboxInput("show_group", "Show whole group", FALSE))),
    s$fluidRow(
      s$column(4,
        s$uiOutput("controls"),
        s$wellPanel(
          s$actionButton("auto_all", "Auto (all channels)"),
          s$actionButton("reset_saved", "Revert to saved"),
          tags$hr(),
          s$fluidRow(s$column(8, s$selectInput("copy_from", "Copy settings from group", choices = NULL)),
                     s$column(4, tags$label(HTML("&nbsp;")), s$actionButton("copy_go", "Copy"))),
          s$fileInput("load_json", "Load settings file (.json from any run)", accept = ".json"),
          tags$small("Copying/loading matches channels by name, then by position."))),
      s$column(8, tags$div(class = "viewer",
        s$conditionalPanel("!input.show_group",
          s$imageOutput("merge_img", height = "auto"), tags$br(),
          s$imageOutput("strip_img", height = "auto")),
        s$conditionalPanel("input.show_group", s$plotOutput("group_plot", height = "auto")))))
  )

  server <- function(input, output, session) {
    rv <- s$reactiveValues(settings = lapply(units, `[[`, "settings"), visited = character(),
                           edited = character(), version = 0)
    cache <- new.env()
    preview <- function(uid) {
      if (is.null(cache[[uid]])) cache[[uid]] <- read_preview(pp, uid)
      cache[[uid]]
    }
    closing <- FALSE
    session$onSessionEnded(function() if (!closing) shiny::stopApp(invisible(list(saved = FALSE))))
    made_auto <- character()

    cur <- s$reactive({ shiny::req(input$unit); units[[input$unit]] })
    st_now <- s$reactive(rv$settings[[shiny::req(input$unit)]])
    st_d <- shiny::debounce(st_now, 150)

    s$observeEvent(input$unit, {
      u <- cur()
      s$updateSelectInput(session, "img", choices = img_label(u$uids))
      s$updateSelectInput(session, "copy_from", choices = setdiff(keys, u$key))
      rv$visited <- union(rv$visited, u$key)
    })

    # ---- per-channel controls (rebuilt when the group changes)
    output$controls <- s$renderUI({
      u <- cur(); st <- s$isolate(rv$settings[[u$key]]); ch <- u$channels
      lapply(seq_len(nrow(st)), function(i) {
        rmax <- ch$range_max[i]; step <- slider_step(rmax)
        tags$div(class = "ch-box", style = paste0("border-left-color:", color_hex(st$color[i]), ";"),
          tags$h5(sprintf("C%d  %s", st$channel_index[i], st$label[i]),
                  if (!is.na(st$dye_name[i]) && st$dye_name[i] != st$label[i])
                    tags$small(style = "font-weight:400;color:#777", paste0(" (", st$dye_name[i], ")"))),
          tags$div(class = "num-row",
            s$numericInput(sid("nmin_", u$key, i), "min", value = st$min[i], min = 0, step = step, width = "110px"),
            s$numericInput(sid("nmax_", u$key, i), "max", value = st$max[i], min = 0, step = step, width = "110px"),
            tags$small(style = "color:#777", sprintf("data up to %s", format(rmax, big.mark = ",")))),
          s$sliderInput(sid("rng_", u$key, i), NULL, min = 0, max = rmax, step = step,
                        value = c(st$min[i], st$max[i]), width = "100%"),
          s$plotOutput(sid("hist_", u$key, i), height = "70px"),
          s$fluidRow(
            s$column(5, s$checkboxInput(sid("mrg_", u$key, i), "in merge", isTRUE(st$in_merge[i]))),
            s$column(5, s$uiOutput(sid("sat_", u$key, i))),
            s$column(2, s$actionButton(sid("auto_", u$key, i), "Auto", class = "btn-xs"))))
      })
    })

    # slider / typed values / checkbox -> settings.
    # `last` remembers what each control last reported, so only a real change by the user
    # counts (prevents the slider and the number boxes from fighting while you drag).
    last <- new.env()
    s$observe({
      u <- cur(); k <- u$key; st <- s$isolate(rv$settings[[k]]); changed <- FALSE
      for (i in seq_len(nrow(st))) {
        stp <- slider_step(u$channels$range_max[i])
        rid <- sid("rng_", k, i); nid <- sid("num_", k, i)
        r <- input[[rid]]; m <- input[[sid("mrg_", k, i)]]
        n <- c(input[[sid("nmin_", k, i)]], input[[sid("nmax_", k, i)]])
        n_ok <- length(n) == 2 && all(is.finite(n))
        typed   <- n_ok && !is.null(last[[nid]]) && any(n != last[[nid]])
        dragged <- !is.null(r) && !is.null(last[[rid]]) && any(abs(r - last[[rid]]) > stp / 2)
        if (n_ok) last[[nid]] <- n
        if (!is.null(r)) last[[rid]] <- r
        if (typed) {                       # typed values win and are kept exactly
          st$min[i] <- max(0, n[1]); st$max[i] <- max(n[2], st$min[i] + 1e-6); changed <- TRUE
          last[[rid]] <- c(st$min[i], st$max[i])
          s$updateSliderInput(session, rid, value = c(st$min[i], st$max[i]))
        } else if (dragged) {
          st$min[i] <- r[1]; st$max[i] <- max(r[2], r[1] + stp); changed <- TRUE
          last[[nid]] <- c(st$min[i], st$max[i])
          s$updateNumericInput(session, sid("nmin_", k, i), value = st$min[i])
          s$updateNumericInput(session, sid("nmax_", k, i), value = st$max[i])
        }
        if (!is.null(m) && m != isTRUE(st$in_merge[i])) { st$in_merge[i] <- m; changed <- TRUE }
      }
      if (changed) s$isolate({ rv$settings[[k]] <- st; rv$edited <- union(rv$edited, k) })
    })

    push_to_inputs <- function(k, st) {
      for (i in seq_len(nrow(st))) {
        last[[sid("rng_", k, i)]] <- c(st$min[i], st$max[i]); last[[sid("num_", k, i)]] <- c(st$min[i], st$max[i])
        s$updateSliderInput(session, sid("rng_", k, i), value = c(st$min[i], st$max[i]))
        s$updateNumericInput(session, sid("nmin_", k, i), value = st$min[i])
        s$updateNumericInput(session, sid("nmax_", k, i), value = st$max[i])
        s$updateCheckboxInput(session, sid("mrg_", k, i), value = isTRUE(st$in_merge[i]))
      }
    }
    set_settings <- function(k, st, note) {
      rv$settings[[k]] <- st; rv$edited <- union(rv$edited, k); push_to_inputs(k, st)
      if (!is.null(note)) s$showNotification(note)
    }

    # per-channel histograms, saturation and Auto buttons
    s$observe({
      u <- cur(); k <- u$key
      for (i in seq_len(nrow(u$channels))) local({
        ii <- i
        output[[sid("hist_", k, ii)]] <- s$renderPlot({
          st <- st_d(); chs <- preview(shiny::req(input$img)); if (is.null(chs) || ii > length(chs)) return(NULL)
          v <- chs[[ii]]; rmax <- u$channels$range_max[ii]
          br <- seq(0, max(rmax, max(v)) + 1e-9, length.out = 80)
          h <- graphics::hist(pmin(v, max(br)), breaks = br, plot = FALSE)
          graphics::par(mar = c(1.6, 0.5, 0.2, 0.5), mgp = c(1, 0.3, 0), cex.axis = 0.7, tcl = -0.2)
          graphics::plot(h$mids, log10(h$counts + 1), type = "h", lwd = 3, col = color_hex(st$color[ii]),
                         xlim = c(0, rmax), yaxt = "n", xlab = "", ylab = "", bty = "n")
          graphics::abline(v = c(st$min[ii], st$max[ii]), col = c("#555555", "#c62828"), lty = 2, lwd = 1.5)
        }, bg = "transparent")
        output[[sid("sat_", k, ii)]] <- s$renderUI({
          st <- st_d(); hi <- st$max[ii]
          fr <- vapply(u$uids, function(x) { c <- preview(x); if (is.null(c) || ii > length(c)) NA else mean(c[[ii]] >= hi) }, 1)
          this <- fr[match(input$img, u$uids)]; worst <- max(fr, na.rm = TRUE)
          tags$span(class = paste("sat", if (isTRUE(worst > 0.01)) "bad"),
                    title = "% of pixels at or above max (clipped): this image / worst image in the group. Red above 1%.",
                    sprintf("saturation %.1f%% / %.1f%%", 100 * this, 100 * worst))
        })
        id <- sid("auto_", k, ii)
        if (!id %in% made_auto) {
          made_auto <<- c(made_auto, id)
          s$observeEvent(input[[id]], {
            st <- rv$settings[[k]]; st$min[ii] <- 0; st$max[ii] <- units[[k]]$defaults$max[ii]
            set_settings(k, st, NULL)
          }, ignoreInit = TRUE)
        }
      })
    })

    s$observeEvent(input$auto_all, {
      k <- cur()$key; st <- rv$settings[[k]]
      st$min <- 0; st$max <- units[[k]]$defaults$max
      set_settings(k, st, "Auto range applied (0 to the 99.9th percentile, typical across the group).")
    })
    s$observeEvent(input$reset_saved, {
      u <- cur()
      if (!file.exists(u$file)) return(s$showNotification("No saved settings for this group yet.", type = "warning"))
      set_settings(u$key, apply_settings(rv$settings[[u$key]], read_unit_settings(u$file)), "Reverted to saved settings.")
    })
    s$observeEvent(input$copy_go, {
      k <- cur()$key; src <- shiny::req(input$copy_from)
      set_settings(k, apply_settings(rv$settings[[k]], rv$settings[[src]]), paste("Copied from", src))
    })
    s$observeEvent(input$load_json, {
      k <- cur()$key
      src <- tryCatch(read_unit_settings(input$load_json$datapath), error = function(e) NULL)
      if (is.null(src)) return(s$showNotification("Couldn't read that settings file.", type = "error"))
      set_settings(k, apply_settings(rv$settings[[k]], src), "Loaded settings from file.")
    })

    # ---- viewer
    output$merge_img <- s$renderImage({
      st <- st_d(); chs <- preview(shiny::req(input$img))
      shiny::validate(shiny::need(!is.null(chs), "No preview for this image."))
      list(src = png_file(compose_merge(chs, st)), contentType = "image/png", alt = "merge")
    }, deleteFile = TRUE)
    output$strip_img <- s$renderImage({
      st <- st_d(); chs <- preview(shiny::req(input$img)); shiny::req(chs)
      pan <- lapply(seq_len(min(nrow(st), length(chs))), function(i)
        downsample(compose_channel(chs[[i]], st$min[i], st$max[i],
                                   if (input$panel_mode == "color") st$color[i] else NULL), 256))
      list(src = png_file(strip_arrays(pan)), contentType = "image/png", alt = "channels")
    }, deleteFile = TRUE)
    output$group_plot <- s$renderPlot({
      st <- st_d(); uids <- cur()$uids; n <- length(uids); nc <- min(3, n); nr <- ceiling(n / nc)
      graphics::par(mfrow = c(nr, nc), mar = c(0.3, 0.3, 1.6, 0.3), bg = "white")
      lab <- names(img_label(uids))
      for (j in seq_along(uids)) {
        chs <- preview(uids[j]); graphics::plot.new()
        if (is.null(chs)) next
        a <- downsample(compose_merge(chs, st), 300)
        graphics::plot.window(xlim = c(0, dim(a)[2]), ylim = c(0, dim(a)[1]), asp = 1)
        graphics::rasterImage(a, 0, 0, dim(a)[2], dim(a)[1])
        graphics::title(lab[j], cex.main = 0.75, font.main = 1)
      }
    }, height = function() 300 * ceiling(length(cur()$uids) / 3))

    # ---- status + save
    output$status <- s$renderUI({
      unset <- keys[!vapply(keys, function(k) file.exists(units[[k]]$file), TRUE) & !keys %in% rv$visited]
      unsaved <- rv$edited
      u <- units[[shiny::req(input$unit)]]
      tags$div(
        tags$div(style = "margin:4px 0;color:#555",
          if (u$saved) sprintf("This group started from its saved settings (saved %s).", u$saved_at)
          else "This group has no saved settings yet - it started from automatic values."),
        if (length(unsaved)) tags$div(class = "warn", "Unsaved changes in: ", paste(unsaved, collapse = "; ")),
        if (length(unset)) tags$div(class = "warn", "Groups not yet set (they will be skipped when rendering): ",
                                    paste(unset, collapse = "; ")))
    })
    do_save <- function() {
      # write groups you changed, plus groups you looked at that have no settings file yet;
      # an existing file is never overwritten just because you viewed that group
      to_write <- union(rv$edited, rv$visited[!vapply(rv$visited, function(k) file.exists(units[[k]]$file), TRUE)])
      if (!length(to_write)) return(s$showNotification("Nothing changed - no files written."))
      for (k in to_write) write_unit_settings(cfg, units[[k]], rv$settings[[k]])
      write_settings_summary(cfg, units, rv$settings)
      write_run_log(cfg, "tune", list(saved_groups = to_write,
        settings = lapply(rv$settings[to_write], function(st) st[, c("label", "min", "max", "in_merge")])))
      rv$edited <- character()
      s$showNotification(sprintf("Saved settings for %d group(s) to %s", length(to_write), settings_dir(cfg)))
    }
    s$observeEvent(input$save, do_save())
    s$observeEvent(input$save_close, { do_save(); closing <<- TRUE; shiny::stopApp(invisible(list(saved = TRUE))) })
  }

  shiny::shinyApp(ui, server)
}
