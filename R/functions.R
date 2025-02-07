# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# functions used in order of appearance

detach_package <- function(pkg){
  detach(
    paste0('package:', pkg),
    character.only = T,
    unload = T,
    force = T
  )
}
load_package <- function(pkg){
  require(pkg, character.only = TRUE)
}
read_naics <- function(pttrn, var) {
  file_names <- list.files(here::here("data","current"), pattern = pttrn)
  vroom::vroom(here::here("data","current", file_names)) %>%
    janitor::clean_names() %>%
    rename(
      name = {{ var }},
      value = count
    ) %>%
    na.omit()%>%
    filter(naics_5 != "Unknown") %>%
    mutate(
      date = lubridate::ymd(paste(syear, smth, "01", sep = "/"))
    ) %>%
    select(-syear, -smth)
}
agg_level <- function(tbbl, var) {
  tbbl %>%
    group_by({{ var }}, name, date) %>%
    summarize(value = sum(value, na.rm=TRUE)) %>%
    group_by({{ var }}, .add = FALSE) %>%
    nest() %>%
    rename(industry = {{ var }})
}
add_vars <- function(tbbl) {
  tbbl %>%
    pivot_wider(names_from = name, values_from = value) %>%
    mutate(
      labour_force = Employed + Unemployed,
      unemployment_rate = Unemployed / labour_force
    ) %>%
    select(-Unknown)%>%
    pivot_longer(cols = -date, names_to = "name", values_to = "value")
}

fix_column_names <- function(tbbl){
  colnames(tbbl) <- make_title(colnames(tbbl))
  return(tbbl)
}

make_title <- function(strng){
  strng <- str_to_title(str_replace_all(strng,"_"," "))
}

get_values <- function(tbbl, num_months) {
  tbbl %>%
    group_by(name) %>%
    filter(near(date, max(date) - months(num_months), tol = 7)) %>%
    #    mutate(value=if_else(value<1500, NA_real_, value))%>% #suppress values
    select(value)
}

ytd_ave <- function(tbbl, num_years) {
  start <- floor_date(max(tbbl$date), unit = "year") - years(num_years)
  end <- max(tbbl$date) - years(num_years)
  tbbl %>%
    group_by(name) %>%
    filter(date >= start & date <= end) %>%
    summarize(ytd_ave = mean(value))
}

apply_formatting <- function(tbbl){
  tbbl|>
    rename(characteristic=name)|>
    arrange(characteristic)|>
    relocate(characteristic, .before = everything())|>
    mutate(industry=as.character(industry),
           industry=case_when(high=="high" ~ industry,
                              medium=="medium" ~ paste0("    ", industry),
                              low=="low" ~ paste0("        ", industry)
           )
    )|>
    select(-high, -medium, -low)%>%
    mutate(characteristic = ifelse(duplicated(characteristic), NA, characteristic))
}

clean_up <- function(tbbl) {
  tbbl %>%
    mutate(characteristic = str_to_title(str_replace_all(characteristic, "_", " "))) %>%
    select(
      Characteristic = characteristic,
      Industry = industry,
      "{previous_year}" := previous_year,
      "{previous_month}" := previous_month,
      "{current}" := current,
      "Jan-{previous_year}" := previous_ytd_average,
      "Jan-{current}" := current_ytd_average,
      `Y/Y` = level_change_year,
      `M/M` = level_change_month,
      `YTD/YTD` = level_change_ytd,
      `Y/Y%` = percent_change_year,
      `M/M%` = percent_change_month,
      `YTD/YTD%` = percent_change_ytd
    )
}
write_sheet <- function(tbbl, long_name) {
  sheet_name <- str_trunc(long_name, width = 31) # excel cant handle sheet names longer than this
  cloneSheet(wb, "layout", sheet_name)
  writeWorksheet( # add the industry name to top
    wb,
    long_name,
    sheet_name,
    startRow = 1,
    startCol = 2,
    header = FALSE,
    rownames = FALSE
  )
  writeWorksheet( # add the maximum date in the data
    wb,
    current,
    sheet_name,
    startRow = 2,
    startCol = 2,
    header = FALSE,
    rownames = FALSE
  )
  writeWorksheet( # add when the sheet was written
    wb,
    now(),
    sheet_name,
    startRow = 3,
    startCol = 2,
    header = FALSE,
    rownames = FALSE
  )
  writeWorksheet( # add the data
    wb,
    tbbl,
    sheet_name,
    startRow = 5,
    startCol = 1,
    header = TRUE,
    rownames = FALSE
  )
}
#for dashboard---------------------



level_change_plot <- function(shared_df){
  plot_ly(shared_df,
          x =~ level_change_year,
          y =~ level_change_month,
          hoverinfo="text",
          hovertext = ~industry,
          type = "scatter",
          mode = 'markers',
          marker = list(size = ~ 2+20*(current-min(current, na.rm=TRUE))/
                          (max(current, na.rm=TRUE)-min(current, na.rm=TRUE)), opacity = 0.5))%>%
    layout(xaxis = list(title = "Level Change Year"),
           yaxis = list(title = "Level Change Month")
    )
}


percent_change_plot <- function(shared_df){
  plot_ly(shared_df,
          x =~ percent_change_year,
          y =~ percent_change_month,
          hoverinfo="text",
          hovertext = ~industry,
          type = "scatter",
          mode = 'markers',
          marker = list(size = ~ 2+20*(current-min(current, na.rm=TRUE))
                        /(max(current, na.rm=TRUE)-min(current, na.rm=TRUE)), opacity = 0.5))%>%
    layout(xaxis = list(title = "Percent Change Year", tickformat = '1%', range=c(-1,1)),
           yaxis = list(title = "Percent Change Month", tickformat = '1%', range=c(-1,1))
    )
}

my_heatmap <- function(var){
  tbbl <- for_heatmaps%>%
    filter(name==var)%>%
    ungroup()|>
    select(data)%>%
    unnest(data)%>%
    column_to_rownames(var="industry")
  heatmaply(tbbl,
            scale="row",
            dendrogram = "row",
            custom_hovertext = tbbl,
            fontsize_col = 6,
            column_text_angle = 90, main = var)
}



mybiplot <- function (pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE,
                      obs.scale = 1 - scale, var.scale = scale, groups = NULL,
                      ellipse = FALSE, ellipse.prob = 0.68, labels = NULL, labels.size = 3,
                      alpha = 1, var.axes = TRUE, circle = FALSE, circle.prob = 0.69,
                      varname.size = 3, varname.adjust = 0, varname.abbrev = FALSE,
                      arrow.alpha=.1, alpha.var=1, axis.text.size=10, ...){
  library(ggplot2)
  library(plyr)
  library(scales)
  library(grid)
  stopifnot(length(choices) == 2)
  if (inherits(pcobj, "prcomp")) {
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1/(d * nobs.factor), FUN = "*")
    v <- pcobj$rotation
  }
  else if (inherits(pcobj, "princomp")) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1/(d * nobs.factor), FUN = "*")
    v <- pcobj$loadings
  }
  else if (inherits(pcobj, "PCA")) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1/(d * nobs.factor), FUN = "*")
    v <- sweep(pcobj$var$coord, 2, sqrt(pcobj$eig[1:ncol(pcobj$var$coord),
                                                  1]), FUN = "/")
  }
  else if (inherits(pcobj, "lda")) {
    nobs.factor <- sqrt(pcobj$N)
    d <- pcobj$svd
    u <- predict(pcobj)$x/nobs.factor
    v <- pcobj$scaling
    d.total <- sum(d^2)
  }
  else {
    stop("Expected a object of class prcomp, princomp, PCA, or lda")
  }
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[, choices], 2, d[choices]^obs.scale,
                              FUN = "*"))
  v <- sweep(v, 2, d^var.scale, FUN = "*")
  df.v <- as.data.frame(v[, choices])
  names(df.u) <- c("xvar", "yvar")
  names(df.v) <- names(df.u)
  if (pc.biplot) {
    df.u <- df.u * nobs.factor
  }
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
  v.scale <- rowSums(v^2)
  df.v <- r * df.v/sqrt(max(v.scale))
  if (obs.scale == 0) {
    u.axis.labs <- paste("standardized PC", choices, sep = "")
  }
  else {
    u.axis.labs <- paste("PC", choices, sep = "")
  }
  u.axis.labs <- paste(u.axis.labs, sprintf("(%0.1f%% explained var.)",
                                            100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
  if (!is.null(labels)) {
    df.u$labels <- labels
  }
  if (!is.null(groups)) {
    df.u$groups <- groups
  }
  if (varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  }
  else {
    df.v$varname <- rownames(v)
  }
  df.v$angle <- with(df.v, (180/pi) * atan(yvar/xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar))/2)
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + xlab(u.axis.labs[1]) +
    ylab(u.axis.labs[2]) + coord_equal()
  if (var.axes) {
    if (circle) {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi,
                                                length = 50))
      circle <- data.frame(xvar = r * cos(theta), yvar = r *
                             sin(theta))
      g <- g + geom_path(data = circle, color = muted("white"),
                         size = 1/2, alpha = 1/3)
    }
    g <- g + geom_segment(data = df.v, aes(x = 0, y = 0,
                                           xend = xvar, yend = yvar), arrow = arrow(length = unit(1/2,
                                                                                                  "picas")), alpha=arrow.alpha, color = muted("red"))
  }
  if (!is.null(df.u$labels)) {
    if (!is.null(df.u$groups)) {
      g <- g + geom_text(aes(label = labels, color = groups),
                         size = labels.size)
    }
    else {
      g <- g + geom_text(aes(label = labels), size = labels.size)
    }
  }
  else {
    if (!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups), alpha = alpha)
    }
    else {
      g <- g + geom_point(alpha = alpha)
    }
  }
  if (!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))
    ell <- ddply(df.u, "groups", function(x) {
      if (nrow(x) <= 2) {
        return(NULL)
      }
      sigma <- var(cbind(x$xvar, x$yvar))
      mu <- c(mean(x$xvar), mean(x$yvar))
      ed <- sqrt(qchisq(ellipse.prob, df = 2))
      data.frame(sweep(circle %*% chol(sigma) * ed, 2,
                       mu, FUN = "+"), groups = x$groups[1])
    })
    names(ell)[1:2] <- c("xvar", "yvar")
    g <- g + geom_path(data = ell, aes(color = groups, group = groups))
  }
  if (var.axes) {
    g <- g + geom_text(data = df.v, aes(label = varname,
                                        x = xvar, y = yvar, angle = angle, hjust = hjust),
                       alpha=alpha.var, color = "darkred", size = varname.size)
  }
  g <- g+
    theme(axis.title=element_text(size=axis.text.size))

  return(g)
}

biplot_wrapper <- function(pcs){
  rownames(pcs$x) <- str_replace_all(word(rownames(pcs$x), 1),",","")
  plt <- mybiplot(pcs, labels=rownames(pcs$x), labels.size=1.5, varname.size=1.5)+
    theme_minimal()+
    theme(plot.margin = unit(c(-.5, -2, 0, -2), "cm"))+
    scale_x_continuous(expand = expansion(mult = 0.15))+
    scale_y_continuous(expand = expansion(mult = 0.15))

}
get_biplot <- function(thing){
  plt <- for_pca%>%
    filter(name==thing)%>%
    pull(biplot)
  plt[[1]]
}

my_heatmap <- function(var){
  tbbl <- for_heatmaps%>%
    filter(name==var)%>%
    ungroup()|>
    select(-name, -agg_level)%>%
    unnest(data)%>%
    column_to_rownames(var="industry")
  heatmaply(tbbl,
            scale="row",
            dendrogram = "row",
            custom_hovertext = tbbl,
            fontsize_col = 6,
            column_text_angle = 90, main = var)
}

area_plot <- function(thing, ind){
  for_ts_plots %>%
    filter(name==thing,
           parent==ind)%>%
    filter(parent!=industry)%>%
    ggplot(aes(date,value, fill=industry))+
    geom_area()+
    scale_y_continuous(labels=scales::comma)+
    theme_minimal()+
    labs(x=NULL,
         y=NULL,
         title=thing,
         fill="")+
    theme(text = element_text(size = 6))+
    theme(legend.text = element_text(size = 4))
}

line_plot <- function(thing, ind){
  for_ts_plots %>%
    filter(name==thing,
           industry==ind)|>
    ggplot(aes(date, value))+
    geom_line()+
    scale_y_continuous(labels = scales::percent)+
    labs(x=NULL,
         y=NULL,
         title=str_to_title(str_replace_all(thing, "_"," ")))+
    theme_minimal()
}

make_patchwork <- function(page){
  employed <- area_plot("Employed", page)
  unemployed <- area_plot("Unemployed", page)
  full_time <- area_plot("Full-time", page)
  part_time <- area_plot("Part-time", page)
  unemployment_rate <- line_plot("unemployment_rate", page)
  combined <- ((employed+unemployed)/(full_time+part_time) | unemployment_rate) & theme(legend.position = "bottom")
  combined +
    plot_layout(guides = "collect")+
    plot_annotation(title = page, theme = theme(plot.title = element_text(size = 10)))
}

file.rename.wrapper <- function(wrong, correct){
  wrong_path <- here("data","current", wrong)
  correct_path <- here("data","current", correct)
  file.rename(wrong_path, correct_path)
}


