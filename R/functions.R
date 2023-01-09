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

#' Pre-processing Naics e.g.  1111-1121, 1200, 1220-1300, replace - with : and then separate by comma, return tibble.

fix_column_names <- function(tbbl){
  colnames(tbbl) <- wrapR::make_title(colnames(tbbl))
  return(tbbl)
}
separate_naics <- function(my_string) {
  vec <- my_string %>%
    str_replace_all("-", ":") %>%
    str_replace_all(" ", "") %>%
    str_split(",") %>%
    unlist()
  tibble("naics" = vec)
}
#' e.g. inputs 1111:1121 and 1200 return c(1111,1112,1113,....) and c(1200)
fill_ranges <- function(rng) {
  eval(parse(text = rng))
}
#' wrapper function for fill_ranges
fill_wrapper <- function(tbbl) {
  tbbl %>%
    mutate(naics = map(naics, fill_ranges))
}
# function to read in the employment data by naics
read_naics <- function(pttrn, var) {
  file_names <- list.files(here::here("data"), pattern = pttrn)
  vroom::vroom(here::here("data", file_names)) %>%
    janitor::clean_names() %>%
    rename(
      name = {{ var }},
      value = count
    ) %>%
    na.omit()%>%
    filter(naics_5 != "Unknown") %>%
    mutate(
      naics= as.numeric(str_sub(naics_5, start=-4)),
      date = lubridate::ymd(paste(syear, smth, "01", sep = "/"))
    ) %>%
     select(-syear, -smth, -naics_5)
}
# aggregates data to level var for each name and date
agg_level <- function(tbbl, var) {
  tbbl %>%
    group_by({{ var }}, name, date) %>%
    summarize(value = sum(value, na.rm=TRUE)) %>%
    group_by({{ var }}, .add = FALSE) %>%
    nest() %>%
    rename(agg_level = {{ var }})
}
# smooths data over previous months
trail_ma <- function(tbbl, months) {
  tbbl %>%
    group_by(name) %>%
    mutate(value = RcppRoll::roll_meanr(value, n = months)+1) #+1 to avoid infinite rates
}
# get the smoothed values for period max(date)-months
get_smoothed <- function(tbbl, num_months) {
  tbbl %>%
    group_by(name) %>%
    filter(near(date, max(date) - months(num_months), tol = 7)) %>%
#    mutate(value=if_else(value<1500, NA_real_, value))%>% #suppress values
    select(value)
}
# calculates the ytd average of the smoothed values
ytd_ave <- function(tbbl, num_years) {
  start <- floor_date(max(tbbl$date), unit = "year") - years(num_years)
  end <- max(tbbl$date) - years(num_years)
  tbbl %>%
    group_by(name) %>%
    filter(date >= start & date <= end) %>%
    summarize(ytd_ave = mean(value))
}
# labour force and unemployment rates need to be calculated
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
# clears out constant values of var (for excel)
unfill_var <- function(tbbl, var) {
  tbbl %>%
    mutate(characteristic = {{ var }} == dplyr::lag({{ var }}), .before = 1) %>%
    mutate(characteristic = if_else(!is.na(characteristic) & characteristic, "", {{ var }})) %>%
    select(-{{ var }})
}
# indicate hierarchy of industries by indentation
indent_industry <- function(tbbl) {
  tbbl %>%
    mutate(
      agg_level = if_else(characteristic == "", paste0("     ", agg_level), agg_level),
      agg_level = if_else(is.na(low), agg_level, paste0("     ", agg_level))
    )
}
# puts columns in the correct order and gives them the correct names
clean_up <- function(tbbl) {
  tbbl %>%
    mutate(characteristic = str_to_title(str_replace_all(characteristic, "_", " "))) %>%
    select(
      Characteristic = characteristic,
      Industry = agg_level,
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
# clones the layout sheet of the template to new sheets with the industry names
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
# stl_smooth <- function(tbbl){
#   tbbl%>%
#     mutate(date=yearmonth(date))%>%
#     tsibble(key=name, index = date)%>%
#     model(stl = STL(value~ trend(window = 5)+ season(window = "periodic"),
#                     robust = TRUE))%>%
#     components()%>%
#     as_tsibble()%>%
#     mutate(date=lubridate::ym(date))%>%
#     select(name, date, smoothed=trend)%>%
#     rename(value=smoothed)%>%
#     as_tibble()
# }

rescale01 <- function(tbbl, var, ...) {
  tbbl%>%
    mutate(value= ({{  var  }} - min({{  var  }}, ...)) / (max({{  var  }}, ...) - min({{  var  }}, ...)))
}

# plot_forecasts <- function(historic, fcast, industry){
#   fcast %>%
#     filter(agg_level==industry) %>%
#     autoplot(historic)+
#     scale_y_continuous(trans="log", labels=scales::comma)+
#     labs(x="",
#          y="",
#          title=paste("12 Month Exponential Smoothing Forecasts for",industry, "Industry"))+
#     theme_minimal()+
#     facet_wrap(~name, nrow=2, scales = "free_y")
# }
make_title <- function(strng){
  strng <- str_to_title(str_replace_all(strng,"_"," "))
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
  plt <- mybiplot(pcs, labels=rownames(pcs$x), labels.size=3, varname.size=3)+
    theme_minimal()+
    theme(plot.margin = unit(c(-.5, -2, 0, -2), "cm"))+
    scale_x_continuous(expand = expansion(mult = 0.15))+
    scale_y_continuous(expand = expansion(mult = 0.15))

}

level_change_plot <- function(shared_df){
  plot_ly(shared_df,
                 x =~ level_change_year,
                 y =~ level_change_month,
                 hoverinfo="text",
                 hovertext = ~agg_level,
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
                 hovertext = ~agg_level,
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
    select(-name)%>%
    unnest(data)%>%
    column_to_rownames(var="agg_level")
  heatmaply(tbbl,
            scale="row",
            dendrogram = "row",
            custom_hovertext = tbbl,
            fontsize_col = 6,
            column_text_angle = 90, main = var)
}





