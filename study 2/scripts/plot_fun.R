library(tidyverse)
library(psych)

# make function for generating heatmap
heatmap_fun <- function(efa, factor_names = NA){
  
  # get factor names
  if(is.na(factor_names)){
    factor_names <- paste("Factor", 1:efa$factors)
  }
  
  # get factor loadings
  loadings <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names)))
  
  # get fa.sort() order
  order <- loadings %>%
    group_by(capacity) %>%
    top_n(1, abs(loading)) %>%
    ungroup() %>%
    arrange(desc(factor), abs(loading)) %>%
    mutate(order = 1:length(levels(factor(loadings$capacity)))) %>%
    select(capacity, order)
  
  # get percent shared variance explained
  shared_var <- efa$Vaccounted %>%
    data.frame() %>%
    rownames_to_column("stat") %>%
    filter(stat == "Proportion Explained") %>%
    select(-stat) %>%
    gather(factor, var) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names))) %>%
    mutate(var = paste0(factor, "\n(", round(var, 2)*100, "% var.)"))
  
  # make plot
  plot <- ggplot(loadings %>% 
                   left_join(order) %>%
                   left_join(shared_var) %>%
                   mutate(capacity = gsub("_", " ", capacity)),
                 aes(x = var, 
                     y = reorder(capacity, order), 
                     fill = loading, 
                     label = format(round(loading, 2), nsmall = 2))) +
    geom_tile(color = "black") +
    geom_text(size = 3) +
    scale_fill_distiller(limits = c(-1, 1), 
                         palette = "RdYlBu",
                         guide = guide_colorbar(barheight = 20)) +
    theme_minimal() +
    scale_x_discrete(position = "top") +
    theme(axis.title = element_blank())

  return(plot)
  
}

# make function for plotting factor scores by factor, target
scoresplot_fun <- function(efa, 
                           target = c("all (study 1)", "all (study 2)",
                                      "newborns", "4-day-olds", "1-month-olds",
                                      "2-month-olds", "4-month-olds",
                                      "6-month-olds", "9-month-olds",
                                      "12-month-olds", "18-month-olds",
                                      "2-year-olds", "3-year-olds",
                                      "4-year-olds", "5-year-olds"), 
                           target_encoding = "numeric",
                           highlight = "none", factor_names = NA){
  
  # generate list of targets
  if(target == "all (study 1)"){
    target_list <- c("target00mo", "target09mo", "target60mo")
  } else if(target == "all (study 2)"){
    target_list <- c("target00mo", "target0Xmo", "target01mo", "target02mo", 
                     "target04mo", "target06mo", "target09mo", "target12mo", 
                     "target18mo", "target24mo", "target36mo", "target48mo", 
                     "target60mo")
  } else {
    target_list <- case_when(target,
                             "newborns" ~ "target00mo",
                             "4-day-olds" ~ "target0Xmo",
                             "1-month-olds" ~ "target01mo",
                             "2-month-olds" ~ "target02mo",
                             "4-month-olds" ~ "target04mo",
                             "6-month-olds" ~ "target06mo",
                             "9-month-olds" ~ "target09mo",
                             "12-month-olds" ~ "target12mo",
                             "18-month-olds" ~ "target18mo",
                             "2-year-olds" ~ "target24mo",
                             "3-year-olds" ~ "target36mo",
                             "4-year-olds" ~ "target48mo",
                             "5-year-olds" ~ "target60mo")
  }
  
  # generate list of targets to highlight
  if(highlight == "none"){
    highlight_list <- c()
  } else if(highlight == "study 1"){
    highlight_list <- c("target00mo", "target09mo", "target60mo")
  } else {
    highlight_list <- highlight
  }

  # get factor names
  if(is.na(factor_names)){
    factor_names <- paste("Factor", 1:efa$factors)
  }
  
  # make usable dataframe
  df <- efa$scores[] %>%
    data.frame() %>%
    rownames_to_column("subid") %>%
    mutate(ResponseId = gsub("_target.*$", "", subid),
           target = gsub("^.*target", "target", subid)) %>%
    filter(target %in% target_list) %>%
    select(-subid) %>%
    gather(factor, score, -c(ResponseId, target)) %>%
    mutate(highlight = factor(ifelse(target %in% highlight_list,
                                     "highlight", "no_highlight"),
                              levels = c("no_highlight", "highlight")),
           factor = as.character(factor(factor, labels = factor_names)))
  
  if(target_encoding == "numeric"){
    df <- df %>%
      mutate(target = recode(target,
                             "target00mo" = 0,
                             "target0Xmo" = round(4/30, 3),
                             "target01mo" = 1,
                             "target02mo" = 2,
                             "target04mo" = 4,
                             "target06mo" = 6,
                             "target09mo" = 9,
                             "target12mo" = 12,
                             "target18mo" = 18,
                             "target24mo" = 24,
                             "target36mo" = 36,
                             "target48mo" = 48,
                             "target60mo" = 60))
    
    x_lab <- "target age (months)"
    
    } else if(target_encoding == "ordinal") {
    df <- df %>%
      mutate(target = recode_factor(target,
                                    "target00mo" = "newborns",
                                    "target0Xmo" = "4-day-olds",
                                    "target01mo" = "1-month-olds",
                                    "target02mo" = "2-month-olds",
                                    "target04mo" = "4-month-olds",
                                    "target06mo" = "6-month-olds",
                                    "target09mo" = "9-month-olds",
                                    "target12mo" = "12-month-olds",
                                    "target18mo" = "18-month-olds",
                                    "target24mo" = "2-year-olds",
                                    "target36mo" = "3-year-olds",
                                    "target48mo" = "4-year-olds",
                                    "target60mo" = "5-year-olds"))
    
    x_lab <- "target age"
    
  }

  # get bootstrapped means
  df_boot <- df %>%
    group_by(target, factor) %>%
    multi_boot_standard("score", na.rm = T) %>%
    ungroup() %>%
    mutate(highlight = factor(ifelse(target %in% highlight_list,
                                     "highlight", "no_highlight"),
                              levels = c("no_highlight", "highlight")))
  
  # get first items for subtitle
  first_items <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names))) %>%
    group_by(factor) %>%
    top_n(3, abs(loading)) %>%
    mutate(capacity = gsub("_", " ", capacity),
           cap_list = str_c(capacity, collapse = ", "),
           cap_list = paste0(cap_list, "...")) %>%
    ungroup() %>%
    select(-capacity, -loading) %>%
    distinct() %>%
    arrange(factor)
  
  # get percent shared variance explained
  shared_var <- efa$Vaccounted %>%
    data.frame() %>%
    rownames_to_column("stat") %>%
    filter(stat == "Proportion Explained") %>%
    select(-stat) %>%
    gather(factor, var) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names)),
           var = paste0(round(var, 2)*100, "% shared variance"))
  
  subtitle <- c()
  for(i in 1:nrow(first_items)){
    subtitle <- paste0(subtitle,
                       first_items[i,1], 
                       " (", shared_var[i,2], "): ",
                       first_items[i,2], 
                       "\n")
  }
  subtitle <- gsub("\\n$", "", subtitle)
  
  # get colors for lines
  line_colors <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854",
                   "#ffd92f", "#e5c494", "#b3b3b3") # Set2 from colorbrewer
  line_colors <- line_colors[1:nrow(shared_var)]
  
  # make plot
  plot <- ggplot(df, aes(x = target, y = score, fill = factor)) +
    facet_grid(cols = vars(factor)) +
    geom_hline(yintercept = 0, lty = 2, color = "darkgray") +
    geom_line(aes(color = factor, group = ResponseId), alpha = 0.1) +
    geom_line(data = df_boot, 
              aes(y = mean, group = factor), 
              lty = 1, color = "black") +
    geom_errorbar(data = df_boot,
                  aes(y = mean, ymin = ci_lower, ymax = ci_upper, 
                      color = highlight),
                  width = 0) +
    geom_point(data = df_boot,
               aes(y = mean, 
                   color = highlight, size = highlight)) +
    scale_fill_brewer(palette = "Set1") +
    scale_color_manual(values = c(line_colors, "black", "#984ea3")) +
    scale_size_manual(values = c(0.75, 2)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    labs(x = x_lab,
         y = "factor score",
         subtitle = subtitle,
         caption = "Error bars are bootstrapped 95% confidence intervals") +
    guides(fill = "none", color = "none", size = "none")
  # guides(color = guide_legend(override.aes = list(alpha = 1, size = 1)))
  
  return(plot)
  
}

# make function for plotting individual item means by factor, target
itemsplot_fun <- function(efa,
                          target = c("all (study 1)", "all (study 2)",
                                     "newborns", "4-day-olds", "1-month-olds",
                                     "2-month-olds", "4-month-olds",
                                     "6-month-olds", "9-month-olds",
                                     "12-month-olds", "18-month-olds",
                                     "2-year-olds", "3-year-olds",
                                     "4-year-olds", "5-year-olds")){
  
  # generate list of targets
  if(target == "all (study 1)"){
    target_list <- c("target00mo", "target09mo", "target60mo")
    } else if(target == "all (study 2)"){
      target_list <- c("target00mo", "target0Xmo", "target01mo", "target02mo",
                       "target04mo", "target06mo", "target09mo", "target12mo",
                       "target18mo", "target24mo", "target36mo", "target48mo",
                       "target60mo")
      } else {
        target_list <- case_when(target,
                                 "newborns" ~ "target00mo",
                                 "4-day-olds" ~ "target0Xmo",
                                 "1-month-olds" ~ "target01mo",
                                 "2-month-olds" ~ "target02mo",
                                 "4-month-olds" = "target04mo",
                                 "6-month-olds" ~ "target06mo",
                                 "9-month-olds" ~ "target09mo",
                                 "12-month-olds" ~ "target12mo",
                                 "18-month-olds" ~ "target18mo",
                                 "2-year-olds" ~ "target24mo",
                                 "3-year-olds" ~ "target36mo",
                                 "4-year-olds" ~ "target48mo",
                                 "5-year-olds" ~ "target60mo")
        }
  
  # make usable dataframe
  df <- d_all %>%
    rownames_to_column("subid") %>%
    mutate(ResponseId = gsub("_target.*$", "", subid),
           target = gsub("^.*target", "target", subid)) %>%
    filter(target %in% target_list) %>%
    select(-subid) %>%
    gather(capacity, response, -c(ResponseId, target)) %>%
    mutate(target = recode_factor(target,
                                  "target00mo" = "newborns",
                                  "target0Xmo" = "4-day-olds",
                                  "target01mo" = "1-month-olds",
                                  "target02mo" = "2-month-olds",
                                  "target04mo" = "4-month-olds",
                                  "target06mo" = "6-month-olds",
                                  "target09mo" = "9-month-olds",
                                  "target12mo" = "12-month-olds",
                                  "target18mo" = "18-month-olds",
                                  "target24mo" = "2-year-olds",
                                  "target36mo" = "3-year-olds",
                                  "target48mo" = "4-year-olds",
                                  "target60mo" = "5-year-olds"))
  
  # get factor loadings
  loadings <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity)
  
  # get fa.sort() order
  order <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    group_by(capacity) %>%
    top_n(1, abs(loading)) %>%
    ungroup() %>%
    arrange(desc(factor), abs(loading)) %>%
    mutate(order = 1:length(levels(factor(loadings$capacity)))) %>%
    select(factor, capacity, order)
  
  # add order to df
  df <- df %>% left_join(order)
  
  # get bootstrapped means
  df_boot <- df %>%
    group_by(target, factor, capacity, order) %>%
    multi_boot_standard("response", na.rm = T) %>%
    ungroup() %>%
    mutate(capacity = gsub("_", " ", capacity))
  
  # make plot
  plot <- ggplot(df %>% 
                   left_join(order) %>%
                   mutate(capacity = gsub("_", " ", capacity)),
                 aes(x = response, 
                     y = reorder(capacity, order), 
                     color = factor)) +
    facet_grid(factor ~ target, scales = "free", space = "free") +
    geom_point(alpha = 0.02) +
    geom_errorbarh(data = df_boot, 
                   aes(xmin = ci_lower, xmax = ci_upper, x = NULL),
                   color = "black", height = 0) +
    geom_point(data = df_boot,
               aes(x = mean),
               color = "black", size = 2) +
    theme_bw() +
    labs(x = "response", y = "",
         subtitle = "Error bars are bootstrapped 95% confidence intervals") +
    guides(color = "none")
  # guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))
  
  return(plot)
  
}