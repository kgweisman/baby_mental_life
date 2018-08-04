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
                           target = c("all", "newborns", "9-month-olds", "5-year-olds")){
  
  # generate list of targets
  target_list <- case_when(
    target == "all" ~ c("target00mo", "target09mo", "target60mo"),
    target == "newborns" ~ "target00mo",
    target == "9-month-olds" ~ "target09mo",
    target == "5-year-olds" ~ "target60mo"
  )
  
  # make usable dataframe
  df <- efa$scores[] %>%
    data.frame() %>%
    rownames_to_column("subid") %>%
    mutate(ResponseId = gsub("_target.*$", "", subid),
           target = gsub("^.*target", "target", subid)) %>%
    filter(target %in% target_list) %>%
    select(-subid) %>%
    gather(factor, score, -c(ResponseId, target)) %>%
    mutate(target = recode_factor(target,
                                  "target00mo" = "newborns",
                                  "target09mo" = "9-month-olds",
                                  "target60mo" = "5-year-olds"))
  
  # get bootstrapped means
  df_boot <- df %>%
    group_by(target, factor) %>%
    multi_boot_standard("score", na.rm = T) %>%
    ungroup()
  
  # get first items for each factor
  first_items <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    group_by(factor) %>%
    top_n(3, abs(loading)) %>%
    mutate(capacity = gsub("_", " ", capacity),
           cap_list = str_c(capacity, collapse = ", "),
           cap_list = paste0(cap_list, "...")) %>%
    ungroup() %>%
    select(-capacity, -loading) %>%
    distinct()
  
  # make plot
  plot <- ggplot(df,
                 aes(x = target, 
                     y = score, 
                     color = factor)) +
    facet_grid(~ factor) +
    geom_path(aes(group = ResponseId), alpha = 0.1) +
    geom_path(data = df_boot,
              aes(y = mean, group = factor), color = "black", lty = 2) +
    geom_pointrange(data = df_boot,
                    aes(y = mean, ymin = ci_lower, ymax = ci_upper),
                    color = "black", fatten = 0.75) +
    geom_text(data = first_items,
              aes(label = gsub('(.{1,30})(\\s|$)', '\\1\n', cap_list)),
              x = 0.5, y = max(df$score), size = 3, color = "black",
              hjust = 0, vjust = 1) +
    theme_bw() +
    labs(x = "target", y = "factor score",
         subtitle = "Error bars are bootstrapped 95% confidence intervals") +
    guides(color = "none")
  # guides(color = guide_legend(override.aes = list(alpha = 1, size = 1)))
  
  return(plot)
  
}

# make function for plotting individual item means by factor, target
itemsplot_fun <- function(efa,
                          target = c("all", "newborns", "9-month-olds", "5-year-olds")){
  
  # generate list of targets
  target_list <- case_when(
    target == "all" ~ c("target00mo", "target09mo", "target60mo"),
    target == "newborns" ~ "target00mo",
    target == "9-month-olds" ~ "target09mo",
    target == "5-year-olds" ~ "target60mo"
  )
  
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
                                  "target09mo" = "9-month-olds",
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