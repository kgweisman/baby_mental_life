table_fun <- function(fa, num_items = NA, pos_abs = "pos"){
  
  # get total of ~32 rows
  if(is.na(num_items)){
    num_items <- min(floor(32/fa$factors), 8)
  } else {
    num_items <- num_items
  }
  
  table <- fa$loadings[] %>% 
    data.frame() %>% 
    rownames_to_column("capacity") %>% 
    gather(factor, loading, -capacity) %>%
    group_by(factor)
  
  if(grepl("pos", pos_abs)){
    table <- table %>%
      top_n(num_items, loading)
  }
  
  if(grepl("abs", pos_abs)){
    table <- table %>%
      top_n(num_items, abs(loading))
  }
  
  table <- table %>%
    mutate(loading = round(loading, 2)) %>%
    ungroup() %>% 
    arrange(factor, desc(loading)) %>%
    select(-factor) %>%
    knitr::kable(format = "html")
  
  for(i in 1:fa$factors){
    factor_name <- paste("Factor", i)
    table <- table %>%
      kableExtra::group_rows(factor_name, 1 + (i - 1) * num_items, i * num_items)
  }
  
  return(table)
}
