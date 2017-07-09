## author:    Mark Kazzaz
## date:      2017-07-09
## desc:      dplyr and tidyr functions operating in batches

## use case:  trying to gather/spread on a large dataset (>2MM)
##            tends to result in too large an operation for the
##            C stack supporting tidyr to handle.  Instead,
##            wanted to create a function that could perform the
##            tidyr functions on a subgroup and ultimately group 
##            all subgroups together.  Surely not the most
##            efficient way, but successfully cleared the memory
##            problem I was encountering.

## expecting the dataset to work on, how many groups to process, which measure columns to gather, and what key to spread on
fct_process_groups <- function(too_large_data, number_of_groups, measure_range, spread_col_name){

  ### create row_id then figure out group_id based on number of groups entered into function
  too_large_data <- too_large_data %>% mutate(row_id = row_number()-1, group_id = row_id %% number_of_groups)

  ### create a list of unique group_ids to loop through
  unique_groups <- too_large_data %>% distinct(group_id)

  ### for every group_id, filter, gather, spread, and stack the results
  for (group in unique_groups$group_id){
    gc()
    
    #### was printing to screen to debug
    # print(paste0('Processing group '
    #              , group
    #              , '...'))
    
    #### filter to group to process
    starting_data <- too_large_data %>% 
      filter(group_id == group)
    
    tidy_data <- starting_data %>% 
      #### convert the columns to rows to enable delta analysis
      gather(measure, value, measure_range) %>% 
      
      #### convert rows to columns on the delta key
      spread_(data = ., key_col = spread_col_name, value_col = 'value')
    
    #### create an empty dataset to capture the tidy data
    if(!exists('running_data')){running_data <- tidy_data[0,]}
    
    #### stack this loops results to the running dataset
    running_data <- rbind(tidy_data, running_data)
  }

  ### send back the final stacked result set as the output
  return(running_data)

}
