

sim_hood_level_ods <- function(hoods){

  mapdf <- hoods %>%
      select(hood) %>%
    rename(from_hood = hood) %>%
    st_drop_geometry() %>%
      crossing(hoods %>% select(hood) %>%
                 rename(to_hood = hood) %>%
                 st_drop_geometry()) %>%
    filter(from_hood != to_hood) %>%
    mutate(pair_ix = 1:n()) %>%
    gather(from_hood,to_hood,key="hood_piece",value="hood") %>%
    left_join(hoods %>% select(hood)) %>%
    st_as_sf() %>%
    st_transform(4326) %>%
    st_centroid() %>%
    group_by(pair_ix) %>%
    summarise(hood_od = str_c(hood,collapse = "_")) %>%
    st_cast("LINESTRING") %>%
    ungroup()

}

sim_hood_level_ptc <- function(hoods){

  hood_pop <- read_csv("~/Documents/CityData/Burgh/hood_population.csv")
  out <- hoods %>%
    select(hood) %>%
    left_join(hood_pop) %>%
    ## data taken from https://www.ucsur.pitt.edu/census_reports.php
    mutate(cycles = rbinom(n = n(), size = pop, prob =.026))

}