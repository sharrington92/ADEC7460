# Setup ----
{
  # https://www.drivendata.org/competitions/44/dengai-predicting-disease-spread/page/82/#features_list
  
  if(!stringr::str_detect(basename(getwd()), "Time Series") & stringr::str_detect(dirname(getwd()), "Time Series")){
    repeat{
      setwd("../")
      if(stringr::str_detect(basename(getwd()), "Time Series")){
        break
      }
    }
  }
  
  if(basename(getwd()) != "Final") setwd(file.path(getwd(), "Final"))
  
  
  source(file.path("00_Setup.R"))
}




# Visualizations ----
{
  ## Univariate ----
  {
    # Cases
    {
      # Time plot
      {
        train %>% 
          autoplot(box_cox(cases_cumulative, lambda.iq_cum))
        
        train %>% 
          autoplot(
            # cases_cumulative
            box_cox(cases_cumulative, lambda.iq_cum) %>% difference()
            # box_cox(cases_ytd, 1)
          )
      }
      
      # Seasonality
      {
        train %>% 
          gg_season(
            box_cox(total_cases,1)
          ) +
          scale_y_continuous(trans = "log10")
        
        train %>% 
          gg_season(
            cases_cumulative %>% difference()
            # box_cox(cases_cumulative, lambda.iq_cum) %>% difference()
            # box_cox(cases_ytd, 1)
          ) +
          scale_y_continuous(trans = "log10")
        
        
        train %>% 
          inner_join(years_with_bgn, by = c("city", "year")) %>% 
          # gg_season(cases_ytd)
          gg_season(box_cox(cases_ytd, .25))
      }
      
      # Autocorrelation
      {
        train %>% 
          filter(city == "iq") %>% 
          gg_tsdisplay(
            total_cases,
            # difference(box_cox(total_cases, 1)),
            # difference(total_cases, 52),
            # difference(total_cases, 52) %>% difference(),
            plot_type = "partial", lag_max = 104
          )
        
        
        train %>% 
          filter(city == "iq") %>% 
          gg_tsdisplay(
            # box_cox(cases_cumulative, .75),
            difference(box_cox(cases_cumulative, .75)),
            # difference(box_cox(cases_cumulative, .75), 52),
            # difference(box_cox(cases_cumulative, .75), 52) %>% difference(),
            plot_type = "partial", lag_max = 104
          )
      }
      
      
      
      # Lag
      train %>% 
        features(cases_cumulative, guerrero)
      
      # Density
      train %>% 
        filter(city == "iq") %>% 
        mutate(
          total_cases = box_cox(total_cases, .151),
          total_cases = difference(total_cases, 1)
        ) %>% 
        ggplot(aes(x = total_cases)) +
        # geom_histogram() +
        geom_density(color = "red3")
      
      
      # Decomp
      train %>% 
        model(STL(box_cox(total_cases, .25))) %>% 
        components() %>% 
        autoplot()
    }
    
    
    # Regressors
    {
      train %>% 
        autoplot(reanalysis_relative_humidity_percent)
      
      train %>% 
        autoplot(reanalysis_specific_humidity_g_per_kg)
      
      train %>% 
        autoplot(ndvi_ne)
    }
  }
  
  # Bivariate ----
  {
    train %>% 
      filter(city == "iq") %>% 
      mutate(total_cases = box_cox(total_cases, .151)) %>% 
      GGally::ggpairs(columns = c(10:15, 4))
    
    
    train %>% 
      filter(city == "iq") %>% 
      mutate(
        # total_cases = difference(total_cases),
        across(
          -c(city, year, weekofyear, week_start_date, yearweek, total_cases, is_missing),
          # difference
          # \(x){log(x) %>% difference()}
          # \(x){lag(x, 1) %>% difference()}
          \(x){lag(x, 4)}
        )
      ) %>%
      as_tibble() %>% 
      select(-c(
        city, year, weekofyear, week_start_date, yearweek, is_missing, 
        # cases_cumulative, 
        cases_ytd
        # total_cases
      )) %>% #filter(!is.na(total_cases)) %>% cor(use = 'complete.obs')
      # select(total_cases, contains("_sa")) %>% 
      select(
        total_cases, cases_cumulative, case_rate, contains("susc"), population,
        # hdd_reanalysis_365d_sa, hdd_reanalysis_365d,
        # hdd_station_4w_sa, 
        # hdd_station_365d, hdd_station_365d_sa,
        # precip_4w, precip_365d_sa,
        # PC20, PC15,
        # station_diur_temp_rng_c
      ) %>% 
      mutate(
        # PC20 = PC20 %>% lag(n = 1) %>% difference(),
        # PC15 = PC15 %>% lag(n = 6),
        # station_diur_temp_rng_c
        across(contains("susc"), \(x){lag(log(x / (population - x)), n = 1)})
      ) %>%
      pivot_longer(-c(total_cases, cases_cumulative, case_rate)) %>% 
      slice_sample(prop = .25) %>% 
      ggplot(aes(
        # x = box_cox(value, 1), 
        x = value, # %>% lag(5),
        # y = box_cox(total_cases, 1), 
        # y = box_cox(cases_cumulative, lambda.iq_cum) %>% difference(),
        y = log(case_rate), 
        color = name
      )) +
      geom_point() +
      geom_smooth(method = "lm", color = "gray50") +
      facet_wrap(name ~ ., scales = "free") +
      theme(
        legend.position = "none"
      )
  }
  
  
  # Multivariate ----
  {
    L = 5
    train %>%
      mutate(
        cases_365d_cut = cut_number(cases_365d, n = L),
        precip_4w_cut = cut_number(precip_4w_sa, n = L),
        humidity_rel_avg_4w_cut = cut_number(humidity_rel_avg_4w, n = L),
        humidity_rel_avg_2w_cut = cut_number(humidity_rel_avg_2w, n = L),
        hdd_reanalysis_4w_cut = cut_number(hdd_reanalysis_4w_sa, n = L),
        hdd_station_4w_cut = cut_number(hdd_station_4w, n = L)
      ) %>% 
      ggplot(aes(
        x = lag(precip_4w_sa, n = L), 
        y = lag(hdd_station_4w_sa, n = L), 
        color = (total_cases_scaled), alpha = (total_cases_scaled)
      )) +
      geom_point() +
      facet_grid(lag(humidity_rel_avg_2w_cut, n = L) ~ city, scales = "free") +
      scale_color_viridis_c()
    
    
    # PCA
    {
      L = 4
      train %>%
        ggplot(aes(
          x = lag(PC2, n = L), 
          y = lag(PC20, n = L), 
          color = (total_cases_scaled), alpha = (total_cases_scaled)
        )) +
        geom_point(size = 3) +
        scale_color_viridis_c()
      
      fn_plot_pca_lags <- function(pcA, pcB, L){
        # if(is.character(pcA)){
          pcA <- rlang::sym(pcA)
          pcB <- rlang::sym(pcB)
        # } else{
        #   pcA <- rlang::ensym(pcA)
        #   pcB <- rlang::ensym(pcB)
        # }
        
        plots <- lapply(c(1:L), \(X){
          train %>%
            as_tibble() %>% 
            # slice_sample(prop = .5) %>% 
            ggplot(aes(
              x = lag(!!pcA, n = X), 
              y = lag(!!pcB, n = X)
            )) +
            geom_point(aes(
              color = (total_cases_scaled), alpha = (total_cases_scaled), size = total_cases_scaled
            )) +
            facet_grid(. ~ city, scales = "free") +
            # geom_contour(aes(z = total_cases), color = "red", size = 4) +
            scale_color_viridis_c() +
            ggtitle(paste0("Lag: ", X)) +
            theme(
              legend.position = "none"
            )
        })
        
        gridExtra::grid.arrange(grobs = plots)
      }
      
      fn_plot_pca_lags("PC4", "PC12", 6) 
      
      plots <- expand.grid(
        pcA = paste("PC", 1:20, sep = ""),
        pcB = paste("PC", 1:20, sep = "")
      ) %>% 
        as_tibble() %>% 
        filter(pcA != pcB) %>% 
        # slice_sample(n = 1) %>% 
        mutate(
          across(contains("pc"), as.character),
          L=6
        ) %>% 
        pmap(., fn_plot_pca_lags)
      
      plots %>% 
        saveRDS("pca_plots.RDS")
      # Save each plot to a separate page in a PDF
      pdf("pca_plots.pdf")
      purrr::walk(plots, print)
      dev.off()
    }
    
    
  }
  
  
  # Time plots ----
  {
    vars.x.sa
    vars.x.pc
    
    train %>% 
      filter(city == "iq") %>% 
      # filter(year %in% c(1993:1998)) %>% #as_tibble() %>% View()
      # mutate(across(all_of(c(vars.x, vars.y)), difference)) %>%
      mutate(across(
        all_of(vars.x),
        \(x){rollmean(x, k = 5, fill = NA, align = "right")}
      )) %>% 
      # mutate(across(all_of(c(vars.x, vars.y)), difference)) %>%
      pivot_longer(-c(any_of(c(vars.id, "days_in_week"))), names_to = "variable") %>% 
      # filter(variable %in% c(
      #   "total_cases", #"total_cases_sa", 
      #   # "hdd_reanalysis_365d_sa", "precip_4w_sa", "precip_365d_sa",
      #   
      #   # "ndvi_nw_sa", "ndvi_sw_sa",
      #   # "ndvi_ne_sa", "ndvi_se_sa"
      #   
      #   # "precipitation_amt_mm_sa", "reanalysis_sat_precip_amt_mm_sa",
      #   # "reanalysis_sat_precip_amt_mm", "reanalysis_precip_amt_kg_per_m2_sa"
      #   
      #   # "reanalysis_air_temp_k_sa", "reanalysis_avg_temp_k_sa",
      #   # "reanalysis_dew_point_temp_k_sa", "reanalysis_max_air_temp_k_sa",
      #   # "reanalysis_min_air_temp_k_sa"
      #   
      #   "reanalysis_relative_humidity_percent_sa", "reanalysis_specific_humidity_g_per_kg_sa",
      #   "reanalysis_tdtr_k_sa", "station_precip_mm_sa"
      # )) %>% 
      filter(variable %in% c(
        "total_cases", #"total_cases_sa", 
        # "hdd_reanalysis_365d", "precip_4w", "precip_365d"
        
        # "ndvi_nw", "ndvi_sw",
        # "ndvi_ne", "ndvi_se"
        
        # "precipitation_amt_mm", "reanalysist_precip_amt_mm",
        # "reanalysist_precip_amt_mm", "reanalysis_precip_amt_kg_per_m2"
        
        # "reanalysis_air_temp_k", "reanalysis_avg_temp_k",
        # "reanalysis_dew_point_temp_k", "reanalysis_max_air_temp_k",
        # "reanalysis_min_air_temp_k"
        
        vars.x.sa[11:15]
        
        # "reanalysis_relative_humidity_percent", "reanalysis_specific_humidity_g_per_kg",
        # "reanalysis_tdtr_k", "station_precip_mm"
        
        # paste("PC", 1:5, sep = "")
      )) %>% 
      ggplot(aes(x = yearweek, y = value, color = variable)) + 
      geom_line() +
      geom_vline(xintercept = as.Date("1994-08-15"), linetype = "dashed", color = "gray30") +
      geom_vline(xintercept = as.Date("1998-06-15"), linetype = "dashed", color = "gray30") +
      geom_vline(
        xintercept = seq.Date(from = as.Date("1990-07-01"), to = as.Date("2015-07-01"), by = "1 year"), 
        linetype = "dotted", color = "red4", alpha = .15, linewidth = 1.25
      ) +
      geom_smooth(method = "lm", se = F, color = "gray40") +
      facet_grid(variable ~ city, scales = "free") +
      theme(
        legend.position = "none"
      )
  }
}


# Decomposition ----
{
  train %>% 
    model(STL(box_cox(total_cases, .25))) %>% 
    components() %>% 
    autoplot()
  
  
  train %>% 
    model(STL(box_cox(cases_ytd, .25))) %>% 
    components() %>% 
    autoplot()
}




{
  train.all %>% 
    filter(city == "sj") %>% 
    # filter(year >= 2000) %>% 
    mutate(
      # humidity_365d = rollmean(reanalysis_relative_humidity_percent, 52, NA, align = "right"),
      reanalysis_tdtr_k_sa_smooth = rollmean(reanalysis_tdtr_k_sa, 26, NA, align = "right") %>% 
        rollmax(., k = 15, NA, align = "right"),
      testvar1 = rollmean(difference(reanalysis_tdtr_k, 52), 26, NA, align = "right"),
      testvar2 = difference(reanalysis_tdtr_k_sa_smooth) %>% 
        rollsum(., k = 52, NA, align = "right"),
      # lag(n = 1)
      # difference(lag = 52)
      total_cases_inertia = (log((total_cases + 1) / (lag(total_cases, n = 1) + 1))) %>%
        rollsum(k = 8, NA, align = "right"),
      case_rate = log(case_rate + .0000001)
      # 
      # across(
      #   c(reanalysis_tdtr_k_sa),
      #   \(x){rollmean(x, 13, NA, align = "right")
      #     # rollmax(x, 52, NA, align = "right")
      #   }
      # )
    ) %>%
    select(
      total_cases, total_cases_inertia,
      yearweek, case_rate, #reanalysis_tdtr_k_sa,
      reanalysis_tdtr_k_sa_smooth,
      # reanalysis_relative_humidity_percent_cut, hdd_reanalysis_365d_cut,
      # reanalysis_tdtr_k_cut,
      # reanalysis_tdtr_k_cut,
      
      # humidity_365d, precip_365d, hdd_reanalysis_365d, humidity_rel_avg_4w, humidity_rel_avg_2w,
      # hdd_reanalysis_4w_sa, hdd_reanalysis_4w,
      # hdd_reanalysis_365d, 
      # testvar2, testvar1
      
      # reanalysis_dew_point_temp_k, reanalysis_air_temp_k, reanalysis_avg_temp_k, reanalysis_max_air_temp_k
      # reanalysis_min_air_temp_k, reanalysis_precip_amt_kg_per_m2, reanalysis_sat_precip_amt_mm, reanalysis_tdtr_k
      # station_diur_temp_rng_c_sa, ndvi_se, ndvi_nw, ndvi_ne, ndvi_sw
    ) %>% 
    # mutate(across(-c(yearweek, total_cases), \(x){lag(x, n = 5)})) %>% 
    drop_na() %>% 
    pivot_longer(-c(yearweek)) %>% 
    ggplot(aes(y = value, x = yearweek, color = name)) +
    geom_line() +
    facet_grid(name ~ ., scales = "free") +
    theme(legend.position = "none")
  
}




{
  train.all %>% 
    filter(city == "sj") %>% 
    filter(year >= 2000) %>% 
    mutate(
      humidity_365d = rollmean(reanalysis_relative_humidity_percent, 52, NA, align = "right"),
      # total_cases = log((total_cases + 1) / (lag(total_cases, n = 1) + 1)),
      total_cases_inertia = (log((total_cases + 1) / (lag(total_cases, n = 1) + 1))) %>%
        rollsum(k = 52, NA, align = "right"),
      # across(
      #   c(station_diur_temp_rng_c_sa, ndvi_se, ndvi_nw, ndvi_ne, ndvi_sw),
      #   \(x){
      #     rollmean(x, 5, NA, align = "right")
      #     # rollmax(x, 52, NA, align = "right")
      #   }
      # )
    ) %>% 
    select(
      yearweek, total_cases, total_cases_inertia, contains("_cut"),
      reanalysis_relative_humidity_percent_cut, hdd_reanalysis_365d_cut,
      reanalysis_tdtr_k_cut, station_diur_temp_rng_c,
      reanalysis_tdtr_k_sa_smooth, hdd_reanalysis_4w, reanalysis_min_air_temp_k_cut,
      reanalysis_min_air_temp_k
      # humidity_365d, precip_365d, hdd_reanalysis_365d, humidity_rel_avg_4w, humidity_rel_avg_2w,
      
      # reanalysis_dew_point_temp_k, reanalysis_air_temp_k, reanalysis_avg_temp_k, reanalysis_max_air_temp_k
      # reanalysis_min_air_temp_k, reanalysis_precip_amt_kg_per_m2, reanalysis_sat_precip_amt_mm, reanalysis_tdtr_k
      # station_diur_temp_rng_c_sa, ndvi_se, ndvi_nw, ndvi_ne, ndvi_sw
    ) %>% 
    mutate(across(-c(yearweek, total_cases, total_cases_inertia, reanalysis_tdtr_k_sa_smooth), \(x){lag(x, n = 8)})) %>%
    drop_na() %>% 
    # pivot_longer(-c(yearweek, total_cases_inertia, total_cases, contains("cut"))) %>% 
    ggplot(aes(
      # y = total_cases_inertia,
      y = log(total_cases + 1),
      # x = (reanalysis_tdtr_k_sa_smooth * station_diur_temp_rng_c), 
      x = reanalysis_tdtr_k_sa_smooth * rollmean(reanalysis_min_air_temp_k, k = 8, NA, align = "right")
      * rollmean(station_diur_temp_rng_c, k = 8, NA, align = "right"),
      color = hdd_reanalysis_4w_cut,
      # color = reanalysis_min_air_temp_k_cut
      # group = hdd_reanalysis_4w_cut
    )) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = F) + #, color = "gray30"
    # facet_grid(hdd_reanalysis_365d_cut ~ name, scales = "free") +
    scale_color_viridis_d(option = "D") #+
  theme(legend.position = "none")
  
}

colnames(train)