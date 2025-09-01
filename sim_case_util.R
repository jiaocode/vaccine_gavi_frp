sim_case_util <-
  function(data = data_sim,
           case_prob = case_prob,
           util_prob = util_prob) {
    N <- nrow(data)

    case_prob1 <- ifelse(case_prob <= 1, case_prob, 1)
    case_prob2 <- ifelse(case_prob <= 1, 0, case_prob - 1)

    util_prob1 <- ifelse(util_prob <= 1, util_prob, 1)
    util_prob2 <- ifelse(util_prob <= 1, 0, util_prob - 1)

    set.seed(123)
    data <-
      data %>%
      mutate(case1 = rbinom(N, 1, ifelse(quintile == 1, case_prob1[1],
        ifelse(quintile == 2, case_prob1[2],
          ifelse(quintile == 3, case_prob1[3],
            ifelse(quintile == 4, case_prob1[4], case_prob1[5])
          )
        )
      ))) %>%
      mutate(case2 = rbinom(N, 1, ifelse(quintile == 1, case_prob2[1],
        ifelse(quintile == 2, case_prob2[2],
          ifelse(quintile == 3, case_prob2[3],
            ifelse(quintile == 4, case_prob2[4], case_prob2[5])
          )
        )
      ))) %>%
      mutate(case = case1 + case2) %>%
      mutate(util1 = ifelse(case == 0, 0, rbinom(N, 1, ifelse(quintile == 1, util_prob1[1],
        ifelse(quintile == 2, util_prob1[2],
          ifelse(quintile == 3, util_prob1[3],
            ifelse(quintile == 4, util_prob1[4], util_prob1[5])
          )
        )
      )))) %>%
      mutate(util2 = ifelse(case == 0, 0, rbinom(N, 1, ifelse(quintile == 1, util_prob2[1],
        ifelse(quintile == 2, util_prob2[2],
          ifelse(quintile == 3, util_prob2[3],
            ifelse(quintile == 4, util_prob2[4], util_prob2[5])
          )
        )
      )))) %>%
      mutate(util = util1 + util2)

    return(data)
  }
