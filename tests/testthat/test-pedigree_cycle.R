test_that("pedigree cycle check", {
  # define pedigree with cycle
  tbl_ped_cycle <- tibble::tibble(ID = c(1:6),
                                  Father = c(4,1,1,6,4,5),
                                  Mother = c(NA,NA,2,NA,NA,NA))
  pcfc <- PedigreeCycleCheck$new()
  pcfc$set_tbl_pedigree(ptbl_pedigree = tbl_ped_cycle)
  pcfc$set_n_ani_col(pn_ani_col = 1)
  pcfc$set_n_sire_col(pn_sire_col = 2)
  pcfc$set_n_dam_col(pn_dam_col = 3)
  expect_true(pcfc$has_cycle())

  # pedigree without cycles
  tbl_ped_no_cycle <- tibble::tibble(ID = c(1:6),
                                     Father = c(4,1,1,NA,4,5),
                                     Mother = c(NA,NA,2,NA,NA,NA))
  pcnc <- PedigreeCycleCheck$new()
  pcnc$set_tbl_pedigree(ptbl_pedigree = tbl_ped_no_cycle)
  pcnc$set_n_ani_col(pn_ani_col = 1)
  pcnc$set_n_sire_col(pn_sire_col = 2)
  pcnc$set_n_dam_col(pn_dam_col = 3)
  expect_false(pcnc$has_cycle())
})
