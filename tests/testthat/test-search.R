ids <- example_ref_ids(n = 2, seed = 2025)
too_many_ids <- example_ref_ids(n = 30, seed = 2025)
ref_profile_names <- c("referenceId", "referenceType", "citation", "visibility",
                       "lifecycle", "newVersion", "masterId", "bibliography",
                       "parent", "series", "filesAndLinks", "children",
                       "products", "projects", "crossReferences", "units",
                       "boundingBoxes", "taxa", "subjects", "keywords",
                       "permissions", "history", "versions",
                       "contentProducerUnits", "programs")

with_mock_dir("reference_profiles_2", {
  test_that("search by reference IDs works", {

    ref_profiles <- search_references_by_id(reference_ids = ids,
                                            nps_internal = FALSE,
                                            dev = FALSE)
    expect_length(ref_profiles, 2)
    expect_setequal(as.integer(names(ref_profiles)), ids)

  })

  test_that("search by reference IDs handles duplicate IDs", {
    ref_profiles <- search_references_by_id(reference_ids = rep(ids, 2),
                                            nps_internal = FALSE,
                                            dev = FALSE)
    expect_length(ref_profiles, 2)
  })

  test_that("reference profile structure is as expected", {
    ref_profiles <- search_references_by_id(reference_ids = ids,
                                            nps_internal = FALSE,
                                            dev = FALSE)
    expect_equal(names(ref_profiles$`2223441`), ref_profile_names)
    expect_equal(names(ref_profiles$`2254708`), ref_profile_names)
  })
})

with_mock_dir("reference_profiles_30", {
  test_that("search by reference IDs works", {

    ref_profiles <- search_references_by_id(reference_ids = too_many_ids,
                                            nps_internal = FALSE,
                                            dev = FALSE)
    expect_length(ref_profiles, 30)
    expect_setequal(as.integer(names(ref_profiles)), too_many_ids)

  })

  test_that("search by reference IDs handles duplicate IDs", {
    ref_profiles <- search_references_by_id(reference_ids = rep(too_many_ids, 2),
                                            nps_internal = FALSE,
                                            dev = FALSE)
    expect_length(ref_profiles, 30)
    expect_setequal(as.integer(names(ref_profiles)), too_many_ids)
  })
})

without_internet({
  test_that("search by reference IDs is a GET request", {
    expect_GET(search_references_by_id(reference_ids = ids,
                                         nps_internal = FALSE,
                                         dev = FALSE))
  })

})

