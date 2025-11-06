
test_that("fips2countyfips works", {

  fipsmix =  c(
    "091701844002024",
    testinput_fips_blockgroups,
    testinput_fips_tracts,
    testinput_fips_cities,
    testinput_fips_counties,
    testinput_fips_states
  )
  # > cbind(fips = fipsmix, countyfips = fips2countyfips(fipsmix), type=fipstype(fipsmix))
  # fips              countyfips type
  # [1,] "091701844002024" "09170"    "block"
  # [2,] "050014801001"    "05001"    "blockgroup"
  # [3,] "050014802001"    "05001"    "blockgroup"
  # [4,] "050014803001"    "05001"    "blockgroup"
  # [5,] "050014803002"    "05001"    "blockgroup"
  # [6,] "050014804001"    "05001"    "blockgroup"
  # [7,] "050014804002"    "05001"    "blockgroup"
  # [8,] "050014805001"    "05001"    "blockgroup"
  # [9,] "050014805002"    "05001"    "blockgroup"
  # [10,] "050014805003"    "05001"    "blockgroup"
  # [11,] "050014806001"    "05001"    "blockgroup"
  # [12,] "050014807001"    "05001"    "blockgroup"
  # [13,] "050014807002"    "05001"    "blockgroup"
  # [14,] "050014808001"    "05001"    "blockgroup"
  # [15,] "050014808002"    "05001"    "blockgroup"
  # [16,] "05001480100"     "05001"    "tract"
  # [17,] "05001480200"     "05001"    "tract"
  # [18,] "05001480300"     "05001"    "tract"
  # [19,] "05001480400"     "05001"    "tract"
  # [20,] "05001480500"     "05001"    "tract"
  # [21,] "05001480600"     "05001"    "tract"
  # [22,] "05001480700"     "05001"    "tract"
  # [23,] "05001480800"     "05001"    "tract"
  # [24,] "2743000"         "27053"    "city"
  # [25,] "2743306"         "27053"    "city"
  # [26,] "10001"           "10001"    "county"
  # [27,] "10003"           "10003"    "county"
  # [28,] "10005"           "10005"    "county"
  # [29,] "10"              NA         "state"
  # [30,] "44"              NA         "state"

expect_no_error({
  x = fips2countyfips( fipsmix)
})
expect_equal(length(x), length(fipsmix))
expect_true(
  all(is.na(x[fipstype(fipsmix) == "state"]))
)
ok = fipstype(fipsmix) %in% c('block', 'blockgroup', 'tract',     'county')
# not true if city or state
expect_true(
  all(nchar(x[ok]) == 5)
)
expect_true(
  all(substr(fipsmix[ok], 1, 5) == x[ok])
)

})
