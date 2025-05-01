# ──────────────────────────────────────────────────────────────────────────────
# Synthetic Florida Medical-Claims Generator
# ──────────────────────────────────────────────────────────────────────────────
# PACKAGES ────────────────────────────────────────────────────────────────────
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  dplyr, tidyr, stringi, lubridate, charlatan, randomNames, faker, tibble
)

# PARAMETERS ──────────────────────────────────────────────────────────────────
set.seed(813)        # reproducibility
n_subs    <- 2_000   # unique subscribers
n_claims  <- 10_000  # claim *lines* (can exceed n_subs)

# helper: Florida ZIPs (first three digits 320–349)
fl_zip_pool <- sprintf("%05d", as.integer(
  c(32003:32099,32102:32199,32201:32299,32301:32399,
    32401:32499,32501:32599,32601:32699,32701:32799,
    32801:32899,32901:32999,33001:33099,33101:33199,
    33201:33299,33301:33399,33401:33499,33501:33599,
    33601:33699,33701:33799,33801:33899,33901:33999,
    34101:34199,34201:34299,34301:34399,34429:34499,
    34601:34699,34705:34799,34945:34999)
))

# helper: tiny pools of codes (expand/replace with real lists if desired)
cpt_pool  <- c("99213", "99214", "99233", "99385", "93000", "12001",
               "70450", "71020", "87635", "45378", "90837")
icd10_pool <- c("J11.1", "E11.9", "I10"  , "M54.5", "Z00.00", "R51",
                "K21.9", "S93.401A", "Z23", "N39.0")

pos_pool  <- c("11"="Office" , "21"="Inpatient Hospital", "22"="Outpatient Hospital",
               "23"="Emergency Room", "24"="Ambulatory Surg Ctr", "31"="Skilled Nursing")

claim_type_pool <- c("Professional", "Facility-Inpatient", "Facility-Outpatient")

# ──────────────────────────────────────────────────────────────────────────────
# 1. BUILD SUBSCRIBER + DEPENDENT LOOKUP
# ──────────────────────────────────────────────────────────────────────────────
subs_tbl <- tibble(
  subscriber_id   = sprintf("S%06d", 1:n_subs),
  subscriber_name = randomNames::randomNames(n_subs, which.names = "both"),
  plan_id         = sample(sprintf("PLN%03d", 1:50), n_subs, TRUE),
  group_id        = sample(sprintf("GRP%05d", 1:500), n_subs, TRUE),
  # give ~60% of subscribers at least one dependent
  n_dependents    = rpois(n_subs, lambda = 1)         # 0,1,2,… (mean~1)
) %>%
  mutate(n_dependents = ifelse(runif(n()) < .4, 0, n_dependents)) %>%
  uncount(n_dependents, .remove = FALSE, .id = "dep_seq") %>%
  mutate(
    person_id   = ifelse(is.na(dep_seq),
                         subscriber_id,                        # primary
                         paste0(subscriber_id, "D", dep_seq)), # dependent
    relationship = ifelse(is.na(dep_seq), "Subscriber",
                          sample(c("Spouse","Child"), n(), TRUE, c(.25,.75))),
    dob          = as_date(Sys.Date() - runif(n(), 1, 80*365)), # age 1-80
    gender       = sample(c("M","F"), n(), TRUE)
  ) %>%
  mutate(dep_seq = NULL)

# ──────────────────────────────────────────────────────────────────────────────
# 2. GENERATE CLAIM LINES
# ──────────────────────────────────────────────────────────────────────────────
claims_df <- tibble(
  claim_line_id   = sprintf("CL%07d", 1:n_claims),
  subscriber_id   = sample(subs_tbl$subscriber_id, n_claims, TRUE),
  person_id       = NA_character_,   # will join later
  provider_npi    = sprintf("%010d", sample(1e9:2e9, n_claims, TRUE)),
  place_of_service= sample(names(pos_pool), n_claims, TRUE),
  facility_name   = faker::CompanyProvider$new()$company(n_claims),
  claim_type      = sample(claim_type_pool, n_claims, TRUE, c(.6,.2,.2)),
  cpt_code        = sample(cpt_pool, n_claims, TRUE),
  diagnosis1      = sample(icd10_pool, n_claims, TRUE),
  diagnosis2      = ifelse(runif(n_claims) < .35,
                           sample(icd10_pool, n_claims, TRUE), NA),
  incurred_date   = sample(seq(as_date("2024-01-01"),
                               as_date("2025-03-31"), by = "day"), n_claims, TRUE)
) %>%
  # logical sequencing of dates
  mutate(
    discharge_date = incurred_date + sample(0:14, n_claims, TRUE), # 0-14 days later
    paid_date      = discharge_date + sample(7:60, n_claims, TRUE) # 1-2 mos later
  ) %>%
  # amounts: billed ≥ allowed ≥ paid
  mutate(
    billed_amount  = round(runif(n_claims, 300, 5000), 2),
    allowed_amount = billed_amount * runif(n_claims, .4, .9) |> round(2),
    paid_amount    = allowed_amount * runif(n_claims, .7, 1) |> round(2)
  ) %>%
  # addresses / ZIPs
  mutate(
    zip            = sample(fl_zip_pool, n_claims, TRUE),
    address_line1  = charlatan::ch_street_address(n_claims),
    city           = charlatan::ch_city(n_claims),
    state          = "FL"
  )

# attach person_id and member-level columns
claims_df <- claims_df %>%
  left_join(
    subs_tbl |> select(subscriber_id, person_id, relationship, dob, gender),
    by = c("subscriber_id")
  ) %>%
  # Each claim line must belong to a *specific* person (subscriber or dependent).
  group_by(subscriber_id) %>%
  mutate(person_id = sample(subs_tbl$person_id[subs_tbl$subscriber_id==first(subscriber_id)],
                            n(), replace = TRUE)) %>%
  ungroup()

# ──────────────────────────────────────────────────────────────────────────────
# 3. OPTIONAL: export
# ──────────────────────────────────────────────────────────────────────────────
# readr::write_csv(claims_df, "florida_synthetic_claims.csv")

# peek
glimpse(claims_df, width = 120)
