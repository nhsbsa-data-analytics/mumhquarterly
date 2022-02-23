#' @title Create queries to extract data from NHSBSA DWH
#'
#' @name create
#'
#' @description
#' A series of `create_*()` functions that generate lazy db tables powered by
#' `dbplyr` that can chained into other `dplyr` functions to query the NHSBSA
#' Data warehouse in a flexible manner.
#'
#' * `create_tdim()` builds lazy table that queries `DIM.YEAR_MONTH_DIM` to give
#' latest complete financial quarter period.
#' * `create_org_dim()` builds lazy table that queries `DIM.CUR_LEVEL_5_FLAT_DIM`.
#' Defaults to England only.
#' * `create_drug_dim()` builds lazy table that queries `DIM.CDR_EP_DRUG_BNF_DIM`.
#' By default returns all BNF hierarchy information. Can be filtered using any
#' level of the BNF hierarchy. Additional fields can be added.
#' * `create_fact()` builds lazy table that queries
#' `AML.PX_FORM_ITEM_ELEM_COMB_FACT_AV`. Returns Items and NIC by default with
#' patient ID level information.
#'
#' @param con a database object created `nhsbsaR::con_nhsbsa` or `DBI`
#' equivalent
#' @param start year month of start of period in YYYYMM format. Defaults to
#' start of patient data in DWH, 201504
#' @param end year month of end of period in YYYYMM format. Defaults to latest
#' year month in DWH using in built `f_get_latest_period('EPACT2')` function
#'
#' @return a lazy table that can be used in `dplyr` chains.
#' @export
#'
#' @examples
#' con <- nhsbsaR::con_nhsbsa(database = "DWCP")
#'
#' tdim <- create_tdim(con, start = 202110)
#' org <- create_org_dim(con, country = 1)
#' drug <- create_drug_dim(con, bnf_codes = c("0401", "0402", "0403"))
#' fact <- create_fact(con)
#'
#' df <- fact %>%
#'   inner_join(tdim, by = "YEAR_MONTH") %>%
#'   inner_join(org, by = c("PRESC_TYPE_PRNT" = "LVL_5_OUPDT",
#'                         "PRESC_ID_PRNT" = "LVL_5_OU")) %>%
#'   inner_join(drug, by = c("CALC_PREC_DRUG_RECORD_ID" = "RECORD_ID",
#'                          "YEAR_MONTH" = "YEAR_MONTH")) %>%
#'   group_by(FINANCIAL_QUARTER, SECTION_DESCR, BNF_SECTION, IDENTIFIED_FLAG) %>%
#'   summarise(ITEM_COUNT = sum(ITEM_COUNT),
#'            ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC)/100)
create_tdim <- function (
  con,
  start = 201504,
  end = sql("MGMT.PKG_PUBLIC_DWH_FUNCTIONS.f_get_latest_period('EPACT2')")
) {

  # handle start and end arguments to pass as integers
  start <- as.integer(start)

  if(is.numeric(end)) {
    end <- as.integer(end)
  }

  dim <- tbl(con, from = in_schema("DIM", "YEAR_MONTH_DIM")) %>%
    # filter table to time period
    select(YEAR_MONTH, FINANCIAL_YEAR, FINANCIAL_QUARTER) %>%
    group_by(FINANCIAL_YEAR, FINANCIAL_QUARTER) %>%
    # create financial quarter column that sorts correctly
    # calculate month counts to give full quarters only
    mutate(
      FINANCIAL_QUARTER = FINANCIAL_YEAR %||% " Q" %||% FINANCIAL_QUARTER,
      # window functions to perform counts within groups
      MONTH_COUNT = n()
    ) %>%
    # filter to only full financial quarters
    filter(MONTH_COUNT == 3L, YEAR_MONTH >= start, YEAR_MONTH <= end)

}

#' @export
#' @rdname create
create_org_dim <- function(con, country = 1) {

  # handle numeric args to pass as integer
  country <- as.integer(country)

  dim <- tbl(con, from = in_schema("DIM", "CUR_LEVEL_5_FLAT_DIM")) %>%
    filter(
      CUR_CTRY_OU %in% country,
      DATA_ADDED_BY_DENTAL == "N",
      PHARM_APP_CUR_IND_DESC == "UNKNOWN"
    ) %>%
    select(
      LVL_5_OUPDT,
      LVL_5_OU
    )


}

#' @export
#' @rdname create
create_drug_dim <- function (con, bnf_codes, ...) {

  # handle bnf_codes to allow pattern matching depending on level of BNF
  # searched.
  bnf <- paste0("^(", paste0(bnf_codes, collapse = "|"), ")")

  dim <- tbl(con, from = in_schema("DIM","CDR_EP_DRUG_BNF_DIM")) %>%
    filter(REGEXP_LIKE(PRESENTATION_BNF, bnf)) %>%
    select(
      YEAR_MONTH,
      RECORD_ID,
      PRESENTATION_BNF_DESCR,
      PRESENTATION_BNF,
      CHEMICAL_SUBSTANCE_BNF_DESCR,
      BNF_CHEMICAL_SUBSTANCE,
      PARAGRAPH_DESCR,
      BNF_PARAGRAPH,
      SECTION_DESCR,
      BNF_SECTION,
      CHAPTER_DESCR,
      BNF_CHAPTER,
      ...
    )

}

#' @export
#' @rdname create
create_fact <- function (con) {

  fact <- tbl(con, in_schema("AML", "PX_FORM_ITEM_ELEM_COMB_FACT_AV")) %>%
    filter(
      PAY_DA_END == "N", # excludes disallowed items
      PAY_ND_END == "N", # excludes not dispensed items
      PAY_RB_END == "N", # excludes referred back items
      CD_REQ == "N", # excludes controlled drug requisitions
      OOHC_IND == 0L, # excludes out of hours dispensing
      PRIVATE_IND == 0L, # excludes private dispensers
      IGNORE_FLAG == "N", # excludes LDP dummy forms
      PRESC_TYPE_PRNT %NOT IN% c(8L, 54L)
    ) %>%
    select(
      YEAR_MONTH,
      PRESC_TYPE_PRNT,
      PRESC_ID_PRNT,
      CALC_PREC_DRUG_RECORD_ID,
      IDENTIFIED_PATIENT_ID,
      PDS_DOB,
      PDS_GENDER,
      ITEM_COUNT,
      ITEM_PAY_DR_NIC
    ) %>%
    mutate(
      IDENTIFIED_FLAG = case_when(
        is.null(PDS_DOB) & PDS_GENDER == 0L ~ "N",
        TRUE ~ "Y"
      )
    ) %>%
    group_by(
      YEAR_MONTH,
      PRESC_TYPE_PRNT,
      PRESC_ID_PRNT,
      CALC_PREC_DRUG_RECORD_ID,
      IDENTIFIED_PATIENT_ID,
      PDS_DOB,
      PDS_GENDER,
      IDENTIFIED_FLAG
    ) %>%
    summarise(
      ITEM_COUNT = sum(ITEM_COUNT),
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC),
      .groups = "drop"
    )

}
