@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Income Statement - Header'
define view entity ZI_FI_INCOME_STATEMENT_HEADER
  with parameters P_CompanyCode : bukrs
  as select from ZCORE_I_PROFILE_COMPANYCODE_V2
{
  key CompanyCode,
      ShortName,
      LongName,
      Address,
      VATNumber
}
where CompanyCode = $parameters.P_CompanyCode
