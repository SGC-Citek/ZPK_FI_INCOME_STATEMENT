@EndUserText.label: 'Income Statement'

@ObjectModel.query.implementedBy: 'ABAP:ZCL_FI_INCOME_STATEMENT'

@UI.headerInfo: { typeName: 'Income Statement',
                  typeNamePlural: 'Income Statement',
                  title: { type: #STANDARD, label: 'Income Statement' } }
define root custom entity ZC_FI_INCOME_STATEMENT
  with parameters
    @Consumption.valueHelpDefinition: [ { entity: { name: 'I_CompanyCodeStdVH', element: 'CompanyCode' } } ]
    @EndUserText.label: 'Company Code'
    P_CompanyCode     : bukrs,

    @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_GLAccountHierarchyVH', element: 'GLAccountHierarchy' } } ]
    @EndUserText.label: 'Financial Statement Version'
    P_FinStatementVer : zde_fi_fs_ver_cf,

    @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_LedgerVH', element: 'Ledger' } } ]
    @EndUserText.label: 'Ledger'
    P_Ledger          : fins_ledger,

    @EndUserText.label: 'From Period'
    P_PeriodFrom      : fins_fiscalperiod,

    @EndUserText.label: 'To Period'
    P_PeriodTo        : fins_fiscalperiod,

    @EndUserText.label: 'Fiscal Year'
    P_FiscalYear      : fis_gjahr_no_conv,

    @EndUserText.label: 'Comparison From Period'
    P_CompPeriodFrom  : fins_fiscalperiod,

    @EndUserText.label: 'Comparison To Period'
    P_CompPeriodTo    : fins_fiscalperiod,

    @EndUserText.label: 'Comparison Fiscal Year'
    P_CompFiscalYear  : fis_gjahr_no_conv,

    @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_LANGUAGE_VH', element: 'value_low' } } ]
    @EndUserText.label: 'Language'
    P_Language        : zde_language,

    @Consumption.valueHelpDefinition: [ { entity: { name: 'ZFI_I_YES_NO_VH', element: 'value_low' } } ]
    @EndUserText.label: 'Tính chênh lệch'
    P_CalDiff         : zde_yes_no
//
//    @EndUserText.label: 'Stock'
//    P_Stock           : zde_amount

{
      @UI.hidden: true
  key fs_hrynode     : zde_hrynode;
      
      @EndUserText.label: 'Stock'
      @UI.selectionField: [{ position: 120   }]
      stock : abap.dec(16,3);

      @EndUserText.label: 'Chỉ tiêu'
      @UI.identification: [ { position: 10, label: 'Chỉ tiêu' } ]
      @UI.lineItem: [ { position: 10, label: 'Chỉ tiêu' } ]
      fs_item_txt_vn : zde_fi_fs_item_txt_vn;

      @EndUserText.label: 'Comparison Period'
      @UI.identification: [ { position: 20, label: 'Item' } ]
      @UI.lineItem: [ { position: 20, label: 'Item' } ]
      fs_item_txt_en : zde_fi_fs_item_txt_en;

      @EndUserText.label: 'Mã số'
      @UI.identification: [ { position: 30, label: 'Mã số' } ]
      @UI.lineItem: [ { position: 30, label: 'Mã số' } ]
      fs_item        : zde_fi_fs_item;

      @UI.hidden: true
      is_bold        : zde_fi_is_bold;

      @UI.hidden: true
      is_nega        : zde_fi_is_negative_posting;

      @EndUserText.label: 'Thuyết minh'
      @UI.identification: [ { position: 40, label: 'Thuyết minh' } ]
      @UI.lineItem: [ { position: 40, label: 'Thuyết minh' } ]
      fs_note        : zde_fi_fs_note;

      @UI.hidden: true
      currency       : waers;

      @EndUserText.label: 'Kỳ này'
      @Semantics.amount.currencyCode: 'currency'
      @UI.identification: [ { position: 50, label: 'Kỳ này' } ]
      @UI.lineItem: [ { position: 50, label: 'Kỳ này' } ]
      val_curr_ped   : zde_fis_wsl;

      @EndUserText.label: 'Kỳ so sánh'
      @Semantics.amount.currencyCode: 'currency'
      @UI.identification: [ { position: 60, label: 'Kỳ so sánh' } ]
      @UI.lineItem: [ { position: 60, label: 'Kỳ so sánh' } ]
      val_comp_ped   : zde_fis_wsl;

      @EndUserText.label: 'Chênh lệch'
      @Semantics.amount.currencyCode: 'currency'
      @UI.identification: [ { position: 70, label: 'Chênh lệch' } ]
      @UI.lineItem: [ { position: 70, label: 'Chênh lệch' } ]
      val_diff       : zde_fis_wsl;

      @EndUserText.label: 'Kỳ này'
//      @Semantics.amount.currencyCode: 'currency'
      @UI.hidden: true

      val_70_curr_ped : abap.dec(25,3);

      @EndUserText.label: 'Kỳ này'
//      @Semantics.amount.currencyCode: 'currency'
      @UI.hidden: true
      val_70_comp_ped : abap.dec(25,3);

      @EndUserText.label: 'Kỳ này'
//      @Semantics.amount.currencyCode: 'currency'
      @UI.hidden: true
      val_70_dif_ped : abap.dec(25,3);

      @EndUserText.label: 'Kỳ này'
//      @Semantics.amount.currencyCode: 'currency'
      @UI.hidden: true
      val_71_curr_ped : abap.dec(25,3);

      @EndUserText.label: 'Kỳ này'
//      @Semantics.amount.currencyCode: 'currency'
      @UI.hidden: true
      val_71_comp_ped : abap.dec(25,3);

      @EndUserText.label: 'Tỷ lệ % chênh lệch'
      @UI.identification: [ { position: 80, label: 'Tỷ lệ % chênh lệch' } ]
      @UI.lineItem: [ { position: 80, label: 'Tỷ lệ % chênh lệch' } ]
      per_diff       : abap.dec(25,3);
}
