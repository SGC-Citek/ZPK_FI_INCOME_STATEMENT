CLASS zcl_fi_income_statement_manage DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA: gt_data TYPE TABLE OF zc_fi_income_statement.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(ro_instance) TYPE REF TO zcl_fi_income_statement_manage,
      get_data
        IMPORTING io_request TYPE REF TO if_rap_query_request
        EXPORTING et_data    LIKE gt_data.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: instance TYPE REF TO zcl_fi_income_statement_manage.
ENDCLASS.



CLASS ZCL_FI_INCOME_STATEMENT_MANAGE IMPLEMENTATION.


  METHOD get_data.
    DATA lv_bukrs            TYPE bukrs.
    DATA lv_ver_cf           TYPE zde_fi_fs_ver_cf.
    DATA lv_ledger           TYPE fins_ledger.
    DATA lv_fiscal_year      TYPE fis_gjahr_no_conv.
    DATA lv_period_from      TYPE fins_fiscalperiod.
    DATA lv_period_to        TYPE fins_fiscalperiod.
    DATA lv_comp_fiscal_year TYPE fis_gjahr_no_conv.
    DATA lv_comp_period_from TYPE fins_fiscalperiod.
    DATA lv_comp_period_to   TYPE fins_fiscalperiod.
    DATA lv_stock            TYPE p LENGTH 16 DECIMALS 0.
    DATA lt_data             TYPE TABLE OF zc_fi_income_statement.
    DATA lv_period_to_pre    TYPE fins_fiscalperiod.

    " get filter by parameter -----------------------
    DATA(lt_paramater) = io_request->get_parameters( ).
    IF lt_paramater IS NOT INITIAL.
      LOOP AT lt_paramater REFERENCE INTO DATA(ls_parameter).
        CASE ls_parameter->parameter_name.
          WHEN 'P_COMPANYCODE'.
            lv_bukrs            = ls_parameter->value.
          WHEN 'P_FINSTATEMENTVER'.
            lv_ver_cf           = ls_parameter->value.
          WHEN 'P_LEDGER'.
            lv_ledger           = ls_parameter->value.
          WHEN 'P_PERIODFROM'.
            lv_period_from      = ls_parameter->value.
          WHEN 'P_PERIODTO'.
            lv_period_to        = ls_parameter->value.
          WHEN 'P_FISCALYEAR'.
            lv_fiscal_year      = ls_parameter->value.
          WHEN 'P_COMPPERIODFROM'.
            lv_comp_period_from = ls_parameter->value.
          WHEN 'P_COMPPERIODTO'.
            lv_comp_period_to   = ls_parameter->value.
          WHEN 'P_COMPFISCALYEAR'.
            lv_comp_fiscal_year = ls_parameter->value.
          WHEN 'P_STOCK'.
            lv_stock = ls_parameter->value.
        ENDCASE.
      ENDLOOP.
    ENDIF.
    " get filter by parameter -----------------------
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(lt_filters) = io_request->get_filter( ).
    DATA(lt_filter) = io_request->get_filter( )->get_as_ranges( ).
    IF lt_filter IS NOT INITIAL.
      LOOP AT lt_filter INTO DATA(ls_filter).
        CASE ls_filter-name.
          WHEN 'STOCK'.
            lv_stock = ls_filter-range[ 1 ]-low.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    SELECT i_financialstatementhiernode~HierarchyNode,
           i_financialstatementhiernode~ParentNode,
           i_financialstatementhiernode~HierarchyLevel,
           ztb_fi_fs_ver_cf~is_bold,
           ztb_fi_fs_ver_cf~is_nega,
           ztb_fi_fs_ver_cf~fs_note,
           ztb_fi_fs_ver_cf~fs_item_txt_vn,
           ztb_fi_fs_ver_cf~fs_item_txt_en,
           ztb_fi_fs_ver_cf~fs_hrynode,
           ztb_fi_fs_ver_cf~fs_item
      FROM I_FinancialStatementHierNode
             LEFT OUTER JOIN
               ztb_fi_fs_ver_cf ON  ztb_fi_fs_ver_cf~fs_ver_cf  = i_financialstatementhiernode~FinancialStatementHierarchy
                                AND ztb_fi_fs_ver_cf~fs_hrynode = i_financialstatementhiernode~HierarchyNode
      WHERE i_financialstatementhiernode~FinancialStatementHierarchy  = @lv_ver_cf
        AND i_financialstatementhiernode~HierarchyNode               <> '00NOTASSGND'
        AND i_financialstatementhiernode~ParentNode                  <> '00NOTASSGND'
        AND i_financialstatementhiernode~FinancialStatementNodeType   = 'I'
      INTO TABLE @DATA(lt_fshierval).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_fshierval ASSIGNING FIELD-SYMBOL(<lfs_fshierval>).
      SHIFT <lfs_fshierval>-hierarchynode LEFT DELETING LEADING '0'.
      SHIFT <lfs_fshierval>-parentnode LEFT DELETING LEADING '0'.
      SHIFT <lfs_fshierval>-hierarchylevel LEFT DELETING LEADING '0'.
      SHIFT <lfs_fshierval>-fs_hrynode LEFT DELETING LEADING '0'.

      IF <lfs_fshierval>-fs_hrynode IS NOT INITIAL.
        APPEND VALUE #( fs_hrynode     = <lfs_fshierval>-fs_hrynode
                        fs_item        = <lfs_fshierval>-fs_item
                        is_bold        = <lfs_fshierval>-is_bold
                        is_nega        = <lfs_fshierval>-is_nega
                        fs_note        = <lfs_fshierval>-fs_note
                        fs_item_txt_vn = <lfs_fshierval>-fs_item_txt_vn
                        fs_item_txt_en = <lfs_fshierval>-fs_item_txt_en
                        currency       = 'VND' )
               TO lt_data.
      ENDIF.
    ENDLOOP.

    SORT lt_fshierval BY HierarchyNode.
    SORT lt_data BY fs_hrynode.

    DATA lv_fromdate TYPE dats.
    DATA lv_todate   TYPE dats.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" get data B
    lv_period_to_pre = lv_period_from - 1.

    " from date
    SELECT SINGLE FirstDayOfMonthDate
      FROM i_yearmonth
      WHERE CalendarYear  = @lv_fiscal_year
        AND CalendarMonth = '001'
      INTO @lv_fromdate.

    " to date
    IF lv_period_to_pre <> 000.
      SELECT SINGLE LastDayOfMonthDate
        FROM i_yearmonth
        WHERE CalendarYear  = @lv_fiscal_year
          AND CalendarMonth = @lv_period_to_pre
        INTO @lv_todate.
    ELSE.
      lv_todate = '00000000'.
    ENDIF.
    SELECT CompanyCode,
           Ledger,
           FiscalYear,
           FinancialStatementVariant,
           HierarchyNodeUniqueID,
           FinancialStatementItem,
           CompanyCodeCurrency,
*           EndingBalanceAmtInCoCodeCrcy - StartingBalanceAmtInCoCodeCrcy AS amount,
           EndingBalanceAmtInCoCodeCrcy   AS amount,
           EndingBalanceAmtInCoCodeCrcy,
           StartingBalanceAmtInCoCodeCrcy
      FROM zi_finstmnt_rptg_cube( P_FromPostingDate           = @lv_fromdate,
                                  P_ToPostingDate             = @lv_todate,
                                  P_FinancialStatementVariant = @lv_ver_cf,
                                  P_AlternativeGLAccount      = ' ' )
      WHERE CompanyCode = @lv_bukrs
        AND Ledger      = @lv_ledger
      INTO TABLE @DATA(lt_trans_b).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" get data A

    " from date
    SELECT SINGLE FirstDayOfMonthDate
      FROM i_yearmonth
      WHERE CalendarYear  = @lv_fiscal_year
        AND CalendarMonth = @lv_period_from
      INTO @lv_fromdate.

    " to date
    SELECT SINGLE LastDayOfMonthDate
      FROM i_yearmonth
      WHERE CalendarYear  = @lv_fiscal_year
        AND CalendarMonth = @lv_period_to
      INTO @lv_todate.

    SELECT CompanyCode,
           Ledger,
           FiscalYear,
           FinancialStatementVariant,
           HierarchyNodeUniqueID,
           FinancialStatementItem,
           CompanyCodeCurrency,
*           EndingBalanceAmtInCoCodeCrcy - StartingBalanceAmtInCoCodeCrcy AS amount,
           EndingBalanceAmtInCoCodeCrcy   AS amount,
           EndingBalanceAmtInCoCodeCrcy,
           StartingBalanceAmtInCoCodeCrcy
      FROM zi_finstmnt_rptg_cube( P_FromPostingDate           = @lv_fromdate,
                                  P_ToPostingDate             = @lv_todate,
                                  P_FinancialStatementVariant = @lv_ver_cf,
                                  P_AlternativeGLAccount      = ' ' )
      WHERE CompanyCode = @lv_bukrs
        AND Ledger      = @lv_ledger
      INTO TABLE @DATA(lt_trans).

    SELECT item~CompanyCode,
           item~Ledger,
           item~FiscalYear,
           item~FinancialStatementVariant,
           item~HierarchyNodeUniqueID,
           item~FinancialStatementItem,
           item~CompanyCodeCurrency,
           SUM( item~Amount )                         AS amount,
           SUM( item~EndingBalanceAmtInCoCodeCrcy )   AS EndingBalanceAmtInCoCodeCrcy,
           SUM( item~StartingBalanceAmtInCoCodeCrcy ) AS StartingBalanceAmtInCoCodeCrcy
      FROM @lt_trans AS item
      GROUP BY item~CompanyCode,
               item~Ledger,
               item~FiscalYear,
               item~FinancialStatementVariant,
               item~HierarchyNodeUniqueID,
               item~FinancialStatementItem,
               item~CompanyCodeCurrency
      " TODO: variable is assigned but never used (ABAP cleaner)
      INTO TABLE @DATA(lt_trans_gr).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" get data Compare

    lv_period_to_pre = lv_comp_period_from - 1.

    " from date
    SELECT SINGLE FirstDayOfMonthDate
      FROM i_yearmonth
      WHERE CalendarYear  = @lv_comp_fiscal_year
        AND CalendarMonth = '001'
      INTO @lv_fromdate.

    " to date
    IF lv_period_to_pre <> 000.
      SELECT SINGLE LastDayOfMonthDate
        FROM i_yearmonth
        WHERE CalendarYear  = @lv_comp_fiscal_year
          AND CalendarMonth = @lv_period_to_pre
        INTO @lv_todate.
    ELSE.
      lv_todate = '00000000'.
    ENDIF.

    SELECT CompanyCode,
           Ledger,
           FiscalYear,
           FinancialStatementVariant,
           HierarchyNodeUniqueID,
           FinancialStatementItem,
           CompanyCodeCurrency,
*           EndingBalanceAmtInCoCodeCrcy - StartingBalanceAmtInCoCodeCrcy AS amount,
           EndingBalanceAmtInCoCodeCrcy   AS amount,
           EndingBalanceAmtInCoCodeCrcy,
           StartingBalanceAmtInCoCodeCrcy
      FROM zi_finstmnt_rptg_cube( P_FromPostingDate           = @lv_fromdate,
                                  P_ToPostingDate             = @lv_todate,
                                  P_FinancialStatementVariant = @lv_ver_cf,
                                  P_AlternativeGLAccount      = ' ' )
      WHERE CompanyCode = @lv_bukrs
        AND Ledger      = @lv_ledger
      INTO TABLE @DATA(lt_trans_comp_b).

    " from date
    SELECT SINGLE FirstDayOfMonthDate
      FROM i_yearmonth
      WHERE CalendarYear  = @lv_comp_fiscal_year
        AND CalendarMonth = @lv_comp_period_from
      INTO @lv_fromdate.

    " to date
    SELECT SINGLE LastDayOfMonthDate
      FROM i_yearmonth
      WHERE CalendarYear  = @lv_comp_fiscal_year
        AND CalendarMonth = @lv_comp_period_to
      INTO @lv_todate.

    SELECT CompanyCode,
           Ledger,
           FiscalYear,
           FinancialStatementVariant,
           HierarchyNodeUniqueID,
           FinancialStatementItem,
           CompanyCodeCurrency,
*           EndingBalanceAmtInCoCodeCrcy - StartingBalanceAmtInCoCodeCrcy AS amount,
           EndingBalanceAmtInCoCodeCrcy   AS amount,
           EndingBalanceAmtInCoCodeCrcy,
           StartingBalanceAmtInCoCodeCrcy
      FROM zi_finstmnt_rptg_cube( P_FromPostingDate           = @lv_fromdate,
                                  P_ToPostingDate             = @lv_todate,
                                  P_FinancialStatementVariant = @lv_ver_cf,
                                  P_AlternativeGLAccount      = ' ' )
      WHERE CompanyCode = @lv_bukrs
        AND Ledger      = @lv_ledger
      INTO TABLE @DATA(lt_trans_comp).

    SELECT item~CompanyCode,
           item~Ledger,
           item~FiscalYear,
           item~FinancialStatementVariant,
           item~HierarchyNodeUniqueID,
           item~FinancialStatementItem,
           item~CompanyCodeCurrency,
           SUM( item~Amount )                         AS amount,
           SUM( item~EndingBalanceAmtInCoCodeCrcy )   AS EndingBalanceAmtInCoCodeCrcy,
           SUM( item~StartingBalanceAmtInCoCodeCrcy ) AS StartingBalanceAmtInCoCodeCrcy
      FROM @lt_trans_comp AS item
      GROUP BY item~CompanyCode,
               item~Ledger,
               item~FiscalYear,
               item~FinancialStatementVariant,
               item~HierarchyNodeUniqueID,
               item~FinancialStatementItem,
               item~CompanyCodeCurrency
      " TODO: variable is assigned but never used (ABAP cleaner)
      INTO TABLE @DATA(lt_trans_comp_gr).

    LOOP AT lt_trans INTO DATA(ls_trans).
      READ TABLE lt_fshierval INTO DATA(ls_fshierval)
           WITH KEY HierarchyNode = ls_trans-FinancialStatementItem BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_data ASSIGNING FIELD-SYMBOL(<lfs_data>)
           WITH KEY fs_hrynode = ls_fshierval-fs_hrynode BINARY SEARCH.
      IF sy-subrc = 0.
        <lfs_data>-val_curr_ped += ls_trans-Amount.
      ELSE.

      ENDIF.
      DO ls_fshierval-hierarchylevel - 1 TIMES.
        READ TABLE lt_fshierval INTO ls_fshierval
             WITH KEY HierarchyNode = ls_fshierval-parentnode BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE lt_data ASSIGNING <lfs_data>
               WITH KEY fs_hrynode = ls_fshierval-fs_hrynode BINARY SEARCH.
          IF sy-subrc = 0.
            <lfs_data>-val_curr_ped += ls_trans-Amount.
          ELSE.

          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.

    IF lt_trans_b IS NOT INITIAL.

      LOOP AT lt_trans_b INTO DATA(ls_trans_b).
        READ TABLE lt_fshierval INTO ls_fshierval
             WITH KEY HierarchyNode = ls_trans_b-FinancialStatementItem BINARY SEARCH.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        READ TABLE lt_data ASSIGNING <lfs_data>
             WITH KEY fs_hrynode = ls_fshierval-fs_hrynode BINARY SEARCH.
        IF sy-subrc = 0.
          <lfs_data>-val_curr_ped -= ls_trans_b-Amount.
        ELSE.

        ENDIF.
        DO ls_fshierval-hierarchylevel - 1 TIMES.
          READ TABLE lt_fshierval INTO ls_fshierval
               WITH KEY HierarchyNode = ls_fshierval-parentnode BINARY SEARCH.
          IF sy-subrc = 0.
            READ TABLE lt_data ASSIGNING <lfs_data>
                 WITH KEY fs_hrynode = ls_fshierval-fs_hrynode BINARY SEARCH.
            IF sy-subrc = 0.
              <lfs_data>-val_curr_ped -= ls_trans_b-Amount.
            ELSE.

            ENDIF.
          ENDIF.
        ENDDO.

      ENDLOOP.
    ENDIF.

    LOOP AT lt_trans_comp INTO DATA(ls_trans_comp).
      READ TABLE lt_fshierval INTO ls_fshierval
           WITH KEY HierarchyNode = ls_trans_comp-FinancialStatementItem BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_data ASSIGNING <lfs_data>
           WITH KEY fs_hrynode = ls_fshierval-fs_hrynode BINARY SEARCH.
      IF sy-subrc = 0.
        <lfs_data>-val_comp_ped += ls_trans_comp-Amount.
      ELSE.

      ENDIF.
      DO ls_fshierval-hierarchylevel - 1 TIMES.
        READ TABLE lt_fshierval INTO ls_fshierval
             WITH KEY HierarchyNode = ls_fshierval-parentnode BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE lt_data ASSIGNING <lfs_data>
               WITH KEY fs_hrynode = ls_fshierval-fs_hrynode BINARY SEARCH.
          IF sy-subrc = 0.
            <lfs_data>-val_comp_ped += ls_trans_comp-Amount.
          ELSE.

          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.

    IF lt_trans_comp_b IS NOT INITIAL.
      LOOP AT lt_trans_comp_b INTO DATA(ls_trans_comp_b).
        READ TABLE lt_fshierval INTO ls_fshierval
             WITH KEY HierarchyNode = ls_trans_comp_b-FinancialStatementItem BINARY SEARCH.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        READ TABLE lt_data ASSIGNING <lfs_data>
             WITH KEY fs_hrynode = ls_fshierval-fs_hrynode BINARY SEARCH.
        IF sy-subrc = 0.
          <lfs_data>-val_comp_ped -= ls_trans_comp_b-Amount.
        ELSE.

        ENDIF.
        DO ls_fshierval-hierarchylevel - 1 TIMES.
          READ TABLE lt_fshierval INTO ls_fshierval
               WITH KEY HierarchyNode = ls_fshierval-parentnode BINARY SEARCH.
          IF sy-subrc = 0.
            READ TABLE lt_data ASSIGNING <lfs_data>
                 WITH KEY fs_hrynode = ls_fshierval-fs_hrynode BINARY SEARCH.
            IF sy-subrc = 0.
              <lfs_data>-val_comp_ped -= ls_trans_comp_b-Amount.
            ELSE.

            ENDIF.
          ENDIF.
        ENDDO.
      ENDLOOP.
    ENDIF.

    DATA ls_temp_60_curr_period TYPE zc_fi_income_statement-val_comp_ped.
    DATA ls_temp_60_comp_period TYPE zc_fi_income_statement-val_comp_ped.

    LOOP AT lt_data ASSIGNING <lfs_data>.
      IF <lfs_data>-is_nega = 'X'.
        <lfs_data>-val_curr_ped *= -1.
        <lfs_data>-val_comp_ped *= -1.
      ENDIF.
      <lfs_data>-val_diff = <lfs_data>-val_curr_ped - <lfs_data>-val_comp_ped.
      IF <lfs_data>-val_comp_ped <> 0.
        <lfs_data>-per_diff = <lfs_data>-val_diff * 100 / <lfs_data>-val_comp_ped.
      ENDIF.

      IF <lfs_data>-fs_hrynode = 60.
        ls_temp_60_curr_period = <lfs_data>-val_curr_ped.
        ls_temp_60_comp_period = <lfs_data>-val_comp_ped.
      ENDIF.
      IF lv_stock IS INITIAL.
        CONTINUE.
      ENDIF.

      IF <lfs_data>-fs_hrynode = 70.
        <lfs_data>-val_70_curr_ped = ls_temp_60_curr_period * 100 / lv_stock.
        <lfs_data>-val_70_comp_ped = ls_temp_60_comp_period * 100 / lv_stock.
        <lfs_data>-val_70_dif_ped  = <lfs_data>-val_70_curr_ped - <lfs_data>-val_70_comp_ped.
        <lfs_data>-per_diff        = <lfs_data>-val_70_dif_ped * 100 / <lfs_data>-val_70_comp_ped.
      ENDIF.
    ENDLOOP.

    SORT lt_data BY fs_item.

    et_data = lt_data.
  ENDMETHOD.


  METHOD get_instance.
    IF instance IS INITIAL.
      CREATE OBJECT instance.
    ENDIF.
    ro_instance = instance.
  ENDMETHOD.
ENDCLASS.
