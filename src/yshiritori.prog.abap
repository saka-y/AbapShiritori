* ポケモンしりとり
REPORT yshiritori.

TYPES:
  tt_result TYPE STANDARD TABLE OF ysshiritoriresult WITH KEY cnt,
  BEGIN OF ts_helplist,
    matnr TYPE yegdictno,
    maktx TYPE yepokename,
    used  TYPE abap_bool,
  END OF ts_helplist,
  tt_helplist TYPE STANDARD TABLE OF ts_helplist WITH EMPTY KEY.

CLASS lcx_not_found DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.
CLASS lcx_already_used DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.
CLASS lcx_not_match DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

CLASS lcl_shiritori DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:
      player_win  TYPE sy-subrc VALUE 1,
      player_lose TYPE sy-subrc VALUE 2.
    CLASS-DATA:
      answer_not_exists TYPE yepokename.
    DATA:
      mv_status         TYPE sy-subrc READ-ONLY. "0:継続,1:プレーヤー勝ち,2:プレーヤー負け
    METHODS:
      constructor,
      check_input IMPORTING iv_txt TYPE yepokename
                  RAISING   lcx_not_found
                            lcx_already_used
                            lcx_not_match,
      get_next IMPORTING iv_current     TYPE yepokename
               RETURNING VALUE(rv_next) TYPE yepokename,
      get_start RETURNING VALUE(rv_start) TYPE yepokename,
      get_helplist RETURNING VALUE(rt_helplist) TYPE tt_helplist,
      get_result RETURNING VALUE(rt_result) TYPE tt_result.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_list,
        matnr TYPE yegdictno,
        maktx TYPE yepokename,
        first TYPE char1,
        last  TYPE char1,
        used  TYPE abap_bool,
      END OF ts_list,
      tt_list TYPE STANDARD TABLE OF ts_list WITH EMPTY KEY,
      BEGIN OF ts_result,
        cnt   TYPE licnt,
        user  TYPE uname,
        maktx TYPE yepokename,
      END OF ts_result,
      tt_result TYPE STANDARD TABLE OF ts_result WITH KEY cnt.
    CONSTANTS:
      endletter TYPE char1 VALUE 'ン'.
    DATA:
      mt_list   TYPE tt_list,
      mt_result TYPE tt_result,
      mv_cnt    TYPE i.
    METHODS:
      get_list,
      get_lastletter IMPORTING iv_maktx             TYPE yepokename
                     RETURNING VALUE(rv_lastletter) TYPE char1,
      get_random IMPORTING iv_max           TYPE sy-tabix
                 RETURNING VALUE(rv_random) TYPE sy-tabix.
ENDCLASS.
CLASS lcl_shiritori IMPLEMENTATION.
  METHOD constructor.
    answer_not_exists = TEXT-m02.
    get_list( ).
  ENDMETHOD.
  METHOD get_list.
    SELECT t~matnr,t~maktx
      FROM makt AS t
      INNER JOIN mara AS a
        ON t~matnr = a~matnr
      WHERE a~mtart = 'UNBW'
        AND a~matnr LIKE 'P____'
        AND t~spras = 'J'
      ORDER BY t~matnr
      INTO TABLE @DATA(lt_makt).

    mt_list = VALUE #( FOR ls_makt IN lt_makt
                         ( matnr = ls_makt-matnr
                           maktx = ls_makt-maktx
                           first = ls_makt-maktx
                           last  = get_lastletter( ls_makt-maktx ) ) ).
  ENDMETHOD.
  METHOD check_input.
*   入力値チェック
    IF line_exists( mt_list[ maktx = iv_txt ] ).
      IF mt_list[ maktx = iv_txt ]-used = abap_true.
        RAISE EXCEPTION TYPE lcx_already_used.
      ELSEIF mt_result IS NOT INITIAL AND
             mt_list[ maktx = iv_txt ]-first <> mt_list[ maktx = mt_result[ lines( mt_result ) ]-maktx ]-last.
        RAISE EXCEPTION TYPE lcx_not_match.
      ELSE.
        mt_list[ maktx = iv_txt ]-used = abap_true.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE lcx_not_found.
    ENDIF.
*   入力値が存在する場合、処理結果に追加
    mt_result = VALUE #( BASE mt_result ( cnt = ( lines( mt_result ) + 1 ) user = sy-uname maktx = iv_txt ) ).
*   入力値が終了の場合、ステータスを設定
    IF get_lastletter( iv_txt ) = endletter.
      mv_status = player_lose.
    ENDIF.
  ENDMETHOD.
  METHOD get_next.
    DATA lt_notused TYPE tt_list.
*   続行可能な値を取得
    lt_notused = VALUE #( FOR ls_list IN mt_list
                            WHERE ( used = abap_false AND first = get_lastletter( iv_current ) AND last <> endletter )
                            ( maktx = ls_list-maktx ) ).
    IF lt_notused IS NOT INITIAL.
      rv_next = lt_notused[ get_random( lines( lt_notused ) ) ]-maktx.
      mt_list[ maktx = rv_next ]-used = abap_true.
      mt_result = VALUE #( BASE mt_result ( cnt = ( lines( mt_result ) + 1 ) user = 'SAP' maktx = rv_next ) ).
*     次が続行可能かを判定
      IF lines( VALUE tt_list( FOR ls_list IN mt_list
                              WHERE ( used = abap_false AND first = get_lastletter( rv_next ) AND last <> endletter )
                              ( maktx = ls_list-maktx ) ) ) = 0.
        mv_status = player_lose.
      ENDIF.
    ELSE.
*     終了となる値を取得
      lt_notused = VALUE #( FOR ls_list IN mt_list
                              WHERE ( used = abap_false AND first = get_lastletter( iv_current ) AND last = endletter )
                              ( maktx = ls_list-maktx ) ).
      IF lt_notused IS NOT INITIAL.
        rv_next = lt_notused[ get_random( lines( lt_notused ) ) ]-maktx.
        mt_list[ maktx = rv_next ]-used = abap_true.
        mt_result = VALUE #( BASE mt_result ( cnt = ( lines( mt_result ) + 1 ) user = 'SAP' maktx = rv_next ) ).
        mv_status = player_win.
      ELSE.
        rv_next = answer_not_exists.
        mv_status = player_win.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_start.
    DATA lt_notused TYPE tt_list.
*   続行可能な値を取得
    lt_notused = VALUE #( FOR ls_list IN mt_list
                            WHERE ( last <> endletter )
                            ( maktx = ls_list-maktx ) ).
    rv_start = lt_notused[ get_random( lines( lt_notused ) ) ]-maktx.
    mt_list[ maktx = rv_start ]-used = abap_true.
    mt_result = VALUE #( BASE mt_result ( cnt = ( lines( mt_result ) + 1 ) user = 'SAP' maktx = rv_start ) ).
  ENDMETHOD.

  METHOD get_helplist.
    rt_helplist = CORRESPONDING #( mt_list ).
  ENDMETHOD.
  METHOD get_result.
    rt_result = mt_result.
  ENDMETHOD.
  METHOD get_lastletter.
    rv_lastletter = reverse( translate( val = iv_maktx from = 'ァィゥェォャュョ♂♀ー' to = 'アイウエオヤユヨスス' ) ).
  ENDMETHOD.
  METHOD get_random.
    DATA lv_random TYPE qfranint.
    CALL FUNCTION 'QF05_RANDOM_INTEGER'
      EXPORTING
        ran_int_max   = iv_max
        ran_int_min   = 1
      IMPORTING
        ran_int       = lv_random
      EXCEPTIONS
        invalid_input = 0
        OTHERS        = 0.
    rv_random = lv_random.
  ENDMETHOD.
ENDCLASS.

DATA:
  go_shiritori TYPE REF TO lcl_shiritori,
  go_cc        TYPE REF TO cl_gui_custom_container,
  go_alv       TYPE REF TO cl_gui_alv_grid,
  gt_alv       TYPE tt_result.

* 条件指定画面
PARAMETERS:
  rb_f    RADIOBUTTON GROUP rb1,
  rb_s    RADIOBUTTON GROUP rb1,
  cb_help AS CHECKBOX DEFAULT abap_true.

* 実行画面
SELECTION-SCREEN BEGIN OF SCREEN 1100.
  SELECTION-SCREEN BEGIN OF BLOCK b01.
    PARAMETERS:
      p_input TYPE yepokename OBLIGATORY,
      p_prev  TYPE yepokename.
  SELECTION-SCREEN END OF BLOCK b01.
  SELECTION-SCREEN BEGIN OF TABBED BLOCK tblock1 FOR 16 LINES.
    SELECTION-SCREEN TAB (10) TEXT-t01 USER-COMMAND tab1 DEFAULT SCREEN 1200.
  SELECTION-SCREEN END OF BLOCK tblock1.
SELECTION-SCREEN END OF SCREEN 1100.

AT SELECTION-SCREEN OUTPUT.
  IF sy-dynnr = '1100'.
*   前文字列を入力不可に設定
    SET PF-STATUS 'MAIN'.
    LOOP AT SCREEN.
      IF screen-name = 'P_PREV' OR
         ( screen-name = 'P_INPUT' AND go_shiritori->mv_status IS NOT INITIAL ).
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
*   結果表示
    CASE go_shiritori->mv_status.
      WHEN lcl_shiritori=>player_win.
        MESSAGE s004(yshiritori) WITH sy-uname.
      WHEN lcl_shiritori=>player_lose.
        MESSAGE s005(yshiritori) WITH sy-uname.
    ENDCASE.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_input.
  CHECK cb_help IS NOT INITIAL.
  DATA(lt_helplist) = go_shiritori->get_helplist( ).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'MAKTX'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'P_INPUT'
      value_org       = 'S'
    TABLES
      value_tab       = lt_helplist
    EXCEPTIONS
      parameter_error = 0
      no_values_found = 0
      OTHERS          = 0.

* 終了判定
AT SELECTION-SCREEN ON EXIT-COMMAND.
  IF sy-dynnr = '1100' AND go_shiritori->mv_status IS INITIAL.
    DATA lv_ans TYPE char1.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question         = TEXT-m01
        default_button        = '2'
        display_cancel_button = ''
      IMPORTING
        answer                = lv_ans
*     TABLES
*       PARAMETER             =
      EXCEPTIONS
        text_not_found        = 0
        OTHERS                = 0.
    IF lv_ans <> '1'.
      LEAVE TO SCREEN sy-dynnr.
    ENDIF.
  ENDIF.

* 値入力時
AT SELECTION-SCREEN ON BLOCK b01.
  CHECK go_shiritori->mv_status IS INITIAL.
* 入力値チェック
  TRY.
      go_shiritori->check_input( iv_txt = p_input ).
    CATCH lcx_not_found.
      MESSAGE e001(yshiritori) WITH p_input.
    CATCH lcx_already_used.
      MESSAGE e003(yshiritori) WITH p_input.
    CATCH lcx_not_match.
      MESSAGE e002(yshiritori) WITH p_input.
  ENDTRY.
* 継続中の場合は次の値を取得
  IF go_shiritori->mv_status IS INITIAL.
    DATA(lv_txt) = go_shiritori->get_next( p_input ).
    IF lv_txt IS NOT INITIAL.
      p_prev = lv_txt.
      IF go_shiritori->mv_status IS INITIAL.
        CLEAR p_input.
      ELSEIF go_shiritori->mv_status = lcl_shiritori=>player_lose.
        p_input = lcl_shiritori=>answer_not_exists.
      ENDIF.
    ENDIF.
  ENDIF.
* 結果表示を更新
  gt_alv = go_shiritori->get_result( ).
  go_alv->refresh_table_display(
    EXCEPTIONS
      finished       = 0
      OTHERS         = 0 ).

* 条件指定画面の実行処理
START-OF-SELECTION.
  go_shiritori = NEW #( ).
* 初期値を取得
  IF rb_s = abap_true.
    DATA(lv_txt) = go_shiritori->get_start( ).
    p_prev = lv_txt.
  ENDIF.
  WHILE sy-subrc = 0.
    CALL SELECTION-SCREEN 1100.
  ENDWHILE.

* カスタムコントロールへのALV設定
MODULE status_1200 OUTPUT.
  CHECK go_cc IS INITIAL.
  go_cc = NEW #( container_name = 'CC_ALV' ).
  go_alv = NEW #( i_parent  = go_cc ).
  gt_alv = go_shiritori->get_result( ).
  DATA(gt_sort) = VALUE lvc_t_sort( ( spos = 1 fieldname = 'CNT' down = abap_true ) ).
  go_alv->set_table_for_first_display(
    EXPORTING
      i_structure_name              = 'YSSHIRITORIRESULT'
    CHANGING
      it_outtab                      = gt_alv
      it_sort                       = gt_sort
    EXCEPTIONS
      invalid_parameter_combination = 0
      program_error                 = 0
      too_many_lines                = 0
      OTHERS                        = 0 ).
ENDMODULE.
