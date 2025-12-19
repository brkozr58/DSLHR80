PROCESS BEFORE OUTPUT.
 MODULE liste_initialisieren.
 LOOP AT extract WITH CONTROL
  tctrl_/dsl/hr80_v003 CURSOR nextline.
   MODULE liste_show_liste.
 ENDLOOP.
 MODULE fill_substflds.
*
PROCESS AFTER INPUT.
 MODULE liste_exit_command AT EXIT-COMMAND.
 MODULE liste_before_loop.
 LOOP AT extract.
   MODULE liste_init_workarea.
   CHAIN.
    FIELD /dsl/hr80_v003-gjahr .
    FIELD /dsl/hr80_v003-vrsid .
    FIELD /dsl/hr80_v003-vrsid_t .
    FIELD /dsl/hr80_v003-statu .
    FIELD /dsl/hr80_v003-begda .
    FIELD /dsl/hr80_v003-endda .
    MODULE set_update_flag ON CHAIN-REQUEST.
   ENDCHAIN.
   FIELD vim_marked MODULE liste_mark_checkbox.
   CHAIN.
    FIELD /dsl/hr80_v003-gjahr .
    FIELD /dsl/hr80_v003-vrsid .
    MODULE liste_update_liste.
   ENDCHAIN.
 ENDLOOP.
 MODULE liste_after_loop.
