PROCESS BEFORE OUTPUT.
 MODULE liste_initialisieren.
 LOOP AT extract WITH CONTROL
  tctrl_/dsl/hr80_v014 CURSOR nextline.
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
    FIELD /dsl/hr80_v014-ktopl .
    FIELD /dsl/hr80_v014-ktosl .
    FIELD /dsl/hr80_v014-bwmod .
    FIELD /dsl/hr80_v014-komok .
    FIELD /dsl/hr80_v014-bklas .
    FIELD /dsl/hr80_v014-konts .
    FIELD /dsl/hr80_v014-konth .
    MODULE set_update_flag ON CHAIN-REQUEST.
   ENDCHAIN.
   FIELD vim_marked MODULE liste_mark_checkbox.
   CHAIN.
    FIELD /dsl/hr80_v014-ktopl .
    FIELD /dsl/hr80_v014-ktosl .
    FIELD /dsl/hr80_v014-bwmod .
    FIELD /dsl/hr80_v014-komok .
    FIELD /dsl/hr80_v014-bklas .
    MODULE liste_update_liste.
   ENDCHAIN.
 ENDLOOP.
 MODULE liste_after_loop.
