PROCESS BEFORE OUTPUT.
 MODULE status_2000.
 MODULE objid_text_2000.

PROCESS AFTER INPUT.

  CHAIN.
    FIELD /dsl/hr80_s005-begda.
    FIELD /dsl/hr80_s005-endda.
    FIELD /dsl/hr80_s005-zcount.
    FIELD /dsl/hr80_s005-rfper.
    MODULE input_check ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD /dsl/hr80_s005-orgeh  .
    FIELD /dsl/hr80_s005-plans.
    FIELD /dsl/hr80_s005-stell.
    MODULE objid_text ON CHAIN-REQUEST.
  ENDCHAIN.


  CHAIN.
    FIELD /dsl/hr80_s005-pernr. MODULE input_pernr ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD /dsl/hr80_s005-rfper. MODULE input_rfper ON CHAIN-REQUEST.
  ENDCHAIN.



 MODULE user_command_2000     .
