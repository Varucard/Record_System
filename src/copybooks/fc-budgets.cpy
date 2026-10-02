      *-----------------------------------------------------------------
      * Archivo físico de Presupuestos (indexado por ID, alternativas
      * por equipo y por DNI del cliente).
      *-----------------------------------------------------------------
           SELECT OPTIONAL BUDGETS-FILE
               ASSIGN DYNAMIC WS-RUTA-BUDGETS
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BUDGETS-ID
               ALTERNATE RECORD KEY IS BUDGETS-EQUIPO-ID
                   WITH DUPLICATES
               ALTERNATE RECORD KEY IS BUDGETS-DNI WITH DUPLICATES
               FILE STATUS IS WS-FS-BUDGETS.
