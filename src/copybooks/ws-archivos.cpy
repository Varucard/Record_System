      *-----------------------------------------------------------------
      * Rutas y estados de los archivos de datos.
      *-----------------------------------------------------------------
       01  WS-RUTA-CUSTOMERS           PIC X(250).
       01  WS-RUTA-EQUIPMENTS          PIC X(250).
       01  WS-RUTA-BUDGETS             PIC X(250).

       01  WS-FS-CUSTOMERS             PIC XX.
           88 FS-CUSTOMERS-OK          VALUE "00" THRU "09".
       01  WS-FS-EQUIPMENTS            PIC XX.
           88 FS-EQUIPMENTS-OK         VALUE "00" THRU "09".
       01  WS-FS-BUDGETS               PIC XX.
           88 FS-BUDGETS-OK            VALUE "00" THRU "09".
