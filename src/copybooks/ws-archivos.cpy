      *-----------------------------------------------------------------
      * Rutas y estados de los archivos de datos.
      *-----------------------------------------------------------------
       01  WS-RUTA-CUSTOMERS           PIC X(250).
       01  WS-RUTA-EQUIPMENTS          PIC X(250).
       01  WS-RUTA-BUDGETS             PIC X(250).
       01  WS-RUTA-CONTROL             PIC X(250).
       01  WS-RUTA-TEXTO               PIC X(300).

       01  WS-FS-CUSTOMERS             PIC XX.
           88 FS-CUSTOMERS-OK          VALUE "00" THRU "09".
       01  WS-FS-EQUIPMENTS            PIC XX.
           88 FS-EQUIPMENTS-OK         VALUE "00" THRU "09".
       01  WS-FS-BUDGETS               PIC XX.
           88 FS-BUDGETS-OK            VALUE "00" THRU "09".
       01  WS-FS-CONTROL               PIC XX.
           88 FS-CONTROL-OK            VALUE "00" THRU "09".
       01  WS-FS-TEXTO                 PIC XX.
           88 FS-TEXTO-OK              VALUE "00" THRU "09".

      * Generación de archivos de texto (comprobantes / CSV).
       01  WS-SUBDIRECTORIO            PIC X(20).
       01  WS-NOMBRE-TEXTO             PIC X(40).
       01  WS-DIRECTORIO-TEXTO         PIC X(250).
       01  WS-RENGLON                  PIC X(1000).
       01  WS-PUNTERO-RENGLON          PIC 9(4).
       01  WS-CLAVE-CONTROL            PIC X(12).
       01  WS-EMPRESA                  PIC X(60).
       01  WS-DATO-ETIQUETA            PIC X(30).
       01  WS-DATO-VALOR               PIC X(200).
