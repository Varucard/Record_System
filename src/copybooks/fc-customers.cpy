      *-----------------------------------------------------------------
      * Archivo físico de Clientes (indexado por DNI).
      *-----------------------------------------------------------------
           SELECT OPTIONAL CUSTOMERS-FILE
               ASSIGN DYNAMIC WS-RUTA-CUSTOMERS
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUSTOMERS-DNI
               FILE STATUS IS WS-FS-CUSTOMERS.
