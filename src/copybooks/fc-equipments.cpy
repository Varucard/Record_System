      *-----------------------------------------------------------------
      * Archivo físico de Equipos (indexado por ID, alternativa DNI).
      *-----------------------------------------------------------------
           SELECT OPTIONAL EQUIPMENTS-FILE
               ASSIGN DYNAMIC WS-RUTA-EQUIPMENTS
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS EQUIPMENTS-ID
               ALTERNATE RECORD KEY IS EQUIPMENTS-DNI WITH DUPLICATES
               FILE STATUS IS WS-FS-EQUIPMENTS.
