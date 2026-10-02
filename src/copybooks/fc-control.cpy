      *-----------------------------------------------------------------
      * Archivo físico de control: último número asignado a cada
      * entidad, para no reutilizar números de registros borrados.
      *-----------------------------------------------------------------
           SELECT OPTIONAL CONTROL-FILE
               ASSIGN DYNAMIC WS-RUTA-CONTROL
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CONTROL-CLAVE
               FILE STATUS IS WS-FS-CONTROL.
