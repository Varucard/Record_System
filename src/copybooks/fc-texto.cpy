      *-----------------------------------------------------------------
      * Archivo de texto de salida (comprobantes y exportaciones CSV).
      *-----------------------------------------------------------------
           SELECT TEXT-FILE
               ASSIGN DYNAMIC WS-RUTA-TEXTO
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-TEXTO.
