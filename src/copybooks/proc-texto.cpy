      *-----------------------------------------------------------------
      * Generación de archivos de texto dentro del directorio de datos
      * (comprobantes/ y exportes/). Requiere fc-texto.cpy, fd-texto.cpy
      * proc-comun.cpy y LK-DIRECTORIO-DATOS.
      *-----------------------------------------------------------------

      * Crea <datos>/<WS-SUBDIRECTORIO>/ y abre <WS-NOMBRE-TEXTO> en
      * modo OUTPUT. El resultado queda en WS-FS-TEXTO.
       ABRIR-ARCHIVO-TEXTO.
           MOVE SPACES TO WS-DIRECTORIO-TEXTO WS-RUTA-TEXTO WS-RENGLON
           MOVE 1 TO WS-PUNTERO-RENGLON
           STRING FUNCTION TRIM(LK-DIRECTORIO-DATOS) "/"
                  FUNCTION TRIM(WS-SUBDIRECTORIO)
               DELIMITED BY SIZE INTO WS-DIRECTORIO-TEXTO
           END-STRING
           CALL "CBL_CREATE_DIR" USING WS-DIRECTORIO-TEXTO END-CALL
           MOVE ZERO TO RETURN-CODE
           STRING FUNCTION TRIM(WS-DIRECTORIO-TEXTO) "/"
                  FUNCTION TRIM(WS-NOMBRE-TEXTO)
               DELIMITED BY SIZE INTO WS-RUTA-TEXTO
           END-STRING
           OPEN OUTPUT TEXT-FILE
           IF NOT FS-TEXTO-OK
               DISPLAY "  No se pudo crear "
                       FUNCTION TRIM(WS-RUTA-TEXTO)
                       " (file status " WS-FS-TEXTO ")." END-DISPLAY
           END-IF.

      * Escribe WS-RENGLON (sin los espacios finales) y lo limpia.
       ESCRIBIR-RENGLON.
           MOVE WS-RENGLON TO TEXT-LINE
           WRITE TEXT-LINE END-WRITE
           MOVE SPACES TO WS-RENGLON
           MOVE 1 TO WS-PUNTERO-RENGLON.

       CERRAR-ARCHIVO-TEXTO.
           CLOSE TEXT-FILE
           DISPLAY "  Archivo generado: " FUNCTION TRIM(WS-RUTA-TEXTO)
           END-DISPLAY.

      *-----------------------------------------------------------------
      * Comprobantes
      *-----------------------------------------------------------------

      * Encabezado con el nombre del negocio (RS_EMPRESA) y el título
      * recibido en WS-TITULO.
       ESCRIBIR-ENCABEZADO-COMPROBANTE.
           MOVE SPACES TO WS-EMPRESA
           ACCEPT WS-EMPRESA FROM ENVIRONMENT "RS_EMPRESA" END-ACCEPT
           IF WS-EMPRESA = SPACES
               MOVE "Servicio Técnico" TO WS-EMPRESA
           END-IF
           PERFORM OBTENER-FECHA-HOY
           MOVE WS-FECHA-HOY TO WS-FECHA
           PERFORM FORMATEAR-FECHA
           MOVE WS-SEPARADOR-DOBLE(1:60) TO WS-RENGLON
           PERFORM ESCRIBIR-RENGLON
           STRING "  " FUNCTION TRIM(WS-EMPRESA)
               DELIMITED BY SIZE INTO WS-RENGLON
           END-STRING
           PERFORM ESCRIBIR-RENGLON
           STRING "  " FUNCTION TRIM(WS-TITULO)
               DELIMITED BY SIZE INTO WS-RENGLON
           END-STRING
           PERFORM ESCRIBIR-RENGLON
           MOVE WS-SEPARADOR-DOBLE(1:60) TO WS-RENGLON
           PERFORM ESCRIBIR-RENGLON
           STRING "Emitido el " WS-FECHA-TXT
               DELIMITED BY SIZE INTO WS-RENGLON
           END-STRING
           PERFORM ESCRIBIR-RENGLON
           PERFORM ESCRIBIR-RENGLON.

      * Renglón "  Etiqueta:       valor" con la etiqueta alineada.
       ESCRIBIR-DATO.
           MOVE WS-DATO-ETIQUETA TO WS-CORTE-ORIGEN
           MOVE 17 TO WS-CORTE-ANCHO
           PERFORM AJUSTAR-ANCHO
           STRING "  " WS-CORTE-RESULTADO(1:WS-CORTE-LARGO)
                  FUNCTION TRIM(WS-DATO-VALOR)
               DELIMITED BY SIZE INTO WS-RENGLON
           END-STRING
           PERFORM ESCRIBIR-RENGLON.

      * Renglón de texto libre tomado de WS-DATO-VALOR.
       ESCRIBIR-TEXTO.
           MOVE WS-DATO-VALOR TO WS-RENGLON
           PERFORM ESCRIBIR-RENGLON.

       ESCRIBIR-PIE-COMPROBANTE.
           PERFORM ESCRIBIR-RENGLON
           PERFORM ESCRIBIR-RENGLON
           MOVE "  Firma del cliente:  ______________________________"
               TO WS-RENGLON
           PERFORM ESCRIBIR-RENGLON
           PERFORM ESCRIBIR-RENGLON
           MOVE "  Aclaración:         ______________________________"
               TO WS-RENGLON
           PERFORM ESCRIBIR-RENGLON
           MOVE WS-SEPARADOR-DOBLE(1:60) TO WS-RENGLON
           PERFORM ESCRIBIR-RENGLON.
