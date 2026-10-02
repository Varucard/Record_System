      *-----------------------------------------------------------------
      * Rutinas compartidas de entrada, validación y formato.
      * Requiere ws-comun.cpy en WORKING-STORAGE y DECIMAL-POINT IS
      * COMMA en SPECIAL-NAMES (importes con coma decimal).
      *-----------------------------------------------------------------

      * Lee una línea de la entrada estándar. Ignora los CR de los
      * finales de línea de Windows y convierte tabulaciones en
      * espacios. Si la entrada terminó (por ejemplo, al redirigir un
      * archivo) cierra el sistema con código de salida 2.
      * Cada programa que incluye este copybook define el párrafo
      * CERRAR-ARCHIVOS con los archivos que tiene abiertos.
       LEER-ENTRADA.
           MOVE SPACES TO WS-ENTRADA
           ACCEPT WS-ENTRADA
               ON EXCEPTION
                   DISPLAY " " END-DISPLAY
                   DISPLAY "Fin de la entrada. Cerrando el sistema."
                   END-DISPLAY
                   PERFORM CERRAR-ARCHIVOS
                   MOVE 2 TO RETURN-CODE
                   STOP RUN
           END-ACCEPT
           INSPECT WS-ENTRADA REPLACING ALL X"0D" BY SPACE
                                        ALL X"09" BY SPACE
           MOVE FUNCTION TRIM(WS-ENTRADA) TO WS-ENTRADA.

       MOSTRAR-PROMPT.
           DISPLAY FUNCTION TRIM(WS-PROMPT) " " WITH NO ADVANCING
           END-DISPLAY.

      * Pide un DNI (7 u 8 dígitos) y lo normaliza a 8 dígitos con
      * ceros a la izquierda (1234567 y 01234567 son el mismo DNI).
      * ENTER vacío cancela.
       LEER-DNI.
           SET OPERACION-EN-CURSO TO TRUE
           MOVE SPACES TO WS-DNI
           PERFORM UNTIL WS-DNI NOT = SPACES OR OPERACION-CANCELADA
               MOVE "DNI (sin puntos, ENTER cancela):" TO WS-PROMPT
               PERFORM MOSTRAR-PROMPT
               PERFORM LEER-ENTRADA
               IF WS-ENTRADA = SPACES
                   SET OPERACION-CANCELADA TO TRUE
               ELSE
                   COMPUTE WS-LARGO =
                       FUNCTION LENGTH(FUNCTION TRIM(WS-ENTRADA))
                   IF (WS-LARGO = 7 OR WS-LARGO = 8)
                      AND WS-ENTRADA(1:WS-LARGO) IS NUMERIC
                      AND WS-ENTRADA(1:WS-LARGO) NOT = ALL "0"
                       MOVE WS-ENTRADA(1:WS-LARGO) TO WS-DNI-NUMERO
                       MOVE WS-DNI-NUMERO TO WS-DNI
                   ELSE
                       DISPLAY "  DNI inválido: debe tener 7 u 8 "
                               "dígitos." END-DISPLAY
                   END-IF
               END-IF
           END-PERFORM.

      * Pide un identificador numérico (1 a 99999) usando WS-PROMPT.
      * ENTER vacío cancela.
       LEER-ID.
           SET OPERACION-EN-CURSO TO TRUE
           MOVE ZERO TO WS-ID
           PERFORM UNTIL WS-ID > ZERO OR OPERACION-CANCELADA
               PERFORM MOSTRAR-PROMPT
               PERFORM LEER-ENTRADA
               IF WS-ENTRADA = SPACES
                   SET OPERACION-CANCELADA TO TRUE
               ELSE
                   COMPUTE WS-LARGO =
                       FUNCTION LENGTH(FUNCTION TRIM(WS-ENTRADA))
                   IF WS-LARGO <= 5
                      AND WS-ENTRADA(1:WS-LARGO) IS NUMERIC
                       MOVE WS-ENTRADA(1:WS-LARGO) TO WS-ID
                   END-IF
                   IF WS-ID = ZERO
                       DISPLAY "  Ingrese un número entre 1 y 99999."
                       END-DISPLAY
                   END-IF
               END-IF
           END-PERFORM.

      * Pide un texto opcional de hasta WS-LARGO-MAXIMO bytes.
       LEER-TEXTO.
           MOVE "N" TO WS-CONFIRMA
           PERFORM UNTIL CONFIRMA-SI
               PERFORM MOSTRAR-PROMPT
               PERFORM LEER-ENTRADA
               PERFORM VALIDAR-LARGO
           END-PERFORM.

      * Pide un texto que no puede quedar vacío, usando WS-PROMPT.
       LEER-TEXTO-OBLIGATORIO.
           MOVE "N" TO WS-CONFIRMA
           PERFORM UNTIL CONFIRMA-SI
               PERFORM MOSTRAR-PROMPT
               PERFORM LEER-ENTRADA
               IF WS-ENTRADA = SPACES
                   DISPLAY "  Este dato es obligatorio." END-DISPLAY
               ELSE
                   PERFORM VALIDAR-LARGO
               END-IF
           END-PERFORM.

      * En una modificación, deja en WS-ENTRADA el valor final de un
      * campo opcional: ENTER mantiene WS-VALOR-ACTUAL y "-" lo borra.
       ACTUALIZAR-CAMPO.
           EVALUATE WS-ENTRADA
               WHEN SPACES
                   MOVE WS-VALOR-ACTUAL TO WS-ENTRADA
               WHEN "-"
                   MOVE SPACES TO WS-ENTRADA
           END-EVALUATE.

      * Evita que el dato se trunque al guardarlo. El largo se mide
      * en bytes: las letras acentuadas ocupan dos.
       VALIDAR-LARGO.
           MOVE "S" TO WS-CONFIRMA
           IF WS-ENTRADA NOT = SPACES
               COMPUTE WS-LARGO =
                   FUNCTION LENGTH(FUNCTION TRIM(WS-ENTRADA))
               IF WS-LARGO > WS-LARGO-MAXIMO
                   MOVE "N" TO WS-CONFIRMA
                   MOVE WS-LARGO-MAXIMO TO WS-LARGO-MAXIMO-ED
                   DISPLAY "  Texto demasiado largo: máximo "
                           FUNCTION TRIM(WS-LARGO-MAXIMO-ED)
                           " caracteres." END-DISPLAY
               END-IF
           END-IF.

      * Pide un email opcional (máximo 50 caracteres).
       LEER-EMAIL.
           MOVE 50 TO WS-LARGO-MAXIMO
           MOVE "N" TO WS-CONFIRMA
           PERFORM UNTIL CONFIRMA-SI
               PERFORM MOSTRAR-PROMPT
               PERFORM LEER-ENTRADA
               PERFORM VALIDAR-LARGO
               IF CONFIRMA-SI
                   PERFORM VALIDAR-EMAIL
               END-IF
           END-PERFORM.

      * Formato usuario@dominio.ext: sin espacios, una sola "@",
      * texto antes de la "@" y un "." dentro del dominio.
       VALIDAR-EMAIL.
           MOVE "S" TO WS-CONFIRMA
           IF WS-ENTRADA NOT = SPACES AND WS-ENTRADA NOT = "-"
               COMPUTE WS-LARGO =
                   FUNCTION LENGTH(FUNCTION TRIM(WS-ENTRADA))
               MOVE ZERO TO WS-CANTIDAD
               INSPECT WS-ENTRADA(1:WS-LARGO)
                   TALLYING WS-CANTIDAD FOR ALL "@" ALL SPACE
               MOVE SPACES TO WS-EMAIL-USUARIO WS-EMAIL-DOMINIO
               UNSTRING WS-ENTRADA DELIMITED BY "@"
                   INTO WS-EMAIL-USUARIO WS-EMAIL-DOMINIO
               END-UNSTRING
               MOVE ZERO TO WS-CANT-PUNTOS
               INSPECT WS-EMAIL-DOMINIO
                   TALLYING WS-CANT-PUNTOS FOR ALL "."
               COMPUTE WS-POSICION = FUNCTION LENGTH(
                   FUNCTION TRIM(WS-EMAIL-DOMINIO))
               IF WS-CANTIDAD NOT = 1
                  OR WS-EMAIL-USUARIO = SPACES
                  OR WS-EMAIL-DOMINIO = SPACES
                  OR WS-CANT-PUNTOS = ZERO
                  OR WS-EMAIL-DOMINIO(1:1) = "."
                  OR WS-EMAIL-DOMINIO(WS-POSICION:1) = "."
                   MOVE "N" TO WS-CONFIRMA
                   DISPLAY "  Email inválido (ej.: juan@mail.com)."
                   END-DISPLAY
               END-IF
           END-IF.

      * Pide un importe mayor a cero en formato argentino: la coma es
      * el separador decimal y el punto el de miles (15.000,50).
      * También acepta 15000.50 (un punto seguido de 1 o 2 dígitos).
       LEER-IMPORTE.
           MOVE ZERO TO WS-IMPORTE
           PERFORM UNTIL WS-IMPORTE > ZERO
               PERFORM MOSTRAR-PROMPT
               PERFORM LEER-ENTRADA
               PERFORM NORMALIZAR-IMPORTE
               MOVE ZERO TO WS-IMPORTE-LEIDO
               IF WS-IMPORTE-TXT NOT = SPACES
                  AND FUNCTION TEST-NUMVAL(WS-IMPORTE-TXT) = ZERO
                   COMPUTE WS-IMPORTE-LEIDO ROUNDED =
                       FUNCTION NUMVAL(WS-IMPORTE-TXT)
                       ON SIZE ERROR MOVE ZERO TO WS-IMPORTE-LEIDO
                   END-COMPUTE
               END-IF
               IF WS-IMPORTE-LEIDO > ZERO
                   MOVE WS-IMPORTE-LEIDO TO WS-IMPORTE
               ELSE
                   DISPLAY "  Importe inválido: ingrese un número "
                           "mayor a cero (ej.: 15.000,50)."
                   END-DISPLAY
               END-IF
           END-PERFORM.

      * Deja en WS-IMPORTE-TXT el importe sin separadores de miles y
      * con coma decimal, o espacios si el formato es inválido.
       NORMALIZAR-IMPORTE.
           MOVE SPACES TO WS-IMPORTE-TXT
           MOVE ZERO TO WS-CANT-COMAS WS-CANT-PUNTOS WS-POSICION
           IF WS-ENTRADA NOT = SPACES
               COMPUTE WS-LARGO =
                   FUNCTION LENGTH(FUNCTION TRIM(WS-ENTRADA))
               INSPECT WS-ENTRADA TALLYING WS-CANT-COMAS FOR ALL ","
                                           WS-CANT-PUNTOS FOR ALL "."
               INSPECT WS-ENTRADA TALLYING WS-POSICION
                   FOR CHARACTERS BEFORE INITIAL "."
               EVALUATE TRUE
                   WHEN WS-CANT-COMAS > 1
                       CONTINUE
                   WHEN WS-CANT-COMAS = 0 AND WS-CANT-PUNTOS = 1
                    AND WS-LARGO - WS-POSICION - 1 < 3
      *                Un punto seguido de 1 o 2 dígitos: decimal.
                       MOVE WS-ENTRADA TO WS-IMPORTE-TXT
                       INSPECT WS-IMPORTE-TXT REPLACING ALL "." BY ","
                   WHEN OTHER
      *                Los puntos son separadores de miles.
                       MOVE ZERO TO WS-J
                       PERFORM VARYING WS-I FROM 1 BY 1
                               UNTIL WS-I > WS-LARGO
                           IF WS-ENTRADA(WS-I:1) NOT = "."
                               ADD 1 TO WS-J
                               MOVE WS-ENTRADA(WS-I:1)
                                   TO WS-IMPORTE-TXT(WS-J:1)
                           END-IF
                       END-PERFORM
               END-EVALUATE
           END-IF.

      * Pide una fecha DD/MM/AAAA. ENTER vacío toma la fecha de hoy.
      * Si WS-FECHA-MINIMA tiene valor, la fecha no puede ser anterior.
       LEER-FECHA.
           PERFORM OBTENER-FECHA-HOY
           MOVE ZERO TO WS-FECHA
           PERFORM UNTIL WS-FECHA > ZERO
               PERFORM MOSTRAR-PROMPT
               PERFORM LEER-ENTRADA
               IF WS-ENTRADA = SPACES
                   MOVE WS-FECHA-HOY TO WS-FECHA
               ELSE
                   IF WS-ENTRADA(3:1) = "/" AND WS-ENTRADA(6:1) = "/"
                      AND WS-ENTRADA(1:2) IS NUMERIC
                      AND WS-ENTRADA(4:2) IS NUMERIC
                      AND WS-ENTRADA(7:4) IS NUMERIC
                      AND WS-ENTRADA(11:) = SPACES
                       MOVE WS-ENTRADA(7:4) TO WS-FECHA-AAAA
                       MOVE WS-ENTRADA(4:2) TO WS-FECHA-MM
                       MOVE WS-ENTRADA(1:2) TO WS-FECHA-DD
                       IF FUNCTION TEST-DATE-YYYYMMDD(WS-FECHA)
                          NOT = ZERO
                          OR WS-FECHA-AAAA < 2000
                          OR WS-FECHA-AAAA > 2099
                           MOVE ZERO TO WS-FECHA
                       END-IF
                   END-IF
                   IF WS-FECHA = ZERO
                       DISPLAY "  Fecha inválida: use DD/MM/AAAA "
                               "(años 2000 a 2099)." END-DISPLAY
                   END-IF
               END-IF
               IF WS-FECHA > ZERO AND WS-FECHA < WS-FECHA-MINIMA
                   MOVE WS-FECHA TO WS-FECHA-HOY
                   MOVE WS-FECHA-MINIMA TO WS-FECHA
                   PERFORM FORMATEAR-FECHA
                   DISPLAY "  La fecha no puede ser anterior al "
                           WS-FECHA-TXT "." END-DISPLAY
                   MOVE ZERO TO WS-FECHA
                   PERFORM OBTENER-FECHA-HOY
               END-IF
           END-PERFORM
           MOVE ZERO TO WS-FECHA-MINIMA.

      * Pregunta S/N usando WS-PROMPT. Deja el resultado en WS-CONFIRMA.
       CONFIRMAR.
           MOVE SPACE TO WS-CONFIRMA
           PERFORM UNTIL CONFIRMA-SI OR CONFIRMA-NO
               PERFORM MOSTRAR-PROMPT
               PERFORM LEER-ENTRADA
               EVALUATE FUNCTION UPPER-CASE(WS-ENTRADA)
                   WHEN "S"
                   WHEN "SI"
                   WHEN "SÍ"
                   WHEN "Sí"
                       SET CONFIRMA-SI TO TRUE
                   WHEN "N"
                   WHEN "NO"
                       SET CONFIRMA-NO TO TRUE
                   WHEN OTHER
                       DISPLAY "  Responda S o N." END-DISPLAY
               END-EVALUATE
           END-PERFORM.

      * Fecha del sistema en formato AAAAMMDD. Puede fijarse con la
      * variable de entorno RS_FECHA_HOY (útil para pruebas).
       OBTENER-FECHA-HOY.
           MOVE SPACES TO WS-IMPORTE-TXT
           ACCEPT WS-IMPORTE-TXT FROM ENVIRONMENT "RS_FECHA_HOY"
           END-ACCEPT
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-FECHA-HOY
           IF WS-IMPORTE-TXT(1:8) IS NUMERIC
              AND WS-IMPORTE-TXT(9:) = SPACES
               IF FUNCTION TEST-DATE-YYYYMMDD(
                      FUNCTION NUMVAL(WS-IMPORTE-TXT(1:8))) = ZERO
                   MOVE WS-IMPORTE-TXT(1:8) TO WS-FECHA-HOY
               END-IF
           END-IF.

      * Convierte WS-FECHA (AAAAMMDD) a WS-FECHA-TXT (DD/MM/AAAA).
       FORMATEAR-FECHA.
           IF WS-FECHA = ZERO
               MOVE "-" TO WS-FECHA-TXT
           ELSE
               STRING WS-FECHA-DD "/" WS-FECHA-MM "/" WS-FECHA-AAAA
                   DELIMITED BY SIZE INTO WS-FECHA-TXT
               END-STRING
           END-IF.

      * Paginado de listados: se invoca ANTES de mostrar cada renglón.
      * Cuando la página está completa pide ENTER; con "0" termina el
      * listado (marca FIN-LECTURA y el renglón no debe mostrarse).
       CONTROLAR-PAGINA.
           IF WS-LINEAS-MOSTRADAS >= WS-LINEAS-POR-PAGINA
               DISPLAY "-- ENTER para continuar, 0 para terminar --"
               END-DISPLAY
               PERFORM LEER-ENTRADA
               MOVE ZERO TO WS-LINEAS-MOSTRADAS
               IF WS-ENTRADA = "0"
                   SET FIN-LECTURA TO TRUE
               END-IF
           END-IF
           ADD 1 TO WS-LINEAS-MOSTRADAS.
