      ******************************************************************
      * Author: Marquez Cristian Ariel
      * Date: 05/02/2023
      * Purpose: Registro de clientes, equipos informáticos,
      *          presupuestos y pagos de un servicio técnico.
      * Tectonics: cobc -x (ver Makefile)
      *
      * Programa principal: pantalla de inicio y menú general.
      * El directorio de datos se toma de la variable de entorno
      * RS_DATA_DIR (por defecto "data"). RS_INTERFAZ=pantalla activa
      * la interfaz de pantalla completa (por defecto, la clásica).
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECORD-SYSTEM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-VERSION                  PIC X(10) VALUE "2.1.0".
       01  WS-DIRECTORIO-DATOS         PIC X(200).
       01  WS-RESULTADO-VERIFICACION   PIC 9 VALUE ZERO.
           88 ARCHIVOS-OK              VALUE 0.
       COPY "ws-comun.cpy".

       PROCEDURE DIVISION.
       PROGRAMA-PRINCIPAL.
           PERFORM INICIALIZAR
           SET SEGUIR-EN-MENU TO TRUE
           PERFORM MENU-PRINCIPAL UNTIL SALIR-MENU
           DISPLAY " " END-DISPLAY
           DISPLAY "Gracias por usar Record System. Hasta luego."
           END-DISPLAY
           PERFORM RESTAURAR-TERMINAL
           MOVE ZERO TO RETURN-CODE
           STOP RUN.

      * Pantalla de inicio, creación del directorio de datos y
      * verificación de los archivos .dat.
       INICIALIZAR.
           PERFORM INICIAR-INTERFAZ
           IF MODO-PANTALLA
               PERFORM PREPARAR-PANTALLA
           END-IF
           DISPLAY WS-SEPARADOR-DOBLE END-DISPLAY
           DISPLAY "  RECORD SYSTEM v" FUNCTION TRIM(WS-VERSION)
           END-DISPLAY
           DISPLAY "  Gestión de clientes, equipos, presupuestos "
                   "y pagos" END-DISPLAY
           DISPLAY WS-SEPARADOR-DOBLE END-DISPLAY

           MOVE SPACES TO WS-DIRECTORIO-DATOS
           ACCEPT WS-DIRECTORIO-DATOS FROM ENVIRONMENT "RS_DATA_DIR"
           END-ACCEPT
           IF WS-DIRECTORIO-DATOS = SPACES
               MOVE "data" TO WS-DIRECTORIO-DATOS
           END-IF
      *    Si el directorio ya existe la llamada devuelve error: se
      *    ignora, la verificación posterior detecta problemas reales.
           CALL "CBL_CREATE_DIR" USING WS-DIRECTORIO-DATOS END-CALL
           MOVE ZERO TO RETURN-CODE

           CALL "VERIFICAR-ARCHIVOS" USING WS-DIRECTORIO-DATOS
                                           WS-RESULTADO-VERIFICACION
           END-CALL
           IF NOT ARCHIVOS-OK
               DISPLAY "No se pudieron abrir los archivos de datos en "
                       FUNCTION TRIM(WS-DIRECTORIO-DATOS) END-DISPLAY
               PERFORM RESTAURAR-TERMINAL
               MOVE 1 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM MARCAR-PAUSA.

       MENU-PRINCIPAL.
           PERFORM PREPARAR-PANTALLA
           MOVE "MENÚ PRINCIPAL" TO WS-TITULO
           PERFORM MOSTRAR-TITULO
           DISPLAY "  1. Clientes" END-DISPLAY
           DISPLAY "  2. Equipos" END-DISPLAY
           DISPLAY "  3. Presupuestos y pagos" END-DISPLAY
           DISPLAY "  4. Reportes" END-DISPLAY
           DISPLAY "  0. Salir" END-DISPLAY
           MOVE "Opción:" TO WS-PROMPT
           PERFORM MOSTRAR-PROMPT
           PERFORM LEER-ENTRADA
           MOVE WS-ENTRADA TO WS-OPCION
           EVALUATE WS-OPCION
               WHEN "1"
                   CALL "CLIENTES" USING WS-DIRECTORIO-DATOS END-CALL
               WHEN "2"
                   CALL "EQUIPOS" USING WS-DIRECTORIO-DATOS END-CALL
               WHEN "3"
                   CALL "PRESUPUESTOS" USING WS-DIRECTORIO-DATOS
                   END-CALL
               WHEN "4"
                   CALL "REPORTES" USING WS-DIRECTORIO-DATOS END-CALL
               WHEN "0"
                   SET SALIR-MENU TO TRUE
               WHEN OTHER
                   DISPLAY "  Opción inválida." END-DISPLAY
                   PERFORM MARCAR-PAUSA
           END-EVALUATE.

       CERRAR-ARCHIVOS.
      *    El programa principal no abre archivos.
           CONTINUE.

       COPY "proc-comun.cpy".

       END PROGRAM RECORD-SYSTEM.
