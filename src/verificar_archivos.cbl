      ******************************************************************
      * Purpose: Verifica que los archivos de datos existan y puedan
      *          abrirse. Si un archivo no existe lo crea vacío.
      * Parámetros:
      *   LK-DIRECTORIO-DATOS (entrada)  directorio de los .dat
      *   LK-RESULTADO        (salida)   0 = OK, 1 = error
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VERIFICAR-ARCHIVOS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "fc-customers.cpy".
           COPY "fc-equipments.cpy".
           COPY "fc-budgets.cpy".
           COPY "fc-control.cpy".

       DATA DIVISION.
       FILE SECTION.
           COPY "fd-customers.cpy".
           COPY "fd-equipments.cpy".
           COPY "fd-budgets.cpy".
           COPY "fd-control.cpy".

       WORKING-STORAGE SECTION.
           COPY "ws-archivos.cpy".
       01  WS-NOMBRE-ARCHIVO           PIC X(20).
       01  WS-ESTADO-ARCHIVO           PIC XX.

       LINKAGE SECTION.
           COPY "lk-comun.cpy".
       01  LK-RESULTADO                PIC 9.

       PROCEDURE DIVISION USING LK-DIRECTORIO-DATOS LK-RESULTADO.
       VERIFICAR.
           MOVE ZERO TO LK-RESULTADO
           PERFORM ARMAR-RUTAS
           DISPLAY "Verificando archivos en "
                   FUNCTION TRIM(LK-DIRECTORIO-DATOS) "/" END-DISPLAY

           OPEN I-O CUSTOMERS-FILE
           MOVE "customers.dat" TO WS-NOMBRE-ARCHIVO
           MOVE WS-FS-CUSTOMERS TO WS-ESTADO-ARCHIVO
           PERFORM INFORMAR-ESTADO
           IF FS-CUSTOMERS-OK
               CLOSE CUSTOMERS-FILE
           END-IF

           OPEN I-O EQUIPMENTS-FILE
           MOVE "equipments.dat" TO WS-NOMBRE-ARCHIVO
           MOVE WS-FS-EQUIPMENTS TO WS-ESTADO-ARCHIVO
           PERFORM INFORMAR-ESTADO
           IF FS-EQUIPMENTS-OK
               CLOSE EQUIPMENTS-FILE
           END-IF

           OPEN I-O BUDGETS-FILE
           MOVE "budgets.dat" TO WS-NOMBRE-ARCHIVO
           MOVE WS-FS-BUDGETS TO WS-ESTADO-ARCHIVO
           PERFORM INFORMAR-ESTADO
           IF FS-BUDGETS-OK
               CLOSE BUDGETS-FILE
           END-IF

           OPEN I-O CONTROL-FILE
           MOVE "control.dat" TO WS-NOMBRE-ARCHIVO
           MOVE WS-FS-CONTROL TO WS-ESTADO-ARCHIVO
           PERFORM INFORMAR-ESTADO
           IF FS-CONTROL-OK
               CLOSE CONTROL-FILE
           END-IF

           GOBACK.

       INFORMAR-ESTADO.
           EVALUATE WS-ESTADO-ARCHIVO
               WHEN "00"
                   DISPLAY "  [OK]     " WS-NOMBRE-ARCHIVO END-DISPLAY
               WHEN "05"
                   DISPLAY "  [CREADO] " WS-NOMBRE-ARCHIVO END-DISPLAY
               WHEN OTHER
                   DISPLAY "  [ERROR]  " WS-NOMBRE-ARCHIVO
                           " (file status " WS-ESTADO-ARCHIVO ")"
                   END-DISPLAY
                   MOVE 1 TO LK-RESULTADO
           END-EVALUATE.

           COPY "proc-rutas.cpy".

       END PROGRAM VERIFICAR-ARCHIVOS.
