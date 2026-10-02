      ******************************************************************
      * Purpose: Resumen general del negocio: clientes, equipos por
      *          estado, presupuestos cobrados / pendientes y
      *          recaudación por forma de pago.
      * Parámetros:
      *   LK-DIRECTORIO-DATOS (entrada)  directorio de los .dat
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTES.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "fc-customers.cpy".
           COPY "fc-equipments.cpy".
           COPY "fc-budgets.cpy".

       DATA DIVISION.
       FILE SECTION.
           COPY "fd-customers.cpy".
           COPY "fd-equipments.cpy".
           COPY "fd-budgets.cpy".

       WORKING-STORAGE SECTION.
           COPY "ws-archivos.cpy".
           COPY "ws-comun.cpy".
       01  WS-TOTALES.
           05 WS-TOT-CLIENTES          PIC 9(5).
           05 WS-TOT-EQUIPOS           PIC 9(5).
           05 WS-TOT-INGRESADOS        PIC 9(5).
           05 WS-TOT-EN-REPARACION     PIC 9(5).
           05 WS-TOT-LISTOS            PIC 9(5).
           05 WS-TOT-ENTREGADOS        PIC 9(5).
           05 WS-TOT-PRESUPUESTOS      PIC 9(5).
           05 WS-TOT-PAGADOS           PIC 9(5).
           05 WS-TOT-PENDIENTES        PIC 9(5).
           05 WS-MONTO-PAGADO          PIC 9(11)V99.
           05 WS-MONTO-PENDIENTE       PIC 9(11)V99.

      * Recaudación agrupada por forma de pago.
       01  WS-FORMAS-PAGO.
           05 WS-CANT-FORMAS           PIC 9(2) VALUE ZERO.
           05 WS-FORMA OCCURS 10 TIMES INDEXED BY IX-FORMA.
              10 WS-FORMA-NOMBRE       PIC X(15).
              10 WS-FORMA-CANTIDAD     PIC 9(5).
              10 WS-FORMA-MONTO        PIC 9(11)V99.

       01  WS-MONTO-ED                 PIC ZZ.ZZZ.ZZZ.ZZ9,99.
       01  WS-ETIQUETA                 PIC X(32).

       LINKAGE SECTION.
           COPY "lk-comun.cpy".

       PROCEDURE DIVISION USING LK-DIRECTORIO-DATOS.
       INICIO-REPORTES.
           PERFORM ARMAR-RUTAS
           INITIALIZE WS-TOTALES WS-FORMAS-PAGO
           OPEN INPUT CUSTOMERS-FILE EQUIPMENTS-FILE BUDGETS-FILE
           IF NOT FS-CUSTOMERS-OK OR NOT FS-EQUIPMENTS-OK
              OR NOT FS-BUDGETS-OK
               DISPLAY "Error al abrir los archivos (file status "
                       WS-FS-CUSTOMERS "/" WS-FS-EQUIPMENTS "/"
                       WS-FS-BUDGETS ")." END-DISPLAY
           ELSE
               PERFORM CONTAR-CLIENTES
               PERFORM CONTAR-EQUIPOS
               PERFORM CONTAR-PRESUPUESTOS
               PERFORM MOSTRAR-RESUMEN
           END-IF
           PERFORM CERRAR-ARCHIVOS
           GOBACK.

       CONTAR-CLIENTES.
           SET HAY-MAS-REGISTROS TO TRUE
           PERFORM UNTIL FIN-LECTURA
               READ CUSTOMERS-FILE NEXT RECORD
                   AT END SET FIN-LECTURA TO TRUE
                   NOT AT END ADD 1 TO WS-TOT-CLIENTES
               END-READ
               IF NOT FS-CUSTOMERS-OK
                   SET FIN-LECTURA TO TRUE
               END-IF
           END-PERFORM.

       CONTAR-EQUIPOS.
           SET HAY-MAS-REGISTROS TO TRUE
           PERFORM UNTIL FIN-LECTURA
               READ EQUIPMENTS-FILE NEXT RECORD
                   AT END SET FIN-LECTURA TO TRUE
                   NOT AT END
                       ADD 1 TO WS-TOT-EQUIPOS
                       EVALUATE TRUE
                           WHEN EQUIPO-INGRESADO
                               ADD 1 TO WS-TOT-INGRESADOS
                           WHEN EQUIPO-EN-REPARACION
                               ADD 1 TO WS-TOT-EN-REPARACION
                           WHEN EQUIPO-LISTO
                               ADD 1 TO WS-TOT-LISTOS
                           WHEN EQUIPO-ENTREGADO
                               ADD 1 TO WS-TOT-ENTREGADOS
                       END-EVALUATE
               END-READ
               IF NOT FS-EQUIPMENTS-OK
                   SET FIN-LECTURA TO TRUE
               END-IF
           END-PERFORM.

       CONTAR-PRESUPUESTOS.
           SET HAY-MAS-REGISTROS TO TRUE
           PERFORM UNTIL FIN-LECTURA
               READ BUDGETS-FILE NEXT RECORD
                   AT END SET FIN-LECTURA TO TRUE
                   NOT AT END
                       ADD 1 TO WS-TOT-PRESUPUESTOS
                       IF PRESUPUESTO-PAGADO
                           ADD 1 TO WS-TOT-PAGADOS
                           ADD BUDGETS-IMPORTE TO WS-MONTO-PAGADO
                           PERFORM ACUMULAR-FORMA-PAGO
                       ELSE
                           ADD 1 TO WS-TOT-PENDIENTES
                           ADD BUDGETS-IMPORTE TO WS-MONTO-PENDIENTE
                       END-IF
               END-READ
               IF NOT FS-BUDGETS-OK
                   SET FIN-LECTURA TO TRUE
               END-IF
           END-PERFORM.

      * Busca la forma de pago en la tabla; si no está, la agrega en
      * la primera posición libre. Con la tabla llena se ignora.
       ACUMULAR-FORMA-PAGO.
           SET IX-FORMA TO 1
           SEARCH WS-FORMA
               AT END
                   CONTINUE
               WHEN IX-FORMA > WS-CANT-FORMAS
                   ADD 1 TO WS-CANT-FORMAS
                   MOVE BUDGETS-FORMA-PAGO TO WS-FORMA-NOMBRE(IX-FORMA)
                   MOVE 1 TO WS-FORMA-CANTIDAD(IX-FORMA)
                   MOVE BUDGETS-IMPORTE TO WS-FORMA-MONTO(IX-FORMA)
               WHEN WS-FORMA-NOMBRE(IX-FORMA) = BUDGETS-FORMA-PAGO
                   ADD 1 TO WS-FORMA-CANTIDAD(IX-FORMA)
                   ADD BUDGETS-IMPORTE TO WS-FORMA-MONTO(IX-FORMA)
           END-SEARCH.

       MOSTRAR-RESUMEN.
           DISPLAY " " END-DISPLAY
           DISPLAY WS-SEPARADOR-DOBLE END-DISPLAY
           DISPLAY "  REPORTE GENERAL" END-DISPLAY
           DISPLAY WS-SEPARADOR-DOBLE END-DISPLAY
           MOVE "Clientes registrados:" TO WS-ETIQUETA
           MOVE WS-TOT-CLIENTES TO WS-CANTIDAD
           PERFORM MOSTRAR-CANTIDAD

           DISPLAY " " END-DISPLAY
           MOVE "Equipos registrados:" TO WS-ETIQUETA
           MOVE WS-TOT-EQUIPOS TO WS-CANTIDAD
           PERFORM MOSTRAR-CANTIDAD
           MOVE "  - Ingresados:" TO WS-ETIQUETA
           MOVE WS-TOT-INGRESADOS TO WS-CANTIDAD
           PERFORM MOSTRAR-CANTIDAD
           MOVE "  - En reparación:" TO WS-ETIQUETA
           MOVE WS-TOT-EN-REPARACION TO WS-CANTIDAD
           PERFORM MOSTRAR-CANTIDAD
           MOVE "  - Listos para retirar:" TO WS-ETIQUETA
           MOVE WS-TOT-LISTOS TO WS-CANTIDAD
           PERFORM MOSTRAR-CANTIDAD
           MOVE "  - Entregados:" TO WS-ETIQUETA
           MOVE WS-TOT-ENTREGADOS TO WS-CANTIDAD
           PERFORM MOSTRAR-CANTIDAD

           DISPLAY " " END-DISPLAY
           MOVE "Presupuestos emitidos:" TO WS-ETIQUETA
           MOVE WS-TOT-PRESUPUESTOS TO WS-CANTIDAD
           PERFORM MOSTRAR-CANTIDAD
           MOVE "  - Pagados:" TO WS-ETIQUETA
           MOVE WS-TOT-PAGADOS TO WS-CANTIDAD
           PERFORM MOSTRAR-CANTIDAD
           MOVE "  - Pendientes de pago:" TO WS-ETIQUETA
           MOVE WS-TOT-PENDIENTES TO WS-CANTIDAD
           PERFORM MOSTRAR-CANTIDAD

           DISPLAY " " END-DISPLAY
           MOVE WS-MONTO-PAGADO TO WS-MONTO-ED
           DISPLAY "Total cobrado:              $ " WS-MONTO-ED
           END-DISPLAY
           MOVE WS-MONTO-PENDIENTE TO WS-MONTO-ED
           DISPLAY "Total pendiente de cobro:   $ " WS-MONTO-ED
           END-DISPLAY

           IF WS-CANT-FORMAS > ZERO
               DISPLAY " " END-DISPLAY
               DISPLAY "Recaudación por forma de pago:" END-DISPLAY
               PERFORM VARYING IX-FORMA FROM 1 BY 1
                       UNTIL IX-FORMA > WS-CANT-FORMAS
                   MOVE WS-FORMA-MONTO(IX-FORMA) TO WS-MONTO-ED
                   MOVE WS-FORMA-CANTIDAD(IX-FORMA) TO WS-CANTIDAD-ED
                   DISPLAY "  - " WS-FORMA-NOMBRE(IX-FORMA)
                           WS-CANTIDAD-ED " pago(s)  $ " WS-MONTO-ED
                   END-DISPLAY
               END-PERFORM
           END-IF
           DISPLAY WS-SEPARADOR-DOBLE END-DISPLAY.

       MOSTRAR-CANTIDAD.
           MOVE WS-CANTIDAD TO WS-CANTIDAD-ED
           DISPLAY WS-ETIQUETA WS-CANTIDAD-ED END-DISPLAY.

       CERRAR-ARCHIVOS.
           CLOSE CUSTOMERS-FILE EQUIPMENTS-FILE BUDGETS-FILE.

           COPY "proc-rutas.cpy".
           COPY "proc-comun.cpy".

       END PROGRAM REPORTES.
