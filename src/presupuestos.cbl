      ******************************************************************
      * Purpose: Presupuestos de trabajos sobre equipos y registro de
      *          pagos (alta, consulta, listados, pago, modificación y
      *          baja).
      * Parámetros:
      *   LK-DIRECTORIO-DATOS (entrada)  directorio de los .dat
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRESUPUESTOS.

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
       01  WS-FILTRO                   PIC X VALUE "T".
           88 FILTRO-TODOS             VALUE "T".
           88 FILTRO-EQUIPO            VALUE "E".
           88 FILTRO-PENDIENTES        VALUE "P".
       01  WS-FILTRO-EQUIPO            PIC 9(5).
       01  WS-TOTAL-LISTADO            PIC 9(11)V99.
       01  WS-TOTAL-LISTADO-ED         PIC ZZ.ZZZ.ZZZ.ZZ9,99.
       01  WS-TEXTO-PAGADO             PIC X(9).

       LINKAGE SECTION.
           COPY "lk-comun.cpy".

       PROCEDURE DIVISION USING LK-DIRECTORIO-DATOS.
       INICIO-PRESUPUESTOS.
           PERFORM ARMAR-RUTAS
           OPEN I-O BUDGETS-FILE
           OPEN INPUT CUSTOMERS-FILE EQUIPMENTS-FILE
           IF NOT FS-BUDGETS-OK OR NOT FS-CUSTOMERS-OK
              OR NOT FS-EQUIPMENTS-OK
               DISPLAY "Error al abrir los archivos (file status "
                       WS-FS-BUDGETS "/" WS-FS-CUSTOMERS "/"
                       WS-FS-EQUIPMENTS ")." END-DISPLAY
               PERFORM CERRAR-ARCHIVOS
               GOBACK
           END-IF
           SET SEGUIR-EN-MENU TO TRUE
           PERFORM MENU-PRESUPUESTOS UNTIL SALIR-MENU
           PERFORM CERRAR-ARCHIVOS
           GOBACK.

       MENU-PRESUPUESTOS.
           DISPLAY " " END-DISPLAY
           DISPLAY WS-SEPARADOR-DOBLE END-DISPLAY
           DISPLAY "  PRESUPUESTOS Y PAGOS" END-DISPLAY
           DISPLAY WS-SEPARADOR-DOBLE END-DISPLAY
           DISPLAY "  1. Crear presupuesto" END-DISPLAY
           DISPLAY "  2. Consultar presupuesto" END-DISPLAY
           DISPLAY "  3. Listar todos los presupuestos" END-DISPLAY
           DISPLAY "  4. Listar presupuestos de un equipo" END-DISPLAY
           DISPLAY "  5. Listar presupuestos pendientes de pago"
           END-DISPLAY
           DISPLAY "  6. Registrar pago" END-DISPLAY
           DISPLAY "  7. Modificar presupuesto" END-DISPLAY
           DISPLAY "  8. Eliminar presupuesto" END-DISPLAY
           DISPLAY "  0. Volver al menú principal" END-DISPLAY
           MOVE "Opción:" TO WS-PROMPT
           PERFORM MOSTRAR-PROMPT
           PERFORM LEER-ENTRADA
           EVALUATE WS-ENTRADA
               WHEN "1" PERFORM ALTA-PRESUPUESTO
               WHEN "2" PERFORM CONSULTA-PRESUPUESTO
               WHEN "3"
                   SET FILTRO-TODOS TO TRUE
                   PERFORM LISTADO-PRESUPUESTOS
               WHEN "4" PERFORM LISTADO-PRESUPUESTOS-EQUIPO
               WHEN "5"
                   SET FILTRO-PENDIENTES TO TRUE
                   PERFORM LISTADO-PRESUPUESTOS
               WHEN "6" PERFORM REGISTRO-PAGO
               WHEN "7" PERFORM MODIFICACION-PRESUPUESTO
               WHEN "8" PERFORM BAJA-PRESUPUESTO
               WHEN "0" SET SALIR-MENU TO TRUE
               WHEN OTHER
                   DISPLAY "  Opción inválida." END-DISPLAY
           END-EVALUATE.

      *-----------------------------------------------------------------
      * Alta: el presupuesto se asocia a un equipo existente.
      *-----------------------------------------------------------------
       ALTA-PRESUPUESTO.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Crear presupuesto --" END-DISPLAY
           PERFORM PEDIR-EQUIPO-EXISTENTE
           IF REGISTRO-ENCONTRADO AND EQUIPO-ENTREGADO
               DISPLAY "  Atención: el equipo ya fue entregado."
               END-DISPLAY
               MOVE "¿Crear el presupuesto igualmente? (S/N):"
                   TO WS-PROMPT
               PERFORM CONFIRMAR
               IF CONFIRMA-NO
                   SET REGISTRO-NO-ENCONTRADO TO TRUE
                   DISPLAY "  Operación cancelada." END-DISPLAY
               END-IF
           END-IF
           IF REGISTRO-ENCONTRADO
               PERFORM OBTENER-PROXIMO-ID
               IF WS-ID = ZERO
                   SET REGISTRO-NO-ENCONTRADO TO TRUE
                   DISPLAY "  Numeración de presupuestos agotada "
                           "(máximo 99999)." END-DISPLAY
               END-IF
           END-IF
           IF REGISTRO-ENCONTRADO
               MOVE SPACES TO BUDGETS-REGISTERS
               MOVE EQUIPMENTS-ID TO BUDGETS-EQUIPO-ID
               MOVE EQUIPMENTS-DNI TO BUDGETS-DNI
               MOVE "Trabajo a realizar:" TO WS-PROMPT
               MOVE 100 TO WS-LARGO-MAXIMO
               PERFORM LEER-TEXTO-OBLIGATORIO
               MOVE WS-ENTRADA TO BUDGETS-DESCRIPCION
               MOVE "Importe $:" TO WS-PROMPT
               PERFORM LEER-IMPORTE
               MOVE WS-IMPORTE TO BUDGETS-IMPORTE
               MOVE "Fecha DD/MM/AAAA (ENTER = hoy):" TO WS-PROMPT
               PERFORM LEER-FECHA
               MOVE WS-FECHA TO BUDGETS-FECHA
               SET PRESUPUESTO-PENDIENTE TO TRUE
               MOVE ZERO TO BUDGETS-FECHA-PAGO
               PERFORM GRABAR-PRESUPUESTO-NUEVO
           END-IF.

      * Próximo número de presupuesto (último + 1). WS-ID queda en 0
      * si la numeración está agotada. Usa el área del registro, por
      * eso se ejecuta antes de cargar los datos del presupuesto.
       OBTENER-PROXIMO-ID.
           MOVE 1 TO WS-ID
           MOVE 99999 TO BUDGETS-ID
           START BUDGETS-FILE KEY IS NOT GREATER THAN BUDGETS-ID
               NOT INVALID KEY
                   READ BUDGETS-FILE PREVIOUS RECORD
                       NOT AT END
                           COMPUTE WS-ID = BUDGETS-ID + 1
                               ON SIZE ERROR MOVE ZERO TO WS-ID
                           END-COMPUTE
                   END-READ
           END-START.

       GRABAR-PRESUPUESTO-NUEVO.
           MOVE WS-ID TO BUDGETS-ID
           WRITE BUDGETS-REGISTERS
               INVALID KEY
                   DISPLAY "  Error al grabar (file status "
                           WS-FS-BUDGETS ")." END-DISPLAY
               NOT INVALID KEY
                   MOVE WS-ID TO WS-ID-ED
                   DISPLAY "  Presupuesto registrado con el número "
                           FUNCTION TRIM(WS-ID-ED) "." END-DISPLAY
           END-WRITE.

       PEDIR-EQUIPO-EXISTENTE.
           SET REGISTRO-NO-ENCONTRADO TO TRUE
           MOVE "Número de equipo (ENTER cancela):" TO WS-PROMPT
           PERFORM LEER-ID
           IF OPERACION-CANCELADA
               DISPLAY "  Operación cancelada." END-DISPLAY
           ELSE
               MOVE WS-ID TO EQUIPMENTS-ID
               READ EQUIPMENTS-FILE RECORD KEY IS EQUIPMENTS-ID
                   INVALID KEY
                       MOVE WS-ID TO WS-ID-ED
                       DISPLAY "  No existe el equipo número "
                               FUNCTION TRIM(WS-ID-ED) "."
                       END-DISPLAY
                   NOT INVALID KEY
                       SET REGISTRO-ENCONTRADO TO TRUE
                       DISPLAY "  Equipo: "
                               FUNCTION TRIM(EQUIPMENTS-TIPO) " - "
                               FUNCTION TRIM(EQUIPMENTS-DESCRIPCION)
                       END-DISPLAY
               END-READ
           END-IF.

      *-----------------------------------------------------------------
      * Consulta
      *-----------------------------------------------------------------
       CONSULTA-PRESUPUESTO.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Consultar presupuesto --" END-DISPLAY
           PERFORM PEDIR-PRESUPUESTO-EXISTENTE
           IF REGISTRO-ENCONTRADO
               PERFORM MOSTRAR-PRESUPUESTO
           END-IF.

       PEDIR-PRESUPUESTO-EXISTENTE.
           SET REGISTRO-NO-ENCONTRADO TO TRUE
           MOVE "Número de presupuesto (ENTER cancela):" TO WS-PROMPT
           PERFORM LEER-ID
           IF OPERACION-CANCELADA
               DISPLAY "  Operación cancelada." END-DISPLAY
           ELSE
               MOVE WS-ID TO BUDGETS-ID
               READ BUDGETS-FILE RECORD KEY IS BUDGETS-ID
                   INVALID KEY
                       MOVE WS-ID TO WS-ID-ED
                       DISPLAY "  No existe el presupuesto número "
                               FUNCTION TRIM(WS-ID-ED) "."
                       END-DISPLAY
                   NOT INVALID KEY
                       SET REGISTRO-ENCONTRADO TO TRUE
               END-READ
           END-IF.

       MOSTRAR-PRESUPUESTO.
           MOVE BUDGETS-DNI TO CUSTOMERS-DNI
           READ CUSTOMERS-FILE RECORD KEY IS CUSTOMERS-DNI
               INVALID KEY
                   MOVE "(cliente inexistente)" TO CUSTOMERS-NAME
           END-READ
           MOVE BUDGETS-EQUIPO-ID TO EQUIPMENTS-ID
           READ EQUIPMENTS-FILE RECORD KEY IS EQUIPMENTS-ID
               INVALID KEY
                   MOVE "(equipo inexistente)" TO EQUIPMENTS-DESCRIPCION
                   MOVE SPACES TO EQUIPMENTS-TIPO
           END-READ
           DISPLAY WS-SEPARADOR END-DISPLAY
           MOVE BUDGETS-ID TO WS-ID-ED
           DISPLAY "  Presupuesto N.:  " FUNCTION TRIM(WS-ID-ED)
           END-DISPLAY
           MOVE BUDGETS-EQUIPO-ID TO WS-ID-ED
           DISPLAY "  Equipo:          #" FUNCTION TRIM(WS-ID-ED) " "
                   FUNCTION TRIM(EQUIPMENTS-TIPO) " - "
                   FUNCTION TRIM(EQUIPMENTS-DESCRIPCION) END-DISPLAY
           DISPLAY "  Cliente:         " BUDGETS-DNI " - "
                   FUNCTION TRIM(CUSTOMERS-NAME) END-DISPLAY
           DISPLAY "  Trabajo:         "
                   FUNCTION TRIM(BUDGETS-DESCRIPCION) END-DISPLAY
           MOVE BUDGETS-IMPORTE TO WS-IMPORTE-ED
           DISPLAY "  Importe:         $ " FUNCTION TRIM(WS-IMPORTE-ED)
           END-DISPLAY
           MOVE BUDGETS-FECHA TO WS-FECHA
           PERFORM FORMATEAR-FECHA
           DISPLAY "  Fecha:           " WS-FECHA-TXT END-DISPLAY
           IF PRESUPUESTO-PAGADO
               MOVE BUDGETS-FECHA-PAGO TO WS-FECHA
               PERFORM FORMATEAR-FECHA
               DISPLAY "  Estado:          PAGADO el " WS-FECHA-TXT
                       " (" FUNCTION TRIM(BUDGETS-FORMA-PAGO) ")"
               END-DISPLAY
           ELSE
               DISPLAY "  Estado:          PENDIENTE DE PAGO"
               END-DISPLAY
           END-IF
           DISPLAY WS-SEPARADOR END-DISPLAY.

      *-----------------------------------------------------------------
      * Listados (todos, por equipo o pendientes de pago)
      *-----------------------------------------------------------------
       LISTADO-PRESUPUESTOS-EQUIPO.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Presupuestos de un equipo --" END-DISPLAY
           PERFORM PEDIR-EQUIPO-EXISTENTE
           IF REGISTRO-ENCONTRADO
               SET FILTRO-EQUIPO TO TRUE
               MOVE EQUIPMENTS-ID TO WS-FILTRO-EQUIPO
               PERFORM LISTADO-PRESUPUESTOS
           END-IF.

       LISTADO-PRESUPUESTOS.
           DISPLAY " " END-DISPLAY
           EVALUATE TRUE
               WHEN FILTRO-TODOS
                   DISPLAY "-- Listado de presupuestos --" END-DISPLAY
               WHEN FILTRO-PENDIENTES
                   DISPLAY "-- Presupuestos pendientes de pago --"
                   END-DISPLAY
               WHEN FILTRO-EQUIPO
                   MOVE WS-FILTRO-EQUIPO TO WS-ID-ED
                   DISPLAY "-- Presupuestos del equipo #"
                           FUNCTION TRIM(WS-ID-ED) " --" END-DISPLAY
           END-EVALUATE
           DISPLAY "N.    EQUIPO DNI      FECHA             IMPORTE "
                   "ESTADO    TRABAJO" END-DISPLAY
           DISPLAY WS-SEPARADOR END-DISPLAY
           MOVE ZERO TO WS-CANTIDAD WS-LINEAS-MOSTRADAS
                        WS-TOTAL-LISTADO
           SET HAY-MAS-REGISTROS TO TRUE
           IF FILTRO-EQUIPO
               MOVE WS-FILTRO-EQUIPO TO BUDGETS-EQUIPO-ID
               START BUDGETS-FILE KEY IS EQUAL TO BUDGETS-EQUIPO-ID
                   INVALID KEY SET FIN-LECTURA TO TRUE
               END-START
           ELSE
               MOVE ZERO TO BUDGETS-ID
               START BUDGETS-FILE KEY IS NOT LESS THAN BUDGETS-ID
                   INVALID KEY SET FIN-LECTURA TO TRUE
               END-START
           END-IF
           PERFORM UNTIL FIN-LECTURA
               READ BUDGETS-FILE NEXT RECORD
                   AT END SET FIN-LECTURA TO TRUE
               END-READ
               IF NOT FS-BUDGETS-OK
                   SET FIN-LECTURA TO TRUE
               END-IF
               IF HAY-MAS-REGISTROS
                   EVALUATE TRUE
                       WHEN FILTRO-EQUIPO
                            AND BUDGETS-EQUIPO-ID NOT = WS-FILTRO-EQUIPO
                           SET FIN-LECTURA TO TRUE
                       WHEN FILTRO-PENDIENTES AND PRESUPUESTO-PAGADO
                           CONTINUE
                       WHEN OTHER
                           PERFORM CONTROLAR-PAGINA
                           IF HAY-MAS-REGISTROS
                               PERFORM MOSTRAR-LINEA-PRESUPUESTO
                           END-IF
                   END-EVALUATE
               END-IF
           END-PERFORM
           DISPLAY WS-SEPARADOR END-DISPLAY
           MOVE WS-CANTIDAD TO WS-CANTIDAD-ED
           MOVE WS-TOTAL-LISTADO TO WS-TOTAL-LISTADO-ED
           DISPLAY "Total: " FUNCTION TRIM(WS-CANTIDAD-ED)
                   " presupuesto(s) por $ "
                   FUNCTION TRIM(WS-TOTAL-LISTADO-ED) END-DISPLAY.

       MOSTRAR-LINEA-PRESUPUESTO.
           ADD 1 TO WS-CANTIDAD
           ADD BUDGETS-IMPORTE TO WS-TOTAL-LISTADO
           MOVE BUDGETS-FECHA TO WS-FECHA
           PERFORM FORMATEAR-FECHA
           MOVE BUDGETS-IMPORTE TO WS-IMPORTE-ED
           IF PRESUPUESTO-PAGADO
               MOVE "PAGADO" TO WS-TEXTO-PAGADO
           ELSE
               MOVE "PENDIENTE" TO WS-TEXTO-PAGADO
           END-IF
           DISPLAY BUDGETS-ID " " BUDGETS-EQUIPO-ID "  " BUDGETS-DNI
                   " " WS-FECHA-TXT " " WS-IMPORTE-ED " "
                   WS-TEXTO-PAGADO " " BUDGETS-DESCRIPCION(1:25)
           END-DISPLAY.

      *-----------------------------------------------------------------
      * Registro de pago
      *-----------------------------------------------------------------
       REGISTRO-PAGO.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Registrar pago --" END-DISPLAY
           PERFORM PEDIR-PRESUPUESTO-EXISTENTE
           IF REGISTRO-ENCONTRADO
               PERFORM MOSTRAR-PRESUPUESTO
               IF PRESUPUESTO-PAGADO
                   DISPLAY "  El presupuesto ya está pagado."
                   END-DISPLAY
               ELSE
                   PERFORM ELEGIR-FORMA-PAGO
                   IF OPERACION-CANCELADA
                       DISPLAY "  Operación cancelada." END-DISPLAY
                   ELSE
                       MOVE "Fecha de pago DD/MM/AAAA (ENTER = hoy):"
                           TO WS-PROMPT
                       MOVE BUDGETS-FECHA TO WS-FECHA-MINIMA
                       PERFORM LEER-FECHA
                       MOVE WS-FECHA TO BUDGETS-FECHA-PAGO
                       SET PRESUPUESTO-PAGADO TO TRUE
                       REWRITE BUDGETS-REGISTERS
                           INVALID KEY
                               DISPLAY "  Error al actualizar (file "
                                       "status " WS-FS-BUDGETS ")."
                               END-DISPLAY
                           NOT INVALID KEY
                               DISPLAY "  Pago registrado "
                                       "correctamente." END-DISPLAY
                       END-REWRITE
                   END-IF
               END-IF
           END-IF.

       ELEGIR-FORMA-PAGO.
           DISPLAY "  Forma de pago:" END-DISPLAY
           DISPLAY "  1. Efectivo" END-DISPLAY
           DISPLAY "  2. Débito" END-DISPLAY
           DISPLAY "  3. Crédito" END-DISPLAY
           DISPLAY "  4. Transferencia" END-DISPLAY
           DISPLAY "  5. Mercado Pago" END-DISPLAY
           DISPLAY "  6. Otra" END-DISPLAY
           MOVE "Opción (ENTER cancela):" TO WS-PROMPT
           SET OPERACION-EN-CURSO TO TRUE
           MOVE SPACES TO BUDGETS-FORMA-PAGO
           PERFORM UNTIL OPERACION-CANCELADA
                   OR BUDGETS-FORMA-PAGO NOT = SPACES
               PERFORM MOSTRAR-PROMPT
               PERFORM LEER-ENTRADA
               EVALUATE WS-ENTRADA
                   WHEN SPACES
                       SET OPERACION-CANCELADA TO TRUE
                   WHEN "1" MOVE "Efectivo" TO BUDGETS-FORMA-PAGO
                   WHEN "2" MOVE "Débito" TO BUDGETS-FORMA-PAGO
                   WHEN "3" MOVE "Crédito" TO BUDGETS-FORMA-PAGO
                   WHEN "4" MOVE "Transferencia" TO BUDGETS-FORMA-PAGO
                   WHEN "5" MOVE "Mercado Pago" TO BUDGETS-FORMA-PAGO
                   WHEN "6" MOVE "Otra" TO BUDGETS-FORMA-PAGO
                   WHEN OTHER
                       DISPLAY "  Opción inválida." END-DISPLAY
               END-EVALUATE
           END-PERFORM.

      *-----------------------------------------------------------------
      * Modificación (sólo presupuestos pendientes de pago)
      *-----------------------------------------------------------------
       MODIFICACION-PRESUPUESTO.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Modificar presupuesto --" END-DISPLAY
           PERFORM PEDIR-PRESUPUESTO-EXISTENTE
           IF REGISTRO-ENCONTRADO
               PERFORM MOSTRAR-PRESUPUESTO
               IF PRESUPUESTO-PAGADO
                   DISPLAY "  No se puede modificar un presupuesto "
                           "pagado." END-DISPLAY
               ELSE
                   DISPLAY "Ingrese los nuevos datos (ENTER mantiene "
                           "el valor actual)." END-DISPLAY
                   MOVE "Trabajo a realizar:" TO WS-PROMPT
                   MOVE 100 TO WS-LARGO-MAXIMO
                   PERFORM LEER-TEXTO
                   IF WS-ENTRADA NOT = SPACES AND WS-ENTRADA NOT = "-"
                       MOVE WS-ENTRADA TO BUDGETS-DESCRIPCION
                   END-IF
                   MOVE "¿Modificar el importe? (S/N):" TO WS-PROMPT
                   PERFORM CONFIRMAR
                   IF CONFIRMA-SI
                       MOVE "Nuevo importe $:" TO WS-PROMPT
                       PERFORM LEER-IMPORTE
                       MOVE WS-IMPORTE TO BUDGETS-IMPORTE
                   END-IF
                   REWRITE BUDGETS-REGISTERS
                       INVALID KEY
                           DISPLAY "  Error al actualizar (file "
                                   "status " WS-FS-BUDGETS ")."
                           END-DISPLAY
                       NOT INVALID KEY
                           DISPLAY "  Presupuesto actualizado "
                                   "correctamente." END-DISPLAY
                   END-REWRITE
               END-IF
           END-IF.

      *-----------------------------------------------------------------
      * Baja (sólo presupuestos pendientes de pago)
      *-----------------------------------------------------------------
       BAJA-PRESUPUESTO.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Eliminar presupuesto --" END-DISPLAY
           PERFORM PEDIR-PRESUPUESTO-EXISTENTE
           IF REGISTRO-ENCONTRADO
               PERFORM MOSTRAR-PRESUPUESTO
               IF PRESUPUESTO-PAGADO
                   DISPLAY "  No se puede eliminar un presupuesto "
                           "pagado." END-DISPLAY
               ELSE
                   MOVE "¿Confirma la eliminación? (S/N):"
                       TO WS-PROMPT
                   PERFORM CONFIRMAR
                   IF CONFIRMA-SI
                       DELETE BUDGETS-FILE RECORD
                           INVALID KEY
                               DISPLAY "  Error al eliminar (file "
                                       "status " WS-FS-BUDGETS ")."
                               END-DISPLAY
                           NOT INVALID KEY
                               DISPLAY "  Presupuesto eliminado."
                               END-DISPLAY
                       END-DELETE
                   ELSE
                       DISPLAY "  Operación cancelada." END-DISPLAY
                   END-IF
               END-IF
           END-IF.

       CERRAR-ARCHIVOS.
           CLOSE BUDGETS-FILE CUSTOMERS-FILE EQUIPMENTS-FILE.

           COPY "proc-rutas.cpy".
           COPY "proc-comun.cpy".

       END PROGRAM PRESUPUESTOS.
