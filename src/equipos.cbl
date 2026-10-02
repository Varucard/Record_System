      ******************************************************************
      * Purpose: Gestión de equipos ingresados al servicio técnico
      *          (ingreso, consulta, listados, modificación, cambio
      *          de estado, baja y comprobante de ingreso).
      * Parámetros:
      *   LK-DIRECTORIO-DATOS (entrada)  directorio de los .dat
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EQUIPOS.

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
           COPY "fc-texto.cpy".

       DATA DIVISION.
       FILE SECTION.
           COPY "fd-customers.cpy".
           COPY "fd-equipments.cpy".
           COPY "fd-budgets.cpy".
           COPY "fd-control.cpy".
           COPY "fd-texto.cpy".

       WORKING-STORAGE SECTION.
           COPY "ws-archivos.cpy".
           COPY "ws-comun.cpy".
       01  WS-TEXTO-ESTADO             PIC X(20).
       01  WS-FILTRO-DNI               PIC X(8).

       LINKAGE SECTION.
           COPY "lk-comun.cpy".

       PROCEDURE DIVISION USING LK-DIRECTORIO-DATOS.
       INICIO-EQUIPOS.
           PERFORM ARMAR-RUTAS
           PERFORM INICIAR-INTERFAZ
           OPEN I-O EQUIPMENTS-FILE CONTROL-FILE
           OPEN INPUT CUSTOMERS-FILE BUDGETS-FILE
           IF NOT FS-EQUIPMENTS-OK OR NOT FS-CUSTOMERS-OK
              OR NOT FS-BUDGETS-OK OR NOT FS-CONTROL-OK
               DISPLAY "Error al abrir los archivos (file status "
                       WS-FS-EQUIPMENTS "/" WS-FS-CUSTOMERS "/"
                       WS-FS-BUDGETS "/" WS-FS-CONTROL ")."
               END-DISPLAY
               PERFORM CERRAR-ARCHIVOS
               GOBACK
           END-IF
           SET SEGUIR-EN-MENU TO TRUE
           PERFORM MENU-EQUIPOS UNTIL SALIR-MENU
           PERFORM CERRAR-ARCHIVOS
           GOBACK.

       MENU-EQUIPOS.
           PERFORM PREPARAR-PANTALLA
           MOVE "EQUIPOS" TO WS-TITULO
           PERFORM MOSTRAR-TITULO
           DISPLAY "  1. Registrar ingreso de equipo" END-DISPLAY
           DISPLAY "  2. Consultar equipo" END-DISPLAY
           DISPLAY "  3. Listar todos los equipos" END-DISPLAY
           DISPLAY "  4. Listar equipos de un cliente" END-DISPLAY
           DISPLAY "  5. Modificar datos del equipo" END-DISPLAY
           DISPLAY "  6. Cambiar estado del equipo" END-DISPLAY
           DISPLAY "  7. Eliminar equipo" END-DISPLAY
           DISPLAY "  8. Generar comprobante de ingreso" END-DISPLAY
           DISPLAY "  0. Volver al menú principal" END-DISPLAY
           MOVE "Opción:" TO WS-PROMPT
           PERFORM MOSTRAR-PROMPT
           PERFORM LEER-ENTRADA
           MOVE WS-ENTRADA TO WS-OPCION
           EVALUATE WS-OPCION
               WHEN "1" PERFORM ALTA-EQUIPO
               WHEN "2" PERFORM CONSULTA-EQUIPO
               WHEN "3" PERFORM LISTADO-EQUIPOS
               WHEN "4" PERFORM LISTADO-EQUIPOS-CLIENTE
               WHEN "5" PERFORM MODIFICACION-EQUIPO
               WHEN "6" PERFORM CAMBIO-ESTADO-EQUIPO
               WHEN "7" PERFORM BAJA-EQUIPO
               WHEN "8" PERFORM COMPROBANTE-INGRESO
               WHEN "0" SET SALIR-MENU TO TRUE
               WHEN OTHER
                   DISPLAY "  Opción inválida." END-DISPLAY
           END-EVALUATE
           IF WS-OPCION NOT = "0"
               PERFORM MARCAR-PAUSA
           END-IF.

      *-----------------------------------------------------------------
      * Alta: el equipo debe pertenecer a un cliente existente.
      *-----------------------------------------------------------------
       ALTA-EQUIPO.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Registrar ingreso de equipo --" END-DISPLAY
           DISPLAY "Cliente dueño del equipo." END-DISPLAY
           PERFORM LEER-DNI
           IF OPERACION-CANCELADA
               DISPLAY "  Operación cancelada." END-DISPLAY
           ELSE
               PERFORM BUSCAR-CLIENTE
               IF REGISTRO-NO-ENCONTRADO
                   DISPLAY "  No existe un cliente con DNI " WS-DNI
                           ". Regístrelo primero en Clientes."
                   END-DISPLAY
               ELSE
                   DISPLAY "  Cliente: " FUNCTION TRIM(CUSTOMERS-NAME)
                   END-DISPLAY
                   PERFORM OBTENER-PROXIMO-ID
                   IF WS-ID = ZERO
                       DISPLAY "  Numeración de equipos agotada "
                               "(máximo 99999)." END-DISPLAY
                   ELSE
                       PERFORM AVISAR-CANCELACION
                       PERFORM CARGAR-DATOS-EQUIPO
                       IF OPERACION-CANCELADA
                           DISPLAY "  Operación cancelada."
                           END-DISPLAY
                       ELSE
                           PERFORM GRABAR-EQUIPO-NUEVO
                       END-IF
                   END-IF
               END-IF
           END-IF.

       CARGAR-DATOS-EQUIPO.
           MOVE SPACES TO EQUIPMENTS-REGISTERS
           MOVE WS-DNI TO EQUIPMENTS-DNI
           MOVE "Tipo (PC, Notebook, Impresora, etc.):" TO WS-PROMPT
           MOVE 15 TO WS-LARGO-MAXIMO
           PERFORM LEER-TEXTO-OBLIGATORIO
           MOVE WS-ENTRADA TO EQUIPMENTS-TIPO
           MOVE "Marca / modelo / descripción:" TO WS-PROMPT
           MOVE 100 TO WS-LARGO-MAXIMO
           PERFORM LEER-TEXTO-OBLIGATORIO
           MOVE WS-ENTRADA TO EQUIPMENTS-DESCRIPCION
           MOVE "Características (opcional):" TO WS-PROMPT
           PERFORM LEER-TEXTO
           MOVE WS-ENTRADA TO EQUIPMENTS-CARACTERISTICAS
           MOVE "Problema informado:" TO WS-PROMPT
           PERFORM LEER-TEXTO-OBLIGATORIO
           MOVE WS-ENTRADA TO EQUIPMENTS-PROBLEMA
           SET EQUIPO-INGRESADO TO TRUE
           PERFORM OBTENER-FECHA-HOY
           MOVE WS-FECHA-HOY TO EQUIPMENTS-FECHA-INGRESO.

      * Graba el equipo con el número calculado por
      * OBTENER-PROXIMO-ID antes de pedir los datos.
       GRABAR-EQUIPO-NUEVO.
           MOVE WS-ID TO EQUIPMENTS-ID
           WRITE EQUIPMENTS-REGISTERS
               INVALID KEY
                   DISPLAY "  Error al grabar (file status "
                           WS-FS-EQUIPMENTS ")." END-DISPLAY
               NOT INVALID KEY
                   PERFORM REGISTRAR-ID-CONTROL
                   MOVE WS-ID TO WS-ID-ED
                   DISPLAY "  Equipo registrado con el número "
                           FUNCTION TRIM(WS-ID-ED) "." END-DISPLAY
           END-WRITE.

      * Próximo número de equipo: el último + 1, sin repetir los de
      * equipos borrados (control.dat). WS-ID queda en cero si la
      * numeración está agotada. Usa el área del registro, por eso
      * se ejecuta antes de cargar los datos del equipo nuevo.
       OBTENER-PROXIMO-ID.
           MOVE 1 TO WS-ID
           MOVE 99999 TO EQUIPMENTS-ID
           START EQUIPMENTS-FILE KEY IS NOT GREATER THAN EQUIPMENTS-ID
               NOT INVALID KEY
                   READ EQUIPMENTS-FILE PREVIOUS RECORD
                       NOT AT END
                           COMPUTE WS-ID = EQUIPMENTS-ID + 1
                               ON SIZE ERROR MOVE ZERO TO WS-ID
                           END-COMPUTE
                   END-READ
           END-START
           MOVE "EQUIPOS" TO WS-CLAVE-CONTROL
           PERFORM AJUSTAR-ID-CONTROL.

       BUSCAR-CLIENTE.
           SET REGISTRO-NO-ENCONTRADO TO TRUE
           MOVE WS-DNI TO CUSTOMERS-DNI
           READ CUSTOMERS-FILE RECORD KEY IS CUSTOMERS-DNI
               INVALID KEY SET REGISTRO-NO-ENCONTRADO TO TRUE
               NOT INVALID KEY SET REGISTRO-ENCONTRADO TO TRUE
           END-READ.

      *-----------------------------------------------------------------
      * Consulta
      *-----------------------------------------------------------------
       CONSULTA-EQUIPO.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Consultar equipo --" END-DISPLAY
           PERFORM PEDIR-EQUIPO-EXISTENTE
           IF REGISTRO-ENCONTRADO
               PERFORM MOSTRAR-EQUIPO
               PERFORM MOSTRAR-PRESUPUESTOS-EQUIPO
           END-IF.

      * Pide un número de equipo y lo lee. Informa si no existe.
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
               END-READ
           END-IF.

       MOSTRAR-EQUIPO.
           PERFORM DESCRIBIR-ESTADO
           MOVE EQUIPMENTS-FECHA-INGRESO TO WS-FECHA
           PERFORM FORMATEAR-FECHA
           MOVE EQUIPMENTS-DNI TO WS-DNI
           PERFORM BUSCAR-CLIENTE
           IF REGISTRO-NO-ENCONTRADO
               MOVE "(cliente inexistente)" TO CUSTOMERS-NAME
           END-IF
           SET REGISTRO-ENCONTRADO TO TRUE
           MOVE EQUIPMENTS-ID TO WS-ID-ED
           DISPLAY WS-SEPARADOR END-DISPLAY
           DISPLAY "  Equipo N.:       " FUNCTION TRIM(WS-ID-ED)
           END-DISPLAY
           DISPLAY "  Cliente:         " EQUIPMENTS-DNI " - "
                   FUNCTION TRIM(CUSTOMERS-NAME) END-DISPLAY
           DISPLAY "  Tipo:            " FUNCTION TRIM(EQUIPMENTS-TIPO)
           END-DISPLAY
           DISPLAY "  Descripción:     "
                   FUNCTION TRIM(EQUIPMENTS-DESCRIPCION) END-DISPLAY
           DISPLAY "  Características: "
                   FUNCTION TRIM(EQUIPMENTS-CARACTERISTICAS)
           END-DISPLAY
           DISPLAY "  Problema:        "
                   FUNCTION TRIM(EQUIPMENTS-PROBLEMA) END-DISPLAY
           DISPLAY "  Estado:          " FUNCTION TRIM(WS-TEXTO-ESTADO)
           END-DISPLAY
           DISPLAY "  Ingreso:         " WS-FECHA-TXT END-DISPLAY
           DISPLAY WS-SEPARADOR END-DISPLAY.

       DESCRIBIR-ESTADO.
           EVALUATE TRUE
               WHEN EQUIPO-INGRESADO
                   MOVE "Ingresado" TO WS-TEXTO-ESTADO
               WHEN EQUIPO-EN-REPARACION
                   MOVE "En reparación" TO WS-TEXTO-ESTADO
               WHEN EQUIPO-LISTO
                   MOVE "Listo p/ retirar" TO WS-TEXTO-ESTADO
               WHEN EQUIPO-ENTREGADO
                   MOVE "Entregado" TO WS-TEXTO-ESTADO
               WHEN OTHER
                   MOVE "?" TO WS-TEXTO-ESTADO
           END-EVALUATE.

       MOSTRAR-PRESUPUESTOS-EQUIPO.
           PERFORM CONTAR-PRESUPUESTOS-EQUIPO
           IF WS-CANTIDAD = ZERO
               DISPLAY "  El equipo no tiene presupuestos." END-DISPLAY
           ELSE
               DISPLAY "  Presupuestos del equipo:" END-DISPLAY
               PERFORM POSICIONAR-PRESUPUESTOS-EQUIPO
               PERFORM UNTIL FIN-LECTURA
                   READ BUDGETS-FILE NEXT RECORD
                       AT END SET FIN-LECTURA TO TRUE
                   END-READ
                   IF NOT FS-BUDGETS-OK
                       SET FIN-LECTURA TO TRUE
                   END-IF
                   IF HAY-MAS-REGISTROS
                       IF BUDGETS-EQUIPO-ID NOT = EQUIPMENTS-ID
                           SET FIN-LECTURA TO TRUE
                       ELSE
                           PERFORM MOSTRAR-RENGLON-PRESUPUESTO
                       END-IF
                   END-IF
               END-PERFORM
           END-IF.

       MOSTRAR-RENGLON-PRESUPUESTO.
           MOVE BUDGETS-IMPORTE TO WS-IMPORTE-ED
           PERFORM INICIAR-LINEA
           STRING "    #" BUDGETS-ID " $" WS-IMPORTE-ED " "
               DELIMITED BY SIZE INTO WS-LINEA WITH POINTER WS-PUNTERO
           END-STRING
           MOVE BUDGETS-DESCRIPCION TO WS-CORTE-ORIGEN
           MOVE 30 TO WS-CORTE-ANCHO
           PERFORM AGREGAR-COLUMNA
           IF PRESUPUESTO-PAGADO
               MOVE "PAGADO" TO WS-CORTE-ORIGEN
           ELSE
               MOVE "PENDIENTE" TO WS-CORTE-ORIGEN
           END-IF
           MOVE 9 TO WS-CORTE-ANCHO
           PERFORM AGREGAR-COLUMNA
           PERFORM MOSTRAR-LINEA.

       POSICIONAR-PRESUPUESTOS-EQUIPO.
           SET HAY-MAS-REGISTROS TO TRUE
           MOVE EQUIPMENTS-ID TO BUDGETS-EQUIPO-ID
           START BUDGETS-FILE KEY IS EQUAL TO BUDGETS-EQUIPO-ID
               INVALID KEY SET FIN-LECTURA TO TRUE
           END-START.

       CONTAR-PRESUPUESTOS-EQUIPO.
           MOVE ZERO TO WS-CANTIDAD
           PERFORM POSICIONAR-PRESUPUESTOS-EQUIPO
           PERFORM UNTIL FIN-LECTURA
               READ BUDGETS-FILE NEXT RECORD
                   AT END SET FIN-LECTURA TO TRUE
               END-READ
               IF NOT FS-BUDGETS-OK
                   SET FIN-LECTURA TO TRUE
               END-IF
               IF HAY-MAS-REGISTROS
                   IF BUDGETS-EQUIPO-ID NOT = EQUIPMENTS-ID
                       SET FIN-LECTURA TO TRUE
                   ELSE
                       ADD 1 TO WS-CANTIDAD
                   END-IF
               END-IF
           END-PERFORM.

      *-----------------------------------------------------------------
      * Listados
      *-----------------------------------------------------------------
       LISTADO-EQUIPOS.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Listado de equipos --" END-DISPLAY
           MOVE SPACES TO WS-FILTRO-DNI
           PERFORM MOSTRAR-ENCABEZADO-LISTADO
           MOVE ZERO TO EQUIPMENTS-ID
           START EQUIPMENTS-FILE KEY IS NOT LESS THAN EQUIPMENTS-ID
               INVALID KEY SET FIN-LECTURA TO TRUE
           END-START
           PERFORM RECORRER-LISTADO.

       LISTADO-EQUIPOS-CLIENTE.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Listado de equipos de un cliente --"
           END-DISPLAY
           PERFORM LEER-DNI
           IF OPERACION-CANCELADA
               DISPLAY "  Operación cancelada." END-DISPLAY
           ELSE
               PERFORM BUSCAR-CLIENTE
               IF REGISTRO-NO-ENCONTRADO
                   DISPLAY "  No existe un cliente con DNI " WS-DNI
                           "." END-DISPLAY
               ELSE
                   DISPLAY "Cliente: " FUNCTION TRIM(CUSTOMERS-NAME)
                   END-DISPLAY
                   MOVE WS-DNI TO WS-FILTRO-DNI
                   PERFORM MOSTRAR-ENCABEZADO-LISTADO
                   MOVE WS-DNI TO EQUIPMENTS-DNI
                   START EQUIPMENTS-FILE
                       KEY IS EQUAL TO EQUIPMENTS-DNI
                       INVALID KEY SET FIN-LECTURA TO TRUE
                   END-START
                   PERFORM RECORRER-LISTADO
               END-IF
           END-IF.

       MOSTRAR-ENCABEZADO-LISTADO.
           DISPLAY "N.    DNI      TIPO            DESCRIPCIÓN"
                   "                    ESTADO" END-DISPLAY
           DISPLAY WS-SEPARADOR END-DISPLAY
           MOVE ZERO TO WS-CANTIDAD WS-LINEAS-MOSTRADAS
           SET HAY-MAS-REGISTROS TO TRUE.

      * Recorre desde la posición actual. Si WS-FILTRO-DNI tiene un
      * valor, corta al cambiar de DNI (lectura por clave alterna).
       RECORRER-LISTADO.
           PERFORM UNTIL FIN-LECTURA
               READ EQUIPMENTS-FILE NEXT RECORD
                   AT END SET FIN-LECTURA TO TRUE
               END-READ
               IF NOT FS-EQUIPMENTS-OK
                   SET FIN-LECTURA TO TRUE
               END-IF
               IF HAY-MAS-REGISTROS
                  AND WS-FILTRO-DNI NOT = SPACES
                  AND EQUIPMENTS-DNI NOT = WS-FILTRO-DNI
                   SET FIN-LECTURA TO TRUE
               END-IF
               IF HAY-MAS-REGISTROS
                   PERFORM CONTROLAR-PAGINA
               END-IF
               IF HAY-MAS-REGISTROS
                   ADD 1 TO WS-CANTIDAD
                   PERFORM DESCRIBIR-ESTADO
                   PERFORM INICIAR-LINEA
                   STRING EQUIPMENTS-ID " " EQUIPMENTS-DNI " "
                       DELIMITED BY SIZE
                       INTO WS-LINEA WITH POINTER WS-PUNTERO
                   END-STRING
                   MOVE EQUIPMENTS-TIPO TO WS-CORTE-ORIGEN
                   MOVE 15 TO WS-CORTE-ANCHO
                   PERFORM AGREGAR-COLUMNA
                   MOVE EQUIPMENTS-DESCRIPCION TO WS-CORTE-ORIGEN
                   MOVE 30 TO WS-CORTE-ANCHO
                   PERFORM AGREGAR-COLUMNA
                   MOVE WS-TEXTO-ESTADO TO WS-CORTE-ORIGEN
                   MOVE 20 TO WS-CORTE-ANCHO
                   PERFORM AGREGAR-COLUMNA
                   PERFORM MOSTRAR-LINEA
               END-IF
           END-PERFORM
           DISPLAY WS-SEPARADOR END-DISPLAY
           MOVE WS-CANTIDAD TO WS-CANTIDAD-ED
           DISPLAY "Total de equipos: " FUNCTION TRIM(WS-CANTIDAD-ED)
           END-DISPLAY.

      *-----------------------------------------------------------------
      * Modificación de datos (ENTER mantiene el valor actual)
      *-----------------------------------------------------------------
       MODIFICACION-EQUIPO.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Modificar datos del equipo --" END-DISPLAY
           PERFORM PEDIR-EQUIPO-EXISTENTE
           IF REGISTRO-ENCONTRADO
               PERFORM MOSTRAR-EQUIPO
               DISPLAY "Ingrese los nuevos datos (ENTER mantiene el "
                       "valor actual, - borra las características)."
               END-DISPLAY
               PERFORM AVISAR-CANCELACION

               MOVE "Tipo:" TO WS-PROMPT
               MOVE 15 TO WS-LARGO-MAXIMO
               PERFORM LEER-TEXTO
               IF WS-ENTRADA NOT = SPACES AND WS-ENTRADA NOT = "-"
                   MOVE WS-ENTRADA TO EQUIPMENTS-TIPO
               END-IF

               MOVE "Marca / modelo / descripción:" TO WS-PROMPT
               MOVE 100 TO WS-LARGO-MAXIMO
               PERFORM LEER-TEXTO
               IF WS-ENTRADA NOT = SPACES AND WS-ENTRADA NOT = "-"
                   MOVE WS-ENTRADA TO EQUIPMENTS-DESCRIPCION
               END-IF

               MOVE "Características:" TO WS-PROMPT
               MOVE EQUIPMENTS-CARACTERISTICAS TO WS-VALOR-ACTUAL
               PERFORM LEER-TEXTO
               PERFORM ACTUALIZAR-CAMPO
               MOVE WS-ENTRADA TO EQUIPMENTS-CARACTERISTICAS

               MOVE "Problema informado:" TO WS-PROMPT
               PERFORM LEER-TEXTO
               IF WS-ENTRADA NOT = SPACES AND WS-ENTRADA NOT = "-"
                   MOVE WS-ENTRADA TO EQUIPMENTS-PROBLEMA
               END-IF

               IF OPERACION-CANCELADA
                   DISPLAY "  Operación cancelada." END-DISPLAY
               ELSE
                   PERFORM REGRABAR-EQUIPO
               END-IF
           END-IF.

       REGRABAR-EQUIPO.
           REWRITE EQUIPMENTS-REGISTERS
               INVALID KEY
                   DISPLAY "  Error al actualizar (file status "
                           WS-FS-EQUIPMENTS ")." END-DISPLAY
               NOT INVALID KEY
                   DISPLAY "  Equipo actualizado correctamente."
                   END-DISPLAY
           END-REWRITE.

      *-----------------------------------------------------------------
      * Cambio de estado del equipo
      *-----------------------------------------------------------------
       CAMBIO-ESTADO-EQUIPO.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Cambiar estado del equipo --" END-DISPLAY
           PERFORM PEDIR-EQUIPO-EXISTENTE
           IF REGISTRO-ENCONTRADO
               PERFORM DESCRIBIR-ESTADO
               DISPLAY "  Estado actual: "
                       FUNCTION TRIM(WS-TEXTO-ESTADO)
               END-DISPLAY
               DISPLAY "  1. Ingresado" END-DISPLAY
               DISPLAY "  2. En reparación" END-DISPLAY
               DISPLAY "  3. Listo para retirar" END-DISPLAY
               DISPLAY "  4. Entregado" END-DISPLAY
               MOVE "Nuevo estado (ENTER cancela):" TO WS-PROMPT
               SET OPERACION-EN-CURSO TO TRUE
               MOVE SPACE TO WS-ENTRADA
               PERFORM UNTIL OPERACION-CANCELADA
                       OR WS-ENTRADA = "1" OR "2" OR "3" OR "4"
                   PERFORM MOSTRAR-PROMPT
                   PERFORM LEER-ENTRADA
                   EVALUATE WS-ENTRADA
                       WHEN SPACES
                           SET OPERACION-CANCELADA TO TRUE
                       WHEN "1" SET EQUIPO-INGRESADO TO TRUE
                       WHEN "2" SET EQUIPO-EN-REPARACION TO TRUE
                       WHEN "3" SET EQUIPO-LISTO TO TRUE
                       WHEN "4" SET EQUIPO-ENTREGADO TO TRUE
                       WHEN OTHER
                           DISPLAY "  Opción inválida." END-DISPLAY
                   END-EVALUATE
               END-PERFORM
               IF EQUIPO-ENTREGADO AND OPERACION-EN-CURSO
                   PERFORM CONTAR-PENDIENTES-EQUIPO
                   IF WS-CANTIDAD > ZERO
                       MOVE WS-CANTIDAD TO WS-CANTIDAD-ED
                       DISPLAY "  Atención: el equipo tiene "
                               FUNCTION TRIM(WS-CANTIDAD-ED)
                               " presupuesto(s) pendiente(s) de pago."
                       END-DISPLAY
                       MOVE "¿Entregarlo igualmente? (S/N):"
                           TO WS-PROMPT
                       PERFORM CONFIRMAR
                       IF CONFIRMA-NO
                           SET OPERACION-CANCELADA TO TRUE
                       END-IF
                   END-IF
               END-IF
               IF OPERACION-CANCELADA
                   DISPLAY "  Operación cancelada." END-DISPLAY
               ELSE
                   PERFORM REGRABAR-EQUIPO
               END-IF
           END-IF.

       CONTAR-PENDIENTES-EQUIPO.
           MOVE ZERO TO WS-CANTIDAD
           PERFORM POSICIONAR-PRESUPUESTOS-EQUIPO
           PERFORM UNTIL FIN-LECTURA
               READ BUDGETS-FILE NEXT RECORD
                   AT END SET FIN-LECTURA TO TRUE
               END-READ
               IF NOT FS-BUDGETS-OK
                  OR BUDGETS-EQUIPO-ID NOT = EQUIPMENTS-ID
                   SET FIN-LECTURA TO TRUE
               END-IF
               IF HAY-MAS-REGISTROS AND PRESUPUESTO-PENDIENTE
                   ADD 1 TO WS-CANTIDAD
               END-IF
           END-PERFORM.

      *-----------------------------------------------------------------
      * Baja (no se permite si el equipo tiene presupuestos)
      *-----------------------------------------------------------------
       BAJA-EQUIPO.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Eliminar equipo --" END-DISPLAY
           PERFORM PEDIR-EQUIPO-EXISTENTE
           IF REGISTRO-ENCONTRADO
               PERFORM MOSTRAR-EQUIPO
               PERFORM CONTAR-PRESUPUESTOS-EQUIPO
               IF WS-CANTIDAD > ZERO
                   MOVE WS-CANTIDAD TO WS-CANTIDAD-ED
                   DISPLAY "  No se puede eliminar: el equipo tiene "
                           FUNCTION TRIM(WS-CANTIDAD-ED)
                           " presupuesto(s)." END-DISPLAY
               ELSE
                   MOVE "¿Confirma la eliminación? (S/N):"
                       TO WS-PROMPT
                   PERFORM CONFIRMAR
                   IF CONFIRMA-SI
                       DELETE EQUIPMENTS-FILE RECORD
                           INVALID KEY
                               DISPLAY "  Error al eliminar (file "
                                       "status " WS-FS-EQUIPMENTS ")."
                               END-DISPLAY
                           NOT INVALID KEY
                               DISPLAY "  Equipo eliminado."
                               END-DISPLAY
                       END-DELETE
                   ELSE
                       DISPLAY "  Operación cancelada." END-DISPLAY
                   END-IF
               END-IF
           END-IF.

      *-----------------------------------------------------------------
      * Comprobante de ingreso (comprobantes/ingreso-NNNNN.txt)
      *-----------------------------------------------------------------
       COMPROBANTE-INGRESO.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Generar comprobante de ingreso --" END-DISPLAY
           PERFORM PEDIR-EQUIPO-EXISTENTE
           IF REGISTRO-ENCONTRADO
               PERFORM DESCRIBIR-ESTADO
               MOVE EQUIPMENTS-DNI TO WS-DNI
               PERFORM BUSCAR-CLIENTE
               MOVE "comprobantes" TO WS-SUBDIRECTORIO
               MOVE SPACES TO WS-NOMBRE-TEXTO
               STRING "ingreso-" EQUIPMENTS-ID ".txt"
                   DELIMITED BY SIZE INTO WS-NOMBRE-TEXTO
               END-STRING
               PERFORM ABRIR-ARCHIVO-TEXTO
               IF FS-TEXTO-OK
                   PERFORM ESCRIBIR-COMPROBANTE-INGRESO
                   PERFORM CERRAR-ARCHIVO-TEXTO
               END-IF
           END-IF.

       ESCRIBIR-COMPROBANTE-INGRESO.
           MOVE EQUIPMENTS-ID TO WS-ID-ED
           MOVE SPACES TO WS-TITULO
           STRING "COMPROBANTE DE INGRESO DE EQUIPO N. "
                  FUNCTION TRIM(WS-ID-ED)
               DELIMITED BY SIZE INTO WS-TITULO
           END-STRING
           PERFORM ESCRIBIR-ENCABEZADO-COMPROBANTE
           MOVE EQUIPMENTS-FECHA-INGRESO TO WS-FECHA
           PERFORM FORMATEAR-FECHA
           MOVE "Ingreso:" TO WS-DATO-ETIQUETA
           MOVE WS-FECHA-TXT TO WS-DATO-VALOR
           PERFORM ESCRIBIR-DATO
           PERFORM ESCRIBIR-RENGLON

           MOVE "CLIENTE" TO WS-DATO-VALOR
           PERFORM ESCRIBIR-TEXTO
           MOVE "DNI:" TO WS-DATO-ETIQUETA
           MOVE CUSTOMERS-DNI TO WS-DATO-VALOR
           PERFORM ESCRIBIR-DATO
           MOVE "Nombre:" TO WS-DATO-ETIQUETA
           MOVE CUSTOMERS-NAME TO WS-DATO-VALOR
           PERFORM ESCRIBIR-DATO
           MOVE "Teléfono:" TO WS-DATO-ETIQUETA
           MOVE CUSTOMERS-CELLPHONE TO WS-DATO-VALOR
           PERFORM ESCRIBIR-DATO
           MOVE "Email:" TO WS-DATO-ETIQUETA
           MOVE CUSTOMERS-EMAIL TO WS-DATO-VALOR
           PERFORM ESCRIBIR-DATO
           PERFORM ESCRIBIR-RENGLON

           MOVE "EQUIPO" TO WS-DATO-VALOR
           PERFORM ESCRIBIR-TEXTO
           MOVE "Tipo:" TO WS-DATO-ETIQUETA
           MOVE EQUIPMENTS-TIPO TO WS-DATO-VALOR
           PERFORM ESCRIBIR-DATO
           MOVE "Descripción:" TO WS-DATO-ETIQUETA
           MOVE EQUIPMENTS-DESCRIPCION TO WS-DATO-VALOR
           PERFORM ESCRIBIR-DATO
           MOVE "Características:" TO WS-DATO-ETIQUETA
           MOVE EQUIPMENTS-CARACTERISTICAS TO WS-DATO-VALOR
           PERFORM ESCRIBIR-DATO
           MOVE "Problema:" TO WS-DATO-ETIQUETA
           MOVE EQUIPMENTS-PROBLEMA TO WS-DATO-VALOR
           PERFORM ESCRIBIR-DATO
           MOVE "Estado:" TO WS-DATO-ETIQUETA
           MOVE WS-TEXTO-ESTADO TO WS-DATO-VALOR
           PERFORM ESCRIBIR-DATO
           PERFORM ESCRIBIR-RENGLON

           MOVE "  Conserve este comprobante: es necesario para retirar"
               TO WS-DATO-VALOR
           PERFORM ESCRIBIR-TEXTO
           MOVE "  el equipo." TO WS-DATO-VALOR
           PERFORM ESCRIBIR-TEXTO
           PERFORM ESCRIBIR-PIE-COMPROBANTE.

       CERRAR-ARCHIVOS.
           CLOSE EQUIPMENTS-FILE CUSTOMERS-FILE BUDGETS-FILE
                 CONTROL-FILE.

           COPY "proc-rutas.cpy".
           COPY "proc-comun.cpy".
           COPY "proc-control.cpy".
           COPY "proc-texto.cpy".

       END PROGRAM EQUIPOS.
