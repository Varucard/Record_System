      ******************************************************************
      * Purpose: ABM de clientes (alta, consulta, listado, búsqueda
      *          por nombre, modificación y baja).
      * Parámetros:
      *   LK-DIRECTORIO-DATOS (entrada)  directorio de los .dat
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIENTES.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "fc-customers.cpy".
           COPY "fc-equipments.cpy".

       DATA DIVISION.
       FILE SECTION.
           COPY "fd-customers.cpy".
           COPY "fd-equipments.cpy".

       WORKING-STORAGE SECTION.
           COPY "ws-archivos.cpy".
           COPY "ws-comun.cpy".
       01  WS-TEXTO-ESTADO             PIC X(20).
       01  WS-BUSQUEDA                 PIC X(40).
       01  WS-LARGO-BUSQUEDA           PIC 9(3).
       01  WS-NOMBRE-MAYUSCULAS        PIC X(40).
       01  WS-COINCIDENCIAS            PIC 9(3).

       LINKAGE SECTION.
           COPY "lk-comun.cpy".

       PROCEDURE DIVISION USING LK-DIRECTORIO-DATOS.
       INICIO-CLIENTES.
           PERFORM ARMAR-RUTAS
           PERFORM INICIAR-INTERFAZ
           OPEN I-O CUSTOMERS-FILE
           OPEN INPUT EQUIPMENTS-FILE
           IF NOT FS-CUSTOMERS-OK OR NOT FS-EQUIPMENTS-OK
               DISPLAY "Error al abrir los archivos (file status "
                       WS-FS-CUSTOMERS "/" WS-FS-EQUIPMENTS ")."
               END-DISPLAY
               PERFORM CERRAR-ARCHIVOS
               GOBACK
           END-IF
           SET SEGUIR-EN-MENU TO TRUE
           PERFORM MENU-CLIENTES UNTIL SALIR-MENU
           PERFORM CERRAR-ARCHIVOS
           GOBACK.

       MENU-CLIENTES.
           PERFORM PREPARAR-PANTALLA
           MOVE "CLIENTES" TO WS-TITULO
           PERFORM MOSTRAR-TITULO
           DISPLAY "  1. Registrar cliente" END-DISPLAY
           DISPLAY "  2. Consultar cliente" END-DISPLAY
           DISPLAY "  3. Listar clientes" END-DISPLAY
           DISPLAY "  4. Modificar cliente" END-DISPLAY
           DISPLAY "  5. Eliminar cliente" END-DISPLAY
           DISPLAY "  6. Buscar clientes por nombre" END-DISPLAY
           DISPLAY "  0. Volver al menú principal" END-DISPLAY
           MOVE "Opción:" TO WS-PROMPT
           PERFORM MOSTRAR-PROMPT
           PERFORM LEER-ENTRADA
           MOVE WS-ENTRADA TO WS-OPCION
           EVALUATE WS-OPCION
               WHEN "1" PERFORM ALTA-CLIENTE
               WHEN "2" PERFORM CONSULTA-CLIENTE
               WHEN "3" PERFORM LISTADO-CLIENTES
               WHEN "4" PERFORM MODIFICACION-CLIENTE
               WHEN "5" PERFORM BAJA-CLIENTE
               WHEN "6" PERFORM BUSQUEDA-CLIENTES
               WHEN "0" SET SALIR-MENU TO TRUE
               WHEN OTHER
                   DISPLAY "  Opción inválida." END-DISPLAY
           END-EVALUATE
           IF WS-OPCION NOT = "0"
               PERFORM MARCAR-PAUSA
           END-IF.

      *-----------------------------------------------------------------
      * Alta
      *-----------------------------------------------------------------
       ALTA-CLIENTE.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Registrar cliente --" END-DISPLAY
           PERFORM LEER-DNI
           IF OPERACION-CANCELADA
               DISPLAY "  Operación cancelada." END-DISPLAY
           ELSE
               PERFORM BUSCAR-CLIENTE
               IF REGISTRO-ENCONTRADO
                   DISPLAY "  Ya existe un cliente con DNI " WS-DNI
                           ": " FUNCTION TRIM(CUSTOMERS-NAME)
                   END-DISPLAY
               ELSE
                   PERFORM AVISAR-CANCELACION
                   PERFORM CARGAR-DATOS-CLIENTE
                   IF OPERACION-CANCELADA
                       DISPLAY "  Operación cancelada." END-DISPLAY
                   ELSE
                       PERFORM GRABAR-CLIENTE-NUEVO
                   END-IF
               END-IF
           END-IF.

       CARGAR-DATOS-CLIENTE.
           MOVE SPACES TO CUSTOMERS-REGISTERS
           MOVE WS-DNI TO CUSTOMERS-DNI
           MOVE "Nombre y apellido:" TO WS-PROMPT
           MOVE 40 TO WS-LARGO-MAXIMO
           PERFORM LEER-TEXTO-OBLIGATORIO
           MOVE WS-ENTRADA TO CUSTOMERS-NAME
           MOVE "Teléfono:" TO WS-PROMPT
           MOVE 15 TO WS-LARGO-MAXIMO
           PERFORM LEER-TEXTO
           MOVE WS-ENTRADA TO CUSTOMERS-CELLPHONE
           MOVE "Email (opcional):" TO WS-PROMPT
           PERFORM LEER-EMAIL
           MOVE WS-ENTRADA TO CUSTOMERS-EMAIL
           MOVE "Dirección:" TO WS-PROMPT
           MOVE 40 TO WS-LARGO-MAXIMO
           PERFORM LEER-TEXTO
           MOVE WS-ENTRADA TO CUSTOMERS-ADDRESS
           PERFORM OBTENER-FECHA-HOY
           MOVE WS-FECHA-HOY TO CUSTOMERS-FECHA-ALTA.

       GRABAR-CLIENTE-NUEVO.
           WRITE CUSTOMERS-REGISTERS
               INVALID KEY
                   DISPLAY "  Error al grabar (file status "
                           WS-FS-CUSTOMERS ")." END-DISPLAY
               NOT INVALID KEY
                   DISPLAY "  Cliente registrado correctamente."
                   END-DISPLAY
           END-WRITE.

      *-----------------------------------------------------------------
      * Consulta
      *-----------------------------------------------------------------
       CONSULTA-CLIENTE.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Consultar cliente --" END-DISPLAY
           PERFORM PEDIR-CLIENTE-EXISTENTE
           IF REGISTRO-ENCONTRADO
               PERFORM MOSTRAR-CLIENTE
               PERFORM MOSTRAR-EQUIPOS-CLIENTE
           END-IF.

      * Pide un DNI y lee el cliente. Informa si no existe.
       PEDIR-CLIENTE-EXISTENTE.
           SET REGISTRO-NO-ENCONTRADO TO TRUE
           PERFORM LEER-DNI
           IF OPERACION-CANCELADA
               DISPLAY "  Operación cancelada." END-DISPLAY
           ELSE
               PERFORM BUSCAR-CLIENTE
               IF REGISTRO-NO-ENCONTRADO
                   DISPLAY "  No existe un cliente con DNI " WS-DNI
                           "." END-DISPLAY
               END-IF
           END-IF.

       BUSCAR-CLIENTE.
           SET REGISTRO-NO-ENCONTRADO TO TRUE
           MOVE WS-DNI TO CUSTOMERS-DNI
           READ CUSTOMERS-FILE RECORD KEY IS CUSTOMERS-DNI
               INVALID KEY SET REGISTRO-NO-ENCONTRADO TO TRUE
               NOT INVALID KEY SET REGISTRO-ENCONTRADO TO TRUE
           END-READ.

       MOSTRAR-CLIENTE.
           MOVE CUSTOMERS-FECHA-ALTA TO WS-FECHA
           PERFORM FORMATEAR-FECHA
           DISPLAY WS-SEPARADOR END-DISPLAY
           DISPLAY "  DNI:        " CUSTOMERS-DNI END-DISPLAY
           DISPLAY "  Nombre:     " FUNCTION TRIM(CUSTOMERS-NAME)
           END-DISPLAY
           DISPLAY "  Teléfono:   " FUNCTION TRIM(CUSTOMERS-CELLPHONE)
           END-DISPLAY
           DISPLAY "  Email:      " FUNCTION TRIM(CUSTOMERS-EMAIL)
           END-DISPLAY
           DISPLAY "  Dirección:  " FUNCTION TRIM(CUSTOMERS-ADDRESS)
           END-DISPLAY
           DISPLAY "  Alta:       " WS-FECHA-TXT END-DISPLAY
           DISPLAY WS-SEPARADOR END-DISPLAY.

       MOSTRAR-EQUIPOS-CLIENTE.
           PERFORM CONTAR-EQUIPOS-CLIENTE
           IF WS-CANTIDAD = ZERO
               DISPLAY "  El cliente no tiene equipos registrados."
               END-DISPLAY
           ELSE
               DISPLAY "  Equipos del cliente:" END-DISPLAY
               PERFORM POSICIONAR-EQUIPOS-CLIENTE
               PERFORM UNTIL FIN-LECTURA
                   READ EQUIPMENTS-FILE NEXT RECORD
                       AT END SET FIN-LECTURA TO TRUE
                   END-READ
                   IF NOT FS-EQUIPMENTS-OK
                       SET FIN-LECTURA TO TRUE
                   END-IF
                   IF HAY-MAS-REGISTROS
                       IF EQUIPMENTS-DNI NOT = WS-DNI
                           SET FIN-LECTURA TO TRUE
                       ELSE
                           PERFORM MOSTRAR-LINEA-EQUIPO
                       END-IF
                   END-IF
               END-PERFORM
           END-IF.

       MOSTRAR-LINEA-EQUIPO.
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
           END-EVALUATE
           PERFORM INICIAR-LINEA
           STRING "    #" EQUIPMENTS-ID " " DELIMITED BY SIZE
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
           PERFORM MOSTRAR-LINEA.

      * Posiciona el archivo de equipos en el primer equipo del DNI.
       POSICIONAR-EQUIPOS-CLIENTE.
           SET HAY-MAS-REGISTROS TO TRUE
           MOVE WS-DNI TO EQUIPMENTS-DNI
           START EQUIPMENTS-FILE KEY IS EQUAL TO EQUIPMENTS-DNI
               INVALID KEY SET FIN-LECTURA TO TRUE
           END-START.

       CONTAR-EQUIPOS-CLIENTE.
           MOVE ZERO TO WS-CANTIDAD
           PERFORM POSICIONAR-EQUIPOS-CLIENTE
           PERFORM UNTIL FIN-LECTURA
               READ EQUIPMENTS-FILE NEXT RECORD
                   AT END SET FIN-LECTURA TO TRUE
               END-READ
               IF NOT FS-EQUIPMENTS-OK
                   SET FIN-LECTURA TO TRUE
               END-IF
               IF HAY-MAS-REGISTROS
                   IF EQUIPMENTS-DNI NOT = WS-DNI
                       SET FIN-LECTURA TO TRUE
                   ELSE
                       ADD 1 TO WS-CANTIDAD
                   END-IF
               END-IF
           END-PERFORM.

      *-----------------------------------------------------------------
      * Listado
      *-----------------------------------------------------------------
       LISTADO-CLIENTES.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Listado de clientes --" END-DISPLAY
           PERFORM MOSTRAR-ENCABEZADO-CLIENTES
           MOVE ZERO TO WS-CANTIDAD WS-LINEAS-MOSTRADAS
           SET HAY-MAS-REGISTROS TO TRUE
           MOVE LOW-VALUES TO CUSTOMERS-DNI
           START CUSTOMERS-FILE KEY IS NOT LESS THAN CUSTOMERS-DNI
               INVALID KEY SET FIN-LECTURA TO TRUE
           END-START
           PERFORM UNTIL FIN-LECTURA
               READ CUSTOMERS-FILE NEXT RECORD
                   AT END SET FIN-LECTURA TO TRUE
               END-READ
               IF NOT FS-CUSTOMERS-OK
                   SET FIN-LECTURA TO TRUE
               END-IF
               IF HAY-MAS-REGISTROS
                   PERFORM CONTROLAR-PAGINA
               END-IF
               IF HAY-MAS-REGISTROS
                   ADD 1 TO WS-CANTIDAD
                   PERFORM MOSTRAR-RENGLON-CLIENTE
               END-IF
           END-PERFORM
           DISPLAY WS-SEPARADOR END-DISPLAY
           MOVE WS-CANTIDAD TO WS-CANTIDAD-ED
           DISPLAY "Total de clientes: " FUNCTION TRIM(WS-CANTIDAD-ED)
           END-DISPLAY.

       MOSTRAR-ENCABEZADO-CLIENTES.
           DISPLAY "DNI      NOMBRE                         "
                   "TELÉFONO        EMAIL" END-DISPLAY
           DISPLAY WS-SEPARADOR END-DISPLAY.

       MOSTRAR-RENGLON-CLIENTE.
           PERFORM INICIAR-LINEA
           MOVE CUSTOMERS-DNI TO WS-CORTE-ORIGEN
           MOVE 8 TO WS-CORTE-ANCHO
           PERFORM AGREGAR-COLUMNA
           MOVE CUSTOMERS-NAME TO WS-CORTE-ORIGEN
           MOVE 30 TO WS-CORTE-ANCHO
           PERFORM AGREGAR-COLUMNA
           MOVE CUSTOMERS-CELLPHONE TO WS-CORTE-ORIGEN
           MOVE 15 TO WS-CORTE-ANCHO
           PERFORM AGREGAR-COLUMNA
           MOVE CUSTOMERS-EMAIL TO WS-CORTE-ORIGEN
           MOVE 50 TO WS-CORTE-ANCHO
           PERFORM AGREGAR-COLUMNA
           PERFORM MOSTRAR-LINEA.

      *-----------------------------------------------------------------
      * Búsqueda por nombre (parte del nombre, sin distinguir
      * mayúsculas de minúsculas)
      *-----------------------------------------------------------------
       BUSQUEDA-CLIENTES.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Buscar clientes por nombre --" END-DISPLAY
           MOVE "Nombre o parte del nombre (ENTER cancela):"
               TO WS-PROMPT
           PERFORM MOSTRAR-PROMPT
           PERFORM LEER-ENTRADA
           IF WS-ENTRADA = SPACES
               DISPLAY "  Operación cancelada." END-DISPLAY
           ELSE
               MOVE FUNCTION UPPER-CASE(WS-ENTRADA) TO WS-BUSQUEDA
               COMPUTE WS-LARGO-BUSQUEDA =
                   FUNCTION LENGTH(FUNCTION TRIM(WS-BUSQUEDA))
               PERFORM MOSTRAR-ENCABEZADO-CLIENTES
               MOVE ZERO TO WS-CANTIDAD WS-LINEAS-MOSTRADAS
               SET HAY-MAS-REGISTROS TO TRUE
               MOVE LOW-VALUES TO CUSTOMERS-DNI
               START CUSTOMERS-FILE KEY IS NOT LESS THAN CUSTOMERS-DNI
                   INVALID KEY SET FIN-LECTURA TO TRUE
               END-START
               PERFORM UNTIL FIN-LECTURA
                   READ CUSTOMERS-FILE NEXT RECORD
                       AT END SET FIN-LECTURA TO TRUE
                   END-READ
                   IF NOT FS-CUSTOMERS-OK
                       SET FIN-LECTURA TO TRUE
                   END-IF
                   IF HAY-MAS-REGISTROS
                       PERFORM EVALUAR-COINCIDENCIA
                   END-IF
               END-PERFORM
               DISPLAY WS-SEPARADOR END-DISPLAY
               MOVE WS-CANTIDAD TO WS-CANTIDAD-ED
               DISPLAY "Clientes encontrados: "
                       FUNCTION TRIM(WS-CANTIDAD-ED) END-DISPLAY
           END-IF.

       EVALUAR-COINCIDENCIA.
           MOVE FUNCTION UPPER-CASE(CUSTOMERS-NAME)
               TO WS-NOMBRE-MAYUSCULAS
           MOVE ZERO TO WS-COINCIDENCIAS
           INSPECT WS-NOMBRE-MAYUSCULAS TALLYING WS-COINCIDENCIAS
               FOR ALL WS-BUSQUEDA(1:WS-LARGO-BUSQUEDA)
           IF WS-COINCIDENCIAS > ZERO
               PERFORM CONTROLAR-PAGINA
               IF HAY-MAS-REGISTROS
                   ADD 1 TO WS-CANTIDAD
                   PERFORM MOSTRAR-RENGLON-CLIENTE
               END-IF
           END-IF.

      *-----------------------------------------------------------------
      * Modificación (ENTER mantiene el valor actual)
      *-----------------------------------------------------------------
       MODIFICACION-CLIENTE.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Modificar cliente --" END-DISPLAY
           PERFORM PEDIR-CLIENTE-EXISTENTE
           IF REGISTRO-ENCONTRADO
               PERFORM MOSTRAR-CLIENTE
               DISPLAY "Ingrese los nuevos datos (ENTER mantiene el "
                       "valor actual, - lo borra)." END-DISPLAY
               PERFORM AVISAR-CANCELACION

               MOVE "Nombre y apellido:" TO WS-PROMPT
               MOVE 40 TO WS-LARGO-MAXIMO
               PERFORM LEER-TEXTO
               IF WS-ENTRADA NOT = SPACES AND WS-ENTRADA NOT = "-"
                   MOVE WS-ENTRADA TO CUSTOMERS-NAME
               END-IF

               MOVE "Teléfono:" TO WS-PROMPT
               MOVE CUSTOMERS-CELLPHONE TO WS-VALOR-ACTUAL
               MOVE 15 TO WS-LARGO-MAXIMO
               PERFORM LEER-TEXTO
               PERFORM ACTUALIZAR-CAMPO
               MOVE WS-ENTRADA TO CUSTOMERS-CELLPHONE

               MOVE "Email:" TO WS-PROMPT
               MOVE CUSTOMERS-EMAIL TO WS-VALOR-ACTUAL
               PERFORM LEER-EMAIL
               PERFORM ACTUALIZAR-CAMPO
               MOVE WS-ENTRADA TO CUSTOMERS-EMAIL

               MOVE "Dirección:" TO WS-PROMPT
               MOVE CUSTOMERS-ADDRESS TO WS-VALOR-ACTUAL
               MOVE 40 TO WS-LARGO-MAXIMO
               PERFORM LEER-TEXTO
               PERFORM ACTUALIZAR-CAMPO
               MOVE WS-ENTRADA TO CUSTOMERS-ADDRESS

               IF OPERACION-CANCELADA
                   DISPLAY "  Operación cancelada." END-DISPLAY
               ELSE
                   REWRITE CUSTOMERS-REGISTERS
                       INVALID KEY
                           DISPLAY "  Error al actualizar (file "
                                   "status " WS-FS-CUSTOMERS ")."
                           END-DISPLAY
                       NOT INVALID KEY
                           DISPLAY "  Cliente actualizado "
                                   "correctamente." END-DISPLAY
                   END-REWRITE
               END-IF
           END-IF.

      *-----------------------------------------------------------------
      * Baja (no se permite si el cliente tiene equipos registrados)
      *-----------------------------------------------------------------
       BAJA-CLIENTE.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Eliminar cliente --" END-DISPLAY
           PERFORM PEDIR-CLIENTE-EXISTENTE
           IF REGISTRO-ENCONTRADO
               PERFORM MOSTRAR-CLIENTE
               PERFORM CONTAR-EQUIPOS-CLIENTE
               IF WS-CANTIDAD > ZERO
                   MOVE WS-CANTIDAD TO WS-CANTIDAD-ED
                   DISPLAY "  No se puede eliminar: el cliente tiene "
                           FUNCTION TRIM(WS-CANTIDAD-ED)
                           " equipo(s) registrado(s)." END-DISPLAY
               ELSE
                   MOVE "¿Confirma la eliminación? (S/N):"
                       TO WS-PROMPT
                   PERFORM CONFIRMAR
                   IF CONFIRMA-SI
                       DELETE CUSTOMERS-FILE RECORD
                           INVALID KEY
                               DISPLAY "  Error al eliminar (file "
                                       "status " WS-FS-CUSTOMERS ")."
                               END-DISPLAY
                           NOT INVALID KEY
                               DISPLAY "  Cliente eliminado."
                               END-DISPLAY
                       END-DELETE
                   ELSE
                       DISPLAY "  Operación cancelada." END-DISPLAY
                   END-IF
               END-IF
           END-IF.

       CERRAR-ARCHIVOS.
           CLOSE CUSTOMERS-FILE EQUIPMENTS-FILE.

           COPY "proc-rutas.cpy".
           COPY "proc-comun.cpy".

       END PROGRAM CLIENTES.
