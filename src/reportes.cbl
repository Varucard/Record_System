      ******************************************************************
      * Purpose: Reportes: resumen general del negocio (clientes,
      *          equipos por estado, presupuestos cobrados/pendientes,
      *          recaudación por forma de pago) y exportación de los
      *          datos a archivos CSV.
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
           COPY "fc-texto.cpy".

       DATA DIVISION.
       FILE SECTION.
           COPY "fd-customers.cpy".
           COPY "fd-equipments.cpy".
           COPY "fd-budgets.cpy".
           COPY "fd-texto.cpy".

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

      * Exportación CSV (separador ";" y UTF-8 con BOM para Excel).
       01  WS-CSV-PRIMER-CAMPO         PIC X VALUE "S".
           88 CSV-PRIMER-CAMPO         VALUE "S".
       01  WS-CSV-REGISTROS            PIC 9(5).
       01  WS-IMPORTE-CSV              PIC Z(10)9,99.
       01  WS-TEXTO-ESTADO             PIC X(20).

       LINKAGE SECTION.
           COPY "lk-comun.cpy".

       PROCEDURE DIVISION USING LK-DIRECTORIO-DATOS.
       INICIO-REPORTES.
           PERFORM ARMAR-RUTAS
           PERFORM INICIAR-INTERFAZ
           SET SEGUIR-EN-MENU TO TRUE
           PERFORM MENU-REPORTES UNTIL SALIR-MENU
           GOBACK.

       MENU-REPORTES.
           PERFORM PREPARAR-PANTALLA
           MOVE "REPORTES" TO WS-TITULO
           PERFORM MOSTRAR-TITULO
           DISPLAY "  1. Reporte general" END-DISPLAY
           DISPLAY "  2. Exportar datos a CSV (Excel)" END-DISPLAY
           DISPLAY "  0. Volver al menú principal" END-DISPLAY
           MOVE "Opción:" TO WS-PROMPT
           PERFORM MOSTRAR-PROMPT
           PERFORM LEER-ENTRADA
           MOVE WS-ENTRADA TO WS-OPCION
           EVALUATE WS-OPCION
               WHEN "1" PERFORM REPORTE-GENERAL
               WHEN "2" PERFORM EXPORTAR-CSV
               WHEN "0" SET SALIR-MENU TO TRUE
               WHEN OTHER
                   DISPLAY "  Opción inválida." END-DISPLAY
           END-EVALUATE
           IF WS-OPCION NOT = "0"
               PERFORM MARCAR-PAUSA
           END-IF.

       ABRIR-ARCHIVOS-DATOS.
           OPEN INPUT CUSTOMERS-FILE EQUIPMENTS-FILE BUDGETS-FILE
           IF NOT FS-CUSTOMERS-OK OR NOT FS-EQUIPMENTS-OK
              OR NOT FS-BUDGETS-OK
               DISPLAY "Error al abrir los archivos (file status "
                       WS-FS-CUSTOMERS "/" WS-FS-EQUIPMENTS "/"
                       WS-FS-BUDGETS ")." END-DISPLAY
               PERFORM CERRAR-ARCHIVOS
               MOVE "99" TO WS-FS-CUSTOMERS
           END-IF.

      *-----------------------------------------------------------------
      * Reporte general
      *-----------------------------------------------------------------
       REPORTE-GENERAL.
           INITIALIZE WS-TOTALES WS-FORMAS-PAGO
           PERFORM ABRIR-ARCHIVOS-DATOS
           IF FS-CUSTOMERS-OK
               PERFORM CONTAR-CLIENTES
               PERFORM CONTAR-EQUIPOS
               PERFORM CONTAR-PRESUPUESTOS
               PERFORM MOSTRAR-RESUMEN
               PERFORM CERRAR-ARCHIVOS
           END-IF.

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
                   MOVE WS-FORMA-NOMBRE(IX-FORMA) TO WS-CORTE-ORIGEN
                   MOVE 15 TO WS-CORTE-ANCHO
                   PERFORM AJUSTAR-ANCHO
                   DISPLAY "  - " WS-CORTE-RESULTADO(1:WS-CORTE-LARGO)
                           WS-CANTIDAD-ED " pago(s)  $ " WS-MONTO-ED
                   END-DISPLAY
               END-PERFORM
           END-IF
           DISPLAY WS-SEPARADOR-DOBLE END-DISPLAY.

      * Etiqueta alineada a 32 columnas (respeta los acentos) y
      * cantidad.
       MOSTRAR-CANTIDAD.
           MOVE WS-CANTIDAD TO WS-CANTIDAD-ED
           MOVE WS-ETIQUETA TO WS-CORTE-ORIGEN
           MOVE 32 TO WS-CORTE-ANCHO
           PERFORM AJUSTAR-ANCHO
           DISPLAY WS-CORTE-RESULTADO(1:WS-CORTE-LARGO) WS-CANTIDAD-ED
           END-DISPLAY.

      *-----------------------------------------------------------------
      * Exportación a CSV: <datos>/exportes/{clientes,equipos,
      * presupuestos}.csv
      *-----------------------------------------------------------------
       EXPORTAR-CSV.
           DISPLAY " " END-DISPLAY
           DISPLAY "-- Exportar datos a CSV --" END-DISPLAY
           PERFORM ABRIR-ARCHIVOS-DATOS
           IF FS-CUSTOMERS-OK
               MOVE "exportes" TO WS-SUBDIRECTORIO
               PERFORM EXPORTAR-CLIENTES
               PERFORM EXPORTAR-EQUIPOS
               PERFORM EXPORTAR-PRESUPUESTOS
               PERFORM CERRAR-ARCHIVOS
           END-IF.

       EXPORTAR-CLIENTES.
           MOVE "clientes.csv" TO WS-NOMBRE-TEXTO
           PERFORM ABRIR-ARCHIVO-TEXTO
           IF FS-TEXTO-OK
               PERFORM CSV-INICIAR-ARCHIVO
               MOVE "DNI" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "Nombre" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "Teléfono" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "Email" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "Dirección" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "Fecha de alta" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               PERFORM CSV-TERMINAR-RENGLON
               SET HAY-MAS-REGISTROS TO TRUE
               PERFORM UNTIL FIN-LECTURA
                   READ CUSTOMERS-FILE NEXT RECORD
                       AT END SET FIN-LECTURA TO TRUE
                   END-READ
                   IF NOT FS-CUSTOMERS-OK
                       SET FIN-LECTURA TO TRUE
                   END-IF
                   IF HAY-MAS-REGISTROS
                       MOVE CUSTOMERS-DNI TO WS-DATO-VALOR
                       PERFORM CSV-AGREGAR-CAMPO
                       MOVE CUSTOMERS-NAME TO WS-DATO-VALOR
                       PERFORM CSV-AGREGAR-CAMPO
                       MOVE CUSTOMERS-CELLPHONE TO WS-DATO-VALOR
                       PERFORM CSV-AGREGAR-CAMPO
                       MOVE CUSTOMERS-EMAIL TO WS-DATO-VALOR
                       PERFORM CSV-AGREGAR-CAMPO
                       MOVE CUSTOMERS-ADDRESS TO WS-DATO-VALOR
                       PERFORM CSV-AGREGAR-CAMPO
                       MOVE CUSTOMERS-FECHA-ALTA TO WS-FECHA
                       PERFORM CSV-AGREGAR-FECHA
                       PERFORM CSV-TERMINAR-RENGLON
                       ADD 1 TO WS-CSV-REGISTROS
                   END-IF
               END-PERFORM
               PERFORM CSV-CERRAR-ARCHIVO
           END-IF.

       EXPORTAR-EQUIPOS.
           MOVE "equipos.csv" TO WS-NOMBRE-TEXTO
           PERFORM ABRIR-ARCHIVO-TEXTO
           IF FS-TEXTO-OK
               PERFORM CSV-INICIAR-ARCHIVO
               MOVE "Número" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "DNI cliente" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "Tipo" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "Descripción" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "Características" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "Problema" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "Estado" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "Fecha de ingreso" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               PERFORM CSV-TERMINAR-RENGLON
               SET HAY-MAS-REGISTROS TO TRUE
               PERFORM UNTIL FIN-LECTURA
                   READ EQUIPMENTS-FILE NEXT RECORD
                       AT END SET FIN-LECTURA TO TRUE
                   END-READ
                   IF NOT FS-EQUIPMENTS-OK
                       SET FIN-LECTURA TO TRUE
                   END-IF
                   IF HAY-MAS-REGISTROS
                       MOVE EQUIPMENTS-ID TO WS-DATO-VALOR
                       PERFORM CSV-AGREGAR-CAMPO
                       MOVE EQUIPMENTS-DNI TO WS-DATO-VALOR
                       PERFORM CSV-AGREGAR-CAMPO
                       MOVE EQUIPMENTS-TIPO TO WS-DATO-VALOR
                       PERFORM CSV-AGREGAR-CAMPO
                       MOVE EQUIPMENTS-DESCRIPCION TO WS-DATO-VALOR
                       PERFORM CSV-AGREGAR-CAMPO
                       MOVE EQUIPMENTS-CARACTERISTICAS TO WS-DATO-VALOR
                       PERFORM CSV-AGREGAR-CAMPO
                       MOVE EQUIPMENTS-PROBLEMA TO WS-DATO-VALOR
                       PERFORM CSV-AGREGAR-CAMPO
                       PERFORM DESCRIBIR-ESTADO
                       MOVE WS-TEXTO-ESTADO TO WS-DATO-VALOR
                       PERFORM CSV-AGREGAR-CAMPO
                       MOVE EQUIPMENTS-FECHA-INGRESO TO WS-FECHA
                       PERFORM CSV-AGREGAR-FECHA
                       PERFORM CSV-TERMINAR-RENGLON
                       ADD 1 TO WS-CSV-REGISTROS
                   END-IF
               END-PERFORM
               PERFORM CSV-CERRAR-ARCHIVO
           END-IF.

       EXPORTAR-PRESUPUESTOS.
           MOVE "presupuestos.csv" TO WS-NOMBRE-TEXTO
           PERFORM ABRIR-ARCHIVO-TEXTO
           IF FS-TEXTO-OK
               PERFORM CSV-INICIAR-ARCHIVO
               MOVE "Número" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "Equipo" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "DNI cliente" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "Trabajo" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "Importe" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "Forma de pago" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "Fecha" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "Pagado" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               MOVE "Fecha de pago" TO WS-DATO-VALOR
               PERFORM CSV-AGREGAR-CAMPO
               PERFORM CSV-TERMINAR-RENGLON
               SET HAY-MAS-REGISTROS TO TRUE
               PERFORM UNTIL FIN-LECTURA
                   READ BUDGETS-FILE NEXT RECORD
                       AT END SET FIN-LECTURA TO TRUE
                   END-READ
                   IF NOT FS-BUDGETS-OK
                       SET FIN-LECTURA TO TRUE
                   END-IF
                   IF HAY-MAS-REGISTROS
                       MOVE BUDGETS-ID TO WS-DATO-VALOR
                       PERFORM CSV-AGREGAR-CAMPO
                       MOVE BUDGETS-EQUIPO-ID TO WS-DATO-VALOR
                       PERFORM CSV-AGREGAR-CAMPO
                       MOVE BUDGETS-DNI TO WS-DATO-VALOR
                       PERFORM CSV-AGREGAR-CAMPO
                       MOVE BUDGETS-DESCRIPCION TO WS-DATO-VALOR
                       PERFORM CSV-AGREGAR-CAMPO
                       MOVE BUDGETS-IMPORTE TO WS-IMPORTE-CSV
                       MOVE FUNCTION TRIM(WS-IMPORTE-CSV)
                           TO WS-DATO-VALOR
                       PERFORM CSV-AGREGAR-VALOR
                       MOVE BUDGETS-FORMA-PAGO TO WS-DATO-VALOR
                       PERFORM CSV-AGREGAR-CAMPO
                       MOVE BUDGETS-FECHA TO WS-FECHA
                       PERFORM CSV-AGREGAR-FECHA
                       IF PRESUPUESTO-PAGADO
                           MOVE "Sí" TO WS-DATO-VALOR
                       ELSE
                           MOVE "No" TO WS-DATO-VALOR
                       END-IF
                       PERFORM CSV-AGREGAR-CAMPO
                       MOVE BUDGETS-FECHA-PAGO TO WS-FECHA
                       PERFORM CSV-AGREGAR-FECHA
                       PERFORM CSV-TERMINAR-RENGLON
                       ADD 1 TO WS-CSV-REGISTROS
                   END-IF
               END-PERFORM
               PERFORM CSV-CERRAR-ARCHIVO
           END-IF.

       DESCRIBIR-ESTADO.
           EVALUATE TRUE
               WHEN EQUIPO-INGRESADO
                   MOVE "Ingresado" TO WS-TEXTO-ESTADO
               WHEN EQUIPO-EN-REPARACION
                   MOVE "En reparación" TO WS-TEXTO-ESTADO
               WHEN EQUIPO-LISTO
                   MOVE "Listo para retirar" TO WS-TEXTO-ESTADO
               WHEN EQUIPO-ENTREGADO
                   MOVE "Entregado" TO WS-TEXTO-ESTADO
               WHEN OTHER
                   MOVE "?" TO WS-TEXTO-ESTADO
           END-EVALUATE.

      * El primer renglón del archivo empieza con la marca BOM de
      * UTF-8 para que Excel reconozca los acentos.
       CSV-INICIAR-ARCHIVO.
           MOVE ZERO TO WS-CSV-REGISTROS
           STRING X"EFBBBF" DELIMITED BY SIZE
               INTO WS-RENGLON WITH POINTER WS-PUNTERO-RENGLON
           END-STRING
           SET CSV-PRIMER-CAMPO TO TRUE.

       CSV-SEPARAR.
           IF CSV-PRIMER-CAMPO
               MOVE "N" TO WS-CSV-PRIMER-CAMPO
           ELSE
               STRING ";" DELIMITED BY SIZE
                   INTO WS-RENGLON WITH POINTER WS-PUNTERO-RENGLON
               END-STRING
           END-IF.

      * Agrega WS-DATO-VALOR entre comillas, duplicando las comillas
      * internas (formato CSV estándar).
       CSV-AGREGAR-CAMPO.
           PERFORM CSV-SEPARAR
           MOVE ZERO TO WS-LARGO
           IF WS-DATO-VALOR NOT = SPACES
               COMPUTE WS-LARGO = FUNCTION LENGTH(
                   FUNCTION TRIM(WS-DATO-VALOR TRAILING))
           END-IF
           STRING '"' DELIMITED BY SIZE
               INTO WS-RENGLON WITH POINTER WS-PUNTERO-RENGLON
           END-STRING
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LARGO
               IF WS-DATO-VALOR(WS-I:1) = '"'
                   STRING '""' DELIMITED BY SIZE
                       INTO WS-RENGLON WITH POINTER WS-PUNTERO-RENGLON
                   END-STRING
               ELSE
                   STRING WS-DATO-VALOR(WS-I:1) DELIMITED BY SIZE
                       INTO WS-RENGLON WITH POINTER WS-PUNTERO-RENGLON
                   END-STRING
               END-IF
           END-PERFORM
           STRING '"' DELIMITED BY SIZE
               INTO WS-RENGLON WITH POINTER WS-PUNTERO-RENGLON
           END-STRING.

      * Agrega WS-DATO-VALOR sin comillas (números).
       CSV-AGREGAR-VALOR.
           PERFORM CSV-SEPARAR
           STRING FUNCTION TRIM(WS-DATO-VALOR) DELIMITED BY SIZE
               INTO WS-RENGLON WITH POINTER WS-PUNTERO-RENGLON
           END-STRING.

      * Agrega WS-FECHA como DD/MM/AAAA (vacío si es cero).
       CSV-AGREGAR-FECHA.
           IF WS-FECHA = ZERO
               MOVE SPACES TO WS-DATO-VALOR
           ELSE
               PERFORM FORMATEAR-FECHA
               MOVE WS-FECHA-TXT TO WS-DATO-VALOR
           END-IF
           PERFORM CSV-AGREGAR-CAMPO.

       CSV-TERMINAR-RENGLON.
           PERFORM ESCRIBIR-RENGLON
           SET CSV-PRIMER-CAMPO TO TRUE.

       CSV-CERRAR-ARCHIVO.
           PERFORM CERRAR-ARCHIVO-TEXTO
           MOVE WS-CSV-REGISTROS TO WS-CANTIDAD-ED
           DISPLAY "    (" FUNCTION TRIM(WS-CANTIDAD-ED)
                   " registro(s))" END-DISPLAY.

       CERRAR-ARCHIVOS.
           CLOSE CUSTOMERS-FILE EQUIPMENTS-FILE BUDGETS-FILE.

           COPY "proc-rutas.cpy".
           COPY "proc-comun.cpy".
           COPY "proc-texto.cpy".

       END PROGRAM REPORTES.
