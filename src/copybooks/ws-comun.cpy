      *-----------------------------------------------------------------
      * Variables de trabajo compartidas por todos los módulos.
      *-----------------------------------------------------------------
       01  WS-ENTRADA                  PIC X(200).
       01  WS-PROMPT                   PIC X(60).
       01  WS-VALOR-ACTUAL             PIC X(200).
       01  WS-LARGO                    PIC 9(3).
      * Largo máximo (en bytes) del dato que se está pidiendo.
       01  WS-LARGO-MAXIMO             PIC 9(3) VALUE 100.
       01  WS-LARGO-MAXIMO-ED          PIC ZZ9.
       01  WS-I                        PIC 9(3).
       01  WS-J                        PIC 9(3).
       01  WS-POSICION                 PIC 9(3).
       01  WS-CANT-COMAS               PIC 9(3).
       01  WS-CANT-PUNTOS              PIC 9(3).
       01  WS-IMPORTE-TXT              PIC X(200).
       01  WS-EMAIL-USUARIO            PIC X(200).
       01  WS-EMAIL-DOMINIO            PIC X(200).
       01  WS-DNI-NUMERO               PIC 9(8).

       01  WS-CANCELADO                PIC X VALUE "N".
           88 OPERACION-CANCELADA      VALUE "S".
           88 OPERACION-EN-CURSO       VALUE "N".
       01  WS-CONFIRMA                 PIC X.
           88 CONFIRMA-SI              VALUE "S".
           88 CONFIRMA-NO              VALUE "N".
       01  WS-SALIR-MENU               PIC X VALUE "N".
           88 SALIR-MENU               VALUE "S".
           88 SEGUIR-EN-MENU           VALUE "N".
       01  WS-FIN-LECTURA              PIC X VALUE "N".
           88 FIN-LECTURA              VALUE "S".
           88 HAY-MAS-REGISTROS        VALUE "N".
       01  WS-ENCONTRADO               PIC X VALUE "N".
           88 REGISTRO-ENCONTRADO      VALUE "S".
           88 REGISTRO-NO-ENCONTRADO   VALUE "N".

       01  WS-DNI                      PIC X(8).
       01  WS-ID                       PIC 9(5).
       01  WS-ID-ED                    PIC Z(4)9.
       01  WS-IMPORTE                  PIC 9(9)V99.
       01  WS-IMPORTE-LEIDO            PIC S9(9)V99.
       01  WS-IMPORTE-ED               PIC ZZZ.ZZZ.ZZ9,99.

       01  WS-FECHA                    PIC 9(8).
       01  WS-FECHA-R REDEFINES WS-FECHA.
           05 WS-FECHA-AAAA            PIC 9(4).
           05 WS-FECHA-MM              PIC 9(2).
           05 WS-FECHA-DD              PIC 9(2).
       01  WS-FECHA-TXT                PIC X(10).
       01  WS-FECHA-HOY                PIC 9(8).
       01  WS-FECHA-MINIMA             PIC 9(8) VALUE ZERO.

       01  WS-LINEAS-MOSTRADAS         PIC 9(3) VALUE ZERO.
       01  WS-LINEAS-POR-PAGINA        PIC 9(3) VALUE 20.
       01  WS-CANTIDAD                 PIC 9(5) VALUE ZERO.
       01  WS-CANTIDAD-ED              PIC ZZZZ9.

      * Opción elegida en el menú actual.
       01  WS-OPCION                   PIC X(10).

      * Ajuste de textos a un ancho en columnas (UTF-8).
       01  WS-CORTE-ORIGEN             PIC X(200).
       01  WS-CORTE-ANCHO              PIC 9(3).
       01  WS-CORTE-RESULTADO          PIC X(400).
       01  WS-CORTE-LARGO              PIC 9(3).
       01  WS-CORTE-COLUMNAS           PIC 9(3).
       01  WS-LINEA                    PIC X(400).
       01  WS-PUNTERO                  PIC 9(3).

      * Interfaz: clásica (por defecto) o pantalla completa
      * (RS_INTERFAZ=pantalla) con tema color o verde (RS_TEMA).
       01  WS-INTERFAZ                 PIC X(20).
       01  WS-MODO-INTERFAZ            PIC X VALUE "C".
           88 MODO-CLASICO             VALUE "C".
           88 MODO-PANTALLA            VALUE "P".
       01  WS-TEMA                     PIC X VALUE "C".
           88 TEMA-COLOR               VALUE "C".
           88 TEMA-VERDE               VALUE "V".
       01  WS-PAUSA                    PIC X VALUE "N".
           88 PAUSA-PENDIENTE          VALUE "S".
           88 SIN-PAUSA                VALUE "N".
       01  WS-TITULO                   PIC X(60).
       01  WS-ESC                      PIC X VALUE X"1B".
       01  WS-COLOR-BASE               PIC X(10).
       01  WS-COLOR-TITULO             PIC X(12).
      * ACCEPT ... FROM TIME devuelve HHMMSScc.
       01  WS-HORA                     PIC 9(8).
       01  WS-HORA-R REDEFINES WS-HORA.
           05 WS-HORA-HH               PIC 99.
           05 WS-HORA-MM               PIC 99.
           05 WS-HORA-SS               PIC 99.
           05 WS-HORA-CC               PIC 99.

       01  WS-SEPARADOR                PIC X(70) VALUE ALL "-".
       01  WS-SEPARADOR-DOBLE          PIC X(70) VALUE ALL "=".
