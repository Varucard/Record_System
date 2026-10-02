      *-----------------------------------------------------------------
      * Archivo lógico de Equipos. Largo de registro: 337 bytes.
      * Estados: I = Ingresado, R = En reparación,
      *          L = Listo para retirar, E = Entregado.
      *-----------------------------------------------------------------
       FD  EQUIPMENTS-FILE.
       01  EQUIPMENTS-REGISTERS.
           05 EQUIPMENTS-ID              PIC 9(5).
           05 EQUIPMENTS-DNI             PIC X(8).
           05 EQUIPMENTS-TIPO            PIC X(15).
           05 EQUIPMENTS-DESCRIPCION     PIC X(100).
           05 EQUIPMENTS-CARACTERISTICAS PIC X(100).
           05 EQUIPMENTS-PROBLEMA        PIC X(100).
           05 EQUIPMENTS-ESTADO          PIC X(1).
              88 EQUIPO-INGRESADO        VALUE "I".
              88 EQUIPO-EN-REPARACION    VALUE "R".
              88 EQUIPO-LISTO            VALUE "L".
              88 EQUIPO-ENTREGADO        VALUE "E".
           05 EQUIPMENTS-FECHA-INGRESO   PIC 9(8).
