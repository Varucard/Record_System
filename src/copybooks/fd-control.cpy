      *-----------------------------------------------------------------
      * Archivo lógico de control. Largo de registro: 17 bytes.
      * Claves: "EQUIPOS" y "PRESUPUESTOS".
      *-----------------------------------------------------------------
       FD  CONTROL-FILE.
       01  CONTROL-REGISTERS.
           05 CONTROL-CLAVE            PIC X(12).
           05 CONTROL-ULTIMO-ID        PIC 9(5).
