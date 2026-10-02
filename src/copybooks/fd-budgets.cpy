      *-----------------------------------------------------------------
      * Archivo lógico de Presupuestos. Largo de registro: 161 bytes.
      * BUDGETS-PAGADO: S = pagado, N = pendiente de pago.
      *-----------------------------------------------------------------
       FD  BUDGETS-FILE.
       01  BUDGETS-REGISTERS.
           05 BUDGETS-ID              PIC 9(5).
           05 BUDGETS-EQUIPO-ID       PIC 9(5).
           05 BUDGETS-DNI             PIC X(8).
           05 BUDGETS-DESCRIPCION     PIC X(100).
           05 BUDGETS-IMPORTE         PIC 9(9)V99.
           05 BUDGETS-FORMA-PAGO      PIC X(15).
           05 BUDGETS-FECHA           PIC 9(8).
           05 BUDGETS-PAGADO          PIC X(1).
              88 PRESUPUESTO-PAGADO    VALUE "S".
              88 PRESUPUESTO-PENDIENTE VALUE "N".
           05 BUDGETS-FECHA-PAGO      PIC 9(8).
