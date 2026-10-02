      *-----------------------------------------------------------------
      * Archivo lógico de Clientes. Largo de registro: 161 bytes.
      *-----------------------------------------------------------------
       FD  CUSTOMERS-FILE.
       01  CUSTOMERS-REGISTERS.
           05 CUSTOMERS-DNI            PIC X(8).
           05 CUSTOMERS-NAME           PIC X(40).
           05 CUSTOMERS-CELLPHONE      PIC X(15).
           05 CUSTOMERS-EMAIL          PIC X(50).
           05 CUSTOMERS-ADDRESS        PIC X(40).
           05 CUSTOMERS-FECHA-ALTA     PIC 9(8).
