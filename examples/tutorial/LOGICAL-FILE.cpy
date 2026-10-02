      *-----------------------------------------------------------------
      * Archivo lógico indexado de empleados.
      *-----------------------------------------------------------------
       FD  EMPLEADOS-ARCHIVO.
       01  EMPLEADOS-REGISTRO.
           05 EMPLEADOS-ID          PIC 9(6).
           05 EMPLEADOS-NOMBRE      PIC X(25).
           05 EMPLEADOS-APELLIDOS   PIC X(35).
           05 EMPLEADOS-EDAD        PIC 9(3).
           05 EMPLEADOS-TELEFONO    PIC X(9).
           05 EMPLEADOS-DIRECCION   PIC X(35).
