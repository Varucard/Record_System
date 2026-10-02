      *-----------------------------------------------------------------
      * Archivo físico indexado de empleados (acceso dinámico).
      * Este copybook no estaba en el repositorio original: se
      * reconstruyó a partir de READ-INDEXED-FILE.
      *-----------------------------------------------------------------
       SELECT EMPLEADOS-ARCHIVO
           ASSIGN TO "empleados-indexado.dat"
           ORGANIZATION IS INDEXED
           RECORD KEY IS EMPLEADOS-ID
           ACCESS MODE IS DYNAMIC.
