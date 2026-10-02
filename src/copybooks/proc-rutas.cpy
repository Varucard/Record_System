      *-----------------------------------------------------------------
      * Arma la ruta completa de cada archivo a partir del directorio
      * de datos recibido en LK-DIRECTORIO-DATOS.
      *-----------------------------------------------------------------
       ARMAR-RUTAS.
           MOVE SPACES TO WS-RUTA-CUSTOMERS
                          WS-RUTA-EQUIPMENTS
                          WS-RUTA-BUDGETS
           STRING FUNCTION TRIM(LK-DIRECTORIO-DATOS) "/customers.dat"
               DELIMITED BY SIZE INTO WS-RUTA-CUSTOMERS
           END-STRING
           STRING FUNCTION TRIM(LK-DIRECTORIO-DATOS) "/equipments.dat"
               DELIMITED BY SIZE INTO WS-RUTA-EQUIPMENTS
           END-STRING
           STRING FUNCTION TRIM(LK-DIRECTORIO-DATOS) "/budgets.dat"
               DELIMITED BY SIZE INTO WS-RUTA-BUDGETS
           END-STRING.
