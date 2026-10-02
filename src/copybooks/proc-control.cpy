      *-----------------------------------------------------------------
      * Numeración que no reutiliza números de registros borrados.
      * Requiere CONTROL-FILE abierto en modo I-O y WS-CLAVE-CONTROL
      * con la entidad ("EQUIPOS" o "PRESUPUESTOS").
      *-----------------------------------------------------------------

      * Recibe en WS-ID el último número del archivo + 1 y lo ajusta
      * para que supere también al último número asignado alguna vez.
      * Deja WS-ID en cero si la numeración está agotada.
       AJUSTAR-ID-CONTROL.
           MOVE WS-CLAVE-CONTROL TO CONTROL-CLAVE
           READ CONTROL-FILE RECORD KEY IS CONTROL-CLAVE
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   IF WS-ID > ZERO AND CONTROL-ULTIMO-ID >= WS-ID
                       IF CONTROL-ULTIMO-ID = 99999
                           MOVE ZERO TO WS-ID
                       ELSE
                           COMPUTE WS-ID = CONTROL-ULTIMO-ID + 1
                       END-IF
                   END-IF
           END-READ.

      * Guarda WS-ID como último número asignado de la entidad.
       REGISTRAR-ID-CONTROL.
           MOVE WS-CLAVE-CONTROL TO CONTROL-CLAVE
           MOVE WS-ID TO CONTROL-ULTIMO-ID
           REWRITE CONTROL-REGISTERS
               INVALID KEY
                   WRITE CONTROL-REGISTERS
                       INVALID KEY
                           DISPLAY "  Aviso: no se pudo actualizar "
                                   "control.dat (file status "
                                   WS-FS-CONTROL ")." END-DISPLAY
                   END-WRITE
           END-REWRITE.
