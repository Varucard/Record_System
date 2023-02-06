      ******************************************************************
      * Author: Marquez Cristian Ariel
      * Date: 05/02/2023
      * Purpose: Registro de Usuarios y trabajos en Computadoras
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Record_System.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
       SELECT OPTIONAL CUSTOMERS-FILE
       ASSIGN TO "C:\Desarrollos\Record_System\customers.dat"
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMERS-FILE.
           01 CUSTOMERS-REGISTERS.
               05 CUSTOMERS-ID PIC X(4).
               05 CUSTOMERS-DNI PIC X(8).
               05 CUSTOMERS-NAME PIC X(25).
               05 CUSTOMERS-CELLPHONE PIC X(11).
               05 CUSTOMERS-EMAIL PIC X(50).
               05 CUSTOMERS-ADDRESS PIC X(35).

       WORKING-STORAGE SECTION.
       01  DNI PIC X(19)
           VALUE "Introduzca el DNI: ".
       01  NOMBRE PIC X(25)
           VALUE "Introduzca el nombre: ".
       01  TELEFONO PIC X(34)
           VALUE "Introduzca el numero de telefono: ".
       01  EMAIL PIC X(50)
           VALUE "Introduzca el Email: ".
       01  DIRECCION PIC X(25)
           VALUE "Introduzca la direccion: ".

       01  SI-NO PIC X.
       01  ENTRADA PIC X.

       PROCEDURE DIVISION.
       MAIN-LOGIC SECTION.
       PROGRAM-BEGIN.

       PERFORM PROCEDIMIENTO-DE-APERTURA.
       MOVE "S" TO SI-NO.
       PERFORM AGREGAR-REGISTROS
       UNTIL SI-NO = "N".
       PERFORM PROCEDIMIENTO-DE-CIERRE.

       PROGRAM-DONE.
       STOP RUN.

       PROCEDIMIENTO-DE-APERTURA.
       OPEN EXTEND CUSTOMERS-FILE.

       PROCEDIMIENTO-DE-CIERRE.
       CLOSE CUSTOMERS-FILE.

       AGREGAR-REGISTROS.
       MOVE "N" TO ENTRADA.
       PERFORM OBTENER-CAMPOS
       UNTIL ENTRADA = "S".
       PERFORM ESCRIBIR-REGISTRO.
       PERFORM REINICIAR.

       OBTENER-CAMPOS.
       MOVE SPACE TO CUSTOMERS-REGISTERS.
       DISPLAY DNI.
       ACCEPT CUSTOMERS-DNI.
       DISPLAY NOMBRE.
       ACCEPT CUSTOMERS-NAME.
       DISPLAY TELEFONO.
       ACCEPT CUSTOMERS-CELLPHONE.
       DISPLAY EMAIL.
       ACCEPT CUSTOMERS-EMAIL.
       DISPLAY DIRECCION.
       ACCEPT CUSTOMERS-ADDRESS.
       PERFORM CONTINUAR.

       CONTINUAR.
       MOVE "S" TO ENTRADA.
       IF  CUSTOMERS-NAME = SPACE
       MOVE "N" TO ENTRADA.

       ESCRIBIR-REGISTRO.
       WRITE CUSTOMERS-REGISTERS.

       REINICIAR.
       DISPLAY "¿Desea almacenar otro registro en la base de datos?".
       ACCEPT SI-NO.
       IF SI-NO = "s"
       MOVE "S" TO SI-NO.
       IF SI-NO NOT = "S"
       MOVE "N" TO SI-NO.

       END PROGRAM Record_System.
