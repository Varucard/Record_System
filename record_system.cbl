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

      *Definición de archivos
       FILE-CONTROL.

      *Archivo fisico de Clientes
       SELECT OPTIONAL CUSTOMERS-FILE
       ASSIGN TO "C:\Desarrollos\Record_System\customers.dat"
       ORGANIZATION IS LINE SEQUENTIAL.

      *Archivo fisico de Equipos
       SELECT OPTIONAL EQUIPMENTS-FILE
       ASSIGN TO "C:\Desarrollos\Record_System\equipments.dat"
       ORGANIZATION IS LINE SEQUENTIAL.

      *Archivo fisico de Presupuestos
       SELECT OPTIONAL BUDGETS-FILE
       ASSIGN TO "C:\Desarrollos\Record_System\budgets.dat"
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

      *Archivo logico de Clientes
       FD CUSTOMERS-FILE.
           01 CUSTOMERS-REGISTERS.
               05 CUSTOMERS-ID PIC X(4).
               05 CUSTOMERS-DNI PIC X(8).
               05 CUSTOMERS-NAME PIC X(25).
               05 CUSTOMERS-CELLPHONE PIC X(11).
               05 CUSTOMERS-EMAIL PIC X(50).
               05 CUSTOMERS-ADDRESS PIC X(35).

      *Archivo logico de Equipos
       FD EQUIPMENTS-FILE.
           01 EQUIPMENTS-REGISTERS.
               05 EQUIPMENTS-ID PIC X(4).
               05 EQUIPMENTS-DNI PIC X(8).
               05 EQUIPMENTS-TIPO PIC X(10).
               05 EQUIPMENTS-DESCRIPCION PIC X(100).
               05 EQUIPMENTS-CARACTERISTICAS PIC X(100).
               05 EQUIPMENTS-PROBLEMA PIC X(100).

      *Archivo logico de Presupuestos
       FD BUDGETS-FILE.
           01 BUDGETS-REGISTERS.
               05 BUDGETS-ID PIC X(4).
               05 BUDGETS-DNI PIC X(8).
               05 BUDGETS-DESCRIPCION PIC X(100).
               05 BUDGETS-FORMA_PAGO PIC X(15).
               05 BUDGETS-FECHA PIC X(10).
               05 BUDGETS-PAGADO PIC X(1).

       WORKING-STORAGE SECTION.

      *Almacenamiento de los datos ingresados del Cliente
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

      *Almacenamiento de los datos ingresados del Equipo
       01  DNI-CLIENTE PIC X(31)
           VALUE "Introduzca el DNI del cliente: ".
       01  TIPO PIC X(30)
           VALUE "Introduzca el tipo de Equipo: ".
       01  DESCRIPCION-EQUIPO PIC X(44)
           VALUE "Introduzca una breve descripcion del mismo: ".
       01  CARACTERISTICAS PIC X(42)
           VALUE "Introduzca las caracteristicas del mismo: ".
       01  PROBLEMA PIC X(40)
           VALUE "Introduzca el inconveniente del equipo: ".

      *Almacenamiento de los datos ingresados del Presupuesto
       01  CLIENTE-DNI PIC X(31)
           VALUE "Introduzca el DNI del cliente: ".
       01  DESCRIPCION-PRESUPUESTO PIC X(40)
           VALUE "Introduzca una descripcion del trabajo: ".
       01  FORMA_PAGO PIC X(29)
           VALUE "Introduzca la forma de pago: ".
       01  FECHA PIC X(37)
           VALUE "Introduzca la fecha del presupuesto: ".
       01  PAGADO PIC X(20)
           VALUE "Introduzca '1' o '0'".

       01  SI-NO PIC X.
       01  ENTRADA PIC X.

       PROCEDURE DIVISION.
       MAIN-LOGIC SECTION.
       PROGRAM-BEGIN.

       PERFORM APERTURA-CLIENTES.
       PERFORM APERTURA-EQUIPOS.
       PERFORM APERTURA-PRESUPUESTOS.
       MOVE "S" TO SI-NO.
       PERFORM AGREGAR-REGISTROS
       UNTIL SI-NO = "N".
       PERFORM CIERRE-CLIENTES.
       PERFORM CIERRE-EQUIPOS.
       PERFORM CIERRE-PRESUPUESTOS.

       PROGRAM-DONE.
       STOP RUN.

      *Apertura de Archivos, si no se encuentran los crea
       APERTURA-CLIENTES.
       OPEN EXTEND CUSTOMERS-FILE.

       APERTURA-EQUIPOS.
       OPEN EXTEND EQUIPMENTS-FILE.

       APERTURA-PRESUPUESTOS.
       OPEN EXTEND BUDGETS-FILE.

      *Cierre de Archivos
       CIERRE-CLIENTES.
       CLOSE CUSTOMERS-FILE.

       CIERRE-EQUIPOS.
       CLOSE EQUIPMENTS-FILE.

       CIERRE-PRESUPUESTOS.
       CLOSE BUDGETS-FILE.

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
