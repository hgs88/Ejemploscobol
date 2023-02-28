       IDENTIFICATION DIVISION.
       PROGRAM-ID. vendedores.
       AUTHOR. J.GAYAN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT entrada ASSIGN TO 'NAT-vendedores.txt'
           organization is line sequential.
           SELECT salida  ASSIGN TO 'NAT-salida'
           organization is line  sequential.
           SELECT salida1  ASSIGN TO 'NAT-vendedores'
           organization is line  sequential.
       DATA DIVISION.
       FILE SECTION.
       FD  ENTRADA
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD IS STANDARD
           RECORDING MODE IS F
           RECORD 120 CHARACTERS
           DATA RECORD IS REG-ENTRADA.
       01 REG-ENTRADA   pic x(120).
       FD  SALIDA
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD IS STANDARD
           RECORDING MODE IS F
           RECORD 132 CHARACTERS
           DATA RECORD IS REG-SALIDA.
       01  reg-salida        pic x(132).
       FD  SALIDA1
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD IS STANDARD
           RECORDING MODE IS F
           RECORD 93 CHARACTERS
           DATA RECORD IS REG-SALIDA1.
       01  REG-SALIDA1.
           03 S1-cod-vendedor   PIC x(4).
           03 S1-nombre       PIC X(10).
           03 S1-fecha-alta.
                 05 S1-A-A    PIC X(4).
                 05 S1-M-A    PIC XX.
                 05 S1-D-A    PIC XX.
           03 S1-NIF          PIC X(10).
           03 S1-fecha-nacimiento.
                 05 S1-A-N    PIC X(4).
                 05 S1-M-N    PIC XX.
                 05 S1-D-N    PIC XX.
           03 S1-DIRECION     PIC X(10).
           03 S1-POBLACION    PIC X(10).
           03 S1-COD-POSTAL   PIC X(10).
           03 S1-TELEFONO     PIC X(10).
           03 S1-ESTADO       PIC X(12).
           03 S1-GUAPO        PIC x.

       WORKING-STORAGE SECTION.
       01  WFECHA.
           03 W-AA  PIC X(4).
           03 FILLER PIC X VALUE '/'.
           03 W-MM  PIC 99 .
           03 FILLER PIC X VALUE '/'.
           03 W-DD  PIC 99 .
       01  WHORA.
           03 W-HH  PIC XX.
           03 W-MI  PIC XX.
           03 W-SS  PIC XX.
       01  DESTRIPAR.
           03 W-cod-vendedor      PIC x(4) justified right.
           03 W-cod-vendedor-n redefines W-cod-vendedor pic 9(4).
           03 w-fecha-alta        PIC X(10).
           03 w-nif.
              05 s-nif-num        pic 9(9).
              05 s-nif-letra      pic x.
           03 w-fecha-nacimiento  PIC X(10).

       01  CABECERA1.
           03 FILLER PIC X(8)   VALUE 'vendedor'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(8)  VALUE 'Nombre'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(15)   VALUE '  fecha-alta  '.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(6)   VALUE 'NIF  '.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(11)   VALUE ' Nacimiento'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(10)   VALUE 'Direccion'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(9)   VALUE 'Poblacion'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(6)   VALUE 'co-pos'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(8)   VALUE 'Telefono'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(11)   VALUE '  estado   '.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(5)   VALUE 'guapo'.
       01  CABECERA2.
           03 FILLER PIC X(8)   VALUE ALL '-'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(10)  VALUE ALL '-'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(10)   VALUE ALL '-'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(9)   VALUE ALL '-'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(11)  VALUE ALL '-'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(10)  VALUE ALL '-'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(9)  VALUE ALL '-'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(6)  VALUE ALL '-'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(9)  VALUE ALL '-'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(9)  VALUE ALL '-'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(9)  VALUE ALL '-'.
           03 FILLER         PIC X(2) VALUE SPACES.
       01  CABECERA3.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 S-cod-vendedor   PIC zzz9.
           03 FILLER         PIC X(5) VALUE SPACES.
           03 S-nombre        PIC X(10).
           03 FILLER         PIC X(1) VALUE SPACES.
           03 S-fecha-alta      PIC X(10).
           03 FILLER         PIC X(2) VALUE SPACES.
           03 s-nif.
              05 s-nif-num     pic 9(9).
              05 s-nif-letra   pic x.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 S-fecha-nacimiento     PIC X(10).
           03 FILLER         PIC X(2) VALUE SPACES.
           03 S-direcion     PIC X(10).
           03 FILLER         PIC X(2) VALUE SPACES.
           03 S-poblacion     PIC X(10).
           03 FILLER         PIC X(2) VALUE SPACES.
           03 S-cod-postal     PIC X(5).
           03 FILLER         PIC X(2) VALUE SPACES.
           03 S-telefono     PIC X(10) justified right.
      *    03 S-telefono     PIC X(10).
           03 FILLER         PIC X(2) VALUE SPACES.
           03 S-estado       PIC X(12).
           03 FILLER         PIC X(2) VALUE SPACES.
           03 S-guapo     PIC X(10).
       77  FE       PIC 9    VALUE 0.
       77 WDDMMAA    PIC X(10).
       77 WHHMMSS    PIC X(8).
       77 WFECHOR    PIC X(18).

       PROCEDURE DIVISION.
       ABRIR.
           OPEN input ENTRADA
           OPEN OUTPUT SALIDA salida1.
           WRITE REG-SALIDA from cabecera1 END-WRITE
           WRITE REG-SALIDA from cabecera2 END-WRITE
           READ ENTRADA AT END MOVE 1 TO FE END-READ
           PERFORM  UNTIL FE = 1
               unstring reg-entrada delimited by ';' into
                 W-cod-vendedor   S-nombre  w-fecha-alta   S-nif
                 w-fecha-nacimiento  S-direcion  S-poblacion
                 s-cod-postal s-telefono s-estado  s-guapo
               end-unstring
               inspect W-cod-vendedor replacing all ' ' by '0'
               move W-cod-vendedor-n to S-cod-vendedor
               move W-cod-vendedor to  S1-cod-vendedor
               move  S-nombre  to S1-nombre
               move  S-nif  to    S1-nif
               move  S-direcion  to  S1-direcion
               move S-poblacion   to   S1-poblacion
               move  s-cod-postal  to   s1-cod-postal
               move s-telefono to  s1-telefono
               move s-estado  to  s1-estado
               move s-guapo    to   s1-guapo
               perform destripa-fechas
               WRITE REG-SALIDA from cabecera3 END-WRITE
               WRITE REG-SALIDA1 END-WRITE
               READ ENTRADA AT END MOVE 1 TO FE END-READ
           END-PERFORM
           CLOSE ENTRADA SALIDA
           STOP RUN.
       DESTRIPA-FECHAS.
               UNSTRING w-fecha-alta DELIMITED BY ' ' INTO
                 WDDMMAA WHHMMSS
               end-unstring
               UNSTRING WDDMMAA DELIMITED BY '/' INTO W-DD W-MM W-AA
               MOVE W-AA TO   S1-A-A
               MOVE W-MM TO   S1-M-A
               MOVE W-DD TO   S1-D-A
               move WFECHA TO  S-fecha-alta
               UNSTRING w-fecha-nacimiento DELIMITED BY ' ' INTO
               WDDMMAA WHHMMSS
               end-unstring
               UNSTRING WDDMMAA DELIMITED BY '/' INTO W-DD W-MM W-AA
               end-unstring
               MOVE W-AA TO   S1-A-N
               MOVE W-MM TO   S1-M-N
               MOVE W-DD TO   S1-D-N
               move WFECHA TO  S-fecha-nacimiento.
