       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRUEBA1.
       AUTHOR. J.GAYAN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT entrada ASSIGN TO 'NAT-ventas.txt'
           organization is line sequential.
           SELECT salida  ASSIGN TO 'NAT-salida'
           organization is line  sequential.
           SELECT salida1  ASSIGN TO 'NAT-ventas'
           organization is line  sequential.
       DATA DIVISION.
       FILE SECTION.
       FD  ENTRADA
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD IS STANDARD
           RECORDING MODE IS F
           RECORD 40 CHARACTERS
           DATA RECORD IS REG-ENTRADA.
       01 REG-ENTRADA   pic x(40).
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
           RECORD 20 CHARACTERS
           DATA RECORD IS REG-SALIDA1.
       01  REG-SALIDA1.
           03 S1-cod-vendedor   PIC x(4).
           03 S1-cod-producto  PIC xx.
           03 S1-fecha-venta.
                 05 S1-A    PIC X(4).
                 05 S1-M    PIC XX.
                 05 S1-D    PIC XX.
           03 S1-kilos       PIC x(6).

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
           03 W-cod-vendedor   PIC x(4) justified right.
           03 W-cod-vendedor-n redefines W-cod-vendedor pic 9(4).
           03 W-cod-producto   PIC x(2) justified right.
           03 W-cod-producto-n redefines   W-cod-producto  pic 99.
           03 W-kilos       PIC X(7).
       01  ton.
             05 kilos   PIC x(4) justified right.
             05 kilosd  pic xx.
       01 tun redefines ton pic 9(4)V99.
       01  CABECERA1.
           03 FILLER PIC X(12)   VALUE 'cod-vendedor'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(12)  VALUE 'cod-producto'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(23)   VALUE '  fecha-venta  '.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(7)   VALUE 'kilos  '.
       01  CABECERA2.
           03 FILLER PIC X(12)   VALUE ALL '-'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(12)  VALUE ALL '-'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(23)   VALUE ALL '-'.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 FILLER PIC X(7)   VALUE ALL '-'.
       01  CABECERA3.
           03 FILLER         PIC X(2) VALUE SPACES.
           03 S-cod-vendedor   PIC zzz9.
           03 FILLER         PIC X(10) VALUE SPACES.
           03 S-cod-producto   PIC z9.
           03 FILLER         PIC X(11) VALUE SPACES.
           03 S-fecha-venta      PIC X(10).
           03 FILLER         PIC X(2) VALUE SPACES.
           03 S-HORA-venta      PIC X(8).
           03 FILLER         PIC X(5) VALUE SPACES.
           03 S-kilos       PIC zzz9,99.
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
                 W-cod-vendedor W-cod-producto   WFECHOR W-kilos
               end-unstring
              UNSTRING WFECHOR DELIMITED BY ' ' INTO WDDMMAA WHHMMSS
               end-unstring
               UNSTRING WDDMMAA DELIMITED BY '/' INTO W-DD W-MM W-AA
               end-unstring
      *        DISPLAY WDDMMAA "    " WFECHA
               MOVE W-AA TO   S1-A
               MOVE W-MM TO   S1-M
               MOVE W-DD TO   S1-D
               UNSTRING WHHMMSS DELIMITED BY ':' INTO W-HH W-MI W-SS
               move WFECHA TO  S-fecha-venta
               move WHHMMSS TO  S-HORA-VENTA
               inspect W-cod-vendedor replacing all ' ' by '0'
               move W-cod-vendedor-n to S-cod-vendedor
               move W-cod-vendedor to  S1-cod-vendedor
               inspect W-cod-producto replacing all ' ' by '0'
               move W-cod-producto-n to S-cod-producto
               move W-cod-producto to S1-cod-producto
               unstring  W-kilos delimited by ',' or ' ' into kilos
                kilosd
               inspect ton replacing all ' ' by '0'
               move tun to s-kilos
               move ton to  S1-kilos
               WRITE REG-SALIDA from cabecera3 END-WRITE
               WRITE REG-SALIDA1 END-WRITE
               READ ENTRADA AT END MOVE 1 TO FE END-READ

           END-PERFORM
           CLOSE ENTRADA SALIDA
           STOP RUN.



