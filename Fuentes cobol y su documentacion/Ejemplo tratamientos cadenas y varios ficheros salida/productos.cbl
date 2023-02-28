       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRODUCTOS.
       AUTHOR. J.GAYAN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT entrada ASSIGN TO 'NAT-productos.txt'
           organization is line sequential.
           SELECT salida  ASSIGN TO 'NAT-salida'
           organization is line  sequential.
           SELECT salida1  ASSIGN TO 'NAT-productos'
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
           RECORD 21 CHARACTERS
           DATA RECORD IS REG-SALIDA1.
       01  REG-SALIDA1.
           03 S1-idproducto   PIC X(4).
           03 S1-NomProducto  PIC X(10).
           03 S1-IdGrupo      PIC X.
           03 S1-precio       PIC X(6).

       WORKING-STORAGE SECTION.
       01    productos1.
          03 id-producto1   pic 9(4).
          03 Nom-producto1  pic x(50).
          03 id-grupo1      pic 9.
          03 precio1       pic 9(6)v99.
       01  WFECHA.
           03 W-DD  PIC XX.
           03 W-MM  PIC XX.
           03 W-AA  PIC X(4).
       01  FILLER REDEFINES WFECHA.
           03 D-AA  PIC XX.
           03 D-MM  PIC XX.
           03 D-DD  PIC XX.
       01  DESTRIPAR.
           03 W-idproducto   PIC x(2) justified right.
           03 W-idproducto-n redefines   W-idproducto  pic 99.
           03 W-precio  PIC X(7).
       01  wprecio-a.
           03 wprecio-e   pic x(4) justified right.
           03 wprecio-d   pic x(2).
       01  wprecio-n redefines wprecio-a pic 9(4)V99.
       01  CABECERA1.
           03 FILLER PIC X(12)   VALUE 'IdProducto  '.
           03 FILLER         PIC X VALUE SPACES.
           03 FILLER PIC X(16)  VALUE 'NomProducto  '.
           03 FILLER         PIC X VALUE SPACES.
           03 FILLER PIC X(10)   VALUE 'IdGrupo   '.
           03 FILLER         PIC X VALUE SPACES.
           03 FILLER PIC X(10)   VALUE '  Precio  '.
       01  CABECERA2.
           03 FILLER PIC X(12)   VALUE ALL '-'.
           03 FILLER         PIC X VALUE SPACES.
           03 FILLER PIC X(16)  VALUE ALL '-'.
           03 FILLER         PIC X VALUE SPACES.
           03 FILLER PIC X(10)   VALUE ALL '-'.
           03 FILLER         PIC X VALUE SPACES.
           03 FILLER PIC X(8)   VALUE ALL '-'.
       01  CABECERA3.
           03 FILLER         PIC X(6) VALUE SPACES.
           03 S-idproducto   PIC z9.
           03 FILLER         PIC X(6) VALUE SPACES.
           03 S-NomProducto  PIC X(10).
           03 FILLER         PIC X(10) VALUE SPACES.
           03 S-IdGrupo      PIC X.
           03 FILLER         PIC X(6) VALUE SPACES.
           03 S-precio       PIC zzz9,99.
           03 FILLER         PIC X(6) VALUE SPACES.
       77  FE       PIC 9    VALUE 0.



       PROCEDURE DIVISION.
       ABRIR.
           OPEN input ENTRADA
           OPEN OUTPUT SALIDA salida1.
           WRITE REG-SALIDA from cabecera1 END-WRITE
           WRITE REG-SALIDA from cabecera2 END-WRITE
           READ ENTRADA AT END MOVE 1 TO FE END-READ
           PERFORM  UNTIL FE = 1
               unstring reg-entrada delimited by ';' into
                 w-idproducto S-NomProducto   S-IdGrupo  w-precio
               unstring w-precio delimited by ',' into
               wprecio-e wprecio-d
               inspect wprecio-a replacing all ' ' by '0'
               inspect W-idproducto replacing all ' ' by '0'
               move W-idproducto   to S1-idproducto
               move W-idproducto-n   to S-idproducto
               move S-NomProducto to S1-NomProducto
               move S-IdGrupo to S1-IdGrupo
               move wprecio-a to S1-precio
               move wprecio-n to S-precio
               WRITE REG-SALIDA from cabecera3 END-WRITE
               WRITE REG-SALIDA1 END-WRITE
               READ ENTRADA AT END MOVE 1 TO FE END-READ

           END-PERFORM
           CLOSE ENTRADA SALIDA
           STOP RUN.



