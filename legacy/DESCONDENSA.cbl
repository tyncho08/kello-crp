           MOVE SPACES TO REG-RELAT
           IF LNK-TIPO = 1
              WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
           ELSE
              WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE
           END-IF

           CLOSE RELAT.
