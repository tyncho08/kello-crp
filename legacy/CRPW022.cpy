      *  Arquivo que armazena observa��es referente a lan�amentos no
      *  contas a pagar
       FD  CRD022.
       01  REG-CRD022.
           05 COD-COMPL-CR22.
              10 CLASS-CLIENTE-CR22           PIC 9.
      *    classifica��o cliente =  0-contrato  1-comum
              10 CLIENTE-CR22                 PIC 9(8).
      *    quando a classifica��o for = 0 - o c�digo do cliente ser�
      *    o nr-contrato+album e = 1(comum) ser� uma sequ�ncia de c�digo
           05 SEQ-CR22                        PIC 9(5).
           05 OBS-CR22                        PIC X(120).
