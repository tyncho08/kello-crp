       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP001.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 07/04/1999
      *DESCRI��O: Cadastro de SITUA��O DE T�TULOS DENTRO DO CTAS A
      *           RECEBER.

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CRPX001.
           COPY LOGX001.
           COPY LOGACESS.SEL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CRPW001.
       COPY LOGW001.
       COPY LOGACESS.FD.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP001.CPB".
           COPY "CRP001.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CRD001             PIC XX      VALUE SPACES.
           05  ST-LOG001             PIC XX      VALUE SPACES.
           05  FS-LOGACESS           PIC XX      VALUE SPACES.
           05  ULT-CODIGO            PIC 9(2)     VALUE ZEROS.
      *    Ult-codigo - ser� utilizado p/ encontrar o �ltimo c�digo
      *    de tipo-lancamento utilizado
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - num�rico
      *    ou alfab�tico
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(63)   VALUE
           "RELACAO - CADASTRO DE SITUACAO DE T�TULO NO CTA.REC".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "COD      SITUACAO       DESCRICAO".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU                 PIC 9(04).
             10 WS-MES-CPU                 PIC 9(02).
             10 WS-DIA-CPU                 PIC 9(02).
          05 FILLER                        PIC X(13).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).


       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
       PROCEDURE DIVISION USING POP-UP.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CRD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD001.
           MOVE "LOG001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG001.
           MOVE "LOGACESS" TO ARQ-REC MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           OPEN I-O CRD001 LOG001
           MOVE 1 TO GRAVA-W.
           IF ST-CRD001 = "35"
              CLOSE CRD001      OPEN OUTPUT CRD001
              CLOSE CRD001      OPEN I-O CRD001
           END-IF.
           IF ST-CRD001 <> "00"
              MOVE "ERRO ABERTURA CRD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG001 <> "00"
              MOVE "ERRO ABERTURA LOG001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP001"            to logacess-programa
           move "ABERTO"            to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess

           IF ERRO-W = ZEROS
                MOVE 1 TO GS-ORDER
                PERFORM ACHAR-CODIGO
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   PERFORM INCREMENTA-CODIGO
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
                   PERFORM ACHAR-CODIGO
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
                    PERFORM MOSTRA-ULT-CODIGO
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 2) TO GS-CODIGO
                   PERFORM CARREGAR-DADOS
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.
       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGAR-DADOS SECTION.
           MOVE ZEROS TO GRAVA-W.
           MOVE GS-CODIGO       TO CODIGO-CR01
           READ CRD001 INVALID KEY INITIALIZE REG-CRD001
                                   MOVE 1 TO GRAVA-W.
           MOVE SITUACAO-TIT-CR01  TO GS-SITUACAO.
           MOVE DESCRICAO-CR01     TO GS-DESCRICAO.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CRD001
           MOVE GS-ORDER TO ORDEM-W
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W TO GS-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE CRD001 NOT INVALID KEY
                  MOVE USUARIO-W   TO LOG1-USUARIO
                  MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                  MOVE WS-DATA-CPU TO LOG1-DATA
                  ACCEPT WS-HORA-SYS FROM TIME
                  MOVE WS-HORA-SYS TO LOG1-HORAS
                  MOVE "E"         TO LOG1-OPERACAO
                  MOVE "CRD001"    TO LOG1-ARQUIVO
                  MOVE "CRP001"    TO LOG1-PROGRAMA
                  MOVE REG-CRD001  TO LOG1-REGISTRO
                  WRITE REG-LOG001
                  END-WRITE.

           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           MOVE GS-CODIGO       TO CODIGO-CR01
           MOVE GS-SITUACAO     TO SITUACAO-TIT-CR01.
           MOVE GS-DESCRICAO    TO DESCRICAO-CR01.
           IF GRAVA-W = 1
              WRITE REG-CRD001 INVALID KEY
                   PERFORM ERRO-GRAVACAO
              NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG1-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG1-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG1-HORAS
                   MOVE "I"         TO LOG1-OPERACAO
                   MOVE "CRD001"    TO LOG1-ARQUIVO
                   MOVE "CRP001"    TO LOG1-PROGRAMA
                   MOVE REG-CRD001  TO LOG1-REGISTRO
                   WRITE REG-LOG001
                   END-WRITE
              END-WRITE
           ELSE
              REWRITE REG-CRD001 INVALID KEY
                   PERFORM ERRO-GRAVACAO
              NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG1-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG1-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG1-HORAS
                   MOVE "A"         TO LOG1-OPERACAO
                   MOVE "CRD001"    TO LOG1-ARQUIVO
                   MOVE "CRP001"    TO LOG1-PROGRAMA
                   MOVE REG-CRD001  TO LOG1-REGISTRO
                   WRITE REG-LOG001
                   END-WRITE
                   SUBTRACT 1 FROM ULT-CODIGO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVA��O" TO GS-MENSAGEM-ERRO
           MOVE ST-CRD001       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO
           PERFORM ACHAR-CODIGO
           SUBTRACT 1 FROM ULT-CODIGO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO CODIGO-CR01.
           START CRD001 KEY IS NOT < CODIGO-CR01
                 INVALID KEY MOVE "10" TO ST-CRD001.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-CRD001 = "10"
              READ CRD001 NEXT RECORD AT END MOVE "10" TO ST-CRD001
              NOT AT END
                ADD 1 TO GS-CONT
                MOVE SPACES TO GS-LINDET
                MOVE CODIGO-CR01       TO GS-LINDET(01: 03)
                MOVE SITUACAO-TIT-CR01 TO GS-LINDET(10: 15)
                MOVE DESCRICAO-CR01    TO GS-LINDET(25: 30)
                MOVE "INSERE-LIST"     TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP001" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO CODIGO-CR01.
           START CRD001 KEY IS NOT < CODIGO-CR01 INVALID KEY
                        MOVE "10" TO ST-CRD001
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-CRD001 = "10"
             READ CRD001 NEXT RECORD AT END MOVE "10" TO ST-CRD001
              NOT AT END
               MOVE SPACES TO LINDET-REL
                MOVE CODIGO-CR01           TO LINDET-REL(01: 03)
                MOVE SITUACAO-TIT-CR01     TO LINDET-REL(10: 15)
                MOVE DESCRICAO-CR01        TO LINDET-REL(25: 30)
                WRITE REG-RELAT FROM LINDET
                ADD 1 TO LIN
                IF LIN > 56 PERFORM CABECALHO
                END-IF
             END-READ
           END-PERFORM.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
       ACHAR-CODIGO SECTION.
           MOVE ZEROS TO CODIGO-CR01 ULT-CODIGO
           START CRD001 KEY IS NOT < CODIGO-CR01 INVALID KEY
                 MOVE "10" TO ST-CRD001
           END-START
           PERFORM UNTIL ST-CRD001 = "10"
              READ CRD001 NEXT RECORD AT END MOVE "10" TO ST-CRD001
                NOT AT END
                 MOVE CODIGO-CR01 TO ULT-CODIGO
              END-READ
           END-PERFORM.
           PERFORM INCREMENTA-CODIGO.
       INCREMENTA-CODIGO SECTION.
           ADD 1 TO ULT-CODIGO.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO GS-CODIGO.
       MOSTRA-ULT-CODIGO SECTION.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO GS-CODIGO
           MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP001"            to logacess-programa
           move "FECHADO"           to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess

           CLOSE CRD001 LOG001
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
