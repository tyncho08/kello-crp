## Software Design Document (SDD)

### 1. System Overview

- **Purpose and objectives**: The codebase contains multiple COBOL programs that implement Accounts Receivable operations and banking remittance/return processing, including:
  - Listing receivables within due date ranges and interactive settlement selection (CRP100).
  - Summary reporting for receivables/cheques per contract (CRP060).
  - Operational management of receivables (financial scheduling/"programação financeira") with printing (CRP022).
  - Bank remittance file generation and bank return report emission for several banks (e.g., Banco do Brasil, Bradesco, Banestes) across CRP910x/CRP911x series.
- **Detected technologies and versions**:
  - Micro Focus COBOL with Dialog System (DS) vocabulary and control blocks. DS Control Block explicitly states “For use with Dialog System Version 2.” The `dslang.cpy` file documents Early Release vocabulary usage and compile notes.
  - OO COBOL features via `class-control` (e.g., classes "alistview", "wclass") for GUI interaction.
  - Printer mapping via `SPECIAL-NAMES` `PRINTER IS LPRINTER`.
  - Decimal format specified via `SPECIAL-NAMES. DECIMAL-POINT IS COMMA` across many programs.
- **High-level architectural style/pattern**: Not specified in the source code.

Evidence List
- `evidence`: [
  {"file": "legacy/CRP100.cbl", "line_start": 6, "line_end": 9},
  {"file": "legacy/CRP060.cbl", "line_start": 5, "line_end": 9},
  {"file": "legacy/CRP022.cbl", "line_start": 6, "line_end": 6},
  {"file": "legacy/crp9102.CBL", "line_start": 4, "line_end": 5},
  {"file": "legacy/CRP9107.cbl", "line_start": 4, "line_end": 5},
  {"file": "legacy/crp9114.CBL", "line_start": 4, "line_end": 6},
  {"file": "legacy/DS-CNTRL.MF", "line_start": 4, "line_end": 8},
  {"file": "legacy/DSLANG.CPY", "line_start": 36, "line_end": 53},
  {"file": "legacy/CRP100.cbl", "line_start": 14, "line_end": 16},
  {"file": "legacy/CRP100.cbl", "line_start": 11, "line_end": 13}
]


### 2. Module Decomposition

- **Module**: CRP100
  - **Description**: "Recebimentos de títulos"; lists titles within a due date interval; interactive UI with listview; prints via printer mapping.
  - **Core responsibilities**:
    - Initializes DS control/data blocks; opens indexed files (CGD010/CGD011, CRD020/CRD020B, CAD018) and logs application open/close in `LOGACESS`.
    - Builds path roots using `"\\PROGRAMA\\KELLO"` and company identifiers.
    - Event-driven loop `PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE` invoking `DSRUN`.
    - Generates working file `WORK` and prints via `RELAT`/`CBPRINT`.
  - **Internal dependencies**: Copybooks `CGPX010`, `CGPX011`, `CRPX020`, `CRPX020B`, `CAPX018`, `LOGACESS.SEL/FD`, `CBPRINT.CPY`, `CBDATA.CPY`, `CPTIME.CPY`, `DS-CNTRL.MF`, `dslang.cpy`.
  - **External interactions**: Calls `DSRUN`, `GRIDAT1/GRIDAT2`, `GRTIME`, `MENSAGEM`, `EXTENSO`; writes to `LOGACESS`.
  - Evidence: `legacy/CRP100.cbl` lines 6-9, 10-16, 26-38, 41-72, 215-233, 268-286, 290-318, 319-412, 871-896, 897-924.

- **Module**: CRP060
  - **Description**: "Resumo de arrecadação de aReceber/Cheques"; lists totals per customer with paid/late/remaining breakdown; supports contract filtering and printing.
  - **Core responsibilities**:
    - Opens and reads CGD010, CRD020, CHD010, CHD013; aggregates amounts into a `WORK` file and prints to `RELAT`.
    - DS-driven interactive flow with `CALL "DSRUN"`.
  - **Dependencies**: Copybooks `CAPX001`, `CGPX010`, `CRPX020`, `CHPX010`, `CHPX013`, `LOGACESS.SEL/FD`, `CBDATA/CPTIME/CPDIAS1`, `CBPRINT.CPY`.
  - Evidence: `legacy/CRP060.cbl` lines 5-14, 17-29, 33-40, 174-183, 186-205, 216-236, 244-257, 613-653, 701-707, 708-736.

- **Module**: CRP022
  - **Description**: Movement management for Accounts Receivable (financial scheduling), with a comprehensive interactive UI, logging, and printing templates for scheduling slips.
  - **Core responsibilities**:
    - Initializes DS blocks, sets up many file paths, opens/creates indexed files (CRD020/021/022/024, LOG001/2/3, CAD/CGD/CXD, CRD099/200/201) and logs open/close.
    - Event loop with many user-driven actions (save, cancel, re-open, revert, list box population, printing) controlled through DS flags/opcodes and `DSRUN`.
    - Printing of programações (financial schedules) and on-screen print.
  - **Dependencies**: Numerous `COPY` includes, DS control block, date/time utilities.
  - Evidence: `legacy/CRP022.cbl` lines 6, 12-14, 15-36, 38-58, 63-71, 374-419, 510-540, 541-666, 669-676, 756-767, 865-887, 921-937, 951-966, 1091-1113, 1751-1755, 1912-2009, 2010-2036.

- **Module**: CRP9102/CRP9105/CRP9106/CRP9107/CRP9109 (Remittance generators)
  - **Description**: Multiple programs to generate bank remittance files (Banco do Brasil, Bradesco) with similar structure.
  - **Core responsibilities**:
    - SELECTs for remittance files `REMESSA/REMESSA2`, sequence files `SEQBRAS/SEQBRAD`, work/printer files; event-driven DS loop.
  - **Dependencies**: Various `COPY` includes; DS control; logging; date utilities.
  - Evidence: `legacy/crp9102.CBL` lines 4-5, 25-42, 280-282; `legacy/CRP9107.cbl` lines 4-5, 25-50.

- **Module**: CRP9114/CRP9115/CRP9116/CRP9117/CRP9118/CRP9119 (Return report emitters)
  - **Description**: Programs to read bank return files (`RETORNO`) and emit reports, optionally generating a `PROBLEMA` file.
  - **Core responsibilities**:
    - SELECTs for `RETORNO`, `PROBLEMA`, `RELAT`; DS event loop; prints via printer.
  - Evidence: `legacy/crp9114.CBL` lines 4-6, 40-48, 278-280; `legacy/CRP9119.cbl` lines 6, 27-35, 267-269.

Evidence List
- `evidence`: [
  {"file": "legacy/CRP100.cbl", "line_start": 6, "line_end": 16},
  {"file": "legacy/CRP100.cbl", "line_start": 26, "line_end": 38},
  {"file": "legacy/CRP100.cbl", "line_start": 215, "line_end": 233},
  {"file": "legacy/CRP100.cbl", "line_start": 268, "line_end": 286},
  {"file": "legacy/CRP100.cbl", "line_start": 298, "line_end": 318},
  {"file": "legacy/CRP060.cbl", "line_start": 5, "line_end": 14},
  {"file": "legacy/CRP060.cbl", "line_start": 186, "line_end": 205},
  {"file": "legacy/CRP022.cbl", "line_start": 6, "line_end": 6},
  {"file": "legacy/CRP022.cbl", "line_start": 374, "line_end": 419},
  {"file": "legacy/crp9102.CBL", "line_start": 25, "line_end": 42},
  {"file": "legacy/CRP9107.cbl", "line_start": 25, "line_end": 50},
  {"file": "legacy/crp9114.CBL", "line_start": 40, "line_end": 48},
  {"file": "legacy/CRP9119.cbl", "line_start": 27, "line_end": 35}
]


### 3. Data Structures

- **Dialog System Control Block (DS-CONTROL-BLOCK)**
  - Structure with version numbers, system error fields, window/object names, control flags, and procedure names for DS runtime interaction.
  - Evidence: `legacy/DS-CNTRL.MF` lines 10-31, 60-88.

- **Dialog Language external programs (GUI vocabulary)**
  - A large set of EXTERNAL PROGRAM-IDs mapping verbs like `SHOW-WINDOW`, `SET-FOCUS`, `REFRESH-OBJECT`, `MOVE-OBJECT-HANDLE`, etc., to specific LINKAGE parameters.
  - Evidence: `legacy/DSLANG.CPY` lines 65-75, 97-105, 281-286, 536-546, 549-558.

- **Runtime parameter block**
  - `PARAMETROS-W` includes user, company, printer code, user code, password, and company name.
  - Evidence: `legacy/PARAMETR.CPY` lines 1-8.

- **System date/time block**
  - `DATA-SISTEMA` provides fields like `DATA6-W`, `DATA-INV`, `HORA-BRA`.
  - Evidence: `legacy/CBDATA.cpy` lines 1-7.

- **Indexed file schemas (examples)**
  - CRD040 (from `CRPX040`/`CRPW040`): fields include due date, bank, duplicate number, name, sequence, amount, etc.
    - Evidence (SELECT): `legacy/CRPX040.cbl` lines 1-10.
    - Evidence (FD): `legacy/CRPW040.cbl` lines 1-14.
  - Program-local `WORK` record examples:
    - CRP100 `WORK` record: classification, client, seq, name, our-number, carrier, due date, amount, type.
      - Evidence: `legacy/CRP100.cbl` lines 49-60.
    - CRP060 `WORK` record: per-customer totals and counts for paid/late/remaining.
      - Evidence: `legacy/CRP060.cbl` lines 40-53.

- **Printer record**
  - Programs define `FD RELAT` with a 130-char record used for line printing; printer defined via `SPECIAL-NAMES`.
  - Evidence: `legacy/CRP100.cbl` lines 60-63; `legacy/CRP060.cbl` lines 53-57; `legacy/CRP100.cbl` lines 11-13.

Evidence List
- `evidence`: [
  {"file": "legacy/DS-CNTRL.MF", "line_start": 10, "line_end": 31},
  {"file": "legacy/DS-CNTRL.MF", "line_start": 60, "line_end": 88},
  {"file": "legacy/DSLANG.CPY", "line_start": 65, "line_end": 75},
  {"file": "legacy/DSLANG.CPY", "line_start": 536, "line_end": 546},
  {"file": "legacy/PARAMETR.CPY", "line_start": 1, "line_end": 8},
  {"file": "legacy/CBDATA.cpy", "line_start": 1, "line_end": 7},
  {"file": "legacy/CRPX040.cbl", "line_start": 1, "line_end": 10},
  {"file": "legacy/CRPW040.cbl", "line_start": 1, "line_end": 14},
  {"file": "legacy/CRP100.cbl", "line_start": 49, "line_end": 60},
  {"file": "legacy/CRP060.cbl", "line_start": 40, "line_end": 53},
  {"file": "legacy/CRP100.cbl", "line_start": 60, "line_end": 63}
]


### 4. APIs and Interfaces

- **External interfaces/endpoints**: Not specified in the source code (no network or web APIs). Programs interface with the OS printer via `PRINTER IS LPRINTER` and file system via indexed files.
- **Internal service interfaces or APIs (program calls)**:
  - Dialog System runtime: `CALL "DSRUN"` with `DS-CONTROL-BLOCK` and `GS-DATA-BLOCK` to render UI and process events.
  - Date/time utilities: `GRIDAT1`, `GRIDAT2`, `GRTIME`.
  - Messaging/formatting: `MENSAGEM`, `EXTENSO`.
  - UI and printing helpers: `IMPRESSORA.CHAMA` (calls `CAP999T`).
- **Third-party system integrations**:
  - Micro Focus Dialog System GUI vocabulary and control blocks.
  - Banking file formats implied by remittance/return programs (`RETORNO`, `PROBLEMA`, `REMESSA`, etc.).

Evidence List
- `evidence`: [
  {"file": "legacy/CRP100.cbl", "line_start": 889, "line_end": 896},
  {"file": "legacy/CRP022.cbl", "line_start": 2003, "line_end": 2009},
  {"file": "legacy/CRP100.cbl", "line_start": 219, "line_end": 227},
  {"file": "legacy/CRP022.cbl", "line_start": 851, "line_end": 860},
  {"file": "legacy/IMPRESSORA.chama", "line_start": 1, "line_end": 1},
  {"file": "legacy/CRP9119.cbl", "line_start": 27, "line_end": 35},
  {"file": "legacy/CRP9117.cbl", "line_start": 54, "line_end": 68}
]


### 5. Workflows and Business Logic

- **Common program lifecycle pattern**:
  - `MAIN-PROCESS`: `PERFORM INICIALIZA-PROGRAMA` → `PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE` → `GO FINALIZAR-PROGRAMA`; within each loop cycle, `CALL "DSRUN"` processes UI events.
  - Evidence: `legacy/CRP100.cbl` lines 210-214, 889-896; `legacy/CRP022.cbl` lines 369-373, 2003-2009.

- **CRP100 critical process (settlement selection and work generation)**:
  - Initializes file paths and opens files; logs application open in `LOGACESS`.
  - Builds `WORK` via `GRAVA-WORK`, iterating CRD020 between date range, writes records, updates UI via DS procedures, and computes totals; handles interactive selection via listview methods; prints via `RELAT`.
  - Evidence: `legacy/CRP100.cbl` lines 227-236, 268-286, 653-680, 793-871, 883-896.

- **CRP060 critical process (contract summary)**:
  - Aggregates receivable and cheque data into `WORK`, computing totals across paid/late/remaining, then prints summary.
  - Evidence: `legacy/CRP060.cbl` lines 278-306, 360-423, 498-529, 613-653.

- **CRP022 critical processes (save/rewrite/cancel/revert/print)**:
  - DS-driven EVALUATE dispatch handling many operations (`SAVE`, `CANCELA`, `REVERTE-...`, `IMPRIME-RELATORIO`, etc.).
  - Writes audit logs to LOG001/LOG002/LOG003 on insert/update/delete; manages program number in CRD024; prints detailed scheduling slip.
  - Evidence: `legacy/CRP022.cbl` lines 541-666, 865-887, 905-919, 1295-1318, 1391-1429, 1756-1795.

- **Bank return reporters (CRP911x)**:
  - Read `RETORNO`, write `PROBLEMA`, and emit `RELAT` depending on return codes; DS event loop and printer usage.
  - Evidence: `legacy/CRP9117.cbl` lines 54-68, 302-304, 386-402; `legacy/CRP9115.cbl` lines 27-35, 264-266.

- **Example high-level data flow diagram (inferred from file operations and DS runtime usage)**:

```mermaid
flowchart TD
  User[User] --> DSUI[Dialog System UI (DSRUN)]
  DSUI --> Programs{COBOL Programs}
  Programs --> Files[(Indexed Files: CRD020/021/022/024, CAD/CGD/CXD, CHD010/013, WORK, RELAT)]
  Programs --> Logs[(LOGACESS, LOG001/LOG002/LOG003)]
  Programs --> Printer[Printer (LPRINTER)]
  Programs --> BankFiles[(RETORNO/PROBLEMA, REMESSA/REMESSA2)]
```

Evidence List
- `evidence`: [
  {"file": "legacy/CRP100.cbl", "line_start": 210, "line_end": 214},
  {"file": "legacy/CRP100.cbl", "line_start": 889, "line_end": 896},
  {"file": "legacy/CRP060.cbl", "line_start": 613, "line_end": 653},
  {"file": "legacy/CRP022.cbl", "line_start": 541, "line_end": 566},
  {"file": "legacy/CRP9117.cbl", "line_start": 54, "line_end": 68}
]


### 6. Configuration and Environment

- **Configuration files/parameters**:
  - Path root concatenation uses hardcoded `"\\PROGRAMA\\KELLO"` combined with company (`EMP-REC`) and requested file name into path variables (e.g., `PATH-CGD010`).
  - Programs include `PARAMETR.CPY` defining `PARAMETROS-W` (user, company, printer, etc.) used during initialization.
  - Printer selection and line-conditioning via `IMPRESSORA`/`CBPRINT.CPY` copybooks and `PRINTER IS LPRINTER`.
- **OS, runtime, libraries, platform**:
  - Dialog System Version 2 per control block; DSLANG Early Release vocabulary; compile note present in `dslang.cpy`.
- **Build, deployment, runtime scripts**: Not specified in the source code.

Evidence List
- `evidence`: [
  {"file": "legacy/CRP100.cbl", "line_start": 87, "line_end": 93},
  {"file": "legacy/CRP100.cbl", "line_start": 227, "line_end": 236},
  {"file": "legacy/PARAMETR.CPY", "line_start": 1, "line_end": 8},
  {"file": "legacy/CRP100.cbl", "line_start": 65, "line_end": 72},
  {"file": "legacy/CRP100.cbl", "line_start": 11, "line_end": 13},
  {"file": "legacy/DS-CNTRL.MF", "line_start": 4, "line_end": 8},
  {"file": "legacy/DSLANG.CPY", "line_start": 59, "line_end": 61}
]


### 7. Non-Functional Requirements (NFRs)

- **Performance requirements/constraints**: Not specified in the source code.
- **Security mechanisms/controls**: Not specified in the source code.
- **Logging/monitoring/alerting**:
  - Programs open `LOGACESS` at start and end, writing user, timestamp, program, and status; some operations also write structured audit logs to `LOG001/LOG002/LOG003`.

Evidence List
- `evidence`: [
  {"file": "legacy/CRP100.cbl", "line_start": 268, "line_end": 286},
  {"file": "legacy/CRP100.cbl", "line_start": 897, "line_end": 919},
  {"file": "legacy/CRP022.cbl", "line_start": 875, "line_end": 887},
  {"file": "legacy/CRP022.cbl", "line_start": 1324, "line_end": 1346}
]


### 8. Risks and Limitations

- **Known issues/technical debt (from comments)**:
  - A comment indicates logic added specifically to remove “trash” records from file CRD020 during recent list generation, suggesting defensive cleanup code.
- **Complexity hotspots**: Not specified in the source code.

Evidence List
- `evidence`: [
  {"file": "legacy/CRP022.cbl", "line_start": 1703, "line_end": 1717}
]


### 9. Test Coverage

- **Existing unit/integration/system tests**: Not specified in the source code.
- **Areas lacking tests**: Not specified in the source code.

Evidence List
- `evidence`: []


### 10. Glossary

- **Contas a Receber (Accounts Receivable)**: Domain of core programs and reports.
  - Evidence: `legacy/CRP060.cbl` lines 5-9; `legacy/CRP022.cbl` lines 165-167.
- **Remessa (Remittance file)**: Outbound bank file generation.
  - Evidence: `legacy/crp9102.CBL` lines 4-5; `legacy/CRP9107.cbl` lines 4-5.
- **Retorno (Return file)**: Inbound bank file for processing.
  - Evidence: `legacy/crp9114.CBL` lines 4-6; `legacy/CRP9119.cbl` lines 6, 27-35.
- **Portador (Carrier/Portfolio holder)**: Field indicating carrier/portfolio, used in receivable records and UI.
  - Evidence: `legacy/CRP100.cbl` lines 55-59, 783-791; `legacy/CRP022.cbl` lines 1054-1063.
- **Carteira (Wallet/Portfolio)**: Receivables portfolio type.
  - Evidence: `legacy/CRP022.cbl` lines 1059-1063.
- **Apuração (Apportionment/Accounting code)**: Accounting code selection and description.
  - Evidence: `legacy/CRP022.cbl` lines 705-713, 1866-1872.
- **Programação Financeira (Financial scheduling)**: Term used in print layout for schedules.
  - Evidence: `legacy/CRP022.cbl` lines 254-261.

Evidence List
- `evidence`: [
  {"file": "legacy/CRP060.cbl", "line_start": 5, "line_end": 9},
  {"file": "legacy/CRP022.cbl", "line_start": 165, "line_end": 167},
  {"file": "legacy/crp9102.CBL", "line_start": 4, "line_end": 5},
  {"file": "legacy/CRP9107.cbl", "line_start": 4, "line_end": 5},
  {"file": "legacy/crp9114.CBL", "line_start": 4, "line_end": 6},
  {"file": "legacy/CRP9119.cbl", "line_start": 27, "line_end": 35},
  {"file": "legacy/CRP100.cbl", "line_start": 783, "line_end": 791},
  {"file": "legacy/CRP022.cbl", "line_start": 1059, "line_end": 1063},
  {"file": "legacy/CRP022.cbl", "line_start": 705, "line_end": 713},
  {"file": "legacy/CRP022.cbl", "line_start": 254, "line_end": 261}
]
