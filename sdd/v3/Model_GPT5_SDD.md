### 1. System Overview

- **Purpose**: Evidence in program headers and comments indicates a legacy Accounts Receivable (A/R) and banking integration suite that:
  - Generates bank remittance files (e.g., Banco do Brasil, Bradesco) and prints summary reports. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 5, "line_end": 5}] [evidence: {"file": "legacy/crp9100.cbl", "line_start": 4, "line_end": 7}]
  - Processes bank return files and updates A/R accordingly, producing reports and exception files. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 5, "line_end": 12}]
  - Provides interactive UI utilities via Dialog System (`DSRUN`) and printer outputs. [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 130, "line_end": 136}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 271, "line_end": 279}]

- **Technologies**: COBOL (Micro Focus style), `SPECIAL-NAMES` with decimal comma and logical printer mapping, pervasive `COPY` books for layouts and DS integration. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 9, "line_end": 13}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 9, "line_end": 13}]

- **Architecture**: Batch/report-style COBOL programs with indexed and sequential files, some interactive flows using Dialog System through a common `CALL-DIALOG-SYSTEM` section calling `DSRUN`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 1214, "line_end": 1217}] [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 130, "line_end": 136}]

- **I/O Overview**: Extensive use of `SELECT`/`FD` for:
  - Indexed control/transaction files (e.g., `SEQBRAS`, `CRD020`, `WORK` families) and line-sequential text files (e.g., `RETORNO`, `REMESSA`, `PROBLEMA`), plus printer `RELAT`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 25, "line_end": 42}] [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 27, "line_end": 37}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 22, "line_end": 39}]

---

### 2. Module Decomposition

Key programs, responsibilities, and dependencies grounded in source code.

#### 2.1 Program: CRP9100 (Bradesco Remittance)
- **Name**: `CRP9100` [evidence: {"file": "legacy/crp9100.cbl", "line_start": 2, "line_end": 2}]
- **Description**: Generates `CBDDMMxx.REM` remittance for Bradesco; alters A/R “portador” during generation. [evidence: {"file": "legacy/crp9100.cbl", "line_start": 4, "line_end": 7}]
- **Core Responsibilities**:
  - Defines and writes `REMESSA` (line sequential). [evidence: {"file": "legacy/crp9100.cbl", "line_start": 49, "line_end": 54}]
  - Uses indexed `SEQREC` for remittance sequencing. [evidence: {"file": "legacy/crp9100.cbl", "line_start": 21, "line_end": 27}] [evidence: {"file": "legacy/crp9100.cbl", "line_start": 55, "line_end": 59}]
  - Produces printer output `RELAT`. [evidence: {"file": "legacy/crp9100.cbl", "line_start": 38, "line_end": 38}] [evidence: {"file": "legacy/crp9100.cbl", "line_start": 616, "line_end": 646}]
- **Dependencies**:
  - Layout copybooks: `CAPX001`, `CAPX018`, `CRPX020`, `CGPX010`, `CGPX011`, `CRPX200`, `CRPX201`. [evidence: {"file": "legacy/crp9100.cbl", "line_start": 14, "line_end": 20}]
  - File sections and print/UI: `CAPW001`, `CAPW018`, `CRPW020`, `CGPW010`, `CGPW011`, `CRPW200`, `CRPW201`, `DS-CNTRL.MF`, `CBPRINT.CPY`. [evidence: {"file": "legacy/crp9100.cbl", "line_start": 42, "line_end": 48}] [evidence: {"file": "legacy/crp9100.cbl", "line_start": 73, "line_end": 77}]

#### 2.2 Program: CRP9102 (Banco do Brasil Remittance)
- **Name**: `CRP9102` [evidence: {"file": "legacy/crp9102.CBL", "line_start": 3, "line_end": 3}]
- **Description**: Generates remittance file for Banco do Brasil. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 5, "line_end": 5}]
- **Core Responsibilities**:
  - Indexed control `SEQBRAS` with key `CONT-SEQUENCIA`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 25, "line_end": 29}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 56, "line_end": 60}]
  - Outputs `REMESSA` and `REMESSA2` records. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 30, "line_end": 35}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 60, "line_end": 69}]
  - Uses indexed `WORK` with alternate key `NOME-WK`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 36, "line_end": 41}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 71, "line_end": 81}]
  - UI loop via `CALL-DIALOG-SYSTEM` calling `DSRUN`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 1214, "line_end": 1217}]
  - Calls helpers (`GRIDAT1/2`, `CAP018T`, `UTI0080`). [evidence: {"file": "legacy/crp9102.CBL", "line_start": 494, "line_end": 505}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 839, "line_end": 840}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 439, "line_end": 444}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 918, "line_end": 919}]
- **Dependencies**:
  - Input-output and logging copybooks: `CAPX010`, `CAPX018`, `CRPX020`, `CGPX010`, `CGPX011`, `CGPX014`, `CRPX200`, `CRPX201`, `RETPORT.SEL`, `LOGACESS.SEL`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 15, "line_end": 24}]
  - File sections and print/UI: `CAPW010`, `CAPW018`, `CRPW020`, `CGPW010`, `CGPW011`, `CGPW014`, `CRPW200`, `CRPW201`, `RETPORT.FD`, `LOGACESS.FD`, `DS-CNTRL.MF`, `CBPRINT.CPY`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 46, "line_end": 55}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 86, "line_end": 90}]

#### 2.3 Program: CRP9117 (Bank Return Processing/Report)
- **Name**: `CRP9117` [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 3, "line_end": 3}]
- **Description**: Emits return report; actions driven by occurrence codes (02/03/06). [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 5, "line_end": 12}]
- **Core Responsibilities**:
  - Reads `RETORNO` (line sequential), writes `PROBLEMA`, prints `RELAT`, maintains indexed `REJEICOES`. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 27, "line_end": 45}] [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 53, "line_end": 72}]
  - UI dialog loop and printing via DS and copybooks. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 391, "line_end": 399}] [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 1787, "line_end": 1793}]
- **Dependencies**: `CRPX020`, `PARX002`, `CRPX020B`, `CGPX010`, `LOGACESS.*`, `CBPRINT.CPY`, `DS-CNTRL.MF`. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 23, "line_end": 33}] [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 49, "line_end": 54}] [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 74, "line_end": 80}]

#### 2.4 Program: CRP020 (A/R Movement)
- **Name**: `CRP020` [evidence: {"file": "legacy/crp020.cbl", "line_start": 3, "line_end": 3}]
- **Description**: Accounts receivable movement; note that write‑offs occur via bank return. [evidence: {"file": "legacy/crp020.cbl", "line_start": 6, "line_end": 10}]
- **Core Responsibilities**:
  - Interacts with multiple A/R files and logs; prints detailed reports; integrates with Dialog System for interactive features. [evidence: {"file": "legacy/crp020.cbl", "line_start": 36, "line_end": 60}] [evidence: {"file": "legacy/crp020.cbl", "line_start": 1442, "line_end": 1511}]
- **Dependencies**: Numerous copybooks for customer, A/R, cash, and logging layouts (`CAPX*`, `CGPX*`, `CRPX*`, `CXPX020`, `LOGX*`) with corresponding `*W*` file sections and `LOGACESS.*`. [evidence: {"file": "legacy/crp020.cbl", "line_start": 20, "line_end": 36}] [evidence: {"file": "legacy/crp020.cbl", "line_start": 40, "line_end": 56}]

#### 2.5 Program: CRP022 (A/R Movement – Financial Scheduling)
- **Name**: `CRP022` [evidence: {"file": "legacy/CRP022.cbl", "line_start": 3, "line_end": 3}]
- **Description**: Accounts receivable movement (financial programming/scheduling). [evidence: {"file": "legacy/CRP022.cbl", "line_start": 6, "line_end": 7}]
- **Core Responsibilities**: Similar to `CRP020`, with additional scheduling artifacts and forms; prints reports. [evidence: {"file": "legacy/CRP022.cbl", "line_start": 59, "line_end": 72}] [evidence: {"file": "legacy/CRP022.cbl", "line_start": 1756, "line_end": 1795}]
- **Dependencies**: Expanded set of `COPY` members including `CRPX022`, `CRPX024`, and bank account master (`CAD030`), with matching `*W*` sections. [evidence: {"file": "legacy/CRP022.cbl", "line_start": 17, "line_end": 36}] [evidence: {"file": "legacy/CRP022.cbl", "line_start": 40, "line_end": 58}]

#### 2.6 Program: CRP063 (Deflated Titles Report)
- **Name**: `CRP063` [evidence: {"file": "legacy/crp063.CBL", "line_start": 3, "line_end": 3}]
- **Description**: Report of deflated titles. [evidence: {"file": "legacy/crp063.CBL", "line_start": 6, "line_end": 7}]
- **Core Responsibilities**:
  - Aggregates metrics into indexed `WORK`/`WORK1`; prints summaries and per‑bucket details. [evidence: {"file": "legacy/crp063.CBL", "line_start": 22, "line_end": 39}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 47, "line_end": 66}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 982, "line_end": 1026}]
- **Dependencies**: File layout copybooks for CAP* and CRP* 040/041; DS printing. [evidence: {"file": "legacy/crp063.CBL", "line_start": 17, "line_end": 21}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 44, "line_end": 45}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 80, "line_end": 81}]

#### 2.7 Program: CRP001T (Popup Inquiry)
- **Name**: `CRP001T` [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 3, "line_end": 3}]
- **Description**: Popup inquiry of A/R title statuses via Dialog System. [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 6, "line_end": 7}]
- **Core Responsibilities**: Opens `CRD001`, loads screenset, populates list box with status items; returns selection. [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 65, "line_end": 74}] [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 97, "line_end": 114}]
- **Dependencies**: `CRPX001`/`CRPW001` for A/R status file, `DS-CNTRL.MF`, `DSSYSINF.CPY`. [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 15, "line_end": 19}] [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 21, "line_end": 25}]

- Note: Many additional programs exist in families `CRP9105–CRP9119`, `CRP050–CRP060`, etc. Their `PROGRAM-ID`s are discoverable; a full inventory listing is in §2.8. [evidence: {"file": "legacy", "line_start": 1, "line_end": 1}]

#### 2.8 Program Inventory (by PROGRAM-ID)
- Examples (non‑exhaustive): CRP9102, CRP063, CRP9114, CRP001T, CRP9102A, CRP9119, CRP9117, CRP9118, CRP9119A, CRP9116A/B, CRP9116, CRP9115, CRP9112A/B/C/D, CRP9109/9109A, CRP9107, CRP9106/A/B, CRP9105, CRP099, CRP100, CRP060, CRP022, CRP020B, CRP062, CRP9103/9104, CRP9110/9111/9113, CRP001, CRP020, CRP057, CRP054/055/056/058/059. Each has `PROGRAM-ID.` at top of file. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 3, "line_end": 3}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 3, "line_end": 3}] [evidence: {"file": "legacy/crp9114.CBL", "line_start": 3, "line_end": 3}]

---

### 3. Data Structures

- **Representative `FD` Sections**:
  - CRP9102 `FD`:
    - `SEQBRAS` record with `CONT-SEQUENCIA` and `SEQUENCIA`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 56, "line_end": 60}]
    - `REMESSA`/`REMESSA2` 500‑byte data payloads plus 2‑byte type. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 60, "line_end": 69}]
    - `WORK` with keys and customer data. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 71, "line_end": 81}]
  - CRP9100 `FD` for `REMESSA` (id + 393‑byte data + sequence), `SEQREC`, `WORK`, and `RELAT`. [evidence: {"file": "legacy/crp9100.cbl", "line_start": 49, "line_end": 72}]
  - CRP063 `FD` for indexed `WORK` and `WORK1` aggregations, and a 150‑char printer record. [evidence: {"file": "legacy/crp063.CBL", "line_start": 47, "line_end": 66}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 68, "line_end": 72}]
  - CRP9117 `FD` for `RETORNO` (400 chars), `PROBLEMA` (399+seq), printer `RELAT`, and `REJEICOES` (key + description). [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 54, "line_end": 72}]

- **Copybooks**:
  - Data/control: large set under `legacy/*.CPY` used for file‑control (`*X*`) and file‑section (`*W*`), logging (`LOGACESS.*`), Dialog System control blocks (`DS-CNTRL.MF`), printing (`CBPRINT.CPY`). [evidence: {"file": "legacy/crp9102.CBL", "line_start": 15, "line_end": 24}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 46, "line_end": 55}] [evidence: {"file": "legacy/CRP022.cbl", "line_start": 17, "line_end": 36}]

- **Record Relationships**: Relationships are implicit via COBOL indexed file keys (`RECORD KEY`, `ALTERNATE RECORD KEY`) and application logic; no relational schema artifacts are present. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 36, "line_end": 41}]

---

### 4. Interfaces and Integration

- **File Interfaces**:
  - Printer `RELAT` assigned to `NOME-IMPRESSORA`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 42, "line_end": 42}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 38, "line_end": 39}]
  - Line sequential text files for bank I/O: `RETORNO`, `PROBLEMA`, `REMESSA/REMESSA2`. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 27, "line_end": 37}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 30, "line_end": 35}]
  - Indexed transaction and master files (e.g., `CRD020`, `CRD200/201/024`, `CGD*`, `CAD*`). [evidence: {"file": "legacy/crp9102.CBL", "line_start": 315, "line_end": 331}]

- **Program Calls**:
  - UI engine: `CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK` inside a standard `CALL-DIALOG-SYSTEM` section. [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 130, "line_end": 136}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 1214, "line_end": 1217}]
  - Utilities: `GRIDAT1/GRIDAT2` (date conversions), `MENSAGEM` (modal message), `CAP018T` (portador popup), `UTI0080` (UF description). [evidence: {"file": "legacy/crp9102.CBL", "line_start": 494, "line_end": 505}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 1042, "line_end": 1049}]

- **Screens/Maps**: Managed by Dialog System screensets referenced via `DS-SET-NAME` per program, e.g., `CRP9102`, `CRP001T`, `CRP063`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 1203, "line_end": 1206}] [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 126, "line_end": 129}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 978, "line_end": 980}]

- **JCL/Batch**: Not specified in the provided source code.

---

### 5. Workflows and Business Logic (selected)

- **CRP9102 – Remittance Generation (Banco do Brasil)**
  - Maintains a running sequence (`SEQBRAS`), builds header/detail/trailer records (`ID-REG-REM` 00/01/99) into `REMESSA`/`REMESSA2`, prints a list, and updates sequence. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 508, "line_end": 517}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 779, "line_end": 941}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 934, "line_end": 941}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 688, "line_end": 696}]

- **CRP9100 – Remittance Generation (Bradesco)**
  - Similar flow with distinct layout; uses control `SEQREC` and writes header/detail/trailer into `REMESSA`. [evidence: {"file": "legacy/crp9100.cbl", "line_start": 458, "line_end": 553}]

- **CRP9117 – Bank Return Processing**
  - Parses `RETORNO` detail records, accumulates totals, categorizes by occurrence code (e.g., 02 confirm, 03 reject, 06 liquidation), records rejections in `REJEICOES`, prints summary and problem listings, and updates A/R (`CRD020/CRD020B`). [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 461, "line_end": 517}] [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 1267, "line_end": 1471}] [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 671, "line_end": 685}]

- **CRP020/CRP022 – A/R Movement**
  - Interactive workflows for saving, updating, canceling, and printing A/R records; logs operations and maintains annotations (`CRD200/CRD201`). [evidence: {"file": "legacy/crp020.cbl", "line_start": 401, "line_end": 448}] [evidence: {"file": "legacy/crp020.cbl", "line_start": 1242, "line_end": 1357}] [evidence: {"file": "legacy/CRP022.cbl", "line_start": 541, "line_end": 567}] [evidence: {"file": "legacy/CRP022.cbl", "line_start": 1547, "line_end": 1670}]

- **CRP063 – Deflation Report**
  - Builds indexed workfiles from `CRD041` within a date window, computes and prints totals per 30/60/90/120/150/180 buckets. [evidence: {"file": "legacy/crp063.CBL", "line_start": 378, "line_end": 407}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 597, "line_end": 614}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 1082, "line_end": 1107}]

---

### 6. Dependency Mapping (selected)

- **Common Infrastructure**:
  - Dialog System control: `DS-CNTRL.MF`, procedures `LOAD-SCREENSET`, `CALL-DIALOG-SYSTEM`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 1201, "line_end": 1207}] [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 126, "line_end": 136}]
  - Logging: `LOGACESS.SEL`/`.FD` to record program open/close events. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 354, "line_end": 375}] [evidence: {"file": "legacy/CRP022.cbl", "line_start": 510, "line_end": 531}]

- **Banks Integration**:
  - Remittance: `CRP9100` (Bradesco), `CRP9102` (Banco do Brasil) read A/R (`CRD020`) and customer files (`CGD*`, `CAD*`) to compose output `.REM` files. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 315, "line_end": 331}] [evidence: {"file": "legacy/crp9100.cbl", "line_start": 254, "line_end": 266}]
  - Returns: `CRP9117` consumes `RETORNO` and updates `CRD020/CRD020B`; raises `PROBLEMA` lines for unmatched items. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 1231, "line_end": 1290}] [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 1517, "line_end": 1525}]

- **Reports/Calculations**:
  - `CRP063` uses `CRD041` (deflation) to produce bucketized summaries via `WORK/WORK1`. [evidence: {"file": "legacy/crp063.CBL", "line_start": 387, "line_end": 407}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 509, "line_end": 556}]

---

### 7. Configuration and Environment

- **Special-Names**: Decimal comma and printer mapping (`PRINTER IS LPRINTER`). [evidence: {"file": "legacy/crp9102.CBL", "line_start": 9, "line_end": 11}] [evidence: {"file": "legacy/crp020.cbl", "line_start": 12, "line_end": 14}]
- **Class-Control**: `Window is class "wclass"` shows GUI binding for Dialog System. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 11, "line_end": 13}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 12, "line_end": 13}]
- **External Jobs/JCL**: Not specified in the source code.

---

### 8. Non-Functional Requirements (NFRs)

- No explicit performance, security, or auditing requirements beyond operational logging in `LOGACESS` were found. Not specified in the source code.

---

### 9. Risks and Technical Debt

- Tight coupling to indexed flat files and Dialog System screensets.
- Multiple near-duplicate program variants (e.g., `CRP9102*`, `CRP911*`) increase maintenance surface.
- No centralized schema documentation beyond copybooks; implicit contracts in COBOL layouts. These are observations from code structure; explicit risk lists are not present. Not specified in the source code.

---

### 10. Test and Operations

- No JCL, batch control, or automated tests are present in `legacy/`. Not specified in the source code.

---

### 11. Glossary (from code terms)

- **REMESSA**: Outbound remittance file produced by programs (e.g., `CRP9100`, `CRP9102`). [evidence: {"file": "legacy/crp9102.CBL", "line_start": 30, "line_end": 35}] [evidence: {"file": "legacy/crp9100.cbl", "line_start": 49, "line_end": 54}]
- **RETORNO**: Bank return text input. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 27, "line_end": 33}]
- **PROBLEMA**: Text file listing unmatched/problematic return records. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 59, "line_end": 66}]
- **RELAT**: Printer-based report file. [evidence: {"file": "legacy/crp020.cbl", "line_start": 56, "line_end": 60}]
- **CRD***, **CGD***, **CAD***, **LOG***: Indexed and sequential files referenced via copybooks; detailed semantics embedded in copybook layouts. Not specified further in the provided data.
