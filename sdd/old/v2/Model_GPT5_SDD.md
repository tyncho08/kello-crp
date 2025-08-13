### 1. System Overview

- **Purpose**: Evidence appears in program comments:
  - CRP9100: “Generate CBDDMMxx.REM file for Bradesco” and alters portador during generation. [evidence: {"file": "legacy/crp9100.cbl", "line_start": 4, "line_end": 7}]
  - CRP9102: “Generate remittance file for Banco do Brasil.” [evidence: {"file": "legacy/crp9102.CBL", "line_start": 5, "line_end": 5}]
  - CRP9117/CRP9116: “Issue return-report for Banco do Brasil” with actions based on occurrence codes (02 accepted; 03 rejected; 06 liquidation). [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 5, "line_end": 12}] [evidence: {"file": "legacy/CRP9116.cbl", "line_start": 5, "line_end": 12}]
  - CRP020: “Accounts receivable movement,” with note that write-offs are performed by bank return file. [evidence: {"file": "legacy/crp020.cbl", "line_start": 6, "line_end": 10}]
  - CRP022: “Accounts receivable movement (financial scheduling).” [evidence: {"file": "legacy/CRP022.cbl", "line_start": 6, "line_end": 6}]
  - CRP063: “Report of deflated titles.” [evidence: {"file": "legacy/crp063.CBL", "line_start": 6, "line_end": 6}]

- **Technologies**: COBOL programs that declare decimal comma and printer mapping via `SPECIAL-NAMES`, and define a `class-control` binding for `Window`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 9, "line_end": 13}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 9, "line_end": 13}] [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 8, "line_end": 12}]

- **Architecture**: Not specified in the source code. UI dialog-system calls (`DSRUN`) are present in several programs. [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 130, "line_end": 133}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 1581, "line_end": 1583}]

- **I/O and Data Access**: Programs define files via `SELECT`/`FD` (indexed, sequential, printer). Examples include indexed WORK files and printer `RELAT`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 25, "line_end": 42}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 22, "line_end": 39}] [evidence: {"file": "legacy/crp020.cbl", "line_start": 36, "line_end": 60}]

- **Logging/Access**: Example program includes `LOGACESS` copybooks for selection/file definitions. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 24, "line_end": 24}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 55, "line_end": 55}]

---

### 2. Module Decomposition

Below are key programs, with responsibilities and dependencies grounded in code.

#### 2.1 Program: `CRP9100`
- **Name**: `CRP9100` [evidence: {"file": "legacy/crp9100.cbl", "line_start": 2, "line_end": 2}]
- **Description**: Generates `CBDDMMxx.REM` remittance for Bradesco and alters the “portador” in Accounts Receivable during generation. [evidence: {"file": "legacy/crp9100.cbl", "line_start": 4, "line_end": 7}]
- **Core Responsibilities**:
  - Defines and writes to `REMESSA` (line sequential). [evidence: {"file": "legacy/crp9100.cbl", "line_start": 28, "line_end": 33}]
  - Uses indexed `SEQREC` for sequencing remittances. [evidence: {"file": "legacy/crp9100.cbl", "line_start": 21, "line_end": 27}] [evidence: {"file": "legacy/crp9100.cbl", "line_start": 55, "line_end": 59}]
  - Produces printer output file `RELAT`. [evidence: {"file": "legacy/crp9100.cbl", "line_start": 38, "line_end": 38}] [evidence: {"file": "legacy/crp9100.cbl", "line_start": 68, "line_end": 72}]
- **Dependencies**:
  - Copybooks for file layouts: `CAPX001`, `CAPX018`, `CRPX020`, `CGPX010`, `CGPX011`, `CRPX200`, `CRPX201`. [evidence: {"file": "legacy/crp9100.cbl", "line_start": 14, "line_end": 20}]
  - Copybooks for file sections: `CAPW001`, `CAPW018`, `CRPW020`, `CGPW010`, `CGPW011`, `CRPW200`, `CRPW201`. [evidence: {"file": "legacy/crp9100.cbl", "line_start": 42, "line_end": 48}]
  - UI/printing: `DS-CNTRL.MF`, `CBPRINT.CPY`. [evidence: {"file": "legacy/crp9100.cbl", "line_start": 75, "line_end": 77}]

#### 2.2 Program: `CRP9102`
- **Name**: `CRP9102` [evidence: {"file": "legacy/crp9102.CBL", "line_start": 3, "line_end": 3}]
- **Description**: Generates remittance file for Banco do Brasil. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 5, "line_end": 5}]
- **Core Responsibilities**:
  - Indexed control `SEQBRAS` with key `CONT-SEQUENCIA`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 25, "line_end": 29}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 56, "line_end": 60}]
  - Outputs `REMESSA`/`REMESSA2` as sequential/line-sequential records. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 30, "line_end": 35}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 60, "line_end": 69}]
  - Uses indexed `WORK` with alternate key `NOME-WK`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 36, "line_end": 41}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 71, "line_end": 81}]
  - Produces printer output via `RELAT`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 42, "line_end": 42}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 81, "line_end": 85}]
  - Invokes UI dialog system `DSRUN` through `CALL-DIALOG-SYSTEM` section. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 1214, "line_end": 1217}]
  - Calls helper programs (e.g., `GRIDAT1`, `GRIDAT2`, `CAP018T`, `UTI0080`). [evidence: {"file": "legacy/crp9102.CBL", "line_start": 839, "line_end": 839}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 494, "line_end": 505}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 439, "line_end": 444}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 918, "line_end": 918}]
- **Dependencies**:
  - Input-output copybooks for account/customer layouts and logging: `CAPX010`, `CAPX018`, `CRPX020`, `CGPX010`, `CGPX011`, `CGPX014`, `CRPX200`, `CRPX201`, `RETPORT.SEL`, `LOGACESS.SEL`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 15, "line_end": 24}]
  - File section copybooks and print/UI support: `CAPW010`, `CAPW018`, `CRPW020`, `CGPW010`, `CGPW011`, `CGPW014`, `CRPW200`, `CRPW201`, `RETPORT.FD`, `LOGACESS.FD`, `DS-CNTRL.MF`, `CBPRINT.CPY`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 46, "line_end": 55}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 87, "line_end": 90}]

#### 2.3 Program: `CRP9117`
- **Name**: `CRP9117` [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 3, "line_end": 3}]
- **Description**: Return report emission for Banco do Brasil; actions per occurrence codes 02/03/06. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 5, "line_end": 12}]
- **Core Responsibilities**:
  - Reads `RETORNO` and writes a text `PROBLEMA` file; prints to `RELAT`. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 28, "line_end": 37}] [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 54, "line_end": 67}]
  - Maintains indexed `REJEICOES` with key `REJ-CHAVE`. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 39, "line_end": 45}] [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 68, "line_end": 72}]
  - UI dialog system calls via `DSRUN`. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 1787, "line_end": 1789}]
- **Dependencies**: `CRPX020`, `PARX002`, `CRPX020B`, `CGPX010`, `LOGACESS.SEL` and corresponding `COPY` for FD layouts and UI/print. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 23, "line_end": 27}] [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 49, "line_end": 53}] [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 73, "line_end": 79}]

#### 2.4 Program: `CRP9116`
- **Name**: `CRP9116` [evidence: {"file": "legacy/CRP9116.cbl", "line_start": 3, "line_end": 3}]
- **Description**: Same domain as CRP9117; emits return report and updates A/R per occurrence codes; maintains `PROBLEMA` and `REJEICOES`. [evidence: {"file": "legacy/CRP9116.cbl", "line_start": 5, "line_end": 12}] [evidence: {"file": "legacy/CRP9116.cbl", "line_start": 33, "line_end": 45}]
- **Core Responsibilities**: Reads `RETORNO`, writes `PROBLEMA`, prints `RELAT`, and maintains `REJEICOES`. [evidence: {"file": "legacy/CRP9116.cbl", "line_start": 28, "line_end": 45}] [evidence: {"file": "legacy/CRP9116.cbl", "line_start": 54, "line_end": 72}]
- **Dependencies**: `CRPX020`, `CRPX020B`, `PARX002`, `CGPX010`, `LOGACESS.*`, print support. [evidence: {"file": "legacy/CRP9116.cbl", "line_start": 23, "line_end": 33}] [evidence: {"file": "legacy/CRP9116.cbl", "line_start": 49, "line_end": 53}] [evidence: {"file": "legacy/CRP9116.cbl", "line_start": 74, "line_end": 79}]

#### 2.5 Program: `CRP020`
- **Name**: `CRP020` [evidence: {"file": "legacy/crp020.cbl", "line_start": 3, "line_end": 3}]
- **Description**: Accounts receivable movement; note that write-offs occur via bank return. [evidence: {"file": "legacy/crp020.cbl", "line_start": 6, "line_end": 10}]
- **Core Responsibilities**: Produces printer `RELAT` output; integrates multiple A/R files via copybooks. [evidence: {"file": "legacy/crp020.cbl", "line_start": 36, "line_end": 60}] [evidence: {"file": "legacy/crp020.cbl", "line_start": 56, "line_end": 60}]
- **Dependencies**: Numerous copybooks for customer, general ledger, A/R, and logging layouts. [evidence: {"file": "legacy/crp020.cbl", "line_start": 20, "line_end": 35}] [evidence: {"file": "legacy/crp020.cbl", "line_start": 40, "line_end": 55}]

#### 2.6 Program: `CRP022`
- **Name**: `CRP022` [evidence: {"file": "legacy/CRP022.cbl", "line_start": 3, "line_end": 3}]
- **Description**: Accounts receivable movement (financial scheduling). [evidence: {"file": "legacy/CRP022.cbl", "line_start": 6, "line_end": 6}]
- **Core Responsibilities**: Produces printer `RELAT` output; integrates many A/R and log files. [evidence: {"file": "legacy/CRP022.cbl", "line_start": 36, "line_end": 63}]
- **Dependencies**: Copybooks for file control and layouts across modules. [evidence: {"file": "legacy/CRP022.cbl", "line_start": 17, "line_end": 35}] [evidence: {"file": "legacy/CRP022.cbl", "line_start": 40, "line_end": 58}]

#### 2.7 Program: `CRP063`
- **Name**: `CRP063` [evidence: {"file": "legacy/crp063.CBL", "line_start": 3, "line_end": 3}]
- **Description**: Deflated titles report. [evidence: {"file": "legacy/crp063.CBL", "line_start": 6, "line_end": 6}]
- **Core Responsibilities**:
  - Uses indexed `WORK` and `WORK1` files to aggregate metrics, keyed by fields shown. [evidence: {"file": "legacy/crp063.CBL", "line_start": 22, "line_end": 37}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 47, "line_end": 66}]
  - Produces printer `RELAT`. [evidence: {"file": "legacy/crp063.CBL", "line_start": 38, "line_end": 39}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 68, "line_end": 72}]
- **Dependencies**: Copybooks for control and printing. [evidence: {"file": "legacy/crp063.CBL", "line_start": 17, "line_end": 20}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 42, "line_end": 45}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 74, "line_end": 80}]

#### 2.8 Program: `CRP001T`
- **Name**: `CRP001T` [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 3, "line_end": 3}]
- **Description**: Popup inquiry on title status in Accounts Receivable (interactive UI). [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 6, "line_end": 6}]
- **Core Responsibilities**:
  - Opens `CRD001`, loads screens, populates list box, handles selection. [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 65, "line_end": 74}] [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 97, "line_end": 114}] [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 125, "line_end": 129}]
  - UI dialog loop via `CALL-DIALOG-SYSTEM` to `DSRUN`. [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 84, "line_end": 85}] [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 130, "line_end": 136}]
- **Dependencies**: `CRPX001`/`CRPW001` for A/R file, DS control and system info copybooks. [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 15, "line_end": 15}] [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 19, "line_end": 24}]

- Note: Many additional programs exist (e.g., CRP9105–CRP9119 families, CRP050–CRP060, CRP051–CRP059, CRP099–CRP100). Their `PROGRAM-ID` are enumerated in the codebase. A full inventory is listed in Section 2.9.

#### 2.9 Program Inventory (by PROGRAM-ID)
- CRP9102 [evidence: {"file": "legacy/crp9102.CBL", "line_start": 3, "line_end": 3}]
- CRP063 [evidence: {"file": "legacy/crp063.CBL", "line_start": 3, "line_end": 3}]
- CRP9114 [evidence: {"file": "legacy/crp9114.CBL", "line_start": 3, "line_end": 3}]
- CRP001T [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 3, "line_end": 3}]
- CRP9102A [evidence: {"file": "legacy/crp9102A.CBL", "line_start": 3, "line_end": 3}]
- CRP9119 [evidence: {"file": "legacy/CRP9119.cbl", "line_start": 3, "line_end": 3}]
- CRP9117 [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 3, "line_end": 3}]
- CRP9118 [evidence: {"file": "legacy/CRP9118.cbl", "line_start": 3, "line_end": 3}]
- CRP9119A [evidence: {"file": "legacy/CRP9119A.cbl", "line_start": 3, "line_end": 3}]
- CRP9116A [evidence: {"file": "legacy/CRP9116A.cbl", "line_start": 3, "line_end": 3}]
- CRP9116B [evidence: {"file": "legacy/CRP9116B.cbl", "line_start": 3, "line_end": 3}]
- CRP9116 [evidence: {"file": "legacy/CRP9116.cbl", "line_start": 3, "line_end": 3}]
- CRP9115 [evidence: {"file": "legacy/CRP9115.cbl", "line_start": 3, "line_end": 3}]
- CRP9112C [evidence: {"file": "legacy/CRP9112C.cbl", "line_start": 3, "line_end": 3}]
- CRP9112B [evidence: {"file": "legacy/CRP9112B.cbl", "line_start": 3, "line_end": 3}]
- CRP9112A [evidence: {"file": "legacy/CRP9112A.cbl", "line_start": 3, "line_end": 3}]
- CRP9112D [evidence: {"file": "legacy/CRP9112D.cbl", "line_start": 3, "line_end": 3}]
- CRP9109A [evidence: {"file": "legacy/CRP9109A.cbl", "line_start": 3, "line_end": 3}]
- CRP9109 [evidence: {"file": "legacy/CRP9109.cbl", "line_start": 3, "line_end": 3}]
- CRP9107 [evidence: {"file": "legacy/CRP9107.cbl", "line_start": 3, "line_end": 3}]
- CRP9105 [evidence: {"file": "legacy/CRP9105.cbl", "line_start": 3, "line_end": 3}]
- CRP9106B [evidence: {"file": "legacy/CRP9106B.cbl", "line_start": 3, "line_end": 3}]
- CRP9106A [evidence: {"file": "legacy/CRP9106A.cbl", "line_start": 3, "line_end": 3}]
- CRP9106 [evidence: {"file": "legacy/CRP9106.cbl", "line_start": 3, "line_end": 3}]
- CRP9102A (alt copy) [evidence: {"file": "legacy/CRP9102A(1).CBL", "line_start": 3, "line_end": 3}]
- CRP9102B [evidence: {"file": "legacy/CRP9102B.cbl", "line_start": 3, "line_end": 3}]
- CRP9102D [evidence: {"file": "legacy/CRP9102D.cbl", "line_start": 3, "line_end": 3}]
- CRP9102C [evidence: {"file": "legacy/CRP9102C.cbl", "line_start": 3, "line_end": 3}]
- CRP9102 (alt copy) [evidence: {"file": "legacy/CRP9102(1).CBL", "line_start": 3, "line_end": 3}]
- CRP099 [evidence: {"file": "legacy/CRP099.cbl", "line_start": 3, "line_end": 3}]
- CRP100 [evidence: {"file": "legacy/CRP100.cbl", "line_start": 3, "line_end": 3}]
- CRP060 [evidence: {"file": "legacy/CRP060.cbl", "line_start": 2, "line_end": 2}]
- CRP063 (alt copy) [evidence: {"file": "legacy/CRP063(1).CBL", "line_start": 3, "line_end": 3}]
- CRP022 [evidence: {"file": "legacy/CRP022.cbl", "line_start": 3, "line_end": 3}]
- CRP020B [evidence: {"file": "legacy/CRP020B.cbl", "line_start": 3, "line_end": 3}]
- CRP062 [evidence: {"file": "legacy/crp062.cbl", "line_start": 3, "line_end": 3}]
- CRP9103 [evidence: {"file": "legacy/crp9103.cbl", "line_start": 3, "line_end": 3}]
- CRP059 [evidence: {"file": "legacy/crp059.cbl", "line_start": 3, "line_end": 3}]
- CRP9110 [evidence: {"file": "legacy/crp9110.cbl", "line_start": 2, "line_end": 2}]
- CRP054 [evidence: {"file": "legacy/crp054.cbl", "line_start": 3, "line_end": 3}]
- CRP9111 [evidence: {"file": "legacy/crp9111.cbl", "line_start": 3, "line_end": 3}]
- CRP061 [evidence: {"file": "legacy/crp061.cbl", "line_start": 3, "line_end": 3}]
- CRP099 (alt copy) [evidence: {"file": "legacy/crp099(1).cbl", "line_start": 3, "line_end": 3}]
- CRP9106 (alt copy) [evidence: {"file": "legacy/crp9106(1).cbl", "line_start": 3, "line_end": 3}]
- CRP9107 (alt copy) [evidence: {"file": "legacy/crp9107(1).cbl", "line_start": 3, "line_end": 3}]
- CRP055 [evidence: {"file": "legacy/crp055.cbl", "line_start": 3, "line_end": 3}]
- CRP058 [evidence: {"file": "legacy/crp058.cbl", "line_start": 3, "line_end": 3}]
- CRP9112C (alt copy) [evidence: {"file": "legacy/crp9112C(1).cbl", "line_start": 3, "line_end": 3}]
- CRP9119A (alt copy) [evidence: {"file": "legacy/crp9119A(1).cbl", "line_start": 3, "line_end": 3}]
- CRP9109A (alt copy) [evidence: {"file": "legacy/crp9109a(1).cbl", "line_start": 3, "line_end": 3}]
- CRP9102B (alt copy) [evidence: {"file": "legacy/crp9102B(1).cbl", "line_start": 3, "line_end": 3}]
- CRP9102C (alt copy) [evidence: {"file": "legacy/crp9102C(1).cbl", "line_start": 3, "line_end": 3}]
- CRP9115 (alt copy) [evidence: {"file": "legacy/crp9115(1).cbl", "line_start": 3, "line_end": 3}]
- CRP9118 (alt copy) [evidence: {"file": "legacy/crp9118(1).cbl", "line_start": 3, "line_end": 3}]
- CRP001 [evidence: {"file": "legacy/crp001.cbl", "line_start": 3, "line_end": 3}]
- CRP020 [evidence: {"file": "legacy/crp020.cbl", "line_start": 3, "line_end": 3}]
- CRP057 [evidence: {"file": "legacy/crp057.cbl", "line_start": 3, "line_end": 3}]
- CRP100 (alt copy) [evidence: {"file": "legacy/crp100(1).cbl", "line_start": 3, "line_end": 3}]
- CRP9104 [evidence: {"file": "legacy/crp9104.cbl", "line_start": 3, "line_end": 3}]
- CRP9106A (alt copy) [evidence: {"file": "legacy/crp9106a(1).cbl", "line_start": 3, "line_end": 3}]
- CRP020A [evidence: {"file": "legacy/crp020a.cbl", "line_start": 3, "line_end": 3}]
- CRP9100 [evidence: {"file": "legacy/crp9100.cbl", "line_start": 2, "line_end": 2}]
- CRP054 [evidence: {"file": "legacy/crp054.cbl", "line_start": 3, "line_end": 3}]
- CRP9101 [evidence: {"file": "legacy/crp9101.cbl", "line_start": 3, "line_end": 3}]
- CRP056 [evidence: {"file": "legacy/crp056.cbl", "line_start": 3, "line_end": 3}]
- CRP053 [evidence: {"file": "legacy/crp053.cbl", "line_start": 3, "line_end": 3}]
- CRP9105 (alt copy) [evidence: {"file": "legacy/crp9105(1).cbl", "line_start": 3, "line_end": 3}]
- CRP9116A (alt copy) [evidence: {"file": "legacy/crp9116a(1).cbl", "line_start": 3, "line_end": 3}]
- CRP9117 (alt copy) [evidence: {"file": "legacy/crp9117(1).cbl", "line_start": 3, "line_end": 3}]
- CRP9106 (alt copy B) [evidence: {"file": "legacy/crp9106(1).cbl", "line_start": 3, "line_end": 3}]
- CRP051 [evidence: {"file": "legacy/crp051.cbl", "line_start": 3, "line_end": 3}]
- CRP9109 (alt copy) [evidence: {"file": "legacy/crp9109(1).cbl", "line_start": 3, "line_end": 3}]
- CRP9112D (alt copy) [evidence: {"file": "legacy/crp9112D(1).cbl", "line_start": 3, "line_end": 3}]
- CRP9102D (alt copy) [evidence: {"file": "legacy/crp9102D(1).cbl", "line_start": 3, "line_end": 3}]
- CRP050 [evidence: {"file": "legacy/crp050.cbl", "line_start": 3, "line_end": 3}]
- CRP022 (alt copy) [evidence: {"file": "legacy/crp022(1).cbl", "line_start": 3, "line_end": 3}]
- CRP9112 [evidence: {"file": "legacy/crp9112.cbl", "line_start": 3, "line_end": 3}]
- CRP060 (alt copy) [evidence: {"file": "legacy/crp060(1).cbl", "line_start": 2, "line_end": 2}]
- CRP9103 [evidence: {"file": "legacy/crp9103.cbl", "line_start": 3, "line_end": 3}]
- CRP9102C (alt copy) [evidence: {"file": "legacy/crp9102C(1).cbl", "line_start": 3, "line_end": 3}]
- CRP9113 [evidence: {"file": "legacy/crp9113.cbl", "line_start": 3, "line_end": 3}]

---

### 3. Data Structures

- **File Definitions (examples)**
  - CRP9102 `FD` sections: `SEQBRAS` record with `CONT-SEQUENCIA` and `SEQUENCIA`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 56, "line_end": 60}]
    - `REMESSA` and `REMESSA2` record layouts including `ID-REG-REM(2)` and 498-byte payloads. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 60, "line_end": 69}]
    - `WORK` with keys and customer-related fields (names/addresses/values). [evidence: {"file": "legacy/crp9102.CBL", "line_start": 71, "line_end": 81}]
  - CRP9100 `FD` sections: `REMESSA` (id + 393-byte data + sequence); `SEQREC` (code + sequence); `WORK` (name/address/cep/docto/valor). [evidence: {"file": "legacy/crp9100.cbl", "line_start": 49, "line_end": 67}]
  - CRP063 `FD` sections:
    - `WORK` and `WORK1` carry aggregates like `VALOR-BRUTO/LIQUIDO/JUROS` and keys `TIPO/DATA-BASE/NOME-ARQUIVO`. [evidence: {"file": "legacy/crp063.CBL", "line_start": 47, "line_end": 66}]
    - `RELAT` printer record. [evidence: {"file": "legacy/crp063.CBL", "line_start": 68, "line_end": 72}]
  - CRP9117/CRP9116 `FD` sections: `RETORNO` 400-bytes, `PROBLEMA` lines, `RELAT` printer, `REJEICOES` with `REJ-CHAVE` and description. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 54, "line_end": 72}] [evidence: {"file": "legacy/CRP9116.cbl", "line_start": 54, "line_end": 72}]

- **Copybooks**: Numerous `COPY` members referenced for file control (`...X...`) and file section (`...W...`) layouts (e.g., `CRPX020`, `CRPW020`, `LOGACESS.*`, `DS-CNTRL.MF`, `CBPRINT.CPY`). [evidence: {"file": "legacy/crp9102.CBL", "line_start": 15, "line_end": 24}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 46, "line_end": 55}] [evidence: {"file": "legacy/crp020.cbl", "line_start": 20, "line_end": 35}] [evidence: {"file": "legacy/crp020.cbl", "line_start": 40, "line_end": 55}]

- **Record Relationships**: Beyond keys specified in `SELECT`/`FD` (e.g., `RECORD KEY`, `ALTERNATE RECORD KEY`), no explicit relational schema is declared. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 36, "line_end": 41}] [evidence: {"file": "legacy/crp9100.cbl", "line_start": 31, "line_end": 37}]

---

### 4. APIs and Interfaces

- **File Interfaces**: Programs declare external files via `SELECT`/`ASSIGN`. Examples:
  - `RELAT` assigned to printer `NOME-IMPRESSORA`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 42, "line_end": 42}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 38, "line_end": 39}]
  - Indexed `WORK`-family files for intermediate data. [evidence: {"file": "legacy/crp063.CBL", "line_start": 22, "line_end": 37}]
  - Text files `RETORNO` and `PROBLEMA` for bank return processing. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 28, "line_end": 36}]

- **Program Calls**:
  - Dialog/UI engine: `CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK` within `CALL-DIALOG-SYSTEM` sections. [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 130, "line_end": 133}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 1581, "line_end": 1583}]
  - Utility/business procedures: `GRIDAT1`, `GRIDAT2`, `CAP018T`, `UTI0080`. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 494, "line_end": 505}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 839, "line_end": 839}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 439, "line_end": 444}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 918, "line_end": 918}]

- **Screens/Maps**: Not specified in the provided source beyond dialog system integration and copybooks (`DS-CNTRL.MF`). [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 21, "line_end": 24}]

- **JCL/Job Control**: Not specified in the source code.

---

### 5. Workflows and Business Logic

- **CRP001T (Interactive popup inquiry)**
  - Initializes DS/GS control blocks, sets `STRING-1` flags, opens A/R file `CRD001`, loads screenset. [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 57, "line_end": 74}]
  - Main loop evaluates UI flags to centralize window, load list-box with items from `CRD001`, and handle selection; then clears flags and calls dialog system. [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 74, "line_end": 85}] [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 97, "line_end": 114}]

- **CRP9102 (Remittance generation)**
  - Maintains a per-record sequence via indexed `SEQBRAS` and writes formatted `REMESSA/REMESSA2` records. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 25, "line_end": 35}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 56, "line_end": 69}]
  - Uses helper procedures for date validation/formatting (`GRIDAT1/2`) and user interactions (`CAP018T` popup). [evidence: {"file": "legacy/crp9102.CBL", "line_start": 494, "line_end": 505}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 839, "line_end": 839}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 439, "line_end": 444}]

- **CRP9117/CRP9116 (Return processing and reporting)**
  - Comments define business rules keyed by bank occurrence codes (02 accepted; 03 rejected → change portador; 06 liquidation → perform write-off). [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 5, "line_end": 12}] [evidence: {"file": "legacy/CRP9116.cbl", "line_start": 5, "line_end": 12}]
  - Input is line-sequential `RETORNO`; `PROBLEMA` collects records not matched/processable; `REJEICOES` stores rejection code descriptions. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 28, "line_end": 45}] [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 68, "line_end": 72}]

- **CRP020/CRP022 (A/R movement and scheduling)**
  - Produce printable reports with headers and integrate multiple A/R-related files via copybooks. [evidence: {"file": "legacy/crp020.cbl", "line_start": 56, "line_end": 69}] [evidence: {"file": "legacy/CRP022.cbl", "line_start": 59, "line_end": 72}] [evidence: {"file": "legacy/crp020.cbl", "line_start": 153, "line_end": 174}] [evidence: {"file": "legacy/CRP022.cbl", "line_start": 159, "line_end": 179}]


---

### 6. Configuration and Environment

- **Special-Names**: Decimal comma and printer mapping. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 9, "line_end": 11}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 9, "line_end": 11}] [evidence: {"file": "legacy/crp020.cbl", "line_start": 12, "line_end": 14}]
- **Class-Control**: Window class binding for UI. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 11, "line_end": 13}] [evidence: {"file": "legacy/crp063.CBL", "line_start": 12, "line_end": 13}]
- **Dialog System Integration**: `DSRUN` invoked via `CALL-DIALOG-SYSTEM`. [evidence: {"file": "legacy/CRP001T.CBL", "line_start": 130, "line_end": 133}]
- **Logging**: Copybooks `LOGACESS.SEL`/`.FD` used in various programs. [evidence: {"file": "legacy/crp9102.CBL", "line_start": 24, "line_end": 24}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 55, "line_end": 55}]

---

### 7. Non-Functional Requirements (NFRs)

- Explicit performance, security, or audit requirements are not stated beyond the presence of logging copybooks. Not specified in the source code.

---

### 8. Risks and Technical Debt

- Explicit TODO/FIXME or deprecated markers were not observed in the examined files. Not specified in the source code.

---

### 9. Test Coverage

- No JCL or test harness artifacts were found in the `legacy` folder. Not specified in the source code.

---

### 10. Glossary (from source terms/comments)

- **REMESSA**: Term used for remittance files. Meaning inferred by filename and comments; no formal definition object is present. [evidence: {"file": "legacy/crp9100.cbl", "line_start": 4, "line_end": 7}] [evidence: {"file": "legacy/crp9102.CBL", "line_start": 30, "line_end": 35}]
- **RETORNO**: Bank return text file input. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 28, "line_end": 33}]
- **PROBLEMA**: Text file for issues detected during return processing. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 33, "line_end": 36}]
- **REJEICOES**: Indexed file with rejection code descriptions. [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 39, "line_end": 45}] [evidence: {"file": "legacy/CRP9117.cbl", "line_start": 68, "line_end": 72}]
- **RELAT**: Printer-based report file. [evidence: {"file": "legacy/crp020.cbl", "line_start": 56, "line_end": 60}]
- Terms like `CRD0xx`, `CAPx`, `CGPx`, `CRPx`, `LOGx` denote copybooks and files; specific domain definitions are not embedded in code comments. Not specified in the source code.
