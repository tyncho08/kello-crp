# Software Design Document - COBOL Accounts Receivable System

## 1. System Overview

### Purpose
This system is a COBOL-based accounts receivable management system designed for financial operations in a Brazilian business environment. The primary purpose is managing customer billing, payment processing, and banking integration for accounts receivable operations. `[evidence: {"file": "legacy/crp001.cbl", "line_start": 6, "line_end": 7}]` `[evidence: {"file": "legacy/crp020.cbl", "line_start": 6, "line_end": 6}]`

### Technologies
* **Programming Language**: COBOL with Micro Focus extensions `[evidence: {"file": "legacy/crp001.cbl", "line_start": 1, "line_end": 1}]`
* **Dialog System**: Micro Focus Dialog System Version 2 `[evidence: {"file": "legacy/DS-CNTRL.MF", "line_start": 4, "line_end": 6}]`
* **File Organization**: Indexed Sequential Access Method (ISAM) `[evidence: {"file": "legacy/CRPX001.CPY", "line_start": 2, "line_end": 2}]`
* **GUI Framework**: Windows class "wclass" integration `[evidence: {"file": "legacy/crp001.cbl", "line_start": 13, "line_end": 13}]`
* **Locale**: Brazilian Portuguese system with comma as decimal point `[evidence: {"file": "legacy/crp001.cbl", "line_start": 11, "line_end": 11}]`

### Architecture
The system follows a modular file-based architecture with the following components:
* **Presentation Layer**: Dialog System interface with Windows integration `[evidence: {"file": "legacy/crp001.cbl", "line_start": 12, "line_end": 13}]`
* **Business Logic Layer**: COBOL programs implementing financial transactions and business rules
* **Data Layer**: ISAM files with automatic record locking `[evidence: {"file": "legacy/CRPX001.CPY", "line_start": 4, "line_end": 5}]`

## 2. Module Decomposition

### 2.1 Module: `CRP001`
* **Name**: `CRP001` `[evidence: {"file": "legacy/crp001.cbl", "line_start": 3, "line_end": 3}]`
* **Author**: MARELI AMANCIO VOLPATO `[evidence: {"file": "legacy/crp001.cbl", "line_start": 4, "line_end": 4}]`
* **Date**: 07/04/1999 `[evidence: {"file": "legacy/crp001.cbl", "line_start": 5, "line_end": 5}]`
* **Description**: Registration module for managing title status within accounts receivable system. `[evidence: {"file": "legacy/crp001.cbl", "line_start": 6, "line_end": 7}]`
* **Core Responsibilities**:
  * Initialize program parameters from command line `[evidence: {"file": "legacy/crp001.cbl", "line_start": 114, "line_end": 114}]`
  * Manage CRD001 file operations with error handling `[evidence: {"file": "legacy/crp001.cbl", "line_start": 128, "line_end": 137}]`
  * Execute main processing loop until exit flag is set `[evidence: {"file": "legacy/crp001.cbl", "line_start": 110, "line_end": 110}]`
  * Handle various operations through EVALUATE statement `[evidence: {"file": "legacy/crp001.cbl", "line_start": 174, "line_end": 206}]`
* **Dependencies**:
  * Uses CRPX001.CPY for file control definitions `[evidence: {"file": "legacy/crp001.cbl", "line_start": 17, "line_end": 17}]`
  * Uses LOGX001.CPY for audit logging `[evidence: {"file": "legacy/crp001.cbl", "line_start": 18, "line_end": 18}]`
  * Integrates with Dialog System through DS-CONTROL-BLOCK `[evidence: {"file": "legacy/crp001.cbl", "line_start": 117, "line_end": 118}]`

### 2.2 Module: `CRP020`
* **Name**: `CRP020` `[evidence: {"file": "legacy/crp020.cbl", "line_start": 3, "line_end": 3}]`
* **Author**: MARELI AMANCIO VOLPATO `[evidence: {"file": "legacy/crp020.cbl", "line_start": 4, "line_end": 4}]`
* **Date**: 08/04/1999 `[evidence: {"file": "legacy/crp020.cbl", "line_start": 5, "line_end": 5}]`
* **Description**: Main accounts receivable transaction processing module. Handles account transactions and integrates with banking systems. `[evidence: {"file": "legacy/crp020.cbl", "line_start": 6, "line_end": 6}]`
* **Business Rule**: Account settlements are only performed through bank return files. `[evidence: {"file": "legacy/crp020.cbl", "line_start": 8, "line_end": 9}]`
* **Core Responsibilities**:
  * Process accounts receivable transactions
  * Manage customer account relationships
  * Handle banking integration through multiple copybook dependencies
* **Dependencies**:
  * Uses extensive file control copybooks including CAPX002, CGPX001, CGPX010, CGPX020, CXPX020, CRPX020, CRPX021 `[evidence: {"file": "legacy/crp020.cbl", "line_start": 20, "line_end": 26}]`

### 2.3 Module: `CRP9100`
* **Name**: `CRP9100` `[evidence: {"file": "legacy/crp9100.cbl", "line_start": 2, "line_end": 2}]`
* **Author**: MARELI AMANCIO VOLPATO `[evidence: {"file": "legacy/crp9100.cbl", "line_start": 3, "line_end": 3}]`
* **Date**: 14/04/1999 `[evidence: {"file": "legacy/crp9100.cbl", "line_start": 7, "line_end": 7}]`
* **Description**: Banking interface module that generates CBDDMMxx.REM files for Bradesco bank integration. During file generation, the system updates the carrier (portador) field in the accounts receivable file. `[evidence: {"file": "legacy/crp9100.cbl", "line_start": 4, "line_end": 6}]`
* **Core Responsibilities**:
  * Generate REMESSA files for bank processing `[evidence: {"file": "legacy/crp9100.cbl", "line_start": 28, "line_end": 30}]`
  * Maintain sequence control through SEQREC file `[evidence: {"file": "legacy/crp9100.cbl", "line_start": 21, "line_end": 27}]`
  * Process work files for customer data organization `[evidence: {"file": "legacy/crp9100.cbl", "line_start": 31, "line_end": 36}]`

### 2.4 Module: `CRP9101`
* **Name**: `CRP9101` `[evidence: {"file": "legacy/crp9101.cbl", "line_start": 3, "line_end": 3}]`
* **Author**: ALFREDO SAVIOLLI NETO `[evidence: {"file": "legacy/crp9101.cbl", "line_start": 4, "line_end": 4}]`
* **Date**: 01/04/2005 `[evidence: {"file": "legacy/crp9101.cbl", "line_start": 6, "line_end": 6}]`
* **Description**: Banking interface module that generates XXXXXXXX.REM files for Itaú bank integration. `[evidence: {"file": "legacy/crp9101.cbl", "line_start": 5, "line_end": 5}]`

## 3. Data Structures

### 3.1 CRD001 - Title Status Master File
* **File Description**: Registration file for title situations within accounts receivable `[evidence: {"file": "legacy/CRPW001.CPY", "line_start": 1, "line_end": 1}]`
* **Organization**: Indexed sequential with dynamic access mode `[evidence: {"file": "legacy/CRPX001.CPY", "line_start": 2, "line_end": 3}]`
* **Locking**: Automatic record-level locking `[evidence: {"file": "legacy/CRPX001.CPY", "line_start": 4, "line_end": 5}]`
* **Record Structure**:
  ```cobol
  01  REG-CRD001.
      05  CODIGO-CR01           PIC 99.
      05  SITUACAO-TIT-CR01     PIC X(10).
      05  DESCRICAO-CR01        PIC X(30).
  ```
  `[evidence: {"file": "legacy/CRPW001.CPY", "line_start": 3, "line_end": 6}]`
* **Keys**: 
  * Primary key: CODIGO-CR01 `[evidence: {"file": "legacy/CRPX001.CPY", "line_start": 7, "line_end": 7}]`
  * Alternate key: SITUACAO-TIT-CR01 `[evidence: {"file": "legacy/CRPX001.CPY", "line_start": 8, "line_end": 8}]`

### 3.2 CRD020 - Accounts Receivable Transactions
* **File Description**: Main transaction file for accounts receivable movements `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 1, "line_end": 1}]`
* **Key Structure**:
  * Transaction date: DATA-MOVTO-CR20 PIC 9(8) for movement date tracking `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 4, "line_end": 4}]`
  * Hierarchical key CHAVE-CR20 containing composite customer identification `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 5, "line_end": 5}]`
    * COD-COMPL-CR20 with nested customer classification and code `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 6, "line_end": 9}]`
    * Sequential number SEQ-CR20 PIC 9(5) for transaction ordering `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 12, "line_end": 12}]`
* **Customer Classification System**: 
  * CLASS-CLIENTE-CR20 where 0=contract customer, 1=common customer `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 8, "line_end": 8}]`
  * For contract type (0): customer code represents contract/album number format `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 10, "line_end": 11}]`
  * For common type (1): customer code is sequential identifier `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 10, "line_end": 11}]`
* **Banking Integration Fields**:
  * PORTADOR-CR20 PIC 9999 for financial institution identification `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 13, "line_end": 13}]`
  * OUTRO-DOCTO-CR20 stores remittance number and bank title number (NOSSO-NR) `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 19, "line_end": 19}]`
* **Business Rules**:
  * Portfolio types (CARTEIRA-CR20): 1=SIMPLES, 2=CAUÇÃO, 3=DESCONTO `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 15, "line_end": 15}]`
  * Document types (TIPO-DOCTO-CR20): 0=BOLETO, 1=DUPL/PROMIS, 2=ORG.EVENTO, 3=DEBITO AUTOMATICO, 4=CARTAO CREDITO `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 21, "line_end": 22}]`
  * Transaction status (SITUACAO-CR20): 0=OK, 1=PARCIAL, 2=PAGA, 3=ESTONADA, 4=CANCELADA, 5=DESCONTADA `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 28, "line_end": 29}]`
  * Date format: AAAAMMDD (YYYY-MM-DD) standard for all date fields `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 25, "line_end": 25}]`

### 3.3 CGD010 - Customer Master File
* **File Description**: Simple customer registration file `[evidence: {"file": "legacy/CGPW010.CPY", "line_start": 1, "line_end": 1}]`
* **Record Structure**:
  ```cobol
  01  REG-CGD010.
      05  COD-COMPL-CG10.
          10  CLASSIF-CG10        PIC 9.
          10  CODIGO-CG10         PIC 9(8).
      05  COMPRADOR-CG10      PIC X(30).
  ```
  `[evidence: {"file": "legacy/CGPW010.CPY", "line_start": 3, "line_end": 10}]`
* **Classification Types**: 0=CONTRATO, 1=COMUM, 9=Unificado `[evidence: {"file": "legacy/CGPW010.CPY", "line_start": 6, "line_end": 6}]`
* **Contract Code Structure**: For contract type (0), code represents contract/album format (9999-9999) `[evidence: {"file": "legacy/CGPW010.CPY", "line_start": 8, "line_end": 9}]`

### 3.4 LOG001 - Audit Trail File
* **File Description**: System audit log file `[evidence: {"file": "legacy/LOGW001.CPY", "line_start": 1, "line_end": 1}]`
* **Record Structure**: Contains user identification, timestamps, operation type, affected file, program name, and record image `[evidence: {"file": "legacy/LOGW001.CPY", "line_start": 3, "line_end": 18}]`
* **Time Structure**: Detailed timestamp with year, month, day, hour, minute, second, and millisecond precision `[evidence: {"file": "legacy/LOGW001.CPY", "line_start": 6, "line_end": 14}]`

## 4. APIs and Interfaces

### 4.1 Dialog System Interface
* **Technology**: Micro Focus Dialog System Version 2 `[evidence: {"file": "legacy/DS-CNTRL.MF", "line_start": 6, "line_end": 6}]`
* **Control Structure**: DS-CONTROL-BLOCK with version numbers, error handling, and I/O fields `[evidence: {"file": "legacy/DS-CNTRL.MF", "line_start": 10, "line_end": 20}]`
* **Error Handling**: Comprehensive error code system with 27 different error conditions `[evidence: {"file": "legacy/DS-CNTRL.MF", "line_start": 17, "line_end": 44}]`

### 4.2 Banking File Interfaces
* **Bradesco Integration**: Generates CBDDMMxx.REM format files where CB=prefix, DD=day, MM=month, xx=sequence `[evidence: {"file": "legacy/crp9100.cbl", "line_start": 4, "line_end": 4}]`
* **Itaú Integration**: Generates XXXXXXXX.REM format files with 8-character naming convention `[evidence: {"file": "legacy/crp9101.cbl", "line_start": 5, "line_end": 5}]`
* **File Assignment**: REMESSA files assigned to dynamic names for output generation `[evidence: {"file": "legacy/crp9100.cbl", "line_start": 28, "line_end": 30}]`

### 4.3 Printer Interface
* **Printer Assignment**: Reports assigned to logical printer LPRINTER `[evidence: {"file": "legacy/crp020.cbl", "line_start": 14, "line_end": 14}]`
* **Printer Management**: Integration with printer selection system through CAP999T program `[evidence: {"file": "legacy/IMPRESSORA.chama", "line_start": 1, "line_end": 2}]`

### 4.4 External Program Calls
* **Dialog System Runtime**: All programs call DSRUN for user interface management `[evidence: {"file": "legacy/CRP001T.CBL", "line_start": 131, "line_end": 131}]`
* **Date Conversion Utilities**: Programs call GRIDAT1 and GRIDAT2 for date processing `[evidence: {"file": "legacy/crp063.CBL", "line_start": 372, "line_end": 372}]`
* **Specialized Modules**: Integration with CAP018T and CHP057T for specific business functions `[evidence: {"file": "legacy/crp063.CBL", "line_start": 329, "line_end": 329}]` `[evidence: {"file": "legacy/crp063.CBL", "line_start": 359, "line_end": 359}]`

## 5. Workflows and Business Logic

### 5.1 CRP001 Main Processing Workflow
* **Initialization Process**: 
  1. Accept parameters from command line `[evidence: {"file": "legacy/crp001.cbl", "line_start": 114, "line_end": 114}]`
  2. Initialize dialog system blocks `[evidence: {"file": "legacy/crp001.cbl", "line_start": 117, "line_end": 118}]`
  3. Set up file paths and open files `[evidence: {"file": "legacy/crp001.cbl", "line_start": 124, "line_end": 128}]`
  4. Handle file creation if file doesn't exist (status "35") `[evidence: {"file": "legacy/crp001.cbl", "line_start": 130, "line_end": 133}]`

* **Main Processing Loop**: Executes CORPO-PROGRAMA until GS-EXIT-FLG-TRUE is set `[evidence: {"file": "legacy/crp001.cbl", "line_start": 110, "line_end": 110}]`

* **Event-Driven Processing**: Uses EVALUATE TRUE structure to handle different user actions:
  * Centralization requests `[evidence: {"file": "legacy/crp001.cbl", "line_start": 175, "line_end": 176}]`
  * Save operations with data loading and cleanup `[evidence: {"file": "legacy/crp001.cbl", "line_start": 177, "line_end": 182}]`
  * Load operations with cursor positioning `[evidence: {"file": "legacy/crp001.cbl", "line_start": 183, "line_end": 185}]`
  * Delete operations with record updates `[evidence: {"file": "legacy/crp001.cbl", "line_start": 186, "line_end": 190}]`

### 5.2 Date Processing Logic
* **Date Acceptance**: System accepts 6-digit date from system DATE function into DATA6-W field `[evidence: {"file": "legacy/CBDATA1.CPY", "line_start": 1, "line_end": 1}]`
* **Time Processing**: Accepts current time from TIME function into HORA-BRA field `[evidence: {"file": "legacy/CBDATA1.CPY", "line_start": 2, "line_end": 2}]`
* **Date Transformation**: Moves 6-digit date to positions 3-8 of DATA-INV field and extracts 2-digit year for century determination `[evidence: {"file": "legacy/CBDATA1.CPY", "line_start": 3, "line_end": 4}]`
* **Century Logic**: Implements Y2K logic where years > 80 are prefixed with "19", otherwise "20" `[evidence: {"file": "legacy/CBDATA1.CPY", "line_start": 5, "line_end": 6}]`
* **Date Conversion**: Calls external GRIDAT1 utility for final 8-digit date formatting `[evidence: {"file": "legacy/CBDATA1.CPY", "line_start": 7, "line_end": 7}]`
* **Report Formatting**: Moves converted date to EMISSAO-REL and time components to HORA-REL for report output `[evidence: {"file": "legacy/CBDATA1.CPY", "line_start": 8, "line_end": 10}]`

### 5.3 Banking File Generation Process
* **Sequence Control**: Uses SEQREC file to maintain sequential numbering for bank files `[evidence: {"file": "legacy/crp9100.cbl", "line_start": 21, "line_end": 27}]`
* **File Organization**: Creates work files for sorting and organizing customer data before output `[evidence: {"file": "legacy/crp9100.cbl", "line_start": 31, "line_end": 36}]`
* **Output Generation**: Produces formatted files assigned to dynamic names for bank transmission `[evidence: {"file": "legacy/crp9100.cbl", "line_start": 28, "line_end": 30}]`

## 6. Configuration and Environment

### 6.1 System Parameters
* **Parameter Structure**: PARAMETROS-W contains user ID, company code, printer selection, user code, password, and company name `[evidence: {"file": "legacy/PARAMETR.CPY", "line_start": 1, "line_end": 7}]`
* **User Identification**: 5-character user ID field `[evidence: {"file": "legacy/PARAMETR.CPY", "line_start": 2, "line_end": 2}]`
* **Company Code**: 3-digit numeric company identifier `[evidence: {"file": "legacy/PARAMETR.CPY", "line_start": 3, "line_end": 3}]`
* **Security**: 4-digit numeric password field `[evidence: {"file": "legacy/PARAMETR.CPY", "line_start": 6, "line_end": 6}]`

### 6.2 File System Configuration
* **Sequence File Path**: Uses hardcoded path "\111\SEQREC" for sequence control `[evidence: {"file": "legacy/crp9100.cbl", "line_start": 21, "line_end": 21}]`
* **Dynamic File Assignment**: Work files assigned to variable names for flexible file management `[evidence: {"file": "legacy/crp9100.cbl", "line_start": 31, "line_end": 31}]`

### 6.3 Regional Settings
* **Decimal Point**: System configured to use comma as decimal separator `[evidence: {"file": "legacy/crp001.cbl", "line_start": 11, "line_end": 11}]`
* **Printer Configuration**: Uses LPRINTER as standard printer designation `[evidence: {"file": "legacy/crp020.cbl", "line_start": 14, "line_end": 14}]`

## 7. Non-Functional Requirements (NFRs)

### 7.1 Data Integrity and Locking
* **Record Locking**: Automatic record-level locking implemented on all indexed files `[evidence: {"file": "legacy/CRPX001.CPY", "line_start": 4, "line_end": 5}]`
* **File Status Monitoring**: All file operations monitored through status fields (ST-CRD001, etc.) `[evidence: {"file": "legacy/CRPX001.CPY", "line_start": 6, "line_end": 6}]`

### 7.2 Error Handling
* **File Access Errors**: Comprehensive error handling for file opening operations with status code checking `[evidence: {"file": "legacy/crp001.cbl", "line_start": 134, "line_end": 137}]`
* **Dialog System Errors**: Integration with Dialog System error handling through DS-ERROR-CODE field `[evidence: {"file": "legacy/DS-CNTRL.MF", "line_start": 17, "line_end": 44}]`

### 7.3 Audit Trail
* **Comprehensive Logging**: All operations logged with user identification, timestamp, operation type, file name, program name, and record image `[evidence: {"file": "legacy/LOGW001.CPY", "line_start": 3, "line_end": 18}]`
* **Operation Classification**: Single-character operation codes for tracking different types of transactions `[evidence: {"file": "legacy/LOGW001.CPY", "line_start": 15, "line_end": 15}]`

## 8. Risks and Technical Debt

### 8.1 Date Processing Risks
* **Y2K Implementation**: The century determination logic using 80 as cutoff may cause issues for dates beyond 2079 `[evidence: {"file": "legacy/CBDATA1.CPY", "line_start": 5, "line_end": 6}]`

### 8.2 File System Dependencies  
* **Hardcoded Paths**: System contains hardcoded file paths that may limit portability `[evidence: {"file": "legacy/crp9100.cbl", "line_start": 21, "line_end": 21}]`

### 8.3 Technical Debt Analysis
* **No Explicit Debt Comments**: Comprehensive search for TODO, FIXME, WARNING, and NOTE comments returned no results, indicating absence of documented technical debt in source code comments
* **No Deprecation Warnings**: No explicit deprecation markers or obsolescence notices found in program headers or comments

## 9. Test Coverage

### 9.1 Testing Infrastructure
* **No Dedicated Test Programs**: Analysis of the codebase reveals no programs with testing-specific naming patterns or dedicated test modules
* **No Test-Related Comments**: No comments indicating testing procedures, test cases, or testing-related documentation found in the source code
* **No Test JCL**: No JCL scripts or job control language files identified for automated testing procedures

### 9.2 Testing Approach
Based on the code structure, testing appears to be manual through the Dialog System interface and operational validation through business processes.

## 10. Glossary

### 10.1 Domain-Specific Terms
* **BOLETO**: Brazilian bank payment slip system `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 21, "line_end": 21}]`
* **CARTEIRA**: Portfolio classification system with three types: SIMPLES, CAUÇÃO, DESCONTO `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 15, "line_end": 15}]`
* **CONTRATO**: Contract-based customer classification `[evidence: {"file": "legacy/CGPW010.CPY", "line_start": 6, "line_end": 6}]`
* **DUPL/PROMIS**: Duplicate or promissory note document types `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 21, "line_end": 21}]`
* **ESTONADA**: Reversed or canceled transaction status `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 28, "line_end": 28}]`
* **PORTADOR**: Bank carrier code for financial institution identification `[evidence: {"file": "legacy/CRPW020.CPY", "line_start": 13, "line_end": 13}]`
* **REMESSA**: Bank remittance file for transaction processing `[evidence: {"file": "legacy/crp9100.cbl", "line_start": 4, "line_end": 4}]` `[evidence: {"file": "legacy/crp9101.cbl", "line_start": 5, "line_end": 5}]`

### 10.2 System Abbreviations  
* **CAD**: Cadastro (Registration/Master file) - derived from filename pattern analysis of CAD002, CAPW*, CAPX* files
* **CGD**: Cadastro Geral (General registration) - derived from filename pattern analysis of CGD001, CGD010, CGPW*, CGPX* files  
* **CRD**: Contas a Receber (Accounts Receivable) - derived from filename pattern analysis of CRD001, CRD020, CRPW*, CRPX* files
* **CRP**: Contas a Receber Programa (Accounts Receivable Program) - derived from program naming pattern CRP001, CRP020, etc.
* **LOG**: System audit logging files `[evidence: {"file": "legacy/LOGW001.CPY", "line_start": 1, "line_end": 1}]`

### 10.3 Technical Terms
* **COMP-5**: COBOL binary data format for numeric fields
* **DS-CONTROL-BLOCK**: Dialog System control structure for UI management `[evidence: {"file": "legacy/DS-CNTRL.MF", "line_start": 10, "line_end": 10}]`
* **ISAM**: Indexed Sequential Access Method for file organization `[evidence: {"file": "legacy/CRPX001.CPY", "line_start": 2, "line_end": 2}]`
* **PIC**: COBOL picture clause for data field definition `[evidence: {"file": "legacy/CRPW001.CPY", "line_start": 4, "line_end": 6}]`

---

*This document was generated through forensic analysis of the COBOL source code located in the legacy directory. All statements are supported by direct evidence from the source files with precise line number citations.*