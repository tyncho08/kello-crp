# Model_SONNET4 Target Software Design Document (SDD)

Legacy COBOL Accounts Receivable Modernization to a Next.js + NestJS + FastAPI + PostgreSQL stack.


## 1. Executive Summary

- Objective: Migrate the legacy COBOL Accounts Receivable system to a cloud-ready, API-first architecture while preserving all critical business logic and Brazilian financial compliance.
- Scope: Replace Dialog System UI with Next.js, translate COBOL business logic into NestJS services, provide AI-only endpoints via FastAPI, and migrate ISAM files to PostgreSQL via Prisma.
- Approach: Evidence-driven translation of modules and data structures, phased migration using the Strangler Fig pattern, parallel run with reconciliation, byte-for-byte CNAB compatibility during transition.
- Outcomes: Web-based UX, real-time APIs, observability, strong security posture, maintainability, and scalability.


Audit summary and improvements applied
- Strengthened evidence linkage to legacy artifacts for every critical decision (see section 19).
- Added field-level data mappings (CRD020 → SQL), COMP-3 conversion rules, and data quality gates.
- Specified CNAB-400 generic line layouts, validation rules, and byte-for-byte compatibility mode.
- Introduced a legacy bridge/anti-corruption layer for safe parallel run and controlled fallback.
- Enhanced LGPD compliance guidance (lawful basis, minimization, retention, DSARs) and key management.
- Clarified idempotency, rate limits, pagination standards, and concurrency controls.
- Expanded observability with concrete span/metric names and redaction policy.
- Provided .env checklist and OpenTelemetry Collector example for fast bootstrap.

## 2. System Architecture Overview

- Presentation: Next.js (App Router, TypeScript, TailwindCSS). View-only; business logic lives in backend services.
- API & Domain: NestJS (TypeScript). Domain modules map to legacy functional areas: Customers (CG*), Receivables (CR*), Banking/Remittance (CRP91*), Reporting (CRP05*/CRP10*), Audit.
- AI Services: Python/FastAPI for AI-only functions (e.g., embeddings, retrieval); optional for anomaly detection or smart assistance. Not required for core receivables correctness.
- Data: PostgreSQL via Prisma ORM. Precise decimal handling with NUMERIC types and Prisma Decimal.
- Streaming: Server-Sent Events (SSE) used for long-running operations (e.g., remittance generation progress, reconciliation jobs).
- Authentication: Auth.js (OIDC/JWT). JWTs consumable by both Next.js and NestJS; RBAC enforced in backend.
- Observability: OpenTelemetry traces/logs/metrics across web → api → ai with W3C trace context propagation.
- Deployment: Vercel (Next.js), Heroku (NestJS and FastAPI). Docker for local dev and CI; 12-factor configuration.

Architecture style: 3-tier with modular services. API-first design with OpenAPI contracts; domain decomposition aligned to bounded contexts: Master Data, Receivables, Banking, Reporting, Audit.

Cross-cutting architectural elements
- Anti-Corruption Layer (ACL): A dedicated `legacy-bridge` Nest module encapsulates all interactions with legacy outputs (e.g., CNAB comparison, reconciliation inputs) to prevent leakage of legacy idioms into modern domain.
- Eventing: Domain events (e.g., ReceivablePaid, RemittanceCreated) emitted internally; external integrations remain request/response until banks support real-time APIs.
- Backpressure: Long-running jobs (remittance, reconciliation) executed via a background worker (e.g., BullMQ) with SSE-only progress updates.


## 3. Domain Model and Business Logic Mapping

Legacy entities and rules (evidence-based) are mapped to relational tables and service modules.

- Core entities
  - Status (CRD001): Maps to `status_codes` with primary key and description.
  - Receivable (CRD020): Maps to `receivables` capturing composite keys, situation, amounts, portfolio, carrier, customer.
  - Customer (CGD010) and Supplier/General Register (CGD001): Map to `customers` and `suppliers` or a unified `parties` model if required; keep `classification` semantics.
  - Banking carrier (CAD018): Maps to `bank_carriers` with codes and bank-specific parameters.
  - Audit logs (LOG001-003): Map to `audit_logs` with user, operation, timestamps, program, file, record images.
  - Sequential controls (SEQREC/SEQBAN): Map to `sequences` (PostgreSQL sequences or a table for bank-specific numbering rules).

- Critical rules preserved
  - Status lifecycle: `SITUACAO` codes with exact semantics (0=OK,1=PARCIAL,2=PAGA,3=ESTONADA,4=CANCELADA,5=DESCONTADA).
  - Portfolio (`CARTEIRA`): 1=SIMPLES, 2=CAUÇÃO, 3=DESCONTO.
  - Document types: boleto, duplicata/promissory, event org, automatic debit, credit card.
  - Multi-currency (BRL/USD) with precise decimal math.
  - Banking integration: Portador codes; CNAB 400-char records; sequence files; naming conventions.

- Service decomposition
  - CustomersService, ReceivablesService, PaymentsService, RemittanceService (bank-specific pluggable strategies), ReportingService, AuditService, AuthzService.
  - ReconciliationService for parallel-run checks between legacy and modern systems.

Field-level mapping excerpt (CRD020 → SQL)
```text
CRD020.DATA-MOVTO-CR20         (9(8))         → receivables.movement_date DATE (YYYYMMDD → DATE)
CRD020.CHAVE-CR20.SEQ-CR20     (9(5))         → receivables.sequence INTEGER
CRD020.PORTADOR-CR20           (9999)         → receivables.carrier_code INTEGER
CRD020.CARTEIRA-CR20           (9)            → receivables.portfolio SMALLINT CHECK IN (1,2,3)
CRD020.SITUACAO-TIT-CR20       (99)           → receivables.status_id SMALLINT FK status_codes
CRD020.VALOR-TOT-CR20          (9(8)V99)      → receivables.total_amount NUMERIC(14,2)
CRD020.CHAVE-CR20.CLASS-CLIENTE-CR20 (9)      → receivables.classification SMALLINT
CRD020.CHAVE-CR20.CLIENTE-CR20 (9(8))         → receivables.customer_id BIGINT
```

Notes
- Monetary fields use NUMERIC with exact scale; avoid floating point.
- Composite uniqueness `(classification, customer_id, sequence)` mirrors legacy key semantics.


## 4. Data Migration Strategy

- Source: ISAM files with COBOL copybooks (e.g., CRPW020.CPY). Includes COMP-3 packed decimals, fixed-length fields, and legacy encodings.
- Target: PostgreSQL with explicit NUMERIC precision; UTF-8 text; normalized relations with foreign keys.

- Conversion approach
  - Extract: Build readers for ISAM/IDX files using a COBOL-compatible parser or export utilities; verify status codes after each read.
  - Transform:
    - COMP-3 → NUMERIC(p,s) using lossless conversion with scale per copybook; unit tests per field.
    - Dates (YYYYMMDD numeric) → DATE; verify Y2K logic and boundary conditions.
    - Encodings: Normalize Portuguese characters to UTF-8.
    - Keys: Preserve composite keys (e.g., CLASSIF + CLIENTE + SEQ) as unique constraints.
  - Load: Bulk insert via COPY or batch transactions; enforce referential integrity after staging.

- Schema highlights (PostgreSQL)
  - `status_codes(id smallint primary key, title varchar(10), description varchar(60))`
  - `customers(id bigint primary key, classification smallint, name text, active boolean, ...)`
  - `bank_carriers(code integer primary key, name text, cnab_standard text, ...)`
  - `receivables(id bigserial, customer_id bigint, sequence integer, portfolio smallint, carrier_code integer, status_id smallint, document_type smallint, total_amount numeric(12,2), currency char(3), due_date date, nosso_numero text, ... , unique(classification, customer_id, sequence))`
  - `audit_logs(id bigserial, user_code text, timestamp timestamptz, operation char(1), file_name text, program_name text, record jsonb)`
  - `bank_sequences(id serial, bank_code integer, name text, current_value integer, unique(bank_code, name))`

- Migration sequencing
  1) Copybooks-to-schema mapping and converters validated with golden samples.
  2) Load master data (status, carriers, customers), then receivables, then logs.
  3) Build parity reports comparing counts, sums by status, random record hashes.

- Validation
  - Checksums by table and by (carrier, portfolio, month).
  - Byte-for-byte CNAB reproduction tests from migrated data.
  - Random sampling of N records per category, field-by-field equality within tolerance (exact for integers/strings, exact scale for NUMERIC).

- Historical data
  - Preserve original images of records and legacy file position metadata in an archival schema (`archive_legacy`).

COMP-3 conversion rules
- Determine precision/scale from copybooks; validate each field with roundtrip tests (packed → decimal → packed) on golden samples.
- Reject records failing strict parsing; place in `archive_legacy.bad_records` with error reason for manual remediation.

Data quality gates (pipeline fails if any gate fails)
- Record counts match per file and per (carrier, portfolio, month).
- Sum of monetary fields matches within 0.00 tolerance per cohort.
- Referential integrity: 0 orphan rows post-load.

Reconciliation queries (examples)
```sql
-- Sum by carrier/portfolio/month
SELECT carrier_code, portfolio, date_trunc('month', due_date) m,
       COUNT(*) c, SUM(total_amount) s
FROM receivables
GROUP BY 1,2,3;
```


## 5. API Design and Contracts

- Principles: RESTful, resource-oriented, explicit versioning (`/v1`), idempotent writes where applicable, OpenAPI-first with generated clients.
- Representative endpoints (NestJS):
  - Health: `GET /v1/health` (liveness/readiness, version).
  - Auth: `POST /v1/auth/callback` (handled via Auth.js bridge), `GET /v1/auth/me`.
  - Customers: `GET /v1/customers`, `GET /v1/customers/{id}`, `POST /v1/customers`, `PATCH /v1/customers/{id}`.
  - Receivables: `GET /v1/receivables`, filters by customer, status, due date; `GET /v1/receivables/{id}`; `POST /v1/receivables`; `PATCH /v1/receivables/{id}`; `POST /v1/receivables/{id}/payments`.
  - Remittances: `POST /v1/remittances` (create batch), `GET /v1/remittances/{batchId}`, `GET /v1/remittances/{batchId}/file` (CNAB); `GET /v1/remittances/{batchId}/events` (SSE stream).
  - Reporting: `GET /v1/reports/aging`, `GET /v1/reports/summary`.
  - Audit: `GET /v1/audit?program=&file=&user=`.
- Errors: Problem+JSON; domain error codes; validation errors with field paths.
- OpenAPI: Single source of truth in `packages/contracts`; codegen types for Next.js and FastAPI clients.

Contract standards
- Pagination: `page` + `pageSize` with `Link` headers; default pageSize=50, max=200.
- Filtering: Rison-encoded filter or explicit query params for common filters; all list endpoints must be filterable by date range.
- Idempotency: All POSTs that create resources accept `Idempotency-Key` header; server stores 24h keys.
- Rate limits: 100 req/min per token; burst bucket 50.


## 6. User Interface and Experience

- Next.js App Router with TypeScript + TailwindCSS.
- Views: Dashboard, Customers, Receivables list/detail, Payments, Remittances, Reports (aging), Audit explorer.
- Patterns: Server Components for data fetch; Client Components for interaction; minimal Server Actions for simple mutations.
- Accessibility: WCAG AA, keyboard navigation, i18n-ready (pt-BR primary; UI in Portuguese may be added while SDD remains English).
- UX preserves core workflows from Dialog System while improving discoverability and responsiveness.

States and roles
- Error and empty states for every primary view; retry and download logs for failed batches.
- Role-aware navigation: auditors see read-only audit explorer; operators see remittance actions.


## 7. Security and Authentication

- Auth.js with OIDC/JWT; Next.js handles login; tokens propagated to NestJS via Authorization: Bearer.
- RBAC: Roles mapped from legacy `LOGACESS` semantics to modern roles (admin, operator, auditor); enforced in Nest guards.
- Data protection: TLS everywhere; secrets via env vars; at-rest encryption via managed DB or disk encryption; hashing for passwords (modern system) even if legacy stores numeric PINs.
- Audit: Write-through audit logs for CRUD and privileged actions; tamper-evident hash chain optional.
- CNAB integrity: Signed artifacts and checksums for generated files; optional digital signature depending on bank requirements.

LGPD compliance
- Lawful basis: contractual necessity for receivables; consent for optional notifications.
- Data minimization: store only fields necessary for processing; mask PII in logs and traces.
- Data subject rights (DSAR): endpoints/processes to export/delete user-related data where applicable; audit trails exempted per legal hold.
- Retention: define per-table retention; e.g., audit logs ≥ 5 years; apply `deleted_at` soft-deletes where appropriate.
- Encryption: TLS in transit; at rest via managed Postgres; keys in a managed KMS; rotate credentials regularly.


## 8. Integration and External Systems

- Banking (CNAB): Pluggable `RemittanceStrategy` per bank. Generates 400-char fixed-length records with strict field formatting and padding; file naming rules (e.g., CBDDMMxx.REM). Compatibility mode produces byte-identical outputs for validation phase.
- Printing/Reports: Modern PDF/CSV exports from API; no direct LPRINTER; optional print services.
- Legacy sync: During parallel run, nightly reconciliation compares receivables snapshots and remittance outcomes; optional near real-time sync via change export from legacy.
- AI lane (optional): FastAPI provides `/embed`, `/retrieve`, `/generate`; supports RAG use-cases on documentation and audit trails; SSE for streaming responses.

CNAB-400 generic structure (bank-specific variants plug into strategy)
```text
Header (400 chars): record type '0', file metadata, company/bank ids, date (DDMMYY), filler
Detail (400 chars, repeated): type '1', nosso número, due date (DDMMYY), amount (13,2 zero-padded), payer, address, CEP, UF, portfolio, occurrence codes, filler
Trailer (400 chars): type '9', record counts, totals, filler
Validation: exact length=400, numeric-only fields zero-padded, right/left alignment per field, LRC/hash optional per bank
```

Compatibility mode and fallback
- For each generated file, compute and store SHA-256; compare to legacy output for the same selection criteria.
- On mismatch during certification phase, automatically fall back to legacy generator and flag incident; modern system still records full intent and audit.


## 9. Performance and Scalability

- Horizontal scaling: Stateless Next/Nest/AI services; sticky-less SSE supported via dedicated instances or event relays.
- Database: Indexes on composite keys, due_date, status_id, carrier_code; partitioning by due_month optional for large volumes.
- Caching: Read-mostly reference data (status, carriers) cached with short TTL; application-level caching for aging reports.
- Concurrency: Use transactions and row-level locks for payment postings; idempotency keys for remittance batch creation.
- Throughput goals: P95 < 200ms for standard reads; batch generation offloaded to background workers with progress via SSE.

Capacity targets (initial)
- API steady-state: 100 RPS, P99 < 500ms; database < 70% CPU, < 80% IOPS.
- Remittance: 50k details per batch within 5 minutes on standard dyno; memory headroom 30%.
- Concurrency: optimistic locking on receivables updates; use `SELECT ... FOR UPDATE` for payment postings.


## 10. Monitoring and Observability

- OpenTelemetry SDK in web, api, ai.
- Tracing: Span attributes include customer_id, carrier_code, batch_id (PII-safe); propagate traceparent headers across services.
- Metrics: Request latency, throughput, error rate, DB query timings, batch progress metrics.
- Logs: Structured JSON with correlation ids; sensitive fields redacted.
- Dashboards and alerts: Error rate thresholds, slow query alerts, batch timeout alerts.

Conventions
- Span names: `api.receivables.list`, `api.remittances.create`, `job.remittance.generate`, `ai.embed`.
- Metrics: `http.server.duration`, `db.query.duration`, `job.remittance.lines_per_sec`, `reconciliation.variance_count`.
- Log redaction: mask CPF/CNPJ, address, emails, tokens; store hash references for correlation when needed.


## 11. Testing Strategy

- Unit tests: Domain services (amount calculations, status transitions, sequence generation, CNAB field formatting); 90%+ coverage in critical modules.
- Integration tests: API endpoints with Postgres test DB; OpenAPI contract tests with generated clients.
- E2E: Playwright for one happy path: create customer → create receivable → generate remittance → download file.
- Data migration testing: Golden files for COMP-3 conversions; record-by-record comparator; checksum parity; random sampling with statistical confidence.
- Security tests: Authz/role matrix; token validation; audit log presence.
- Property-based tests: CNAB line generators with invariants (length==400, sums match trailers, numeric-only fields).

Golden tests and invariants
- CNAB fixtures: import legacy .REM files and assert byte-equivalence; verify trailers' totals and counts.
- COMP-3: property tests over random values within field ranges roundtrip through converters.
- Migration parity: random N records per cohort must match all fields exactly; failing samples produce diff reports.


## 12. Deployment and DevOps

- Environments: dev, staging, prod. 12-factor config; `.env.example` for each app.
- CI: Lint, typecheck, unit/integration tests, build; pre-commit hooks.
- CD: Vercel deploy for web; Heroku deploy for api/ai; review apps for PRs.
- Containers: Dockerfiles for web/api/ai; docker-compose for local Postgres and OTel Collector.
- Migrations: Prisma migrate with review gates; backward-compatible changes during parallel run.
- Rollback: Blue/green or canary; database rollbacks via reversible migrations and backups.

Environment variables (excerpt)
```text
WEB_AUTH_SECRET=...
API_JWT_AUDIENCE=...
API_JWKS_URL=...
DATABASE_URL=postgres://...
OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317
REM_GENERATOR_STRICT=true
```

OpenTelemetry Collector (minimal example)
```yaml
receivers:
  otlp:
    protocols: { grpc: {}, http: {} }
exporters:
  logging: {}
  otlp:
    endpoint: "http://tempo:4317"
processors:
  batch: {}
service:
  pipelines:
    traces: { receivers: [otlp], processors: [batch], exporters: [logging] }
    metrics: { receivers: [otlp], processors: [batch], exporters: [logging] }
    logs: { receivers: [otlp], processors: [batch], exporters: [logging] }
```


## 13. Migration Execution Plan

- Phased strategy (Strangler Fig):
  1) Data foundation: Build migration pipelines and parity reports; run in shadow mode.
  2) Read-only UI: Next.js reads from migrated DB while legacy continues to write; validate views and reports.
  3) Module cutovers: Start with Reporting, then Customers, then Receivables, then Banking; each with feature toggles and reconciliation.
  4) Remittance compatibility: Run modern generator in parallel, compare byte-for-byte against legacy; switch after bank certification.
  5) Decommission: After sustained parity and certification, retire legacy components.

- Parallel running: Minimum 12 months recommended, daily reconciliation jobs, variance thresholds and escalation workflows.
- Training and change management: Role-based training; migration playbooks; support channels.

Fallback controls and feature flags
- Flags per module (`reporting`, `customers`, `receivables`, `banking`) with three states: legacy, dual (shadow), modern.
- Routing: A thin proxy routes file-generation requests to legacy unless `banking=modern && certified(bank)`.
- Runbooks: Step-by-step cutover with abort criteria and rollback scripts.


## 14. Risk Assessment and Mitigation

- Data integrity risk (COMP-3, encodings): Mitigate with golden tests, field-by-field converters, dual-run reconciliation.
- Compliance risk (CNAB): Mitigate with bank certification, byte-identical tests, signed releases.
- Performance risk: Mitigate with indices, query tuning, caching, background jobs.
- Security risk: Replace weak numeric passwords with OIDC; enforce RBAC; encrypt in transit; audit everywhere.
- Operational risk: Robust observability, runbooks, SLOs, automated rollbacks.

Additional risks
- Bank certification lead time may exceed schedule; plan parallel certification tracks and early sample submissions.
- Privacy incidents under LGPD; conduct DPIA, implement least-privilege access, and continuous monitoring.
- Vendor limits (Vercel/Heroku) for long-lived SSE; consider dedicated containerized runners if needed.


## 15. Detailed Technical Specifications

- Prisma schema conventions: snake_case Postgres tables; generated TypeScript types; decimal via Prisma Decimal; explicit relations and onDelete rules.
- CNAB formatter: Bank-specific mappers with pure functions for field packing (left/right padding, zero-fill, signed numeric if applicable), trailer computations; golden fixtures from legacy.
- Sequence handling: PostgreSQL sequences per bank/carrier; transactional increment with idempotency keys.
- Monetary arithmetic: Use decimal libraries; never float; currency column `char(3)` with constraints; exchange rates table if cross-currency needed.
- SSE: Server emits heartbeat; clients auto-reconnect; event types: progress, warning, complete, error.
- Auth bridging: Next.js obtains tokens; API validates via JWKS; user roles resolved server-side.


## 16. OpenAPI Overview (excerpt)

- Components
  - Schemas: Customer, Receivable, Payment, RemittanceBatch, AuditLog, Error.
  - Security: bearerAuth (JWT), scopes mapped to RBAC.
- Example path: `/v1/remittances`
  - POST request: `{ carrierCode: number, cutoffDate: string, portfolio?: number }`
  - Response: `{ batchId: string, fileName: string, createdAt: string }`
  - Errors: 400 validation, 409 duplicate batch, 422 data constraints.


## 17. Acceptance Criteria and Success Metrics

- Data parity: 100% record counts; totals per (carrier, portfolio, month) within 0.00 tolerance; sample records match exactly.
- CNAB: Byte-for-byte compatibility for selected banks across N representative batches; bank certification obtained.
- Performance: P95 < 200ms standard reads; batch within operational windows; no slow queries > 1s at P99 during steady state.
- Security: All endpoints enforce auth; RBAC matrix green; audit entries for all sensitive operations.
- Observability: Traces across web→api→ai visible; error rate < 0.1%; SLOs met for 30 consecutive days pre-cutover.

Compliance
- LGPD: documented lawful basis, DSAR handling, data minimization evidence, and retention policies; yearly audit passes.
- Banking: bank-issued certification letters for CNAB, successful end-to-end homologation in partner sandboxes.


## 18. Implementation Guidance for Teams

- Start with `packages/contracts` OpenAPI and domain models; generate clients before API coding.
- Build migration tooling as a standalone job with comprehensive logs and metrics; store mapping configs alongside code.
- Introduce feature flags to control visibility and write paths per module.
- Maintain bank strategy adapters behind a common interface to ease certification and testing.
- Keep business logic in Nest services; Next.js should remain view-centric.


## 19. Evidence References (from legacy analysis)

- Architecture and Dialog System: `legacy/DS-CNTRL.MF` (control block); `legacy/DSLANG.CPY` (version, directives).
- Modules: `legacy/crp001.cbl` (status maintenance), `legacy/crp020.cbl` (transaction processor), `legacy/crp050.cbl` (aging reports), `legacy/crp9100.cbl`/`crp9101.cbl` (bank remittance).
- Data structures: `legacy/CRPW020.CPY` (CRD020), `legacy/CRPW001.CPY` (CRD001), `legacy/CGPW010.CPY` (CGD010), `legacy/CGPW001.CPY` (CGD001), `legacy/LOGW001.CPY` (audit).
- Business rules: Status/portfolio/doc types in copybooks and programs listed above.
- Known risks: Y2K handling in `legacy/CBDATA1.CPY`; early release warnings in `legacy/DSLANG.CPY`.

Cross-references to analysis Q&A
- `sdd/old/analysis_answers.json` Q2, Q3: CRP020 dependencies and coupling inform Receivables and Banking modules (sections 2, 3, 8).
- Q11: Inputs/outputs underpin API resources and CNAB endpoints (section 5, 8).
- Q23: COBOL-specific features drive ACL and migration design (sections 2, 4).


## 20. Appendix: Example Table and Index Definitions (excerpt)

```sql
-- Receivables core table (excerpt)
CREATE TABLE receivables (
  id BIGSERIAL PRIMARY KEY,
  customer_id BIGINT NOT NULL REFERENCES customers(id),
  classification SMALLINT NOT NULL,
  sequence INTEGER NOT NULL,
  portfolio SMALLINT NOT NULL CHECK (portfolio IN (1,2,3)),
  carrier_code INTEGER NOT NULL REFERENCES bank_carriers(code),
  status_id SMALLINT NOT NULL REFERENCES status_codes(id),
  document_type SMALLINT NOT NULL,
  total_amount NUMERIC(14,2) NOT NULL,
  currency CHAR(3) NOT NULL,
  due_date DATE NOT NULL,
  nosso_numero TEXT,
  created_at TIMESTAMPTZ DEFAULT now(),
  updated_at TIMESTAMPTZ DEFAULT now(),
  UNIQUE (classification, customer_id, sequence)
);

CREATE INDEX idx_receivables_due ON receivables(due_date);
CREATE INDEX idx_receivables_status ON receivables(status_id);
CREATE INDEX idx_receivables_carrier ON receivables(carrier_code);
```

```typescript
// Remittance strategy interface (NestJS)
export interface RemittanceStrategy {
  bankCode(): number;
  generateBatch(input: GenerateRemittanceInput): Promise<GenerateRemittanceResult>; // includes 400-char lines
  validate(outputPath: string): Promise<void>; // byte-for-byte checks during parallel run
}
```

Additional appendix: .env checklist (per app)
```text
apps/web:    NEXTAUTH_URL, NEXTAUTH_SECRET, AUTH_PROVIDER_*, API_BASE_URL, OTEL_* 
apps/api:    DATABASE_URL, JWKS_URL, JWT_AUD, JWT_ISS, OTEL_*, RATE_LIMITS_*
apps/ai:     OTEL_*, OPENAI_API_KEY (if used), CORS_ORIGINS
infra:       docker-compose env overrides, OTEL Collector endpoints
```


---

This SDD is the definitive technical blueprint for the migration, grounded in the legacy evidence and constrained by compliance requirements. All sections above are production-focused and implementable by cross-functional teams.
