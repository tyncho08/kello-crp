# Modernization SDD — Legacy COBOL Accounts Receivable → Modern Stack

Version: v2 (CursorCLI ModelGPT5)
Status: Draft for implementation
Owners: Architecture / Modernization Team

---

## 1. Executive Summary

This Software Design Document (SDD) defines the architecture and plan to migrate the legacy COBOL-based Accounts Receivable Management System to a modern, cloud-ready platform. The migration preserves all critical business logic and compliance while enabling API-first access, web UI, observability, and scalable operations.

- Business drivers: replace unsupported technologies; reduce operational risk; enable integrations; improve security and auditing; meet Brazilian regulations; support real-time flows.
- Technical approach: Strangler Fig pattern with parallel run; API-first domain services on NestJS; frontend in Next.js; AI-only services in Python/FastAPI; PostgreSQL with Prisma; OpenTelemetry; SSE for streaming; Auth.js for OIDC/JWT.
- Expected outcomes: functionally equivalent system for receivables, CNAB generation, and reporting; improved UX; robust security; traceable operations; cloud deployments (Vercel/Heroku); gradual cutover.

Evidence basis:
- Legacy SDD: `sdd/old/v1/Model_SONNET4_SDD.md` (e.g., modules CRP001/CRP020/CRP050/CRP91xx, ISAM, Dialog System, CNAB) lines cited throughout.
- Legacy Q&A: `sdd/old/analysis_answers.json` (architecture, dependencies, risks).
- Target stack: `sdd/new/target_stack.md` (Next.js, NestJS, FastAPI, Prisma, OTel, SSE, Auth.js, deployments).

---

## 2. System Architecture Overview

Target architecture is a service-oriented, API-first, cloud-ready system following a modern 3-tier design with focused services and shared contracts.

- Presentation: Next.js (App Router, TypeScript, TailwindCSS). Minimal business logic; server actions sparingly.
- Domain/API: NestJS services exposing REST endpoints with OpenAPI. Modules by bounded contexts: Receivables, Masters, Banking (CNAB), Reporting, Audit.
- AI lane: Python/FastAPI for AI-only endpoints (e.g., embeddings, retrieval, generation). SSE for long-running/streaming tasks.
- Data: PostgreSQL with Prisma ORM. Optional `pgvector` for AI retrieval.
- Cross-cutting: Auth.js (OIDC/JWT), OpenTelemetry end-to-end tracing/logs/metrics, centralized configuration via env, Docker for local.
- Deployments: Vercel (web), Heroku (API/AI). Containers runnable locally.

Key integrations and flows (high level):
- Web UI → API (Nest) → DB (Postgres) and Banking adapters → File outputs (CNAB) and callbacks.
- Web UI → API → AI (FastAPI) → Back to API via SSE.

Evidence mapping to legacy:
- 3-tier structure and modular programs (Presentation/Dialog System, COBOL logic, ISAM files) [Model_SONNET4_SDD.md §1, §2; evidence lists].
- Banking CNAB generation CRP91xx [Model_SONNET4_SDD.md §2 CRP9100-CRP9119].

Unknowns:
- Exact number of concurrent users and performance SLOs: Unknown.
- Exact bank variants beyond Bradesco/Itaú detailed rules: Unknown.

---

## 3. Domain Model & Business Logic Mapping

Bounded contexts and aggregates (DDD):
- Masters Context: Status codes (CRD001), Customers (CGD010), Suppliers (CGD001). Aggregates: `Status`, `Customer`, `Supplier`.
- Receivables Context: Transactions (CRD020), Payments, Portador/Carteira, Situacao, NossoNumero. Aggregate: `Receivable` with value objects: `Amount`, `Currency`, `DueDate`, `Portfolio`, `Status`.
- Banking Context: CNAB remittance batches, sequence controls (SEQREC/SEQBAN), bank-specific layouts. Aggregates: `RemittanceBatch`, `RemittanceItem`.
- Reporting Context: Aging (CRP050), summaries, work files → reified as materialized views or optimized queries.
- Audit Context: Audit entries (LOG001-003), Access logs (LOGACESS).

Entity mappings (from legacy evidence):
- CRD001 (Title Status): code, description [Model_SONNET4_SDD.md §3 CRD001].
- CRD020 (Receivables): includes CHAVE (CLASS-CLIENTE, CLIENTE, SEQ), PORTADOR (4d), CARTEIRA, SITUACAO, VALOR-TOT (9(8)V99) [§3 CRD020].
- CGD001 (Suppliers), CGD010 (Customers) [§3 CGD001/CGD010].
- Audit LOG001 fields for user, period, operation, record [§3 LOG001].

Key business rules to preserve:
- CARTEIRA: 1=SIMPLES, 2=CAUÇÃO, 3=DESCONTO [§3 CRD020].
- SITUACAO: 0=OK, 1=PARCIAL, 2=PAGA, 3=ESTONADA, 4=CANCELADA, 5=DESCONTADA [§3 CRD020].
- TIPO-DOCTO: 0=BOLETO, 1=DUPL/PROMIS, 2=ORG.EVENTO, 3=DÉBITO AUTOMÁTICO, 4=CARTÃO CRÉDITO [§3 CRD020].
- Multi-currency BRL/USD handling [§2 CRP020 responsibilities].
- Audit operations: I/A/E with before/after images [§7 NFRs].

Unknowns requiring discovery:
- Full 58-field mapping of CRD020 (beyond excerpt) → Unknown; will extract from source copybook during migration.
- Calculation details for interest/penalties/discounts → Partially captured in legacy; complete formula set Unknown.

---

## 4. Data Migration Strategy

Goals: lossless migration of ISAM datasets to PostgreSQL; preservation of audit trails; byte-accurate CNAB reproduction during transition; precise decimal handling.

- Inventory & schema derivation:
  - Extract copybooks (e.g., CRPW020.CPY) to derive exact schemas and constraints.
  - Map COMP-3 to numeric/decimal columns with correct precision/scale; store money with DECIMAL(18,2) minimum; consider DECIMAL(19,4) for interest/FX.
- Encoding:
  - Normalize legacy encodings to UTF-8; repair Portuguese characters noted as garbled [Model_SONNET4_SDD.md §8 Character Encoding Issues].
- Keys & relationships:
  - Preserve composite keys (e.g., CHAVE-CR20) as unique indexes; add surrogate primary keys for ORM convenience.
- Sequencing and referential integrity:
  - Use staged loads: masters → transactions → audit. Enforce foreign keys post-load.
- Migration tooling:
  - Build ETL utilities (COBOL loaders or Python) to read ISAM, decode COMP-3, and bulk insert into Postgres (COPY).
  - Validation harness: row counts, checksums per file; field-by-field comparisons on samples; business-rule validations (e.g., CARTEIRA enumerations).
- Historical preservation:
  - Archive raw ISAM files in immutable storage for forensic needs.
  - Store original record images and legacy keys in shadow columns.
- Parallel sync:
  - Implement changelog capture in legacy (if feasible) or nightly replays during parallel run; reconcile daily.

Unknowns:
- Exact sizes/row counts by file: Unknown.
- True source system access method (local ISAM vs remote share): Unknown.

Evidence: [Model_SONNET4_SDD.md §3, §6, §11]; [analysis_answers.json q13].

---

## 5. API Design & Contracts

Principles: RESTful, resource-oriented; explicit versioning (/v1); OpenAPI-first; typed clients generated for web and AI services.

Top-level resources:
- /receivables, /payments, /customers, /suppliers, /statuses, /banking/remittances, /reports/aging, /audit/logs, /health, /auth.

Examples (OpenAPI excerpts):
- GET /v1/receivables?customerId=&status=&dueDateFrom=&dueDateTo=
- POST /v1/receivables { customerId, amountBRL, currency, dueDate, carteira, tipoDocumento, portador, metadata }
- POST /v1/payments { receivableId, amount, currency, method, paidAt }
- POST /v1/banking/remittances { bankCode, carteira, portador, selectionCriteria } → returns remittance id; stream status via SSE at /v1/banking/remittances/{id}/events; GET file when ready.
- GET /v1/reports/aging?portfolio=&asOfDate=
- GET /v1/audit/logs?program=&user=&operation=

Error model:
- RFC7807-like problem+json; consistent error codes; validation errors with field paths.

Auth:
- Bearer JWT from Auth.js; scopes/roles for RBAC.

Contracts delivery:
- `packages/contracts`: OpenAPI spec; generated clients for `apps/web` (TypeScript) and `apps/ai` (Python).

Evidence mapping:
- Legacy exposes no HTTP APIs [analysis_answers.json q12]. New APIs designed to cover CRP001/020/050/91xx functions per evidence in `Model_SONNET4_SDD.md`.

Unknowns:
- Bank-specific optional fields per CNAB variant beyond Bradesco/Itaú: Unknown (to be parameterized per bank adapter).

---

## 6. User Interface & Experience

- Replace Dialog System screens with responsive web UI (Next.js + TailwindCSS).
- Preserve workflows: status maintenance (CRP001), receivable lifecycle (CRP020), aging/reporting (CRP050), CNAB batch generation (CRP91xx).
- Accessibility: WCAG AA, keyboard navigation, bilingual i18n (pt-BR primary, en-US optional).
- Navigation: domain-driven sidebar; filterable datagrids; wizards for remittance creation.
- State: use React Query/Server Actions minimally; business logic stays server-side.

Evidence: Dialog System-based UI and workflows [Model_SONNET4_SDD.md §1, §4, §5].

Unknowns: exact number of UI forms and custom reports: Unknown.

---

## 7. Security & Authentication

- Replace 4-digit password mechanism with OIDC via Auth.js. Support enterprise IdPs.
- JWTs with short TTL, refresh tokens, PCKE; rotate signing keys.
- RBAC: roles (admin, operator, auditor, integration); fine-grained permissions (banking:generate, receivables:write, audit:read, etc.).
- Data protection: TLS everywhere; encrypt secrets at rest; DB encryption; KMS for key management.
- Audit: immutable append-only audit log table mirroring LOG001-003 semantics with actor, time, operation, entity, before/after hash.
- Compliance: log retention policies; consent and access tracking.

Evidence of legacy gaps to be fixed: weak numeric passwords, plain-text logs, no encryption [Model_SONNET4_SDD.md §7 Security, §8 Risks].

Unknowns: regulatory retention period specifics and DPO requirements: Unknown.

---

## 8. Integration & External Systems

- Banking integration via CNAB adapters:
  - Adapter per bank (e.g., Bradesco, Itaú). Generate CNAB400/240 with byte-for-byte compatibility during transition.
  - Sequence control persistence replacing SEQREC/SEQBAN; strict counters and idempotency.
  - Output remittance files with naming rules (e.g., CBDDMMxx.REM) [Model_SONNET4_SDD.md §4 Banking Interfaces].
  - Validation: totals, record counts, checksums; bank certification before go-live.
- File exchange and archival:
  - Store generated remittances; provide downloads; sign files (optional) and audit emission.
- Eventing:
  - SSE for long-running operations (remittance generation, AI processing).
- Third-party potential:
  - Email or webhook notifications: optional.

Unknowns: full list of partner banks and exact CNAB layout variants: Unknown.

---

## 9. Performance & Scalability

- Stateles services; horizontal scaling on Heroku; CDN caching on Vercel for static assets.
- DB tuning: indexes for composite keys (customer+seq), due date, status, portador; partitioning by company or by year if large.
- Caching: read-mostly masters cached with TTL; idempotency keys for write endpoints.
- Concurrency: transactional integrity; avoid hot-spot sequences; optimistic concurrency on aggregates.
- High availability: multi-instance API; DB backups and PITR.

Unknowns: expected throughput and latency SLOs; dataset size distribution: Unknown.

Evidence: legacy limits (ISAM locking) motivate scalable multi-user design [Model_SONNET4_SDD.md §8 Scalability].

---

## 10. Monitoring & Observability

- OpenTelemetry tracing across web → api → ai; trace IDs propagated via headers.
- Metrics: request rates, latencies, error rates, queue depths; CNAB batch metrics (records, totals); DB performance metrics.
- Logging: structured JSON; PII controls; sampling for high-volume traces.
- Dashboards: service health, banking batch status, migration reconciliation.
- Alerting: SLO breaches, CNAB validation failures, auth errors, DB saturation.

Deliverables: sample OTel collector config; basic dashboards defined in IaC.

---

## 11. Testing Strategy

- Unit tests: domain services (status transitions, amount math, BRL/USD precision), CNAB record formatting.
- Contract tests: OpenAPI schemas validated; generated clients compile and run.
- Integration tests: DB + API for end-to-end behaviors (create receivable → pay → status).
- E2E tests: Playwright for one happy path (login → create receivable → generate remittance → download file).
- Migration validation: ETL checksums; golden-record comparisons for CNAB (byte-for-byte) and sample transactions; parallel reconciliation.
- Security tests: auth, RBAC, privilege escalation, audit trails.

Evidence: lack of legacy automated tests [Model_SONNET4_SDD.md §9] drives need for strong modern test coverage.

---

## 12. Deployment & DevOps

- Repos/structure:
  - `apps/web` (Next.js), `apps/api` (NestJS), `apps/ai` (FastAPI), `packages/ui`, `packages/contracts`, `infra` (docker-compose, collector, deploy configs).
- CI/CD:
  - Lint/format/typecheck; unit/integration tests; Playwright E2E (selected); build containers; preview deployments; OpenAPI generation and client publishing.
- Environments: dev, staging, prod. `.env.example` for each app; 12-factor env vars only.
- Deploy targets: Vercel (web) and Heroku (api/ai); Docker for local dev; Postgres via addon or managed.
- Rollback: versioned releases; blue/green or phased; DB migrations with safe forward-only strategy and rollback scripts.

---

## 13. Migration Execution Plan

- Strangler Fig strategy:
  - Phase 0: Read-only data sync into Postgres; reporting PoC.
  - Phase 1: Masters (status, customers) write path moves to new API with back-write to legacy if needed.
  - Phase 2: Receivables CRUD in parallel; nightly reconciliation and drift detection.
  - Phase 3: Banking CNAB generation via new adapter; bank certification; shadow compare with legacy outputs.
  - Phase 4: Decommission legacy UI; final cutover after stable period (≥12 months recommended in legacy SDD §11).
- Parallel run:
  - Dual-write/dual-generate where feasible; reconcile daily; fall back to legacy on discrepancies.
- Training & change management:
  - Operator training; documentation; staged onboarding.

---

## 14. Risk Assessment & Mitigation

- Regulatory/CNAB mismatch → bank certification, golden files, byte-compare gates.
- Data loss or corruption → staged ETL with checksums, backups, forensic archives.
- Performance regressions → load tests, indexes, partitioning, caching.
- Security gaps → OIDC, JWT best practices, RBAC, encrypted storage, audit immutability.
- Unknown business rules in code paths → domain discovery sessions; keep legacy in parallel, add tests per scenario.
- Character encoding issues → UTF-8 normalization with automated validators.

Evidence: risks and limitations from legacy [Model_SONNET4_SDD.md §8, §11].

---

## Appendices

A. Database Schema (initial cut; subject to refinement from copybooks)
- `status` (id, code, description, created_at)
- `customer` (id, class, code, name, status, …)
- `receivable` (id, customer_id, seq, portador, carteira, situacao, tipo_documento, amount, currency, due_date, nosso_numero, …)
- `payment` (id, receivable_id, method, amount, currency, paid_at, …)
- `remittance_batch` (id, bank_code, carteira, portador, generated_by, generated_at, status)
- `remittance_item` (id, batch_id, receivable_id, value, …)
- `audit_log` (id, user, timestamp, operation, program, entity, record_before_hash, record_after_hash, payload)

B. Contracts and Types
- OpenAPI will define request/response DTOs; generated clients in `packages/contracts`.

C. Evidence References (selected)
- 3-tier/dialog system and ISAM: [Model_SONNET4_SDD.md §1.2–1.3, §4, §6]
- Modules CRP001/020/050 and CRP91xx: [Model_SONNET4_SDD.md §2]
- Data structures CRD001/CRD020/CGD001/CGD010/LOG001: [Model_SONNET4_SDD.md §3]
- Banking CNAB formats and filenames: [Model_SONNET4_SDD.md §4]
- NFRs and security gaps: [Model_SONNET4_SDD.md §7, §8]
- Modernization guidance (parallel run, certification): [Model_SONNET4_SDD.md §11]

D. Unknowns Log
- Full CRD020 58-field mapping; complete calculation formulas; partner banks list; performance SLOs; dataset sizes; regulatory retention specifics; exact UI inventory.

---

Delivery compliance:
- Evidence-based decisions with explicit Unknowns per prompt.
- English-only document.
- Targets stack and deliverables per `sdd/new/target_stack.md`.
