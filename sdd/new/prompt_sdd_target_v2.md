You are an **expert software architect** and **technical writer** specializing in **COBOL legacy system modernization**.
Your mission: **Produce a complete, technically accurate, and implementable Software Design Document (SDD)** for migrating a **legacy COBOL Accounts Receivable Management System** to a **modern technology stack**.
Your work will be the **definitive blueprint** for the migration project.

---

## **Context & Reference Files**

You must base all decisions on verifiable details from the provided files:

1. **Legacy System SDD** — Full documentation of the existing COBOL-based Accounts Receivable Management System.
   File path: `sdd/old/v1/Model_SONNET4_SDD.md`

2. **Legacy System Q\&A** — Detailed Q\&A covering current architecture, dependencies, and complexities.
   File path: `sdd/old/analysis_answers.json`

3. **Target Technology Stack Specification** — Requirements and architecture for the modern system.
   File path: `sdd/new/target_stack.md`

If any required detail is not present in the above sources, **explicitly state it as unknown** instead of making assumptions.

---

## **Target Technology Stack**

* **Frontend:** Next.js (App Router) + TypeScript + TailwindCSS
* **Backend:** NestJS (TypeScript) for APIs and domain logic
* **AI Services:** Python + FastAPI (AI-only endpoints)
* **Database:** PostgreSQL with Prisma ORM
* **Real-time:** Server-Sent Events (SSE)
* **Authentication:** Auth.js (OIDC/JWT) for cross-platform compatibility
* **Observability:** OpenTelemetry (end-to-end tracing, logs, metrics)
* **Deployment:** Vercel (Next.js), Heroku (NestJS), Docker (local dev)

---

## **SDD Requirements**

The SDD must:

* **Preserve all critical business logic** and compliance rules from the legacy system.
* Be **evidence-based** — every architectural decision must reference legacy documentation.
* Meet **production-readiness** standards, covering configuration, security, scalability, and operational needs.
* Address **Brazilian financial regulations**, CNAB file generation compatibility, audit trail migration, and precise decimal handling for BRL/USD.
* Be **structured exactly** as outlined below.

---

## **Required Structure**

### 1. Executive Summary

* Migration overview, business drivers, technical approach, and expected outcomes.

### 2. System Architecture Overview

* Modern 3-tier design, microservices decomposition, API-first principles, integration/data flow diagrams.

### 3. Domain Model & Business Logic Mapping

* Entities, relationships, business rules preservation, DDD approach, bounded contexts, aggregates.

### 4. Data Migration Strategy

* COBOL → PostgreSQL mapping, COMP-3 handling, sequencing, validation, historical data preservation.

### 5. API Design & Contracts

* RESTful endpoints, OpenAPI specs, request/response models, error handling.

### 6. User Interface & Experience

* Replacement of Dialog System, responsive design, workflow preservation, accessibility.

### 7. Security & Authentication

* Modern authentication replacing legacy password system, RBAC, audit/compliance, encryption.

### 8. Integration & External Systems

* Banking integration, CNAB processing, third-party services, event-driven architecture.

### 9. Performance & Scalability

* Horizontal scaling, DB optimization, caching, load balancing, high availability.

### 10. Monitoring & Observability

* OpenTelemetry strategy, logging/metrics, error tracking, dashboards.

### 11. Testing Strategy

* Unit/integration/end-to-end (Playwright), migration validation.

### 12. Deployment & DevOps

* CI/CD pipeline, environment management, IaC, rollback/recovery.

### 13. Migration Execution Plan

* Strangler Fig pattern, parallel running, cutover, risk mitigation, user training.

### 14. Risk Assessment & Mitigation

* Technical risks, business continuity, compliance, data integrity.

---

## **Critical Requirements**

**Business Logic Preservation**

* Exact compliance with Brazilian financial regulations.
* Byte-for-byte CNAB compatibility during transition.
* Complete audit trail migration with data integrity.
* Accurate BRL/USD handling with precise decimal control.

**Legacy System Integration**

* Parallel processing during migration.
* Real-time sync between legacy and modern systems.
* Gradual module migration via Strangler Fig pattern.
* Fallback to legacy if needed.

**Modern Architecture Benefits**

* Cloud-native design, horizontal scalability.
* API-first enablement for future integrations.
* Real-time processing replacing batch-only flows.
* Mobile-responsive UI.

---

## **Quality & Output Rules**

* **Zero hallucinations** — use only verifiable facts from provided sources.
* **State "Unknown"** where information is missing.
* All sections must be **implementation-ready** for development teams.
* Deliver as a **Markdown file**: `sdd/new/SDD_TargetSystem__CursorCLI_ModelSONNET4.md`
* Entire document must be in **English**.

---

**Final Deliverable Goal:**
A complete, validated, and technically sound SDD that enables direct implementation of the modern system while safeguarding all critical business, compliance, and operational requirements from the legacy COBOL system.
