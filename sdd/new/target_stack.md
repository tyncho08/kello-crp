Target Stack - Expressed as a Prompt
You are the Tech Stack Implementation Agent for a modern web app.

## Stack (must use)
- Frontend: Next.js (App Router) + TypeScript + TailwindCSS.
- Backend: NestJS (TypeScript) for core APIs and domain logic.
- AI lane: (Python/FastAPI for AI-only endpoints like /embed, /retrieve, /generate).
- Database: PostgreSQL (use Prisma as the ORM).
- Streaming: Server-Sent Events (SSE) for long-running/AI responses.
- Auth: Auth.js (OIDC/JWT) with session/JWT usable by Next.js and NestJS.
- Observability: OpenTelemetry traces/logs/metrics end-to-end.

## Deployment (prefer)
- Prefer Vercel for the Next.js app and Heroku for NestJS (and the Python service, if used).
- Keep 12-factor config; all secrets via environment variables.
- Provide Dockerfiles for local dev and CI.

## Deliverables
1) Monorepo scaffold:
   /apps/web      → Next.js (TS, Tailwind, App Router; minimal Server Actions)
   /apps/api      → NestJS (REST, OpenAPI; modules per domain)
   /apps/ai       → (FastAPI for AI endpoints; SSE streaming)
   /packages/ui   → shared React UI components
   /packages/contracts → OpenAPI spec + generated typed clients (web + Python)
   /infra         → docker-compose for local Postgres; Heroku/Vercel deploy configs

2) Core features:
   - Health endpoints and version info in each app.
   - Auth: login/logout, protected routes, and token validation across Next/Nest.
   - Postgres schema + Prisma migrations (users, sessions, documents, messages).
   - Example feature: “RAG search” flow:
     web → api (Nest) → ai (FastAPI) → Postgres (pg + pgvector if needed).
   
3) Quality bar:
   - Type-safe clients generated from OpenAPI into web and ai.
   - E2E tests for one happy path (Playwright) + unit tests (Vitest/Jest, Pytest).
   - Lint/format/typecheck in CI; precommit hooks.
   - Basic OTel tracing (web → api → ai) with sample collector config.

## Constraints
- Keep frontend “view-only”: put business logic in Nest modules/services.
- Use Server Actions sparingly (simple mutations only).
- No vendor lock-in beyond Vercel/Heroku configs; containers must run locally.
- Clear .env.example files for all apps.

Research
Short answer: the “modern default” you’ll see most often is TypeScript + React/Next.js on the front, with either (a) full‑stack Next.js (Server Actions + Route Handlers) for greenfield apps, or (b) Next.js in front of existing Java/.NET/Go services via a small Node/TypeScript BFF or a GraphQL gateway when enterprises modernize. AWS/Azure remain the main clouds; Kubernetes is everywhere; and Postgres is the safe, boring database choice. 
Here’s the picture with receipts and what it means for stack choices:
What’s dominant right now
Front end: React still towers over other UI frameworks, and Next.js is the leading React meta‑framework by usage in the State of JS results (57% “used it” among respondents). That aligns with broader developer surveys where React and Node/JS frameworks sit at the top. 

 Why this matters: if you want the biggest hiring pool and ecosystem, React/Next.js is the safe bet.

Full‑stack Next.js is real (and popular for new builds): With Server Actions and Route Handlers, Next can handle data mutations and APIs inside the app—no separate backend service required for many apps. Teams use this to ship fast with one TypeScript codebase. 

Enterprise back ends during “modernization”: Most large companies keep or expand Java (Spring Boot) and/or .NET and Go for services, while adopting React/Next on the front. They often add a BFF (backend‑for‑frontend) in Node/TypeScript or a GraphQL layer to insulate the UI from microservice sprawl. Netflix’s engineering blog is a well‑known example of a Node‑based BFF in front of services. The Jakarta EE survey continues to show Spring/Spring Boot as the top Java stack. 

API style: REST is still the majority, but GraphQL keeps gaining (especially for aggregating many services into UI‑ready shapes). Postman’s data shows 86% use REST and ~29% use GraphQL; Gartner/Apollo forecasts have pushed enterprises toward GraphQL gateways and federation over time. 

Cloud & infra: AWS and Azure are neck‑and‑neck overall (AWS tends to lead with SMBs; Azure is very strong in the enterprise), while multi‑cloud/hybrid is normal. Containers are now standard and Kubernetes is widely used in production (CNCF reports 91% using containers in production in 2024). 

Databases: PostgreSQL continues to be the default OLTP pick for modern app work, ranking in the top 4 on DB‑Engines and earning “DBMS of the Year 2023.” 

So… do people use Next.js front‑to‑back, or Next.js + Java back end?
Both patterns are common; it depends on the starting point.
Greenfield / startup / product teams often go all‑TypeScript with full‑stack Next.js (Server Actions/Route Handlers), Postgres (Prisma/Drizzle), and deploy to Vercel/AWS. It’s minimal coordination overhead, fast DX, and plenty of scale headroom. 

Modernizing companies typically keep Java/.NET/Go microservices, add Next.js for the web UI, and place a Node/TypeScript BFF or GraphQL in front to shape backend data for the UI. Netflix’s published BFF usage is the canonical example of this pattern. 

What tech stacks teams are moving toward (by scenario)
Greenfield web app (ship fast, small team):

 Next.js 14+ (TS) + Server Actions/Route Handlers → Postgres → host on Vercel/AWS. Add Auth.js/Clerk, Prisma/Drizzle, and Redis for caching/queues if needed. (Leans on the dominant React/Next ecosystem; full‑stack TypeScript.) 

Enterprise UI modernization (keep core services):

 Next.js for the web, Node/TS BFF or GraphQL gateway → existing Java (Spring Boot)/.NET/Go microservices → Kubernetes on AWS/Azure. You get modern UX while preserving battle‑tested services; the BFF decouples the UI from backend churn. 

AI/ML‑heavy product:

 Next.js front‑end + Python FastAPI (for model endpoints) alongside your main services; Postgres for app data + object store for artifacts; deploy on AWS/Azure with GPUs where needed. (Common pattern; aligns with language strengths and cloud maturity.) [General practice; no single canonical source]

A few useful market signals behind these picks
React/Next dominance: React tops front‑end usage; Next.js leads meta‑framework usage by a wide margin in State of JS. Stack Overflow’s 2025 survey data keeps React/Node/Next in the front‑of‑mind set for working pros. 

Spring Boot remains king in Java land: That’s why so many “Next.js + existing Java” combos show up in modernization programs. 

REST still the baseline, GraphQL growing: Most teams stick to REST; GraphQL is expanding where UIs need aggregated data from many microservices or where schema‑based contracts help large orgs. 

Cloud reality: AWS/Azure lead usage; multi‑cloud is common; containers & K8s are the default for anything beyond serverless‑only apps. 

Postgres is the boring, good choice: Top‑ranked and still climbing; widely available as a managed service across clouds. 

Practical combos I recommend (pick what matches your context)
All‑TS full‑stack (fastest to value):

 Next.js 14+ (App Router + Server Actions), Postgres, Prisma/Drizzle, Auth.js, deploy on Vercel or AWS (Lambda/Edge). Great for new products and internal tools. 

UI refresh without backend rewrites:

 Next.js + Node/TS BFF (or Apollo GraphQL) in front of existing Java/.NET/Go services; deploy on K8s in AWS/Azure. This is the most common enterprise modernization path I see reported. 

Data‑/ML‑centric app:

 Next.js UI + Python FastAPI for model endpoints + Postgres; batch/stream jobs on K8s; REST or GraphQL at the edge depending on consumer needs. (Common industry practice; pair with the cloud you already standardized on.)

Got it. For a COBOL → cloud reference implementation, I’d ship this stack and migration shape. It plays nice with mainframe realities (CICS/IMS, Db2 z/OS, MQ), lets you move in safe slices, and leaves room for AI where it helps.
Recommended target stack
Front end
Next.js (TypeScript, App Router). Fast to build, easy hiring.

Edge/BFF
Node/TypeScript (Fastify or NestJS), REST with OpenAPI. Keeps the web app clean and hides backend complexity.

Domain services (pick one lane)
TypeScript-first orgs: NestJS services, Postgres, Prisma/Drizzle, Kafka.

Java-heavy orgs: Kotlin/Java with Spring Boot, Postgres, Kafka. Same patterns (hexagonal, outbox, idempotency).

AI & data helpers
Python FastAPI service for RAG, code/search assistants, batch transforms.

Data
PostgreSQL for new app data.

Kafka for events and change streams.

Platform
AWS EKS (Kubernetes), RDS (Postgres), MSK (Kafka).

Observability: OTel + your vendor.

How you bridge the mainframe (the key bit)
Use whichever of these the z/OS team is comfortable enabling; you can mix them during transition:
Expose transactions as REST/JSON

 Via IBM z/OS Connect EE to front CICS/IMS/Db2. Your BFF calls clean HTTP endpoints while z/OS Connect handles schema mapping, data conversion, and auth. 

Asynchronous integration via MQ

 For decoupled, back-pressure-friendly calls, put IBM MQ between the cloud services and CICS (adapter/bridge options exist). Great for batchy or high-latency work. 

Live data mirror for read-heavy features

 Use IBM Data Replication (CDC) to stream Db2 z/OS changes into Kafka; consume into Postgres for read models. This unlocks modern UX fast without touching COBOL first. (Debezium has emerging z/OS support, but today the stable path is IBM’s CDC engines.) 

Cutover plan (strangler pattern)
Inventory and wrap: expose a thin REST layer for the highest-value CICS transactions (z/OS Connect) and stand up CDC to Kafka for their data. 

Build new beside old: implement the first cloud service that uses the mirrored data for reads; keep writes on the mainframe.

Dual-write and switch: introduce the outbox pattern in the new service, write to Postgres and publish events; flip individual endpoints when parity is proven.

Retire in slices: keep routing more capabilities to the new path until the legacy slice can be turned off. This is the classic strangler fig approach. 

What goes in the reference repo
/apps/web: Next.js app hitting a sample “Order Inquiry” that’s actually backed by z/OS Connect for writes and Postgres (CDC mirror) for reads.
/apps/bff: Node/TS API that talks REST to z/OS Connect, MQ for async flows, and Kafka for events; OpenAPI with generated clients.
/services/order: Either NestJS or Spring Boot, with outbox + Kafka; Postgres schema and migrations.
/services/ai: FastAPI RAG endpoint for docs and code-assist (optional but persuasive in demos).
/infra: Terraform + Helm for EKS/RDS/MSK; GitHub Actions; OTel collector.
/tools: Seed scripts to stand up CDC topics and load sample data.

Why this works for COBOL migrations
You can start without ripping the core: expose a few APIs, mirror data, and ship a modern UI immediately. z/OS Connect and CDC are built for exactly this bridge. 

You can prove value in weeks: a read-only dashboard off the mirror, then one transactional path via z/OS Connect, then deeper re-implementation when ready.

You stay polyglot where it pays: TypeScript for product speed, Java where your ops and risk teams are strongest, Python where AI/data shine.

If you tell me your mainframe setup (CICS vs IMS, Db2 footprint, MQ usage) and your team’s preferred language lane, I’ll tailor this into a concrete repo plan with the first two slices implemented and sample CDC wiring.
