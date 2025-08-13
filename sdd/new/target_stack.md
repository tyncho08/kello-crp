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