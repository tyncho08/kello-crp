# Prompt for Creating a Software Design Document (SDD) for Legacy COBOL to Modern Stack Migration

You are an expert software architect and technical writer specializing in legacy system modernization. Your task is to create a comprehensive Software Design Document (SDD) for migrating a legacy COBOL Accounts Receivable system to a modern technology stack.

## Context
You will be provided with three key inputs:
1. **Legacy System SDD**: Complete documentation of the existing COBOL-based Accounts Receivable Management System. File path 'sdd/old/v1/Model_SONNET4_SDD.md'
2. **Legacy System Q&A**: Detailed questions and answers about the current system's architecture, dependencies, and complexities. File path 'sdd/old/analysis_answers.json'
3. **Target Tech Stack**: Modern technology stack specifications for the new system. File Path 'sdd/new/target_stack.md'

## Your Mission
Create a professional, comprehensive SDD that serves as the blueprint for migrating from the legacy COBOL system to the modern stack. The document must be technically accurate, implementable, and account for all critical business logic preservation.

## Target Technology Stack
- **Frontend**: Next.js (App Router) + TypeScript + TailwindCSS
- **Backend**: NestJS (TypeScript) for core APIs and domain logic
- **AI Services**: Python/FastAPI for AI-only endpoints
- **Database**: PostgreSQL with Prisma ORM
- **Real-time**: Server-Sent Events (SSE) for streaming responses
- **Authentication**: Auth.js (OIDC/JWT) compatible across Next.js and NestJS
- **Observability**: OpenTelemetry for end-to-end tracing/logs/metrics
- **Deployment**: Vercel (Next.js), Heroku (NestJS), Docker for local development

## Required SDD Structure

### 1. Executive Summary
- Brief overview of the migration project
- Key business drivers and objectives
- High-level technical approach
- Expected benefits and outcomes

### 2. System Architecture Overview
- Modern 3-tier architecture design (Presentation, API, Data)
- Microservices decomposition strategy
- API-first design principles
- Integration patterns and data flow

### 3. Domain Model and Business Logic Mapping
- Core business entities and their relationships
- Critical business rules preservation from legacy system
- Domain-driven design approach
- Bounded contexts and aggregates

### 4. Data Migration Strategy
- Legacy COBOL data structures to PostgreSQL schema mapping
- Data transformation requirements (COMP-3, packed decimal handling)
- Migration sequencing and validation approach
- Historical data preservation strategy

### 5. API Design and Contracts
- RESTful API endpoints design
- OpenAPI specification structure
- Request/response models
- Error handling and validation patterns

### 6. User Interface and Experience
- Modern web interface replacing Dialog System
- Responsive design for multiple devices
- User workflow preservation and enhancement
- Accessibility considerations

### 7. Security and Authentication
- Modern authentication flow (replacing legacy password system)
- Role-based access control design
- Audit trail and compliance requirements
- Data protection and encryption

### 8. Integration and External Systems
- Banking system integration modernization
- CNAB file processing in modern architecture
- Third-party service integrations
- Event-driven architecture for real-time processing

### 9. Performance and Scalability
- Horizontal scaling capabilities
- Database optimization strategies
- Caching and performance improvements
- Load balancing and high availability

### 10. Monitoring and Observability
- OpenTelemetry implementation strategy
- Logging and metrics collection
- Error tracking and alerting
- Performance monitoring dashboards

### 11. Testing Strategy
- Unit testing approach for business logic
- Integration testing for APIs
- End-to-end testing with Playwright
- Data migration testing and validation

### 12. Deployment and DevOps
- CI/CD pipeline design
- Environment management (dev, staging, production)
- Infrastructure as Code approach
- Rollback and disaster recovery procedures

### 13. Migration Execution Plan
- Phased migration approach (Strangler Fig pattern)
- Parallel running strategy
- Cutover planning and risk mitigation
- User training and change management

### 14. Risk Assessment and Mitigation
- Technical risks and mitigation strategies
- Business continuity considerations
- Regulatory compliance maintenance
- Data integrity assurance

## Critical Requirements

### Business Logic Preservation
- **Exact preservation** of Brazilian financial regulations compliance
- **Byte-for-byte compatibility** for CNAB file generation during transition
- **Complete audit trail** migration with historical data integrity
- **Multi-currency support** (BRL/USD) with precise decimal handling

### Legacy System Integration
- **Parallel processing capability** during migration period
- **Real-time data synchronization** between legacy and modern systems
- **Gradual module migration** following Strangler Fig pattern
- **Fallback mechanisms** to legacy system if needed

### Modern Architecture Benefits
- **Cloud-native design** with horizontal scalability
- **API-first approach** enabling future integrations
- **Real-time processing** replacing batch-only operations
- **Mobile-responsive interface** for modern user experience

## Quality Standards
- **Zero hallucinations**: All technical specifications must be implementable and accurate
- **Evidence-based design**: Every architectural decision must reference legacy system evidence
- **Production-ready**: Include all necessary configuration, security, and operational considerations
- **Compliance-focused**: Maintain all regulatory requirements for Brazilian financial operations

## Deliverable Format
Provide a complete SDD document that:
- Is structured according to the sections above
- Includes detailed technical specifications
- Contains implementation guidance for development teams
- Addresses all critical migration challenges identified in the legacy system analysis
- Provides clear success criteria and acceptance tests

The resulting SDD should serve as the definitive technical blueprint for the migration project, enabling development teams to implement the modern system while preserving all critical business functionality from the legacy COBOL system.

Generate the SDD following the structure above, strictly abiding by all rules.
Save the output in a Markdown file named `Model_SONNET4_TargetSDD.md` inside the folder `sdd/new`.
The entire output must be written in English.