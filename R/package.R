#' PhysioCompliance: evidence, privacy, and lifecycle controls
#'
#' Deterministic SHA-256 audit chains and externally authenticated electronic
#' signatures, explicit de-identification policies, keyed pseudonymization,
#' structured header scrubbing, and plan-first data-subject helpers for
#' [PhysioCore::PhysioExperiment] records. The package also supplies original
#' lifecycle/risk templates, deterministic traceability graphs with
#' objective-evidence hashing, and read-only package engineering-readiness
#' checks.
#'
#' PhysioCompliance provides compliance-supporting technical controls. It does
#' not determine device status, assign a software safety class, decide risk
#' acceptability, authorize release, or certify conformity with HIPAA, GDPR,
#' 21 CFR Part 11, IEC 62304, ISO 14971, an FDA guidance, a quality-system
#' regulation, or a Bioconductor policy. A regulated deployment also requires
#' qualified review, validated systems, access controls, identity proofing,
#' credential lifecycle management, policies, training, retention, and
#' operational controls outside this package.
#'
#' Signing and verification callbacks must be supplied by the deployment.
#' Their credential service and algorithm determine authentication,
#' authorization, cryptographic strength, identity assurance, and
#' non-repudiation properties. Passwords, private keys, tokens, and reusable
#' secrets must not be placed in package objects.
#'
#' An internally consistent earlier object is indistinguishable from the record
#' as it existed at that time. Deployments that must detect whole-object
#' rollback or removal of the current tail event need to retain verified audit
#' head hashes in a validated external append-only store and compare them when
#' records are retrieved.
#'
#' A `safe_harbor_candidate` report is a conservative configured-field result,
#' not a legal determination. Pseudonymized data remain personal data when
#' separately held information permits re-identification. Structured header
#' helpers do not rewrite binary files or inspect DICOM pixels. Data-subject
#' erasure changes only the returned in-memory record collection.
#'
#' Lifecycle and risk templates contain neutral project-authored headings and
#' blank schemas, not licensed standard content. Risk decisions always come
#' from a caller-supplied matrix and rationale. Conformance reports record
#' PhysioExperiment project-policy checks, `NOT_EVALUATED` evidence, and
#' relative paths; they are engineering-readiness records rather than
#' conformity conclusions.
#'
#' @references
#' HHS OCR, Guidance Regarding Methods for De-identification of Protected
#' Health Information in Accordance with the HIPAA Privacy Rule,
#' \url{https://www.hhs.gov/hipaa/for-professionals/special-topics/de-identification/}.
#'
#' Regulation (EU) 2016/679,
#' \url{https://eur-lex.europa.eu/eli/reg/2016/679/oj}.
#'
#' DICOM PS3.15, Security and System Management Profiles,
#' \url{https://dicom.nema.org/medical/dicom/current/output/html/part15.html}.
#'
#' IEC 62304:2006+A1:2015, public catalog metadata,
#' \url{https://webstore.iec.ch/en/publication/22794}.
#'
#' ISO 14971:2019, public catalog metadata,
#' \url{https://www.iso.org/standard/72704.html}.
#'
#' @keywords internal
#' @importClassesFrom PhysioCore PhysioExperiment
"_PACKAGE"
