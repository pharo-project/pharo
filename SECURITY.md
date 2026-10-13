# Security Policy

## Reporting a Vulnerability

The Pharo project welcomes responsible reports of security vulnerabilities affecting Pharo, its runtime, standard libraries, and related components.

Please report security vulnerabilities privately. Do not disclose vulnerability details, exploit code, or proof-of-concept payloads in public issues or pull requests before coordinating with the maintainers.

### Reporting channel

Use GitHub's **Report a vulnerability** feature in the Security Advisories section of this repository.

If you cannot access this feature, contact the Pharo project maintainers privately and request the designated security contact. Do not include exploit details in a public issue.

### Scope

Reports may involve vulnerabilities spanning multiple components or repositories, including the Pharo runtime, standard libraries, serialization frameworks, and third-party packages.

Please report the issue even if you are unsure which component is responsible. The maintainers will coordinate with the relevant component maintainers to investigate the root cause, determine the affected versions, and agree on remediation.

### Information to include

Please provide, when available:

- Affected Pharo versions and components.
- The relevant package versions or commits.
- The conditions required to trigger the vulnerability.
- The demonstrated security impact.
- Minimal reproduction steps and a proof of concept, shared privately.
- Known mitigations or proposed fixes, if any.

### Coordinated disclosure

The Pharo project will coordinate investigation, remediation, and disclosure with the reporter and relevant maintainers.

The project will assess the vulnerability's scope and severity, identify affected and fixed versions, and coordinate public communication.

Where appropriate, the maintainers will publish a security advisory and coordinate the assignment of a CVE identifier.

Please allow reasonable time for investigation and remediation before public disclosure.

### Supported versions

Security fixes are provided according to the maintenance status of each supported Pharo release. The maintainers will identify the affected and fixed versions for each reported vulnerability.
