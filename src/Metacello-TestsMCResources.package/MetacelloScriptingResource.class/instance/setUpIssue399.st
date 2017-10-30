issue 399
setUpIssue399
  | versionInfo |
  versionInfo := self setUpIssue399CoreSampledkh1.
  versionInfo := self setUpIssue399CoreSampledkh2: {versionInfo}.
  self
    setUpIssue399CoreExternaldkh1;
    setUpBaselineIssue399;
    setUpBaselineIssue399Cypress;
    yourself