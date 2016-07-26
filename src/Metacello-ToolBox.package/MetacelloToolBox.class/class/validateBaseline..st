validation
validateBaseline: baselineClass
  "Check the baseline for Errors, Critical Warnings, and Warnings (see class comment for MetacelloMCVersionValidator for more information). "

  <apiDocumentation>
  ^ self validateBaseline: baselineClass debug: #() recurse: false