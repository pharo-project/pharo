project lookup
setDefaultsAndValidate: aProjectSpec
  "NOTE: aProjectSpec has defaults assigned if versionString or repository missing"

  | issues |
  issues := aProjectSpec
    validateForScriptLoad: self
    withDefaultVersionString: self defaultVersionString
    withDefaultRepositoryDecription: self defaultRepositoryDescription.
  issues isEmpty
    ifTrue: [ ^ self ].
  (MetacelloValidationFailure
    issues: issues
    message: 'Project spec validation failure') signal