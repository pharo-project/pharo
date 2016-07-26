validation
validateBaselineProject: project
  | version spec |
  (self visited includes: project configuration class)
    ifTrue: [ ^ self validationReport ].
  self visited add: project configuration class.
  version := project version.
  spec := version spec.
  [ self validateBaselineVersionSpec: spec ]
    on: Error
    do: [ :ex | 
      self
        recordValidationError:
          'Error composing version ' , version versionString printString , ': '
            , ex description
        versionString: version versionString
        callSite: #'validateBaselineProject:'
        reasonCode: #'versionCompositionError' ].
  ^ self validationReport