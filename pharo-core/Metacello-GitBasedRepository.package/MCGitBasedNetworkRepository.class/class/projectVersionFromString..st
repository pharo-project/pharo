version patterns
projectVersionFromString: aString
  "strip leading $v if present and return an instance of MetacelloVersionNumber"

  | versionString |
  versionString := aString.
  (versionString beginsWith: 'v')
    ifTrue: [ versionString := versionString copyFrom: 2 to: versionString size ].
  ^ MetacelloVersionNumber fromString: versionString