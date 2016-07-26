testing
isValid
  " has a name and one or the other of the projectSpecs is non-nil, but not both ... this is CRITICAL"

  projectName ifNil: [ ^ false ].
  configurationProjectSpec isNil
    ifTrue: [ ^ baselineProjectSpec notNil ].
  ^ baselineProjectSpec isNil