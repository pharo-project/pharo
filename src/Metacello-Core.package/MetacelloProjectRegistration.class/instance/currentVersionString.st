accessing
currentVersionString
  ^ self configurationProjectSpec
    ifNotNil: [ :aSpec | aSpec versionString ]
    ifNil: [ baselineProjectSpec repositoryVersionString ]