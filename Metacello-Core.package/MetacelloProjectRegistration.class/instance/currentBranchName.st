accessing
currentBranchName
  ^ self configurationProjectSpec
    ifNotNil: [ 
      configurationProjectSpec versionOrNil
        ifNil: [ '' ]
        ifNotNil: [ :vrsn | vrsn blessing asString ] ]
    ifNil: [ baselineProjectSpec repositoryBranchName ]