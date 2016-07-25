scripting
canUpgradeTo: aMetacelloProjectSpec
  (self className = aMetacelloProjectSpec className
    and: [ self operator == aMetacelloProjectSpec operator ])
    ifFalse: [ ^ false ].
  self versionOrNil == nil
    ifTrue: [ 
      "https://github.com/dalehenrich/metacello-work/issues/198#issuecomment-21737458"
      ^ true ].
  ^ self compareVersions: aMetacelloProjectSpec usingOperator: self operator