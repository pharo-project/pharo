scripting
compareVersions: aMetacelloProjectSpec usingOperator: anOperator
  ^ aMetacelloProjectSpec versionOrNil == nil
    ifTrue: [ 
      "https://github.com/dalehenrich/metacello-work/issues/199#issuecomment-21739622"
      aMetacelloProjectSpec versionString asMetacelloVersionNumber
        perform: anOperator
        with: self version versionNumber ]
    ifFalse: [ ^ aMetacelloProjectSpec version perform: anOperator with: self version ]