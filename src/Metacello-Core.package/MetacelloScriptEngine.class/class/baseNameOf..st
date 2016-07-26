utility
baseNameOf: className
  ^ (className beginsWith: 'BaselineOf')
    ifTrue: [ className copyFrom: 'BaselineOf' size + 1 to: className size ]
    ifFalse: [ 
      (className beginsWith: 'ConfigurationOf')
        ifTrue: [ className copyFrom: 'ConfigurationOf' size + 1 to: className size ]
        ifFalse: [ className ] ]