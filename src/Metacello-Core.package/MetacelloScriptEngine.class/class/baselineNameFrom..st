utility
baselineNameFrom: baseName
    "Return the fully-qualified configuration class name."

    ^ (baseName indexOfSubCollection: 'BaselineOf') > 0
        ifTrue: [ baseName ]
        ifFalse: [ 'BaselineOf' , baseName ]