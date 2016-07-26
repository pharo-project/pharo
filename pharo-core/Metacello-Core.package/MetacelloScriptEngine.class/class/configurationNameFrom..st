utility
configurationNameFrom: baseName
    "Return the fully-qualified configuration class name."

    ^ (baseName indexOfSubCollection: 'ConfigurationOf') > 0
        ifTrue: [ baseName ]
        ifFalse: [ 'ConfigurationOf' , baseName ]