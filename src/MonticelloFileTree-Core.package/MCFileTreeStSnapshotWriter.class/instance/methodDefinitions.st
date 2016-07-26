accessing
methodDefinitions
    methodDefinitions ifNil: [ methodDefinitions := Dictionary new ].
    ^ methodDefinitions