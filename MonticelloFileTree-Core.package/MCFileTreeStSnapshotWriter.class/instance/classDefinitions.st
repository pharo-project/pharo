accessing
classDefinitions
    classDefinitions ifNil: [ classDefinitions := Dictionary new ].
    ^ classDefinitions