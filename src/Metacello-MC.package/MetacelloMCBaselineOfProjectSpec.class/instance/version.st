querying
version
    self projectClass == nil
        ifTrue: [ ^ nil ].
    ^ self projectClassProject version