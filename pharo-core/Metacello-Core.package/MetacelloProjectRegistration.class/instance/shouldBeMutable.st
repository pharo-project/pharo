mutability
shouldBeMutable
    self isMutable
        ifTrue: [ ^ self ].
    self error: 'Not allowed to modify an immutable object'