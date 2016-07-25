truncation and round off
reduce
    "If self is close to an integer, return that integer"

    (self closeTo: self rounded) ifTrue: [^ self rounded]