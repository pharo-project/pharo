accessing
baselineRegistry
    baselineRegistry ifNil: [ baselineRegistry := Dictionary new ].
    ^ baselineRegistry