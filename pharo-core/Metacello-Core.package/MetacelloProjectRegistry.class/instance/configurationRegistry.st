accessing
configurationRegistry
    configurationRegistry ifNil: [ configurationRegistry := Dictionary new ].
    ^ configurationRegistry