tests
testRemoveGroupA
    "used by MetacelloAbstractVersionConstructor>>removeGroup:"

    | packages group removed |
    packages := self packagesSpec.
    packages
        add:
            (self groupSpec
                name: 'Platform';
                includes: 'Core';
                yourself).
    packages
        merge:
            (self groupSpec
                name: 'Platform';
                includes: 'Tests';
                yourself).
    group := packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    self assert: (group includes includes: 'Core').
    self assert: (group includes includes: 'Tests').
    packages
        remove:
            (self groupSpec
                name: 'Platform';
                includes: 'Core';
                yourself).
    removed := false.
    packages packageNamed: 'Platform' ifAbsent: [ removed := true ].
    self assert: removed