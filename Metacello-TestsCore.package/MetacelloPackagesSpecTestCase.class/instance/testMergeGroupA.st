tests
testMergeGroupA
    "used by MetacelloAbstractVersionConstructor>>group:with:"

    | packages group |
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
    self assert: (group includes includes: 'Tests')