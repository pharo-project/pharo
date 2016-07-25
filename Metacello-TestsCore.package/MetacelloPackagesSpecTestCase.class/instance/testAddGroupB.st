tests
testAddGroupB
    "used by MetacelloAbstractVersionConstructor>>group:overrides:"

    | packages group |
    packages := self packagesSpec.
    packages
        add:
            {(self groupSpec
                name: 'Platform';
                includes: 'Core';
                yourself).
            (self groupSpec
                name: 'Platform';
                includes: 'Tests';
                yourself)}.
    group := packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    self deny: (group includes includes: 'Core').
    self assert: (group includes includes: 'Tests')