tests
testRemoveGroupC
    "used by MetacelloAbstractVersionConstructor>>removeGroup:"

    | packages group removed |
    packages := self packagesSpec.
    packages
        add:
            {(self groupSpec
                name: 'Platform';
                includes: 'Core';
                yourself).
            (self groupSpec
                name: 'Base';
                includes: 'Base';
                yourself).
            (self groupSpec
                name: 'Tests';
                includes: 'Tests';
                yourself)}.
    packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    packages packageNamed: 'Base' ifAbsent: [ self assert: false ].
    packages packageNamed: 'Tests' ifAbsent: [ self assert: false ].
    packages remove: 'Tests'.
    packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    packages packageNamed: 'Base' ifAbsent: [ self assert: false ].
    removed := false.
    packages packageNamed: 'Tests' ifAbsent: [ removed := true ].
    self assert: removed