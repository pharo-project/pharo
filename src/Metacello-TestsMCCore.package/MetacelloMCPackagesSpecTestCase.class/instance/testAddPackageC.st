tests
testAddPackageC
    "used by MetacelloAbstractVersionConstructor>>package: and MetacelloAbstractVersionConstructor>>package:overrides:"

    | packages |
    packages := self packagesSpec.
    packages
        add:
            {'Platform'.
            'Base'.
            'Tests'}.
    packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    packages packageNamed: 'Base' ifAbsent: [ self assert: false ].
    packages packageNamed: 'Tests' ifAbsent: [ self assert: false ]