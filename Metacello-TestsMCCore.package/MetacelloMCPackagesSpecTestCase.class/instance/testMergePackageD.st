tests
testMergePackageD
    "used by MetacelloAbstractVersionConstructor>>package:with:"

    | packages |
    packages := self packagesSpec.
    packages
        add: 'Platform';
        merge: 'Tests'.
    packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    packages packageNamed: 'Tests' ifAbsent: [ self assert: false ]