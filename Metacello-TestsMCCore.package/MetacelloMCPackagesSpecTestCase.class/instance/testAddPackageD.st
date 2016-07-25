tests
testAddPackageD
    "used by MetacelloAbstractVersionConstructor>>package: and MetacelloAbstractVersionConstructor>>package:overrides:"

    | packages |
    packages := self packagesSpec.
    packages add: 'Platform'.
    packages packageNamed: 'Platform' ifAbsent: [ self assert: false ]