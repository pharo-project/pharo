tests
testAddPackageB
    "used by MetacelloAbstractVersionConstructor>>package: and MetacelloAbstractVersionConstructor>>package:overrides:"

    | packages package |
    packages := self packagesSpec.
    packages
        add:
            {(self packageSpec
                name: 'Package';
                requires: 'AnotherPackage';
                includes: 'IncludedPackage';
                answers: #(#('preload' 'preload answer') #('postload' 'postload answer'));
                file: 'Package-dkh.1';
                preLoadDoIt: #'preLoadDoIt';
                postLoadDoIt: #'postLoadDoIt';
                yourself).
            (self packageSpec
                name: 'Package';
                requires: 'AndAnotherPackage';
                includes: 'AndIncludedPackage';
                answers: #(#('postload' 'postload answer'));
                file: 'Package-dkh.2';
                yourself)}.
    package := packages packageNamed: 'Package' ifAbsent: [ self assert: false ].
    self assert: package name = 'Package'.
    self assert: package requires = #('AndAnotherPackage').
    self assert: package includes = #('AndIncludedPackage').
    self assert: package answers = #(#('postload' 'postload answer')).
    self assert: package file = 'Package-dkh.2'.
    self assert: package preLoadDoIt value == nil.
    self assert: package postLoadDoIt value == nil