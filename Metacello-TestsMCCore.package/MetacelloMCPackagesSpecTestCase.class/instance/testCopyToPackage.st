tests
testCopyToPackage
    "not currently used by MetacelloAbstractVersionConstructor"

    | packages package |
    packages := self packagesSpec.
    packages
        add:
            (self packageSpec
                name: 'Package';
                requires: 'AnotherPackage';
                includes: 'IncludedPackage';
                answers: #(#('preload' 'preload answer') #('postload' 'postload answer'));
                file: 'Package-dkh.1';
                preLoadDoIt: #'preLoadDoIt';
                postLoadDoIt: #'postLoadDoIt';
                yourself).
    packages
        copy: 'Package'
        to:
            (self packageSpec
                name: 'PackageCopy';
                yourself).
    package := packages packageNamed: 'PackageCopy' ifAbsent: [ self assert: false ].
    self assert: package name = 'PackageCopy'.
    self assert: package requires = #('AnotherPackage').
    self assert: package includes = #('IncludedPackage').
    self assert: package answers = #(#('preload' 'preload answer') #('postload' 'postload answer')).
    self assert: package file = 'Package-dkh.1'.
    self assert: package preLoadDoIt value == #'preLoadDoIt'.
    self assert: package postLoadDoIt value == #'postLoadDoIt'