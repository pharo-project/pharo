tests
testRemovePackageB
    "used by MetacelloAbstractVersionConstructor>>removePackage:"

    | packages removed |
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
        remove:
            {(self packageSpec
                name: 'Package';
                yourself)}.
    removed := false.
    packages packageNamed: 'Package' ifAbsent: [ removed := true ].
    self assert: removed