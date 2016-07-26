tests
testMergePackageB
    "used by MetacelloAbstractVersionConstructor>>package:with:"

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
        merge:
            {(self packageSpec
                name: 'Package';
                requires: 'AndAnotherPackage';
                includes: 'AndIncludedPackage';
                answers: #(#('xpostload' 'xpostload answer'));
                file: 'Package-dkh.2';
                yourself)}.
    package := packages packageNamed: 'Package' ifAbsent: [ self assert: false ].
    self assert: package name = 'Package'.
    self assert: package requires = #('AnotherPackage' 'AndAnotherPackage').
    self assert: package includes = #('IncludedPackage' 'AndIncludedPackage').
    self
        assert:
            package answers = #(#('preload' 'preload answer') #('postload' 'postload answer') #('xpostload' 'xpostload answer')).
    self assert: package file = 'Package-dkh.2'.
    self assert: package preLoadDoIt value == #'preLoadDoIt'.
    self assert: package postLoadDoIt value == #'postLoadDoIt'