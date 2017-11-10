Class {
	#name : #MetacelloMCPackagesSpecTestCase,
	#superclass : #MetacelloPackagesSpecTestCase,
	#category : #Metacello-TestsMCCore
}

{ #category : #accessing }
MetacelloMCPackagesSpecTestCase >> projectClass [
    ^ MetacelloMCProject
]

{ #category : #tests }
MetacelloMCPackagesSpecTestCase >> testAddPackageA [
    "used by MetacelloAbstractVersionConstructor>>package: and MetacelloAbstractVersionConstructor>>package:overrides:"

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
        add:
            (self packageSpec
                name: 'Package';
                requires: 'AndAnotherPackage';
                includes: 'AndIncludedPackage';
                answers: #(#('postload' 'postload answer'));
                file: 'Package-dkh.2';
                yourself).
    package := packages packageNamed: 'Package' ifAbsent: [ self assert: false ].
    self assert: package name = 'Package'.
    self assert: package requires = #('AndAnotherPackage').
    self assert: package includes = #('AndIncludedPackage').
    self assert: package answers = #(#('postload' 'postload answer')).
    self assert: package file = 'Package-dkh.2'.
    self assert: package preLoadDoIt value == nil.
    self assert: package postLoadDoIt value == nil
]

{ #category : #tests }
MetacelloMCPackagesSpecTestCase >> testAddPackageB [
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
]

{ #category : #tests }
MetacelloMCPackagesSpecTestCase >> testAddPackageC [
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
]

{ #category : #tests }
MetacelloMCPackagesSpecTestCase >> testAddPackageD [
    "used by MetacelloAbstractVersionConstructor>>package: and MetacelloAbstractVersionConstructor>>package:overrides:"

    | packages |
    packages := self packagesSpec.
    packages add: 'Platform'.
    packages packageNamed: 'Platform' ifAbsent: [ self assert: false ]
]

{ #category : #tests }
MetacelloMCPackagesSpecTestCase >> testCopyToPackage [
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
]

{ #category : #tests }
MetacelloMCPackagesSpecTestCase >> testMergePackageA [
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
            (self packageSpec
                name: 'Package';
                requires: 'AndAnotherPackage';
                includes: 'AndIncludedPackage';
                answers: #(#('xpostload' 'xpostload answer'));
                file: 'Package-dkh.2';
                yourself).
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
]

{ #category : #tests }
MetacelloMCPackagesSpecTestCase >> testMergePackageB [
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
]

{ #category : #tests }
MetacelloMCPackagesSpecTestCase >> testMergePackageD [
    "used by MetacelloAbstractVersionConstructor>>package:with:"

    | packages |
    packages := self packagesSpec.
    packages
        add: 'Platform';
        merge: 'Tests'.
    packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    packages packageNamed: 'Tests' ifAbsent: [ self assert: false ]
]

{ #category : #tests }
MetacelloMCPackagesSpecTestCase >> testRemovePackageA [
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
            (self packageSpec
                name: 'Package';
                yourself).
    removed := false.
    packages packageNamed: 'Package' ifAbsent: [ removed := true ].
    self assert: removed
]

{ #category : #tests }
MetacelloMCPackagesSpecTestCase >> testRemovePackageB [
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
]

{ #category : #tests }
MetacelloMCPackagesSpecTestCase >> testRemovePackageC [
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
    packages remove: {'Package'}.
    removed := false.
    packages packageNamed: 'Package' ifAbsent: [ removed := true ].
    self assert: removed
]

{ #category : #tests }
MetacelloMCPackagesSpecTestCase >> testRemovePackageD [
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
    packages remove: 'Package'.
    removed := false.
    packages packageNamed: 'Package' ifAbsent: [ removed := true ].
    self assert: removed
]
