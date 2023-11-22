Class {
	#name : 'UndefinedPackageTest',
	#superclass : 'PackageTestCase',
	#category : 'Kernel-Packages-Tests',
	#package : 'Kernel-Packages-Tests'
}

{ #category : 'tests' }
UndefinedPackageTest >> testCannotRemoveUndefinedPackage [

	| undefinedPackage |
	undefinedPackage := self undefinedPackage.

	undefinedPackage removeFromSystem.

	self assert: self undefinedPackage isNotNil.
	self assert: self undefinedPackage identicalTo: undefinedPackage
]

{ #category : 'tests' }
UndefinedPackageTest >> testIsUndefined [

	self assert: self undefinedPackage isUndefined.

	self deny: (self ensurePackage: #Package1) isUndefined
]

{ #category : 'tests' }
UndefinedPackageTest >> testName [

	self assert: self undefinedPackage name equals: UndefinedPackage undefinedPackageName
]

{ #category : 'tests' }
UndefinedPackageTest >> testPackageOfUnpackagedClass [

	| class |
	class := self class classInstaller make: [ :aClassBuilder |
		         aClassBuilder
			         name: #MyTestClass;
			         installingEnvironment: testEnvironment ].

	self assert: class package identicalTo: self undefinedPackage
]

{ #category : 'tests' }
UndefinedPackageTest >> testTagOf [

	self assert: (self undefinedPackage tagOf: self class) equals: self undefinedPackage undefinedTag.
	self assert: (self undefinedPackage tagOf: self class) name equals: UndefinedPackageTag undefinedPackageTagName
]

{ #category : 'tests' }
UndefinedPackageTest >> undefinedPackage [

	^ self organizer undefinedPackage
]
