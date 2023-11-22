Class {
	#name : 'UndefinedPackageTagTest',
	#superclass : 'PackageTestCase',
	#category : 'Kernel-Packages-Tests',
	#package : 'Kernel-Packages-Tests'
}

{ #category : 'tests' }
UndefinedPackageTagTest >> testCannotRemoveUndefinedPackageTag [

	| undefinedTag |
	undefinedTag := self undefinedTag.

	undefinedTag removeFromSystem.

	self assert: self undefinedTag isNotNil.
	self assert: self undefinedTag identicalTo: undefinedTag
]

{ #category : 'tests' }
UndefinedPackageTagTest >> testIsUndefined [

	self assert: self undefinedTag isUndefined.

	self deny: (self organizer ensureTag: #Tag inPackage: #Package1) isUndefined
]

{ #category : 'tests' }
UndefinedPackageTagTest >> testName [

	self assert: self undefinedTag name equals: UndefinedPackageTag undefinedPackageTagName
]

{ #category : 'tests' }
UndefinedPackageTagTest >> testPackageTagOfUnpackagedClass [

	| class |
	class := self class classInstaller make: [ :aClassBuilder |
		         aClassBuilder
			         name: #MyTestClass;
			         installingEnvironment: testEnvironment ].

	self assert: class packageTag identicalTo: self undefinedTag
]

{ #category : 'helpers' }
UndefinedPackageTagTest >> undefinedTag [

	^ self organizer undefinedPackage undefinedTag
]
