Class {
	#name : 'UndefinedPackageTest',
	#superclass : 'RPackageTestCase',
	#category : 'RPackage-Tests',
	#package : 'RPackage-Tests'
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
	class := self newClassNamed: #MyTestClass in: #Package1.
	self organizer removeClass: class.

	self assert: class package identicalTo: self undefinedPackage
]

{ #category : 'tests' }
UndefinedPackageTest >> testTagOf [

	self assert: (self undefinedPackage tagOf: self class) equals: self undefinedPackage undefinedTag.
	self assert: (self undefinedPackage tagOf: self class) name equals: UndefinedPackageTag undefinedPackageTagName
]

{ #category : 'helpers' }
UndefinedPackageTest >> undefinedPackage [

	^ self organizer undefinedPackage
]
