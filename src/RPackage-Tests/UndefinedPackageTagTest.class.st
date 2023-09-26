Class {
	#name : 'UndefinedPackageTagTest',
	#superclass : 'RPackageTestCase',
	#category : 'RPackage-Tests',
	#package : 'RPackage-Tests'
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

	self deny: (self organizer ensureTagNamed: #Tag inPackageNamed: #Package1) isUndefined
]

{ #category : 'tests' }
UndefinedPackageTagTest >> testName [

	self assert: self undefinedTag name equals: UndefinedPackageTag undefinedPackageTagName
]

{ #category : 'accessing' }
UndefinedPackageTagTest >> undefinedTag [

	^ self organizer undefinedPackage classTags anyOne
]
