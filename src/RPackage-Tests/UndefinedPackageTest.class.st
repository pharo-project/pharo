Class {
	#name : 'UndefinedPackageTest',
	#superclass : 'RPackageTestCase',
	#category : 'RPackage-Tests',
	#package : 'RPackage-Tests'
}

{ #category : 'tests' }
UndefinedPackageTest >> testIsUndefined [

	self assert: self organizer undefinedPackage isUndefined.

	self deny: (self ensurePackage: #Package1) isUndefined
]

{ #category : 'tests' }
UndefinedPackageTest >> testName [

	self assert: self organizer undefinedPackage name equals: UndefinedPackage undefinedPackageName
]
