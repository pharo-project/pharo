Class {
	#name : #RPackageStringExtensionTest,
	#superclass : #TestCase,
	#category : 'RPackage-Tests'
}

{ #category : #tests }
RPackageStringExtensionTest >> testAsPackage [
	self assert: self class package name asPackage notNil
]

{ #category : #tests }
RPackageStringExtensionTest >> testAsPackageIfAbsent [
	self assert: ('___inexistent___package___' asPackageIfAbsent: [ true ])
]

{ #category : #tests }
RPackageStringExtensionTest >> testAsPackageWithError [
	self 
		should: ['___inexistent___package___' asPackage]
		raise: NotFound
	
]
