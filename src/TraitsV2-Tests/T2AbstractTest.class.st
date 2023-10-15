"
Abstract Test of all the TraitsV2 tests
"
Class {
	#name : 'T2AbstractTest',
	#superclass : 'TestCase',
	#category : 'TraitsV2-Tests',
	#package : 'TraitsV2-Tests'
}

{ #category : 'testing' }
T2AbstractTest class >> isAbstract [

	^self == T2AbstractTest
]

{ #category : 'instance creation' }
T2AbstractTest >> make: aBlock [

	^ self class classInstaller make: [ :aBuilder |
		  aBuilder package: self packageNameForTests.
		  aBlock value: aBuilder ]
]

{ #category : 'instance creation' }
T2AbstractTest >> makeTrait: aBlock [

	^ self make: [ :aBuilder |
		  aBuilder beTrait.
		  aBlock value: aBuilder ]
]

{ #category : 'instance creation' }
T2AbstractTest >> packageNameForTests [

	^ #'Generated-Trait-Test-Package'
]

{ #category : 'running' }
T2AbstractTest >> tearDown [
	self packageOrganizer removePackage: self packageNameForTests.

	super tearDown
]
