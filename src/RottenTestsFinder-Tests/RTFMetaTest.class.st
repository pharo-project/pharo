Class {
	#name : 'RTFMetaTest',
	#superclass : 'TestCase',
	#category : 'RottenTestsFinder-Tests-Core',
	#package : 'RottenTestsFinder-Tests',
	#tag : 'Core'
}

{ #category : 'testing' }
RTFMetaTest class >> isAbstract [

	^ self == RTFMetaTest
]

{ #category : 'running' }
RTFMetaTest >> setUp [
	super setUp.

	RTFFakeTestSuperClass doNotIgnore 
]

{ #category : 'running' }
RTFMetaTest >> tearDown [

	RTFFakeTestSuperClass ignore.
	super tearDown 
]
