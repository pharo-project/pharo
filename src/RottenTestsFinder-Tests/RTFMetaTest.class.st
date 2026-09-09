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

	RTFFakeTestSuperClassTest doNotIgnore 
]

{ #category : 'running' }
RTFMetaTest >> tearDown [

	RTFFakeTestSuperClassTest ignore.
	super tearDown 
]
