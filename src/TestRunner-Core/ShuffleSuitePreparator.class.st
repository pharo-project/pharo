"
I am a suite preparator who shuffles the order of tests in a suite. 
I store an instance of `Random` that can be injected to control shuffling and seeding.
My objective is to collaborate on detecting test execution that depend on test case order.
"
Class {
	#name : 'ShuffleSuitePreparator',
	#superclass : 'SuitePreparator',
	#instVars : [
		'randomGenerator'
	],
	#category : 'TestRunner-Core-SuitePreparators',
	#package : 'TestRunner-Core',
	#tag : 'SuitePreparators'
}

{ #category : 'applying' }
ShuffleSuitePreparator >> applyOn: aTestSuite [

	aTestSuite tests shuffleBy: self randomGenerator 
]

{ #category : 'accessing' }
ShuffleSuitePreparator >> randomGenerator [ 

	^ randomGenerator ifNil: [ randomGenerator := Random new ]
]

{ #category : 'accessing' }
ShuffleSuitePreparator >> randomGenerator: anObject [

	randomGenerator := anObject
]
