Class {
	#name : 'ShuffleSuitePreparatorTest',
	#superclass : 'TestCase',
	#instVars : [
		'runner'
	],
	#category : 'TestRunner-Tests-Preparators',
	#package : 'TestRunner-Tests',
	#tag : 'Preparators'
}

{ #category : 'tests' }
ShuffleSuitePreparatorTest >> testSuitePreparator [
	
	|initialSuite initialSuiteCopy|
	initialSuite := GreenTest suite.
	initialSuiteCopy := initialSuite copy.
	self 
		assert: initialSuiteCopy tests
		equals: initialSuite tests.
	
	ShuffleSuitePreparator applyOn: initialSuite.
	
	self
		assert: initialSuite tests size
		equals: initialSuiteCopy tests size.
	
	self assert: (initialSuiteCopy tests includesAll: initialSuite tests)
]
