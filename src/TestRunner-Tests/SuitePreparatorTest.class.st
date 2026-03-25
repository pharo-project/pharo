Class {
	#name : 'SuitePreparatorTest',
	#superclass : 'ParametrizedTestCase',
	#instVars : [
		'suitePreparator'
	],
	#category : 'TestRunner-Tests-Preparators',
	#package : 'TestRunner-Tests',
	#tag : 'Preparators'
}

{ #category : 'building suites' }
SuitePreparatorTest class >> testParameters [

	^ ParametrizedTestMatrix new
		forSelector: #suitePreparator addOptions: { SuitePreparator new . ShuffleSuitePreparator new }
]

{ #category : 'accessing' }
SuitePreparatorTest >> suitePreparator [

	^ suitePreparator
]

{ #category : 'accessing' }
SuitePreparatorTest >> suitePreparator: anObject [

	suitePreparator := anObject
]

{ #category : 'tests' }
SuitePreparatorTest >> testSuitePreparator [
	
	|initialSuite initialSuiteCopy|
	initialSuite := GreenTest suite.
	initialSuiteCopy := initialSuite copy.
	self 
		assert: initialSuiteCopy tests
		equals: initialSuite tests.
	
	self suitePreparator applyOn: initialSuite.
	
	self 
		assertCollection: initialSuiteCopy tests
		hasSameElements: initialSuite tests.
]
