"
A RTFMethodCallsCollectorTest is a test class for testing the behavior of RTFMethodCallsCollector
"
Class {
	#name : #RTFMethodCallsCollectorTest,
	#superclass : #TestCase,
	#instVars : [
		'tree'
	],
	#category : #'RottenTestsFinder-Tests'
}

{ #category : #'tests-visiting' }
RTFMethodCallsCollectorTest >> testVisitRTFSelfCallNode [
	| visitor methods |
	visitor := RTFMethodCallsCollector new.
	methods := tree acceptVisitor: visitor.
	
	self
		assertCollection: visitor compiledMethods
		hasSameElements: { 'Fake CM' . 'Fake CM 2' . 'Fake CM 3' . (TestAsserter lookupSelector: #assert:) . 'Fake CM 4' . 'Fake CM 5' }
]
