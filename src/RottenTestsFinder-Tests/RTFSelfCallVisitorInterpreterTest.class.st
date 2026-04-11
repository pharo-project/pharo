"
A RTFSelfCallVisitorInterpreterTest is a test class for testing the behavior of RTFSelfCallVisitorInterpreter
"
Class {
	#name : 'RTFSelfCallVisitorInterpreterTest',
	#superclass : 'TestCase',
	#instVars : [
		'interpreter'
	],
	#category : 'RottenTestsFinder-Tests-SelfInterpreter',
	#package : 'RottenTestsFinder-Tests',
	#tag : 'SelfInterpreter'
}

{ #category : 'accessing' }
RTFSelfCallVisitorInterpreterTest >> fixtureClass [

	^ RTFFakeTestClassTest
]

{ #category : 'accessing' }
RTFSelfCallVisitorInterpreterTest >> interpreter [ 

	^ interpreter
]

{ #category : 'running' }
RTFSelfCallVisitorInterpreterTest >> setUp [
	super setUp.

	interpreter := RTFSelfCallVisitorInterpreter new 
			selfClass: self fixtureClass; yourself
]

{ #category : 'tests' }
RTFSelfCallVisitorInterpreterTest >> testReset [

	self interpreter visit: (self fixtureClass >> #testHelperHelperAssertHelper) parseTree.
	
	self 
		assertCollection: self interpreter selfSentMethods 
		equals: { self fixtureClass >> #helperHelper . self fixtureClass >> #assertHelper  } asOrderedCollection.
		
	self interpreter reset; visit: (self fixtureClass >> #testWithNoAssertionInHelperRun) parseTree.
	
	self 
		assertCollection: self interpreter selfSentMethods 
		hasSameElements: { self fixtureClass >> #rottenHelper }
	
	
]

{ #category : 'tests' }
RTFSelfCallVisitorInterpreterTest >> testSelfClass [

	self interpreter selfClass: nil.
	self should: [ self interpreter selfClass] raise: Error
]
