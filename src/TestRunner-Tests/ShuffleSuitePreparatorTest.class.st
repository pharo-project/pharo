"
A ShuffleSuitePreparatorTest is a test class for testing the behavior of ShuffleSuitePreparator
"
Class {
	#name : 'ShuffleSuitePreparatorTest',
	#superclass : 'TestCase',
	#instVars : [
		'preparator',
		'randomGenerator',
		'suite'
	],
	#category : 'TestRunner-Tests-Preparators',
	#package : 'TestRunner-Tests',
	#tag : 'Preparators'
}

{ #category : 'asserting' }
ShuffleSuitePreparatorTest >> assertSuiteSelectorsEquals: aCollection [

	^ self 
		assertCollection: self suiteSelectors 
		equals: aCollection asOrderedCollection 
]

{ #category : 'accessing' }
ShuffleSuitePreparatorTest >> preparator [

	^ preparator ifNil: [ preparator := ShuffleSuitePreparator new 
		randomGenerator: self randomGenerator; yourself. ]
]

{ #category : 'accessing' }
ShuffleSuitePreparatorTest >> randomGenerator [ 

	^ randomGenerator ifNil: [ randomGenerator := Random seed: 4 ]
]

{ #category : 'accessing' }
ShuffleSuitePreparatorTest >> suite [

	^ suite ifNil: [ suite := EmptyTest suite ]
]

{ #category : 'enumerating' }
ShuffleSuitePreparatorTest >> suiteSelectors [

	^ self suite tests collect: #selector
]

{ #category : 'tests' }
ShuffleSuitePreparatorTest >> testIsSuiteShuffled [

	self 
		assertCollection: self suiteSelectors 
		equals: { #test1 . #test2 . #test3 . #test4 } asOrderedCollection .
		
	self preparator applyOn: self suite.
	
	self 
		assertCollection: self suiteSelectors 
		equals: { #test3 . #test4 . #test2 . #test1 } asOrderedCollection .
]

{ #category : 'tests' }
ShuffleSuitePreparatorTest >> testShuffleTwice [

	self preparator applyOn: self suite.

	self assertSuiteSelectorsEquals: { #test3. #test4. #test2. #test1 }.

	suite := nil.
	self preparator applyOn: self suite.

	self denyCollection: self suiteSelectors equals: { #test3. #test4. #test2. #test1 } asOrderedCollection
]
