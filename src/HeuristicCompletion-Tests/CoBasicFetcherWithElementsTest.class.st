Class {
	#name : 'CoBasicFetcherWithElementsTest',
	#superclass : 'CoBasicFetcherTest',
	#instVars : [
		'expectedElements'
	],
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'testing' }
CoBasicFetcherWithElementsTest class >> isAbstract [

	^ self == CoBasicFetcherWithElementsTest
]

{ #category : 'helpers' }
CoBasicFetcherWithElementsTest >> firstExpectedElement [

	^ expectedElements first
]

{ #category : 'helpers' }
CoBasicFetcherWithElementsTest >> secondExpectedElement [

	^ expectedElements second
]

{ #category : 'tests' }
CoBasicFetcherWithElementsTest >> testNewFetcherWithElementsShouldNotBeAtEnd [

	self deny: self fetcher atEnd
]

{ #category : 'tests' }
CoBasicFetcherWithElementsTest >> testNextManyAfterLimitShouldStopAfterLimit [

	| fetchedElements |
	fetchedElements := self fetcher next: expectedElements size + 1.
	fetchedElements withIndexDo: [ :each :index |
		self assertEntry: each equals: (expectedElements at: index) ].
	self assert: fetchedElements size equals: expectedElements size
]

{ #category : 'tests' }
CoBasicFetcherWithElementsTest >> testNextManyShouldReturnCollection [

	| fetchedElements expected |
	fetchedElements := self fetcher next: 2.
	expected := { self firstExpectedElement . self secondExpectedElement }.
	fetchedElements withIndexDo: [ :each :index |
		self assertEntry: each equals: (expected at: index) ].
	self assert: fetchedElements size equals: expected size
]

{ #category : 'tests' }
CoBasicFetcherWithElementsTest >> testNextNextShouldReturnSecondExpectedElement [

	self fetcher next.
	self assertEntry: self fetcher next equals: self secondExpectedElement
]

{ #category : 'tests' }
CoBasicFetcherWithElementsTest >> testNextShouldReturnFirstElement [

	self assertEntry: self fetcher next equals: self firstExpectedElement
]

{ #category : 'tests' }
CoBasicFetcherWithElementsTest >> testResetShouldRestartGenerator [

	| fetchedElements |
	self fetcher upToEnd; reset.
	fetchedElements := self fetcher next: expectedElements size + 1.
	fetchedElements withIndexDo: [ :each :index |
		self assertEntry: each equals: (expectedElements at: index) ].
	self assert: fetchedElements size equals: expectedElements size
]
