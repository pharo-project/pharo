Class {
	#name : #SptImplementorsProcessorTest,
	#superclass : #AbstractSptProcessorTest,
	#category : #'GT-Tests-Spotter-Processors'
}

{ #category : #tests }
SptImplementorsProcessorTest >> processor [

	^ SptImplementorsProcessor new
]

{ #category : #tests }
SptImplementorsProcessorTest >> testFindThisMethod [

	self runForText: 'testFindThisMethod'.
	self assertResultsIncludes: thisContext method
]

{ #category : #tests }
SptImplementorsProcessorTest >> testFindThisMethodAfterAddingText [

	self runForText: 'test'.
	self runForText: 'testFindThisMethod'.
	
	self assertResultsIncludes: thisContext method
]

{ #category : #tests }
SptImplementorsProcessorTest >> testPopularStringDoesReturnOnlyTenResults [

	self runForText: 'test'.
	self assertQuantityOfResults: 25.
]

{ #category : #tests }
SptImplementorsProcessorTest >> testPopularStringDoesReturnOnlyTenResultsWithUppercase [

	self runForText: 'Test'.
	self assertQuantityOfResults: 25.
]
