Class {
	#name : #SptHistoryProcessorTest,
	#superclass : #AbstractSptProcessorTest,
	#instVars : [
		'firstValue',
		'secondValue'
	],
	#category : #'GT-Tests-Spotter-Processors'
}

{ #category : #tests }
SptHistoryProcessorTest >> processor [

	^ SptHistoryProcessor new
]

{ #category : #running }
SptHistoryProcessorTest >> setUp [
	super setUp. 
	
	firstValue := SptClassEntry on: self class.
	secondValue := SptClassEntry on: SptUnifiedProcessorTest.
	
	GTSpotter new historize: firstValue.
	GTSpotter new historize: secondValue.
]

{ #category : #running }
SptHistoryProcessorTest >> tearDown [

	GTSpotter resetHistory.
	super tearDown.	
	
]

{ #category : #tests }
SptHistoryProcessorTest >> testEmptyQueryShowsResults [

	self runForText: ''.
	self assertResultsIncludes: self class.

]

{ #category : #tests }
SptHistoryProcessorTest >> testLookingHistoryQueryShowsResults [

	self runForText: 'spthistory'.
	self assertResultsIncludes: self class.

]

{ #category : #tests }
SptHistoryProcessorTest >> testLookingSubstringShowsResults [

	self runForText: 'unifi'.
	self denyResultsIncludes: self class.
	self assertResultsIncludes: SptUnifiedProcessorTest.

]
