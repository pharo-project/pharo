Class {
	#name : #ClyLongQueryExample,
	#superclass : #ClyQueryExampleReturningScopeBasis,
	#instVars : [
		'executionSemaphore'
	],
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #'items type' }
ClyLongQueryExample class >> resultItemsType [
	self shouldBeImplemented
]

{ #category : #execution }
ClyLongQueryExample >> buildResult: aQueryResult [
	executionSemaphore wait.
	
	super buildResult: aQueryResult
]

{ #category : #execution }
ClyLongQueryExample >> checkEmptyResult [
	executionSemaphore wait.
	super checkEmptyResult
]

{ #category : #controlling }
ClyLongQueryExample >> delayExecution [
	executionSemaphore consumeAllSignals
]

{ #category : #initialization }
ClyLongQueryExample >> initialize [
	super initialize.
	executionSemaphore := Semaphore new
]

{ #category : #controlling }
ClyLongQueryExample >> passExecution [
	executionSemaphore signal
]
