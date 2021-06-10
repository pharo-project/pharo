Class {
	#name : #ClyLongExampleQuery,
	#superclass : #ClyReturningScopeBasisExampleQuery,
	#instVars : [
		'executionSemaphore'
	],
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #'items type' }
ClyLongExampleQuery class >> resultItemsType [
	self shouldBeImplemented
]

{ #category : #execution }
ClyLongExampleQuery >> buildResult: aQueryResult [
	executionSemaphore wait.
	
	super buildResult: aQueryResult
]

{ #category : #execution }
ClyLongExampleQuery >> checkEmptyResult [
	executionSemaphore wait.
	super checkEmptyResult
]

{ #category : #controlling }
ClyLongExampleQuery >> delayExecution [
	executionSemaphore consumeAllSignals
]

{ #category : #initialization }
ClyLongExampleQuery >> initialize [
	super initialize.
	executionSemaphore := Semaphore new
]

{ #category : #controlling }
ClyLongExampleQuery >> passExecution [
	executionSemaphore signal
]
