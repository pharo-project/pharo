Class {
	#name : #GTSpotterDebuggingExceptionHandler,
	#superclass : #GTSpotterExceptionHandler,
	#category : #'GT-Spotter-Exceptions'
}

{ #category : #public }
GTSpotterDebuggingExceptionHandler >> handleException: anException do: aBlock [
	anException pass.
	^ super handleException: anException do: aBlock.
	
	
]
