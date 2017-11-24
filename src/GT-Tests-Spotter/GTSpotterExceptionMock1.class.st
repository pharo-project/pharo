Class {
	#name : #GTSpotterExceptionMock1,
	#superclass : #Object,
	#category : #'GT-Tests-Spotter-Exceptions-Mocks'
}

{ #category : #mocks }
GTSpotterExceptionMock1 >> messageText [
	^ self class name
]

{ #category : #mocks }
GTSpotterExceptionMock1 >> mockProcessorFor: aSpotterStep [
	<spotterOrder: 1>
	AssertionFailure signal: self messageText " Exception while creating the processor "
]
