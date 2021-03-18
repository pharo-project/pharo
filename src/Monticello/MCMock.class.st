"
Common superclass for mocks
"
Class {
	#name : #MCMock,
	#superclass : #Object,
	#category : #'Monticello-Mocks'
}

{ #category : #testing }
MCMock class >> wantsChangeSetLogging [
	^ false
]
