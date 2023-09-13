"
Common superclass for mocks
"
Class {
	#name : #MCMock,
	#superclass : #Object,
	#category : #'Monticello-Mocks',
	#package : #Monticello,
	#tag : #Mocks
}

{ #category : #'as yet unclassified' }
MCMock class >> wantsChangeSetLogging [
	^ false
]
