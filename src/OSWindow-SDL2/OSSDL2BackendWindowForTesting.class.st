Class {
	#name : #OSSDL2BackendWindowForTesting,
	#superclass : #OSSDL2BackendWindow,
	#instVars : [
		'receivedMessages'
	],
	#category : #'OSWindow-SDL2-Tests'
}

{ #category : #'event handling' }
OSSDL2BackendWindowForTesting >> deliverGlobalEvent: e [
	self receivedMessages add: (Message selector: #deliverGlobalEvent: argument: e)
]

{ #category : #'as yet unclassified' }
OSSDL2BackendWindowForTesting >> receivedMessages [
	^ receivedMessages ifNil: [ receivedMessages := Dictionary new ]
]
