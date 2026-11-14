"
I am the abstract root of the commands declared by the mock anchors of the command registration tests (ClyMockToolContext and ClyMockToolSubContext).
"
Class {
	#name : 'ClyMockRegisteredCommand',
	#superclass : 'ClyCommand',
	#category : 'Calypso-Commands-Tests',
	#package : 'Calypso-Commands-Tests'
}

{ #category : 'testing' }
ClyMockRegisteredCommand class >> isAbstract [

	^ self == ClyMockRegisteredCommand
]
