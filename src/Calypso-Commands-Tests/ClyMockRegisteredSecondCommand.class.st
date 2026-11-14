"
I am a concrete command declared by the mock anchors of the command registration tests.
"
Class {
	#name : 'ClyMockRegisteredSecondCommand',
	#superclass : 'ClyMockRegisteredCommand',
	#category : 'Calypso-Commands-Tests',
	#package : 'Calypso-Commands-Tests'
}

{ #category : 'accessing' }
ClyMockRegisteredSecondCommand >> name [

	^ 'Second'
]

{ #category : 'accessing' }
ClyMockRegisteredSecondCommand >> order [

	^ 500
]
