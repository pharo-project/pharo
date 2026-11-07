"
I am a concrete test command used to exercise ClyCommandGroupBuilder.
"
Class {
	#name : 'ClyMockSecondCommand',
	#superclass : 'ClyMockBaseCommand',
	#category : 'Calypso-Commands-Tests',
	#package : 'Calypso-Commands-Tests'
}

{ #category : 'executing' }
ClyMockSecondCommand >> execute [

	^ self
]

{ #category : 'accessing' }
ClyMockSecondCommand >> order [

	^ 10
]
