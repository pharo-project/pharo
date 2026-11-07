"
I am a concrete test command used to exercise ClyCommandGroupBuilder.
"
Class {
	#name : 'ClyMockFirstCommand',
	#superclass : 'ClyMockBaseCommand',
	#category : 'Calypso-Commands-Tests',
	#package : 'Calypso-Commands-Tests'
}

{ #category : 'executing' }
ClyMockFirstCommand >> execute [

	^ self
]

{ #category : 'accessing' }
ClyMockFirstCommand >> order [

	^ 20
]
