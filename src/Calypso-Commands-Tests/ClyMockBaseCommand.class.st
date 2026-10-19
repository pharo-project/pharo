"
I am an abstract command used to exercise ClyCommandGroupBuilder in tests.
"
Class {
	#name : 'ClyMockBaseCommand',
	#superclass : 'CmCommand',
	#category : 'Calypso-Commands-Tests',
	#package : 'Calypso-Commands-Tests'
}

{ #category : 'accessing' }
ClyMockBaseCommand class >> defaultName [

	^ self name asString
]

{ #category : 'executing' }
ClyMockBaseCommand >> execute [

	^ self subclassResponsibility
]
