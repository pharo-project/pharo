"
Ignore the exception silently and continue the execution
"
Class {
	#name : #ReIgnoreExceptionStrategy,
	#superclass : #ReExceptionStrategy,
	#category : #Renraku-Utility
}

{ #category : #'error handling' }
ReIgnoreExceptionStrategy class >> handle: anException about: anEntity forProppertiesDo: aBlock [

	"do nothing"
]

{ #category : #accessing }
ReIgnoreExceptionStrategy class >> strategyDescription [

	^ 'Ignore the exception silently and continue the execution'
]

{ #category : #accessing }
ReIgnoreExceptionStrategy class >> strategyTitle [

	^ 'Ignore Exception'
]
