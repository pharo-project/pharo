"
Report exception as a property (critique) which can be debuged afterwards
"
Class {
	#name : #ReLogExceptionStrategy,
	#superclass : #ReExceptionStrategy,
	#category : #Renraku-Utility
}

{ #category : #'error handling' }
ReLogExceptionStrategy class >> handle: anException about: anEntity forProppertiesDo: aBlock [

	aBlock value: (
		ReExceptionProperty
			for: anEntity
			with: anException)
	
	
]

{ #category : #accessing }
ReLogExceptionStrategy class >> strategyDescription [

	^ 'Report exception as a property (critique) which can be debuged afterwards.'
]

{ #category : #accessing }
ReLogExceptionStrategy class >> strategyTitle [

	^ 'Log Exception'
]
