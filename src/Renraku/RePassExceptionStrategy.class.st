"
Pass the exception on, allowing handlers up the stach to deal with it, or open the debugger if there are no handlers.
"
Class {
	#name : #RePassExceptionStrategy,
	#superclass : #ReExceptionStrategy,
	#category : #'Renraku-Utility'
}

{ #category : #'error handing' }
RePassExceptionStrategy class >> handle: anException about: anEntity forProppertiesDo: aBlock [

	anException pass
]

{ #category : #accessing }
RePassExceptionStrategy class >> strategyDescription [

	^ 'Pass the exception on, allowing handlers up the stach to deal with it, or open the debugger if there are no handlers.'
]

{ #category : #accessing }
RePassExceptionStrategy class >> strategyTitle [

	^ 'Pass Exception'
]
