"
Used to test if at least a defined  arguments of a condition is not nil.

"
Class {
	#name : #GLMSomeNotNilCondition,
	#superclass : #GLMCondition,
	#category : #Glamour-Helpers
}

{ #category : #glamour }
GLMSomeNotNilCondition >> glamourValueWithArgs: anArray [ 
	 
	^anArray anySatisfy: [:each | each notNil]
]
