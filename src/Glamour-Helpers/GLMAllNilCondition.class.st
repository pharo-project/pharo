"
Used to test if all defined arguments of a condition are nil.

"
Class {
	#name : #GLMAllNilCondition,
	#superclass : #GLMCondition,
	#category : #Glamour-Helpers
}

{ #category : #glamour }
GLMAllNilCondition >> glamourValueWithArgs: anArray [ 
	 
	^anArray allSatisfy: [:each | each isNil]
]
