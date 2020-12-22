"
An abstract variable to be used for style properties (values will be expressed in instance of `SpStyleAbstractVariable` children instead its _atomic_ values.
"
Class {
	#name : #SpStyleAbstractVariable,
	#superclass : #Object,
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #converting }
SpStyleAbstractVariable >> asStyleVariable [

	^ self
]

{ #category : #testing }
SpStyleAbstractVariable >> isEnvironmentVariable [

	^ false
]

{ #category : #evaluating }
SpStyleAbstractVariable >> preferredValueWith: anObject [

	^ self value
]

{ #category : #evaluating }
SpStyleAbstractVariable >> value [

	^ self subclassResponsibility
]
