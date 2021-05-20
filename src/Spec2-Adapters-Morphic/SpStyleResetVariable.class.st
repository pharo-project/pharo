"
A variable to reset previous values. 
This variable has to be used when user want to override the value of a parent property and replace it for the default value.

## Example

`Geometry { #height: Reset }`
"
Class {
	#name : #SpStyleResetVariable,
	#superclass : #SpStyleAbstractVariable,
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #'ston-core' }
SpStyleResetVariable class >> stonName [

	^ 'Reset'
]

{ #category : #'instance creation' }
SpStyleResetVariable >> fromSton: stonReader [
]

{ #category : #evaluating }
SpStyleResetVariable >> preferredValueWith: anObject [

	^ nil
]

{ #category : #evaluating }
SpStyleResetVariable >> value [

	^ nil
]
