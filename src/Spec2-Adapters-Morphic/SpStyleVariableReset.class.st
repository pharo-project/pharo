"
A variable to reset previous values. 
This variable has to be used when user want to override the value of a parent property and replace it for the default value.

## Example

`Geometry { #height: Reset }`
"
Class {
	#name : #SpStyleVariableReset,
	#superclass : #SpStyleAbstractVariable,
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #'ston-core' }
SpStyleVariableReset class >> stonName [

	^ 'Reset'
]

{ #category : #'instance creation' }
SpStyleVariableReset >> fromSton: stonReader [
]

{ #category : #evaluating }
SpStyleVariableReset >> preferredValueWith: anObject [

	^ nil
]

{ #category : #evaluating }
SpStyleVariableReset >> value [

	^ nil
]
