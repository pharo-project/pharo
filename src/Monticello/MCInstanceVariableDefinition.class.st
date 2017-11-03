"
A MCInstanceVariableDefinition represents an instance variable definition.
"
Class {
	#name : #MCInstanceVariableDefinition,
	#superclass : #MCVariableDefinition,
	#category : #Monticello-Modeling
}

{ #category : #accessing }
MCInstanceVariableDefinition class >> type [
	^ #instance
]

{ #category : #testing }
MCInstanceVariableDefinition >> isInstanceVariable [
	^ true
]
