"
I represent a ClassVariable.
I am stored as a symbol.

When serialized I get the name of the class variable
When deserialized I look up the class variable binding in the class that is being deserialized.
"
Class {
	#name : #HEClassVariable,
	#superclass : #HESymbol,
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HEClassVariable class >> tag [
	^ 6.
]

{ #category : #converting }
HEClassVariable >> asLiteralIn: env [
	"I ask the binding to the class that is currently being deserialized"
	^ env newClass bindingOf: value 
]

{ #category : #accessing }
HEClassVariable >> value: aClassVariable [
	value := aClassVariable name.
]
