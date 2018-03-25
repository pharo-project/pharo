"
I represent a Global variable.
I am stored as a symbol.

When serialized I get the name of the variable.
When deserialized I find the global variable in the target environment.
I use the class being created to look up the binding. If there is no binding I create an undeclared. 
"
Class {
	#name : #HEGlobalVariable,
	#superclass : #HESymbol,
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HEGlobalVariable class >> tag [
	^ 7.
]

{ #category : #converting }
HEGlobalVariable >> asLiteralIn: env [
	| bind undec |
	bind := env newClass bindingOf: value.
	
	bind ifNotNil: [ ^ bind ].
	
	"If the Undeclared exists I should use the existing."
	Undeclared associationAt: value ifPresent: [ :e | ^e  ].
	
	undec := UndeclaredVariable key: value value: nil.
	Undeclared add: undec.
	
	^ undec.
]

{ #category : #accessing }
HEGlobalVariable >> value: aClassVariable [
	value := aClassVariable name.
]
