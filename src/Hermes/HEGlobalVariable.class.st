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
	
	undec := UndeclaredVariable key: value value: nil.
	Undeclared add: undec.
	
	^ undec.
]

{ #category : #accessing }
HEGlobalVariable >> value: aClassVariable [
	value := aClassVariable name.
]
