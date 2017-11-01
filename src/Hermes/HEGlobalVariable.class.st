Class {
	#name : #HEGlobalVariable,
	#superclass : #HESymbol,
	#category : 'Hermes-Model'
}

{ #category : #accessing }
HEGlobalVariable class >> tag [
	^ 7.
]

{ #category : #converting }
HEGlobalVariable >> asLiteralIn: env [
	^ env newClass bindingOf: value
]

{ #category : #accessing }
HEGlobalVariable >> value: aClassVariable [
	value := aClassVariable name.
]
