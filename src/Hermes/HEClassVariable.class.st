Class {
	#name : #HEClassVariable,
	#superclass : #HESymbol,
	#category : 'Hermes-Model'
}

{ #category : #accessing }
HEClassVariable class >> tag [
	^ 6.
]

{ #category : #converting }
HEClassVariable >> asLiteralIn: env [
	^ env newClass bindingOf: value 
]

{ #category : #accessing }
HEClassVariable >> value: aClassVariable [
	value := aClassVariable name.
]
