Class {
	#name : #HEArray,
	#superclass : #HEExportedLiteral,
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HEArray class >> tag [
	^ 2.
]

{ #category : #converting }
HEArray >> asLiteralIn: env [
	^ value collect: [ :e | e asLiteralIn: env ]
]

{ #category : #reading }
HEArray >> doReadFrom: aReader [
	| size | 
	size := aReader readInt32.
	value := Array new:size.
	
	1 to:size do:[:idx | value at:idx put: (HEExportedLiteral readFrom: aReader)].
]

{ #category : #accessing }
HEArray >> value: anArray [
	value := anArray collect:#asExportedLiteral
]
