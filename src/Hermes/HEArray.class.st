"
I represent an instance of an Array
My value instance variable has an array in it. 
Each of the values of the original array is transformed in a HEExportedLiteral. 
When writing to the stream the array is written in the following way:

1. The size of the array is written as a 32bits integer.
2. Each of the literals is written.

This allows us to export any kind of array, if it is limited to a size of 32 bits integer.

The reading is perform in the inverse order
"
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
	"A new array is returned with all its elements as target literals in the environment"
	^ value collect: [ :e | e asLiteralIn: env ]
]

{ #category : #reading }
HEArray >> doReadFrom: aReader [
	| size | 
	"The size of the array is stored as a 32bits integer. Before any of the elements"
	size := aReader readInt32.
	value := Array new:size.
	
	1 to:size do:[:idx | value at:idx put: (HEExportedLiteral readFrom: aReader)].
]

{ #category : #accessing }
HEArray >> value: anArray [
	value := anArray collect:#asExportedLiteral
]
