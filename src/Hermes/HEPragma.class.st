"
I represent a Pragma.
I am stored as a methodLiteral, a keyword and an array of  arguments.
The literals are exported as HEAbstractLiteral.

The method is a HEMethodLiteral
The keyword is a HESymbol
and the arguments si a HEArray
"
Class {
	#name : #HEPragma,
	#superclass : #HEExportedLiteral,
	#instVars : [
		'method',
		'arguments',
		'keyword'
	],
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HEPragma class >> tag [
	^ 11.
]

{ #category : #accessing }
HEPragma >> arguments [
	^ arguments
]

{ #category : #converting }
HEPragma >> asLiteralIn: env [
	^ Pragma for: (method asLiteralIn: env) selector: (keyword asLiteralIn: env) arguments: (arguments asLiteralIn: env)
]

{ #category : #reading }
HEPragma >> doReadFrom: aReader [
	method := HEExportedLiteral readFrom: aReader.
	keyword := HEExportedLiteral readFrom: aReader.
	arguments := HEExportedLiteral readFrom: aReader.
]

{ #category : #accessing }
HEPragma >> keyword [
	^ keyword
]

{ #category : #accessing }
HEPragma >> method [
	^ method
]

{ #category : #accessing }
HEPragma >> value: aPragma [
	method := aPragma method asExportedLiteral.
	keyword := aPragma selector asExportedLiteral.
	arguments := aPragma arguments asExportedLiteral 
]
