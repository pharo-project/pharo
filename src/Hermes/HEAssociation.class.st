"
I am simple representation of an association.
I store both values, the key and the value.
The key and the value are transformed as HEExportedLiterals.
When writing or reading, the key is before of the value.
"
Class {
	#name : #HEAssociation,
	#superclass : #HEExportedLiteral,
	#instVars : [
		'key'
	],
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HEAssociation class >> tag [
	^ 12
]

{ #category : #converting }
HEAssociation >> asLiteralIn: env [
	^ (key asLiteralIn: env) -> (value asLiteralIn: env)
]

{ #category : #reading }
HEAssociation >> doReadFrom: aReader [
	key := HEExportedLiteral readFrom: aReader.
	value := HEExportedLiteral readFrom: aReader.

]

{ #category : #accessing }
HEAssociation >> value: anAssociation [
	value := anAssociation value asExportedLiteral.
	key := anAssociation key asExportedLiteral.
]
