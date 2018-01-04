Class {
	#name : #HEUndefinedObject,
	#superclass : #HEExportedLiteral,
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HEUndefinedObject class >> tag [
	^ 13
]

{ #category : #converting }
HEUndefinedObject >> asLiteralIn: env [
	^ nil
]

{ #category : #reading }
HEUndefinedObject >> doReadFrom: aReader [
]
