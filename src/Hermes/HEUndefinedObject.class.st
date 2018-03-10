"
I represent nil as a literal.
I am only stored as my tag. I have no content.
"
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
