Class {
	#name : #HETraitTransformation,
	#superclass : #HEArray,
	#instVars : [
		'subject'
	],
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HETraitTransformation class >> tag [
	^ 23
]

{ #category : #reading }
HETraitTransformation >> doReadFrom: aReader [
	super doReadFrom: aReader.
	subject := HEExportedLiteral readFrom: aReader.
]

{ #category : #accessing }
HETraitTransformation >> value: aTraitTransformation [
	subject := aTraitTransformation subject asExportedLiteral.
]
