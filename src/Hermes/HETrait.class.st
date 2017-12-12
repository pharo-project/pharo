Class {
	#name : #HETrait,
	#superclass : #HEMethodContainer,
	#instVars : [
		'traitName'
	],
	#category : #'Hermes-Model'
}

{ #category : #reading }
HETrait >> doReadHeaderFrom: aReader [
	traitName := aReader readByteSymbol.
]

{ #category : #accessing }
HETrait >> isTrait [
	^ true
]

{ #category : #accessing }
HETrait >> traitName [
	^ traitName
]

{ #category : #accessing }
HETrait >> traitName: anObject [
	traitName := anObject
]
