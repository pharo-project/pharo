Class {
	#name : #HETrait,
	#superclass : #HEMethodContainer,
	#instVars : [
		'traitName'
	],
	#category : #Hermes-Model
}

{ #category : #'instance creation' }
HETrait class >> for: aTrait [
	^ self new
		fromTrait: aTrait;
		yourself
]

{ #category : #reading }
HETrait >> doReadHeaderFrom: aReader [
	traitName := aReader readByteSymbol.
]

{ #category : #writing }
HETrait >> doWriteHeaderInto: aWriter [
	aWriter writeByteString: traitName
]

{ #category : #accessing }
HETrait >> fromTrait: aTrait [
	traitName := aTrait name.

	self doFromMethodContainer: aTrait
]

{ #category : #accessing }
HETrait >> traitName [
	^ traitName
]

{ #category : #accessing }
HETrait >> traitName: anObject [
	traitName := anObject
]
