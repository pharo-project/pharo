"
I implement the custom behavior for exporting a trait.
My header only includes the name of the trait.
Mostly of my behavior is from HEMethodContainer.
"
Class {
	#name : 'HETrait',
	#superclass : 'HEMethodContainer',
	#instVars : [
		'traitName'
	],
	#category : 'Hermes-Model',
	#package : 'Hermes',
	#tag : 'Model'
}

{ #category : 'reading' }
HETrait >> doReadHeaderFrom: aReader [
	traitName := aReader readByteSymbol
]

{ #category : 'accessing' }
HETrait >> isTrait [
	^ true
]

{ #category : 'accessing' }
HETrait >> name [

	^ self traitName
]

{ #category : 'printing' }
HETrait >> printOn: aStream [

	aStream
		nextPutAll: 'HETrait ( ';
		print: traitName;
		nextPutAll: ' )'
]

{ #category : 'accessing' }
HETrait >> traitName [
	^ traitName
]

{ #category : 'accessing' }
HETrait >> traitName: anObject [
	traitName := anObject
]
